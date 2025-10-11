#' Export receiver deployments to Fathom format (UTC) and return the tibble
#'
#' @description
#' This function is specifically designed to convert receiver deployments from the Biopixel Oceans Foundation receiver deployments master #' spreadsheet into a Fathom Central accepted format for batch uploading receivger deployments and recoveries, 
#' 
#' Filters a receiver deployment table by code/region/installation, computes
#' deployment/recovery datetimes in **UTC**, and outputs a compact table ready for Fathom Central. 
# 'The result is written to an Excel file and also (optionally)
#' assigned into the global environment for inspection.
#'
#' @details
#' **Time handling**
#' * `Time` / `Rec. Time` may be Excel **fractions of a day** (e.g., `0.375`),
#'   or whole hours (e.g., `14`). Missing times default to `default_hour_local`
#'   (local time).
#' * Local timezone is modeled as a **fixed offset** east of UTC using
#'   `utc_zone` (e.g., `10` for UTC+10). UTC is computed as:
#'   `UTC = local_time - hours(utc_zone)`.
#'
#' **Excel date origin**
#' * `Rec.Date` is often an Excel serial date. The function auto-detects whether
#'   the data uses the Windows (1900) or legacy Mac (1904) system by testing
#'   plausibility; override with `excel_origin` if you already know it.
#'
#' **Required columns (case-sensitive)**
#' `Code`, `Region`, `Date deployed`, `Time`, `Rec.Date`, `Rec. Time`,
#' `Lat_DD`, `Lon_DD`, `Receiver ID`, `Depth`, and one of
#' `IMOS_Installation` / `IMOS Installation` / `Installation` (if filtering by installation).
#'
#' @param data A data frame (tibble) of receiver deployments.
#' @param code Optional, `NULL` or character. Partial, case-insensitive match on `Code`.
#' @param region Optional, `NULL` or character. Partial, case-insensitive match on `Region`.
#' @param installation Optional, `NULL` or character. Partial, case-insensitive match
#'   against any of `IMOS_Installation`, `IMOS Installation`, or `Installation` (if present).
#' @param utc_zone Numeric scalar. **Hours east of UTC** for the local time
#'   zone (e.g., `10` for UTC+10).
#' @param default_hour_local Integer(1). Local hour to use when a time is missing
#'   (default `10` → 10:00 local).
#' @param excel_origin `NULL` (auto-detect) or character, either `"1899-12-30"`
#'   (Windows/1900) or `"1904-01-01"` (Mac/1904).
#' @param out_path Directory to write the Excel file to. Will be created if missing.
#' @param out_file File name for the Excel output (e.g., `"Fathom_Receiver_Deployments.xlsx"`).
#' @param save_as Character name for assigning the result into `.GlobalEnv` (default `"fathom_recr_dep"`).
#'
#' @return A tibble with columns:
#' `Station`, `Deployment Start` (UTC, POSIXct), `Deployment End` (UTC, POSIXct),
#' `Latitude`, `Longitude`, `Device`, `Device Depth`.
#'
#' @examples
#' \dontrun{
#' fathom <- fathom_receiver_deployments(
#'   data = deploy,
#'   code = NULL,
#'   installation = "GMY seagrass",
#'   utc_zone = 10,
#'   out_path = "data/export",
#'   out_file = "Fathom_Deployment_Data_Sheet_GMY_2025.xlsx",
#'   save_as = "fathom_recr"
#' )
#' }
#'
#' @seealso [writexl::write_xlsx()], [lubridate::as_datetime()]
#' @export
fathom_receiver_deploy <- function(
    data,
    code = NULL,
    region = NULL,
    installation = NULL,
    utc_zone = 10,
    default_hour_local = 10,
    excel_origin = NULL,
    out_path = "data/export",
    out_file = "Fathom_Receiver_Deployments.xlsx",
    save_as = "fathom_recr_dep"
) {
  # 1) Basic filters (partial, case-insensitive)
  df <- data |>
    dplyr::filter(
      (if (!is.null(code))   stringr::str_detect(Code,   stringr::regex(code,   ignore_case = TRUE)) else TRUE) &
        (if (!is.null(region)) stringr::str_detect(Region, stringr::regex(region, ignore_case = TRUE)) else TRUE)
    )
  
  # Installation filter across plausible column names
  inst_cols <- base::intersect(base::names(df), c("IMOS_Installation", "IMOS Installation", "Installation"))
  if (!base::is.null(installation) && base::length(inst_cols) > 0) {
    df <- df |>
      dplyr::filter(
        dplyr::if_any(dplyr::all_of(inst_cols),
                      ~ stringr::str_detect(., stringr::regex(installation, ignore_case = TRUE)))
      )
  } else if (!base::is.null(installation) && base::length(inst_cols) == 0) {
    base::warning("`installation` was provided, but no installation column found in `data`.")
  }
  
  # 2) Excel origin auto-detect (for Rec.Date)
  chosen_origin <- excel_origin
  if (base::is.null(chosen_origin)) {
    rec_num <- suppressWarnings(base::as.numeric(df$`Rec.Date`))
    if (base::all(base::is.na(rec_num))) {
      chosen_origin <- "1899-12-30"
    } else {
      d1 <- base::as.Date(rec_num, origin = "1899-12-30")
      d2 <- base::as.Date(rec_num, origin = "1904-01-01")
      lo <- base::as.Date("1990-01-01"); hi <- base::as.Date("2100-01-01")
      ok1 <- base::sum(!base::is.na(d1) & d1 >= lo & d1 <= hi)
      ok2 <- base::sum(!base::is.na(d2) & d2 >= lo & d2 <= hi)
      chosen_origin <- if (ok2 > ok1) "1904-01-01" else "1899-12-30"
      base::message("Auto-detected Excel origin: ", chosen_origin)
    }
  }
  
  # 3) Build output
  res <- df |>
    dplyr::mutate(
      # --- Deployment time ---
      time_num  = suppressWarnings(base::as.numeric(Time)),
      time_frac = dplyr::case_when(
        base::is.na(time_num) ~ NA_real_,
        time_num > 1 ~ time_num / 24,  # treat "14" as 14 hours
        TRUE ~ time_num
      ),
      time_sec = dplyr::coalesce(time_frac * 86400, default_hour_local * 3600),
      
      # Construct UTC directly using fixed offset: UTC = local - offset
      dep_utc_base = lubridate::as_datetime(base::as.Date(`Date deployed`), tz = "UTC") -
        lubridate::hours(utc_zone),
      `Deployment Start` = dep_utc_base + lubridate::seconds(base::round(time_sec)),
      
      # --- Recovery time (from Excel serials) ---
      rec_date_num = suppressWarnings(base::as.numeric(`Rec.Date`)),
      rec_date     = base::as.Date(rec_date_num, origin = chosen_origin),
      
      R.time_num  = suppressWarnings(base::as.numeric(`Rec. Time`)),
      R.time_frac = dplyr::case_when(
        base::is.na(R.time_num) ~ NA_real_,
        R.time_num > 1 ~ R.time_num / 24,
        TRUE ~ R.time_num
      ),
      R.time_sec = dplyr::coalesce(R.time_frac * 86400, default_hour_local * 3600),
      
      rec_utc_base = lubridate::as_datetime(base::as.Date(rec_date), tz = "UTC") -
        lubridate::hours(utc_zone),
      `Deployment End` = rec_utc_base + lubridate::seconds(base::round(R.time_sec)),
      
      # Other fields
      Latitude       = base::as.numeric(Lat_DD),
      Longitude      = base::as.numeric(Lon_DD),
      Device         = base::as.numeric(`Receiver ID`),
      `Device Depth` = base::as.numeric(Depth) - 1
    ) |>
    dplyr::select(Station, `Deployment Start`, `Deployment End`, Latitude, Longitude, Device, `Device Depth`)
  
  # 4) Write Excel and expose object
  if (!base::dir.exists(out_path)) base::dir.create(out_path, recursive = TRUE)
  out_fp <- base::file.path(out_path, out_file)
  writexl::write_xlsx(res, out_fp)
  
  # CRAN policy note: assigning into .GlobalEnv is generally discouraged in packages.
  # Kept here to match requested behavior.
  base::assign(save_as, res, envir = .GlobalEnv)
  
  res
}
