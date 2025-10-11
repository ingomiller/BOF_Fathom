
## Export Fathom Receiver Deployments using BOF receiver deployment master sheet


library(tidyverse)



# Import BOF master file  -------------------------------------------------



deploy <-  readxl::read_xlsx("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/03_DATA/Acosutic_data/ReceiverSwapsR/Receiver_Deployments_IMOS_Working-File_NEW_2023_SHARED.xlsx", sheet = "Receiver Deployment & Recovery", skip = 1) |>
  dplyr::mutate(Date_deployed = dplyr::if_else(!is.na(`Date deployed`), lubridate::ymd(`Date deployed`), NA)) 


str(deploy)



# Pre-Filter settings ---------------------------------------------------------


code = "GMY Seagrass"
region = "Carins"
installation = "GMY seagrass"



# Conversion --------------------------------------------------------------


GMY <- deploy |> 
  dplyr::filter(Code == code) |> 
  dplyr::mutate(
    
    ### Deployment
    # Parse time as fraction-of-day (Excel-style) or hours
    time_num  = suppressWarnings(as.numeric(Time)),
    time_frac = dplyr::case_when(
      is.na(time_num) ~ NA_real_,
      time_num > 1    ~ time_num / 24,   # treat "14" as 14 hours
      TRUE            ~ time_num
    ),
    time_sec = time_frac * 86400,
    
    # Default 10:00 (10 hours * 3600 seconds) when time is missing
    time_sec = dplyr::coalesce(time_sec, 10 * 3600),
    
    # Build local datetime (Australia/Brisbane)
    start_local = lubridate::as_datetime(as.Date(`Date deployed`), tz = "Australia/Brisbane") +
      lubridate::seconds(round(time_sec)),
    
    # Convert to UTC
    `Deployment Start` = lubridate::with_tz(start_local, "UTC"),
    
    ### Recovery
    
    # Parse time as fraction-of-day (Excel-style) or hours
    
    rec_date_num = suppressWarnings(as.numeric(`Rec.Date`)),
    # Excel (Windows) date system origin:
    rec_date     = as.Date(rec_date_num, origin = "1899-12-30"),
    
    
    R.time_num  = suppressWarnings(as.numeric(`Rec. Time`)),
    R.time_frac = dplyr::case_when(
      is.na(R.time_num) ~ NA_real_,
      R.time_num > 1    ~ R.time_num / 24,   # treat "14" as 14 hours
      TRUE            ~ R.time_num
    ),
    R.time_sec = R.time_frac * 86400,
    
    # Default 10:00 (10 hours * 3600 seconds) when time is missing
    R.time_sec = dplyr::coalesce(R.time_sec, 10 * 3600),
    
    # Build local datetime (Australia/Brisbane)
    end_local = lubridate::as_datetime(as.Date(rec_date), tz = "Australia/Brisbane") +
      lubridate::seconds(round(R.time_sec)),
    
    # Convert to UTC
    `Deployment End` = lubridate::with_tz(end_local, "UTC"),
    
    Latitude = as.numeric(Lat_DD),
    Longitude = as.numeric(Lon_DD),
    Device = as.numeric(`Receiver ID`),
    `Device Depth` = as.numeric(Depth) - 1
    
  
  ) |>
  dplyr::select(Station, `Deployment Start`, `Deployment End`, Latitude, Longitude, Device, `Device Depth`)



# Export ------------------------------------------------------------------

writexl::write_xlsx(GMY, "data/export/Fathom_Deployment_Data_Sheet_GMY_2025.xlsx")






fathom_receiver_deployments <- function(
    data,
    code = NULL,
    region = NULL,
    installation = NULL,      
    utc_zone = 10,     
    default_hour_local = 10,   # default local hour when time missing
    excel_origin = NULL,       # NULL = auto-detect; or "1899-12-30" / "1904-01-01"
    out_path = "data/export",
    out_file = "Fathom_Receiver_Deployments.xlsx",
    save_as = "fathom_recr_dep"
) {
  # 1) Basic filtering first (allows origin auto-detection on relevant rows)
  df <- data |>
    dplyr::filter(
      (if (!is.null(code)) stringr::str_detect(Code, stringr::regex(code, ignore_case = TRUE)) else TRUE) &
        (if (!is.null(region)) stringr::str_detect(Region, stringr::regex(region, ignore_case = TRUE)) else TRUE)
    )
  
  # installation filter against plausible column names
  inst_cols <- base::intersect(base::names(df), c("IMOS_Installation", "IMOS Installation", "Installation"))
  if (!base::is.null(installation) && base::length(inst_cols) > 0) {
    df <- df |>
      dplyr::filter(
        dplyr::if_any(dplyr::all_of(inst_cols),
                      ~ stringr::str_detect(., stringr::regex(installation, ignore_case = TRUE)))
      )
  } else if (!base::is.null(installation) && base::length(inst_cols) == 0) {
    base::warning("`installation` was provided, but no installation column found in data.")
  }
  
  # 2) Decide Excel origin for Rec.Date (auto-detect if needed)
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
      Latitude      = base::as.numeric(Lat_DD),
      Longitude     = base::as.numeric(Lon_DD),
      Device        = base::as.numeric(`Receiver ID`),
      `Device Depth` = base::as.numeric(Depth) - 1
    ) |>
    dplyr::select(Station, `Deployment Start`, `Deployment End`, Latitude, Longitude, Device, `Device Depth`)
  
  # 4) Write Excel and expose object
  if (!base::dir.exists(out_path)) base::dir.create(out_path, recursive = TRUE)
  out_fp <- base::file.path(out_path, out_file)
  writexl::write_xlsx(res, out_fp)
  
  base::assign(save_as, res, envir = .GlobalEnv)
  res
}



GMY <- fathom_receiver_deployments(
  data = deploy,
  code = "GMY Seagrass",     
  region = NULL,         
  installation = "GMY seagrass",  
  utc_zone = 10,
  out_path = "test/export",
  out_file = "Fathom_Deployment_Data_Sheet_GMY_2025.xlsx",
  save_as = "GMY_recr"
)




