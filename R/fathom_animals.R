#' Convert CATCH tagging metadata to Fathom Animals template (clean, export, return)
#'
#' @description
#' Cleans Excel-style **Date/Catch_Time/Release_Time** (serial dates or day-fractions),
#' transforms to a Fathom-compatible Animals table, converts times from
#' `"Australia/Brisbane"` to **UTC**, writes an Excel file, and returns the tibble.
#'
#' @param data A tibble/data.frame of CATCH metadata.
#' @param region_filter Optional exact match on `Region`.
#' @param species_filter Optional partial/regex match (case-insensitive) over
#'   `Common_Name` and `Scientific_Name`.
#' @param project_filter Optional partial/regex match (case-insensitive) across
#'   `Project` and/or `Subproject` (errors if both missing).
#' @param date_start,date_end Optional `"YYYY-MM-DD"` bounds applied to `Date`.
#' @param out_path Directory for Excel output (created if missing).
#' @param out_file File name for Excel output.
#'
#' @return Tibble formatted for Fathom Animals (also written to Excel).
#' @export
fathom_animals <- function(data,
                           region_filter = NULL,
                           project_filter = NULL,
                           species_filter = NULL,
                           date_start = NULL,
                           date_end   = NULL,
                           out_path   = "data/export",
                           out_file   = "Fathom_Animals.xlsx",
                           export_transmitter = TRUE) {
  
  # --- Clean Date/Times (robust & type-safe) ---
  data_clean <- data |>
    dplyr::mutate(
      Date_num       = suppressWarnings(base::as.numeric(Date)),
      Date_from_num  = base::as.Date(Date_num, origin = "1899-12-30"),
      Date_parsed    = suppressWarnings(lubridate::as_date(Date)),
      Date           = dplyr::if_else(!base::is.na(Date_num), Date_from_num, Date_parsed),
      Catch_Time = dplyr::case_when(
        !base::is.na(Catch_Time) & stringr::str_detect(Catch_Time, "^[0-9.]+$") ~
          base::format(base::as.POSIXct(base::as.numeric(Catch_Time) * 86400,
                                        origin = "1970-01-01", tz = "UTC"), "%H:%M"),
        TRUE ~ Catch_Time
      ),
      Release_Time = dplyr::case_when(
        !base::is.na(Release_Time) & stringr::str_detect(Release_Time, "^[0-9.]+$") ~
          base::format(base::as.POSIXct(base::as.numeric(Release_Time) * 86400,
                                        origin = "1970-01-01", tz = "UTC"), "%H:%M"),
        TRUE ~ Release_Time
      )
    ) |>
    dplyr::select(-Date_num, -Date_from_num, -Date_parsed)
  
  # Validate project cols if filter used
  proj_cols <- base::intersect(base::names(data_clean), c("Project", "Subproject"))
  if (!base::is.null(project_filter) && base::length(proj_cols) == 0) {
    base::stop("Project filter requested, but neither 'Project' nor 'Subproject' columns exist in `data`.")
  }
  
  # --- Filter + build times ---
  animals <- data_clean |>
    dplyr::filter(Acoustic_Tag == TRUE) |>
    dplyr::filter(
      (if (!base::is.null(region_filter)) Region == region_filter else TRUE) &
        (if (!base::is.null(species_filter)) (
          stringr::str_detect(Common_Name,     stringr::regex(species_filter, ignore_case = TRUE)) |
            stringr::str_detect(Scientific_Name, stringr::regex(species_filter, ignore_case = TRUE))
        ) else TRUE) &
        (if (!base::is.null(project_filter)) (
          dplyr::if_any(dplyr::all_of(proj_cols),
                        ~ stringr::str_detect(., stringr::regex(project_filter, ignore_case = TRUE)))
        ) else TRUE) &
        (if (!base::is.null(date_start)) Date >= lubridate::ymd(date_start) else TRUE) &
        (if (!base::is.null(date_end))   Date <= lubridate::ymd(date_end)   else TRUE)
    ) |>
    dplyr::mutate(
      # Brisbane -> UTC
      `Tagging Time` = lubridate::with_tz(
        lubridate::ymd_hm(
          base::paste0(base::format(Date, "%Y-%m-%d"), " ",
                       dplyr::if_else(!base::is.na(Catch_Time), Catch_Time, "12:00")),
          tz = "Australia/Brisbane"
        ),
        tzone = "Etc/UTC"
      ),
      clean_release_time = dplyr::if_else(
        stringr::str_detect(Release_Time, "^\\d{2}:\\d{2}$"),
        Release_Time,
        NA_character_
      ),
      `Release Time` = dplyr::if_else(
        base::is.na(clean_release_time),
        `Tagging Time`,
        lubridate::with_tz(
          suppressWarnings(
            lubridate::ymd_hm(
              base::paste0(base::format(Date, "%Y-%m-%d"), " ", clean_release_time),
              tz = "Australia/Brisbane"
            )
          ),
          tzone = "Etc/UTC"
        )
      ),
      # Tag IDs: expand codespace + multiple ids separated by "/" and "_"
      `Tag ID` = purrr::map2_chr(Acoustic_Codespace, Acoustic_ID, function(codespace, id) {
        tag_groups  <- base::strsplit(id, "_")[[1]]
        tag_strings <- purrr::map_chr(tag_groups, function(tag) {
          ids <- base::strsplit(tag, "/")[[1]]
          base::paste(base::paste0(codespace, "-", ids), collapse = " ")
        })
        base::paste(tag_strings, collapse = " ")
      }),
      `Tagging Latitude`  = Lat_DD,
      `Tagging Longitude` = Lon_DD
    ) |>
    dplyr::transmute(
      Name = NA,
      `Species Common Name`     = Common_Name,
      `Species Scientific Name` = Scientific_Name,
      Origin = "WILD",
      Stock = NA,
      `Animal Notes` = NA,
      `Capture Time`  = `Tagging Time`,
      `Captured By`   = "BOF",
      `Capture Location`  = dplyr::if_else(!base::is.na(Location), Location, Region),
      `Capture Latitude`   = Lat_DD,
      `Capture Longitude`  = Lon_DD,
      `Capture Method`     = Catch_Method,
      `Capture Notes`      = NA,
      `Tagging Time`       = `Tagging Time`,
      `Tag ID`             = `Tag ID`,
      `Tagged By`          = Tagger,
      `Tagging Location`   = dplyr::if_else(!base::is.na(Location), Location, Region),
      `Tagging Latitude`   = `Tagging Latitude`,
      `Tagging Longitude`  = `Tagging Longitude`,
      `Tagging Method`     = dplyr::if_else(Acoustic_Location == "Internal", "SURGERY", "EXTERNAL"),
      `Anatomic Location`  = "Middle",
      `Tagging Notes`      = NA,
      Anaesthetic = NA,
      Sedative    = NA,
      Buffer      = NA,
      `PIT Tag`   = NA,
      `FLOY Tag`  = Ext_Tag,
      `Release Time`       = `Release Time`,
      `Released By`        = Tagger,
      `Release Location`   = dplyr::if_else(!base::is.na(Location), Location, Region),
      `Release Latitude`   = Lat_DD,
      `Release Longitude`  = Lon_DD,
      `Release Notes`      = Comments,
      `Measurement Time` = NA,
      `Measured By`     = Tagger,
      Sex = Sex,
      `Life Stage` = NA,
      Age = NA,
      `Age Unit` = NA,
      Mass = NA,
      `Mass Unit` = NA,
      `Total Length` = TL_cm,
      `Total Length Unit` = "cm",
      `Fork Length` = FL_cm,
      `Fork Length Unit` = "cm",
      `Standard Length` = PC_cm,
      `Standard Length Unit` = "cm",
      `Hood Length` = NA,
      `Hood Length Unit` = NA,
      Width = NA,
      `Width Unit` = "cm",
      Girth = NA,
      `Girth Unit` = "cm"
    )
  
  # Ensure out dir, write Animals
  if (!base::dir.exists(out_path)) base::dir.create(out_path, recursive = TRUE)
  writexl::write_xlsx(animals, base::file.path(out_path, out_file))
  
  # Optionally write Transmitter Deployments with suffix "_transmitter_deployments.xlsx"
  if (isTRUE(export_transmitter)) {
    base_name <- base::sub("\\.xlsx$", "", out_file, ignore.case = TRUE)
    tx_file   <- paste0(base_name, "_transmitter_deployments.xlsx")
    
    transmitters <- fathom_transmitter_deployments(animals)
    writexl::write_xlsx(transmitters, base::file.path(out_path, tx_file))
  }
  
  animals
}