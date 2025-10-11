
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

