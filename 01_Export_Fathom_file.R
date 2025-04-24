
### helper functions to convert BOF Catch/tagging master sheet into Fathom Connect file for batch uploading Animal Data
### This will ONLY work with the BOF Tagging_Catch_Tissue_MASTERFILE.xlsx file!!!


library(readxl)
library(openxlsx)
library(tidyverse)
source("Conversion_helper_functions.R")



CATCH <- readxl::read_excel("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/03_DATA/Tagging_DATA_SHEETS/DATASHEETS_Review/Tagging_Catch_Tissue_MASTERFILE.xlsx", sheet = "Elasmo_CATCH_Master",
                            col_types = "text")

str(CATCH)
head(CATCH)


# Fix date and time
CATCH2 <- CATCH |> 
  dplyr::mutate(
    Date = as.Date(as.numeric(Date), origin = "1899-12-30"),  # Excel numeric to Date
    Catch_Time = case_when(
      !is.na(Catch_Time) & grepl("^[0-9.]+$", Catch_Time) ~
        format(as.POSIXct(as.numeric(Catch_Time) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"),
      TRUE ~ Catch_Time),
    Release_Time = case_when(
        !is.na(Release_Time) & grepl("^[0-9.]+$", Release_Time) ~
          format(as.POSIXct(as.numeric(Release_Time) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M"),
        TRUE ~ Release_Time
    )
  )

str(CATCH2)





# extract metadata from Study ---------------------------------------------



# nwi_fathom <- convert_to_fathom(data = CATCH2, region_filter = "North_West_Island", species_filter = NULL, date_start = "2022-01-01", date_end = "2022-12-31")

nwi_fathom <- convert_to_fathom(data = CATCH2, 
                                region_filter = "North_West_Island", 
                                species_filter = NULL, 
                                date_start = NULL, 
                                date_end = NULL)


glimpse(nwi_fathom)


# export as Fathom Connect compatible xlxs

writexl::write_xlsx(nwi_fathom, path = "NWI_fathom_export.xlsx")

