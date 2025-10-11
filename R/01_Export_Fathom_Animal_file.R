
### helper functions to convert BOF Catch/tagging master sheet into Fathom Connect file for batch uploading Animal Data
### This will ONLY work with the BOF Tagging_Catch_Tissue_MASTERFILE.xlsx file!!!


library(readxl)
library(openxlsx)
library(tidyverse)
source("R/fathom_animals.R")
source("R/fathom_transmitter_deployments.R")



CATCH <- readxl::read_excel("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/03_DATA/Tagging_DATA_SHEETS/DATASHEETS_Review/Tagging_Catch_Tissue_MASTERFILE.xlsx", sheet = "Elasmo_CATCH_Master",
                            col_types = "text")

str(CATCH)
head(CATCH)




# extract metadata from Study ---------------------------------------------



nwi_fathom <- fathom_animals(data = CATCH, 
                             region_filter = "North_West_Island", 
                             project_filter = NULL,
                             species_filter = NULL, 
                             date_start = NULL, 
                             date_end = NULL,
                             out_path       = "test/export",
                             out_file       = "Fathom_Animals.xlsx",
                             export_transmitter = TRUE)




glimpse(nwi_fathom)




# GMY seagrass project ----------------------------------------------------


gmy_fathom <- fathom_animals(data = CATCH, 
                                region_filter = NULL, 
                                project_filter = "GMY_Cairns",
                                species_filter = NULL, 
                                date_start = NULL, 
                                date_end = NULL,
                             out_path       = "test/export",
                             out_file       = "Fathom_Animals.xlsx",
                             export_transmitter = TRUE)


head(gmy_fathom)






