
## Export Fathom Receiver Deployments using BOF receiver deployment master sheet



library(readxl)
library(openxlsx)
library(tidyverse)
source("R/fathom_animals.R")
source("R/fathom_transmitter_deployments.R")
source("R/fathom_receiver_deploy.R")




# Import BOF master file  -------------------------------------------------



deploy <-  readxl::read_xlsx("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/03_DATA/Acosutic_data/ReceiverSwapsR/Receiver_Deployments_IMOS_Working-File_NEW_2023_SHARED.xlsx", sheet = "Receiver Deployment & Recovery", skip = 1) |>
  dplyr::mutate(Date_deployed = dplyr::if_else(!is.na(`Date deployed`), lubridate::ymd(`Date deployed`), NA)) 


str(deploy)




# Exporting Receiver Deployment data  -------------------------------------



GMY <- fathom_receiver_deploy(
  data = deploy,
  code = NULL,     
  region = NULL,         
  installation = "GMY seagrass",  
  utc_zone = 10,
  out_path = "data/export",
  out_file = "Fathom_Rcr_Deployment_Data_Sheet_GMY_2026.xlsx",
  save_as = "GMY_recr"
)



scp <- fathom_receiver_deploy(
    data = deploy,
    code = "Shark Control",
    region = NULL,
    responsible_org = "BOF",
    recovered_only = FALSE,
    installation = NULL,
    utc_zone = 10,
    default_hour_local = 10,
    out_path = "data/export",
    out_file = "Fathom_Rcvr_Deployment_Data_Sheet_SCP_2026.xlsx",
    save_as = "fathom_recr_dep"
  )

scp


