


## helper functions to convert BOF Catch/tagging master sheet into Fathom Connect file for batch uploading Animal Data

convert_to_fathom <- function(data, region_filter = NULL) {
  data |>
    dplyr::filter(Acoustic_Tag == TRUE) |>
    # Apply region filter if provided
    dplyr::filter(if (!is.null(region_filter)) Region == region_filter else TRUE) |>
    
    dplyr::mutate(
      capture_time_calc = lubridate::with_tz(
        lubridate::ymd_hm(
          paste0(
            format(Date, "%Y-%m-%d"),
            " ",
            dplyr::if_else(!is.na(Catch_Time), Catch_Time, "12:00")
          ),
          tz = "Australia/Brisbane"
        ),
        tzone = "Etc/UTC"
      ),
      
      clean_release_time = dplyr::if_else(
        stringr::str_detect(Release_Time, "^\\d{2}:\\d{2}$"),
        Release_Time,
        NA_character_
      ),
      
      
      release_time_calc = dplyr::if_else(
        is.na(clean_release_time),
        capture_time_calc,
        lubridate::with_tz(
          suppressWarnings(
            lubridate::ymd_hm(
              paste0(format(Date, "%Y-%m-%d"), " ", clean_release_time),
              tz = "Australia/Brisbane"
            )
          ),
          tzone = "Etc/UTC"
        )
      ),
      
      tag_id_calc = purrr::map2_chr(Acoustic_Codespace, Acoustic_ID, function(codespace, id) {
        tag_groups <- strsplit(id, "_")[[1]]
        tag_strings <- purrr::map_chr(tag_groups, function(tag) {
          ids <- strsplit(tag, "/")[[1]]
          paste(paste0(codespace, "-", ids), collapse = " ")
        })
        paste(tag_strings, collapse = " ")
      })
    ) |>
    
    dplyr::transmute(
      Name = NA,                      
      `Species Common Name` = Common_Name,     
      `Species Scientific Name` = Scientific_Name,
      Origin = "WILD",                  
      Stock = NA,                     
      `Animal Notes` = NA,          
      `Capture Time` = capture_time_calc,
      `Captured By` = "BOF",            
      `Capture Location` = dplyr::if_else(!is.na(Location), Location, Region),     
      `Capture Latitude` = Lat_DD,       
      `Capture Longitude` = Lon_DD,       
      `Capture Method` = Catch_Method,          
      `Capture Notes` = NA,           
      `Tagging Time` = capture_time_calc,            
      `Tag ID` = tag_id_calc,                
      `Tagged By` = Tagger,              
      `Tagging Location` = dplyr::if_else(!is.na(Location), Location, Region),        
      `Tagging Latitude` = Lat_DD,       
      `Tagging Longitude` = Lon_DD,      
      `Tagging Method` = dplyr::if_else(Acoustic_Location == "Internal", "SURGERY", "EXTERNAL"),        
      `Anatomic Location` = "Middle",       
      `Tagging Notes` = NA,         
      Anaesthetic = NA,              
      Sedative = NA,                  
      Buffer = NA,                   
      `PIT Tag` = NA,                 
      `FLOY Tag` = Ext_Tag,              
      `Release Time` = release_time_calc,
      `Released By` = Tagger,            
      `Release Location` = dplyr::if_else(!is.na(Location), Location, Region),       
      `Release Latitude` = Lat_DD,        
      `Release Longitude` = Lon_DD,       
      `Release Notes` = Comments,           
      `Measurement Time` = NA,        
      `Measured By` = Tagger,             
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
}
