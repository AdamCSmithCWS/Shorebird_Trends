
library(readxl)
library(tidyverse)

acss <- readxl::read_xlsx("data/acss/ACSS data 1971-2023_15-10-2024.xlsx",
                          col_types = c(rep("guess",16),
                                        rep("date",2),
                                        rep("numeric",7),
                                        "text")) %>% 
  rename(checklist_id = `Survey code`,
         common_name = Species,
         observation_count = OBcount,
         latitude = LATdec,
         longitude = LONGdec,
         observation_date = Date,
         time_observations_started = `Time Start`,
         time_observations_ended = `Time End`,
         locality = `SurveySite`,
         locality_id = `Site code`) %>% 
  mutate(country_code = "CA",
         country = "Canada",
         state_code = paste0("CA-",Province),
         observer_id = paste0("OB_",as.integer(factor(`Primary surveyor`))),
         protocol_type = "International Shorebird Survey (ISS)",
         duration_minutes = as.numeric((time_observations_ended-time_observations_started))/60) %>% 
  select(checklist_id,locality,locality_id,
         observation_date,common_name,observation_count,
         latitude,longitude,time_observations_started,
         country_code,country,state_code,observer_id,protocol_type,duration_minutes)


explore_duplicates <- function(df,
                               x = "",
                               y = ""){
  df1 <- df %>% 
    select(matches(c(x,y))) %>% 
    distinct() %>% 
    arrange(.data[[x]]) 
  
  df2 <- df1 %>% 
    group_by(.data[[x]]) %>% 
    summarise(n_duplicates = n()-1) %>% 
    arrange(-n_duplicates)
  
  retlist <- list(unique_combinations = df1,# this table shows the unique combinations of x and y
                  number_of_duplicates = df2) 
  # the second table shows the number of duplicates of x for every unique values of y
  # n_duplicates is 0 for any value of x that has only one unique value of y
  # n_duplicates is >0 for any value of x that has >1 unique value.
  return(retlist)
}


acss_unique_locality_id_province <- acss %>% 
  select(locality_id,
         state_code) %>% 
  distinct() %>% 
  arrange(locality_id) 
# counting the number of unique province values for each Site code
n_locality_id_gt1_prov <- acss_unique_locality_id_province %>% 
  group_by(locality_id) %>% 
  summarise(n_provinces = n()) %>% 
  arrange(-n_provinces)

paste("There are ",length(which(n_locality_id_gt1_prov$n_provinces > 1)),
      "Site codes with greater than 1 province assigned")

# this table shows the unique combinations of Site code (locality_id) and SurveySite (locality)
acss_unique_locality_locality_id <- acss %>% 
  select(locality,
         locality_id) %>% 
  distinct() %>% 
  arrange(locality_id)

n_locality_id_gt1_locality <- acss_unique_locality_locality_id %>% 
  group_by(locality_id) %>% 
  summarise(n_locality = n()) %>% 
  arrange(-n_locality)

paste("There are ",length(which(n_locality_id_gt1_locality$n_locality > 1)),
      "Site codes with greater than 1 SurveySite assigned")

# this table shows the unique combinations of Site code (locality_id) and coordinates 
acss_unique_locality_id_coord <- acss %>% 
  select(locality_id,
         latitude,longitude) %>% 
  distinct() %>% 
  arrange(locality_id)


n_locality_id_gt1_coord <- acss_unique_locality_id_coord %>% 
  group_by(locality_id) %>% 
  summarise(n_coords = n()) %>% 
  arrange(-n_coords)

paste("There are ",length(which(n_locality_id_gt1_coord$n_coords > 1)),
      "Site codes with greater than 1 set of coordinates")
