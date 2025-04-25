
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

byprov <- explore_duplicates(acss,
                             x = "locality_id",
                             y = "state_code")
acss_unique_locality_id_province <- byprov$unique_combinations
# counting the number of unique province values for each Site code
n_locality_id_gt1_prov <- byprov$number_of_duplicates

paste("There are ",length(which(n_locality_id_gt1_prov$n_duplicates > 0)),
      "Site codes with greater than 1 province assigned")

# this table shows the unique combinations of Site code (locality_id) and SurveySite (locality)
bylocality <- explore_duplicates(acss,
                             x = "locality_id",
                             y = "locality")
acss_unique_locality_id_locality <- bylocality$unique_combinations
n_locality_id_gt1_locality <- bylocality$number_of_duplicates

paste("There are ",length(which(n_locality_id_gt1_locality$n_duplicates > 0)),
      "Site codes with greater than 1 SurveySite assigned")

# this table shows the unique combinations of Site code (locality_id) and coordinates 


bycoord <- explore_duplicates(acss,
                                 x = "locality_id",
                                 y = c("latitude","longitude"))
acss_unique_locality_id_coord <- bycoord$unique_combinations
n_locality_id_gt1_coord <- bycoord$number_of_duplicates


paste("There are ",length(which(n_locality_id_gt1_coord$n_duplicates > 0)),
      "Site codes with greater than 1 set of coordinates")
