
library(readxl)
library(tidyverse)
source("functions/explore_duplicates.R")

acss <- readxl::read_xlsx("data/acss/ACSS data 1971-2023_15-10-2024.xlsx",
                          col_types = c(rep("guess",16),
                                        rep("date",2),
                                        rep("numeric",7),
                                        "text")) %>% 
  rename(checklist_id = `Survey code`,  #renaming and mutating to match columns in eBird data
         common_name = Species,
         observation_count = OBcount,
         latitude = LATdec,
         longitude = LONGdec,
         time_observations_started = `Time Start`,
         time_observations_ended = `Time End`,
         locality = `SurveySite`,
         locality_id = `Site code`) %>% 
  mutate(survey_code = checklist_id,
         hours = hour(time_observations_started),
         mins = minute(time_observations_started),
         observation_date = dmy(paste(DD,MM,YYYY,sep = "-")),
         country_code = "CA",
         country = "Canada",
         state_code = paste0("CA-",Province),
         observer_id = paste0("OB_",as.integer(factor(`Primary surveyor`))),
         protocol_type = "International Shorebird Survey (ISS)",
         time_observations_started = hm(paste(hours,mins,sep = ":")),
         checklist_id = paste(sep = "_",checklist_id,locality_id,DD,MM,YYYY,hours,mins,observer_id),
         duration_minutes = as.numeric((time_observations_ended-time_observations_started))/60) %>% 
  select(checklist_id,survey_code,locality,locality_id,
         observation_date,common_name,observation_count,
         latitude,longitude,time_observations_started,
         country_code,country,state_code,observer_id,protocol_type,duration_minutes)


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


# Unique list of coordinates for each acss locality_id -------------------------

acss_unique_site_coord <- acss %>% 
  arrange(observation_date) %>% #sorting by observation date oldest obs first
  select(locality_id,locality,state_code,latitude,longitude) %>% 
  drop_na() %>% #dropping any rows where one of the selected columns is missing
  slice_tail(n = 1,by = locality_id) #selects the most recent set of coordinates that come up in the dataset

write_csv(acss_unique_site_coord,
        "first_draft_coord_name_province_by_sitecode.csv")



# Checklist ID ------------------------------------------------------------



bychecklist <- explore_duplicates(acss,
                             x = "checklist_id",
                             y = "observer_id")
unique_start_checklist <- bychecklist$unique_combinations
# counting the number of unique province values for each Site code
n_starts_checklist <- bychecklist$number_of_duplicates

tt2d <- n_starts_checklist %>% filter(n_duplicates > 0)

tt3 <- unique_start_checklist %>% 
  filter(checklist_id %in% tt2d$checklist_id)


paste("There are ",length(which(n_starts_checklist$n_duplicates > 0)),
      "Checklist_id codes with greater than locality_id assigned")




# this table shows the unique combinations of checklist and species 

acsstmp <- acss %>% 
  mutate(chk_sp = paste0(survey_code,common_name))

sp_checkl <- explore_duplicates(acsstmp,
                                x = "chk_sp",
                                y = c("observation_count"))

unique_spobs_checklist <- sp_checkl$unique_combinations
# counting the number of unique province values for each Site code
n_spobs_checklist <- sp_checkl$number_of_duplicates

tt2d <- n_spobs_checklist %>% filter(n_duplicates > 0)

tt3 <- unique_spobs_checklist %>% 
  filter(chk_sp %in% tt2d$chk_sp)


dbls <- acss %>% 
  group_by(survey_code,common_name) %>% 
  summarise(n_obs = n()) %>% 
  filter(n_obs > 1) %>% 
  select(survey_code) %>% 
  distinct()

dbls <- acss %>% 
  group_by(checklist_id,common_name) %>% 
  summarise(n_obs = n()) %>% 
  filter(n_obs > 1) %>% 
  select(checklist_id) %>% 
  distinct()


paste("There are ",length(which(n_starts_checklist$n_duplicates > 0)),
      "Checklist_id codes with greater than locality_id assigned")

