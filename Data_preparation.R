
# species list from the last analysis - useful to filter down to the species likely to provide sufficient info to estimate trends
sps1 = read.csv("data/species_list.csv")
sps = unique(sps1$Species)


### extracting the ISS, OSS, and ACSS data from eBird


### only need running once
library(R.utils)
library(tidyverse)


# unzipping the downloaded EBD datsets for Canada and US ------------------


# archive_extract("data/ebd_CA_smp_relDec-2024.tar",dir = "data/eBird")
#  archive_extract("data/ebd_US_smp_relDec-2024.tar",dir = "data/eBird")
# 
#   gunzip("data/eBird/ebd_CA_relDec-2024.txt.gz")
#  gunzip("data/eBird/ebd_US_relDec-2024.txt.gz")
#  gunzip("data/eBird/ebd_CA_relDec-2024_sampling.txt.gz")
#  gunzip("data/eBird/ebd_US_relDec-2024_sampling.txt.gz")

  


# Filtering to ISS-protocol and fall surveys ------------------------------

re_filter_ebird_full_data <- FALSE

if(re_filter_ebird_full_data){
# 
# 
  library(auk)
  library(archive)
  
iss <- NULL
iss_samp <- NULL
# 
for(rr in c("CA","US")){
  data_dir <- paste0("data/eBird")
  auk::auk_set_ebd_path(data_dir,overwrite = T)
  out_sampling <- file.path(data_dir, paste0("ebd_",rr,"_ISS_sampling.txt"))
  out_ebd <- file.path(data_dir, paste0("ebd_",rr,"_ISS.txt"))
  #
# ebd_filters <- auk_ebd(file = paste0("data/eBird/ebd_",rr,"_relDec-2024.txt"),
#                        file_sampling = paste0("data/eBird/ebd_",rr,"_relDec-2024_sampling.txt"),
#                        sep = "\t") %>%
#   auk_species(species = sps) %>%
#   auk_date(date = c("*-07-01", "*-11-30")) %>% # fall surveys only
#   auk_protocol(protocol = c("International Shorebird Survey (ISS)")) %>% # restrict to the ISS protocol
#   #auk_unique(checklists_only = TRUE) %>% 
#     auk_filter(file = out_ebd,
#                file_sampling = out_sampling,
#                overwrite = TRUE)




iss_tmp1 <- read_ebd(out_ebd)

iss_tmp <- auk_unique(iss_tmp1, checklists_only = TRUE)


iss_samp_tmp <- read_sampling(out_sampling)

iss <- bind_rows(iss,iss_tmp)
iss_samp <- bind_rows(iss_samp,iss_samp_tmp)

}

save(list = c("iss","iss_samp"),
     file = "data/all_iss_eBird.RData")
}
# 
# 
# ebd_filters <- ebd %>% 
#    
#   # # southeastern coastal plain bcr
#   auk_country(country = c("US","CA")) %>% 
#   
#   
#   
#    
# 


# post filtering ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)

load("data/all_iss_eBird.RData")

iss <- iss %>% 
  mutate(observation_count = as.integer(observation_count)) %>% 
  filter(!is.na(observation_count))

# replace ebird-ACSS data with local copy ---------------------------------
# The ACSS data in ebird are missing many of the highest counts
# These have presumably been flagged or removed by eBird reviewers
# Untill this error gets corrected, replacing all ACSS data with a
# local copy of the full database, supplied by Sarah Neima at Canadian
# Wildlife Service. Shared via one-drive in email April 15, 2025
#


acss <- readxl::read_xlsx("data/acss/ACSS data 1971-2023_15-10-2024.xlsx",
                          col_types = c(rep("guess",16),
                                        rep("date",2),
                                        rep("numeric",7),
                                        "text")) %>% 
  filter(MM > 06 & MM < 12) %>% 
  rename(checklist_id = `Survey code`,  #renaming and mutating to match columns in eBird data
         common_name = Species,
         observation_count = OBcount,
         latitude = LATdec,
         longitude = LONGdec,
         time_observations_started = `Time Start`,
         time_observations_ended = `Time End`,
         locality = `SurveySite`,
         locality_id = `Site code`) %>% 
  mutate(observation_count = as.integer(observation_count),
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
         duration_minutes = as.numeric((time_observations_ended-time_observations_started))/60,
         time_observations_started = as.character(time_observations_started)) %>% 
  select(checklist_id,locality,locality_id,
         observation_date,common_name,observation_count,
         latitude,longitude,time_observations_started,
         country_code,country,state_code,observer_id,protocol_type,duration_minutes) %>% 
  filter(!is.na(observation_count))



acss_sites <- read_csv("first_draft_coord_name_province_by_sitecode.csv")

acss <- acss %>% 
  select(-c(locality,state_code,latitude,longitude)) %>% 
  inner_join(acss_sites, by = "locality_id")

acss_samp <- acss %>% 
  select(checklist_id,locality_id,
         observation_date,
         time_observations_started,
         country_code,country,
         state_code,
         observer_id,
         protocol_type,
         latitude,
         longitude
         ) %>% 
  distinct()
  
acss <- acss %>% 
  select(-c(locality_id,
            observation_date,
            time_observations_started,
            country_code,country,
            state_code,
            observer_id,
            protocol_type,
            latitude,
            longitude))
  

# drop ACSS data that may be in eBird -------------------------------------
iss <- iss %>% 
  filter(!state %in% c("Alaska","Hawaii",
                       "Northwest Territories",
                       "New Brunswick",  # removing all atlantic province data to replace with ACSS
                       "Nova Scotia",
                       "Prince Edward Island",
                       "Newfoundland and Labrador"))%>% 
  select(-c(locality_id,
            observation_date,
            time_observations_started,
            country_code,country,
            state_code,
            observer_id,
            protocol_type,
            latitude,
            longitude))


# replace with local ACSS data --------------------------------------------
iss <- iss %>% 
  bind_rows(acss)



# Combine the sampling event data ACSS with eBird -------------------------

iss_samp <- iss_samp %>% 
  filter(!state %in% c("Alaska","Hawaii",
                       "Northwest Territories",
                       "New Brunswick",   # removing all atlantic province data to replace with ACSS
                       "Nova Scotia",
                       "Prince Edward Island",
                       "Newfoundland and Labrador"))


iss_samp <- iss_samp %>% 
  bind_rows(acss_samp)


# Adding coarse region information- geographic overlay ----------------------------------
map <- read_sf(dsn = "data",
               layer ="region_polygons")

st_crs(map)

iss_sites <- unique(iss_samp[,c("locality_id",
                                "latitude",
                              "longitude")])

iss_sites = st_as_sf(iss_sites,coords = c("longitude","latitude"), crs = 4326)

regions <- sf::st_read(dsn = "data", layer = "region_polygons")

iss_sites_regs <- iss_sites %>% 
  st_join(regions, join = st_nearest_feature) %>% 
  st_drop_geometry()


# replace location information in event data with regions -----------------
iss_samp <- iss_samp %>% 
   select(checklist_id,
          country,
          country_code,
          state,
          state_code,
          locality_id,
          locality,
          locality_type,
          latitude,
          longitude,
          observation_date,
          time_observations_started,
          #observer_id,
          #sampling_event_identifier,
          protocol_code,
          protocol_type,
          project_code,
          duration_minutes,
          effort_distance_km,
          effort_area_ha,
          number_observers) %>% 
   left_join(iss_sites_regs)

 # function to convert time observation to hours since midnight
 time_to_decimal <- function(x) {
   x <- hms(x, quiet = TRUE)
   hour(x) + minute(x) / 60 + second(x) / 3600
 }
 
 
 # clean up variables and further filtering to 1980 - 2024
 iss_samp <- iss_samp %>% 
   mutate(# convert time to decimal hours since midnight
          time_observations_started = time_to_decimal(time_observations_started),
          # split date into year and day of year
          year = year(observation_date),
          day_of_year = yday(observation_date),
          program = ifelse(country_code == "US","ISS",NA),
          program = ifelse(state_code == "CA-ON","OSS",program),
          program = ifelse(state_code %in% c("CA-NS",
                                             "CA-PE",
                                             "CA-NB",
                                             "CA-NL"),"ACSS",program),
          program = ifelse(is.na(program),"Canada_other",program)) %>% 
   filter(.,year > 1979 & year < 2025) %>% 
   arrange(observation_date,checklist_id,locality_id)
 
 
  
# complete the zero-filling -
issj <- iss %>% 
  select(checklist_id,common_name,observation_count) %>% 
  distinct()

dbls <- issj %>% 
  group_by(checklist_id,common_name) %>% 
  summarise(n_obs = n()) %>% 
  filter(n_obs > 1) %>% 
  select(checklist_id) %>% 
  distinct()

issj <- issj %>% 
  filter(!checklist_id %in% dbls$checklist_id)

iss_samp <- iss_samp %>% 
  filter(!checklist_id %in% dbls$checklist_id)

# Zero-fill manual --------------------------------------------------------
iss_zero_fill <- expand_grid(iss_samp,sps) #making complete list of checklists and species
iss_zero_fill <- mutate(iss_zero_fill,common_name = sps)



iss_w_zero <- iss_zero_fill %>% 
  left_join(.,issj) %>% 
  mutate(common_name = ifelse(!is.na(common_name), common_name,sps),
         observation_count = as.integer(observation_count),
         observation_count = ifelse(!is.na(observation_count), observation_count,0)) %>% 
  arrange(observation_date,checklist_id,locality_id)


# 
# # Check for surveys with no birds -----------------------------------------
# 
# n_sp <- iss_m %>% 
#   group_by(checklist_id) %>% 
#   summarise(n_sp = n(),
#             sum_count = sum(observation_count),
#             .groups = "drop") %>% 
#   group_by(sum_count) %>% 
#   summarise(n_checklists = n())
# 
# 
# ### effort info in ISS is only common in the last ~10 years.
# wEMin = which(!is.na(iss_m$duration_minutes))
# wEArea = which(!is.na(iss_m$effort_area_ha))
# wEdist = which(!is.na(iss_m$effort_distance_km))
# 
# wEffort = unique(c(wEMin,wEArea,wEdist))
# eff_y = table(iss_m[wEffort,"year"])
# all_y = table(iss_m$year)
# plot(eff_y/all_y)
# eff_y/all_y
# # 1974      1975      1976      1977      1978      1979      1980      1981      1982      1983      1984      1985 
# # 0.4188034 0.4412752 0.3415162 0.2337221 0.2905103 0.2585416 0.3300330 0.3435805 0.3095578 0.2559955 0.3068966 0.3361470 
# # 1986      1987      1988      1989      1990      1991      1992      1993      1994      1995      1996      1997 
# # 0.3109850 0.3214062 0.3064085 0.4472882 0.2912683 0.2485323 0.2618182 0.3253542 0.3085686 0.4301018 0.3348534 0.3226758 
# # 1998      1999      2000      2001      2002      2003      2004      2005      2006      2007      2008      2009 
# # 0.3284133 0.3166360 0.3834489 0.3850475 0.3961010 0.5032614 0.3950942 0.4113322 0.8941849 0.8581560 0.8751234 0.8972858 
# # 2010      2011      2012      2013      2014      2015      2016      2017      2018      2019      2020      2021 
# # 0.8965922 0.9891892 0.9971231 0.9973133 0.9978881 0.9970603 0.9995112 0.9945504 1.0000000 1.0000000 1.0000000 1.0000000   
# 
# all_y  # what happened in 2006?
# # 1974  1975  1976  1977  1978  1979  1980  1981  1982  1983  1984  1985  1986  1987  1988  1989  1990  1991  1992  1993 
# # 13104 33376 38780 57624 62552 54908 50904 46452 58884 50204 48720 56392 54292 44604 41944 45948 43932 42924 38500 57316 
# # 1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004  2005  2006  2007  2008  2009  2010  2011  2012  2013 
# # 56532 52276 42980 56924 53116 60928 64624 64792 63196 55804 63924 64736 29372 43428 56728 52612 47656 46620 48664 52108 
# # 2014  2015  2016  2017  2018  2019  2020  2021 
# # 53032 57148 57288 61656 46592 52612 67928 49252 
# 
# n_country_y <- iss_m %>% 
#   select(program,year,checklist_id) %>% 
#   distinct() %>% 
#   ungroup() %>% 
#   group_by(program,year) %>% 
#   summarise(n = n(),
#             .groups = "drop") %>% 
#   arrange(year)
# 
# tmpp <- ggplot(data = n_country_y,
#                aes(x = year, y = n,colour = program))+
#   geom_line(alpha = 0.3)+
#   geom_point()+
#   geom_hline(yintercept = 0)+
#   labs(subtitle = "Number of ISS-protocol checklists in eBird, during fall (July 01 - November 30)")+
#   ylab("Number of checklists in eBird")+
#   scale_y_continuous(limits = c(0,NA))
# 
# pdf("Number of unique checklist_ids from ISS protocols by program and year.pdf",
#     width = 7,
#     height = 6)
# print(tmpp)
# dev.off()
# 
# 
# # Not Approved ------------------------------------------------------------
# 
# not_appr <- iss %>% filter(approved == FALSE)

# ISS data to bind --------------------------------------------------------


write_csv(iss_w_zero,"full_iss_data.csv")


ssData <- iss_w_zero[,c("checklist_id",
                   "common_name",
                   "observation_count",
                   "country",
                   "state_code",
                   "locality",
                   "locality_id",
                   "latitude",
                   "longitude",
                   "Region",
                   "year",
                   "day_of_year")]

#renaming to match AKN headers and following code from previous analyses
ssData <- rename(ssData,
                   SamplingEventIdentifier = checklist_id,
                   CommonName = common_name,
                   ObservationCount = observation_count,
                   Country = country,
                   StateProvince = state_code,
                   SiteName = locality,
                   SurveyAreaIdentifier = locality_id,
                   DecimalLatitude = latitude,
                   DecimalLongitude = longitude,
                   YearCollected = year,
                   doy = day_of_year)




  

# adding national divisions to regions ------------------------------------


ssData[which(ssData$Region == "East Inland" &
               ssData$StateProvince == "CA-ON"),"Region"] <- "Ontario"
ssData[which(ssData$Region == "Northeast Coastal" &
               ssData$Country == "Canada"),"Region"] <- "Atlantic Canada"
ssData[which(ssData$Region == "Northeast Coastal" &
               ssData$Country == "United States"),"Region"] <- "Northeast US Coastal"

table(ssData$Country,ssData$Region)



# Connecting sites to hexagon grid ----------------------------------------


poly_grid <- readRDS("hexagon_grid.rds")
laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area

  all_sites = ssData %>% distinct(SurveyAreaIdentifier,DecimalLatitude,DecimalLongitude)
  iss_sites = st_as_sf(all_sites,coords = c("DecimalLongitude","DecimalLatitude"), crs = 4326) #wgs84
  iss_sites_laea <- st_transform(iss_sites, laea)
  saveRDS(iss_sites_laea, "Data/site_map.rds")
  
  iss_sites_laea <- st_join(iss_sites_laea, poly_grid, join = st_nearest_feature)
  
  strats <- iss_sites_laea
  st_geometry(strats) <- NULL
  ssData <- left_join(ssData,strats,by = "SurveyAreaIdentifier")

  saveRDS(ssData,"Data/ssData.rds")
  saveRDS(sps,"Data/sps_list.rds")
  saveRDS(map,"Data/regional_map.rds")
  







# ADD a table to describe the sampling events at each site


# ADD a table to describe the sampling events at each site


# ADD a table to describe the sampling events at each site


# ADD a table to describe the sampling events at each site


# ADD a table to describe the sampling events at each site


# ADD a table to describe the sampling events at each site


# ADD a table to describe the sampling events at each site

library(tidyverse)
library(ggforce)
load("data/allShorebirdPrismFallCounts.RData")

events <- ssData %>% select(-c(CommonName,ObservationCount)) %>% 
  distinct() %>% 
  group_by(Country,StateProvince,SurveyAreaIdentifier,Region,YearCollected,SiteName) %>% 
  summarise(n_surveys = n()) %>% 
  ungroup() %>% 
  group_by(Region)


 ev_split <- group_split(events)
 ev_key <- group_keys(events)
 dd = 5 #dimension of plots below
 
 for(i in 1:nrow(ev_key)){
   tmp = ev_split[[i]]
   nm = as.character(ev_key[i,"Region"])
   ns <- length(unique(tmp$SurveyAreaIdentifier))
   np <- floor(ns/dd^2)
   nrem = ns-np*(dd^2)
   yup = as.numeric(quantile(tmp$n_surveys,0.95))
   if(nrem >= dd){np = np+1}
   pdf(paste0("Figures/",nm,"N_counts_by_year.pdf"),
       width = 11,
       height = 8.5)
   for(j in 1:np){
   ns_y <- ggplot(data = tmp,aes(x = YearCollected,y = n_surveys))+
     geom_col(aes(fill = StateProvince))+
     theme_minimal()+
     ylab("Number of Surveys Conducted")+
     coord_cartesian(ylim = c(0,yup),xlim = c(1974,2019))+
     theme(legend.position = "top")+
     facet_wrap_paginate(~SurveyAreaIdentifier,ncol = dd,nrow = dd,
                         page = j, scales = "fixed")
   print(ns_y)
   } 
     
   dev.off()
   
 }











