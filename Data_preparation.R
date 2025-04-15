### compiling ISS data from eBird with OSS and ACSS data from naturecounts


# species list from the last analysis - useful to filter down to the species likely to provide sufficient info to estimate trends
sps1 = read.csv("data/species_list.csv")
sps = unique(sps1$Species)


### extracting the ISS, OSS, and ACSS data from eBird


### only need running once
library(auk)
library(archive)
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
  filter(!state %in% c("Alaska","Hawaii",
                       "Northwest Territories"))

iss_samp <- iss_samp %>% 
  filter(!state %in% c("Alaska","Hawaii",
                       "Northwest Territories"))

# fitting to strata - geographic overlay ----------------------------------
map <- read_sf(dsn = "data",
               layer ="region_polygons")

st_crs(map)

# iss_sites = unique(iss_samp[,c("locality","locality_id",
#                         "latitude",
#                         "longitude")])


iss_obs <- unique(iss_samp[,c("locality","locality_id",
                              "country_code","state_code",
                              "latitude",
                              "longitude")])

iss_obs = st_as_sf(iss_obs,coords = c("longitude","latitude"), crs = 4326)



# Checking for very close sites that have different names -----------------
iss_obs_dist <- iss_obs %>% 
  st_transform(crs = 4087) # WGS 84 World Equidistant Cylindrical

iss_distmat <- as.matrix(units::drop_units(sf::st_distance(iss_obs_dist)))

for(j in 1:nrow(iss_obs)){
  js <- which(iss_distmat[j,] < 200)
  iss_obs[j,"n_near"] <- length(js)
  iss_obs[j,"locality_id_LT_220m"] <- paste(unname(unlist(st_drop_geometry(iss_obs[js,"locality_id"]))),collapse = "-")
  iss_obs[j,"locality_LT_220m"] <- paste(unname(unlist(st_drop_geometry(iss_obs[js,"locality"]))),collapse = "-")
}





iss_obs_regs <- st_join(iss_obs, map, join = st_nearest_feature)

iss_regs_j <- iss_obs_regs %>% 
  as.data.frame() %>% 
  select(locality,locality_id,Region,Region_FR,n_near,locality_id_LT_220m,locality_LT_220m) %>% 
  distinct()



 iss_samp <- iss_samp %>% 
   select(checklist_id,
          country,
          country_code,
          state,
          state_code,
          bcr_code,
          locality,
          locality_id,
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
   distinct()

 length(unique(iss_samp$checklist_id))
 

  
# Zero-fill manual --------------------------------------------------------
iss_s2 <- expand_grid(iss_samp,sps) #making complete list of checklists and species
iss_s2 <- mutate(iss_s2,common_name = sps)

# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}


# clean up variables and further filtering to 1974 -
iss_m <- iss %>% 
  select(checklist_id,common_name,observation_count) %>% 
  filter(observation_count != "X") %>% 
  full_join(.,iss_s2) %>% 
  mutate(common_name = ifelse(!is.na(common_name), common_name,sps),
         observation_count = as.integer(observation_count),
         observation_count = ifelse(!is.na(observation_count), observation_count,0),
         # convert time to decimal hours since midnight
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
  filter(.,year > 1973 & year < 2025) %>% 
  inner_join(.,iss_regs_j) %>% 
  arrange(observation_date,checklist_id,locality_id)



# Check for surveys with no birds -----------------------------------------

n_sp <- iss_m %>% 
  group_by(checklist_id) %>% 
  summarise(n_sp = n(),
            sum_count = sum(observation_count),
            .groups = "drop") %>% 
  group_by(sum_count) %>% 
  summarise(n_checklists = n())


### effort info in ISS is only common in the last ~10 years.
wEMin = which(!is.na(iss_m$duration_minutes))
wEArea = which(!is.na(iss_m$effort_area_ha))
wEdist = which(!is.na(iss_m$effort_distance_km))

wEffort = unique(c(wEMin,wEArea,wEdist))
eff_y = table(iss_m[wEffort,"year"])
all_y = table(iss_m$year)
plot(eff_y/all_y)
eff_y/all_y
# 1974      1975      1976      1977      1978      1979      1980      1981      1982      1983      1984      1985 
# 0.4188034 0.4412752 0.3415162 0.2337221 0.2905103 0.2585416 0.3300330 0.3435805 0.3095578 0.2559955 0.3068966 0.3361470 
# 1986      1987      1988      1989      1990      1991      1992      1993      1994      1995      1996      1997 
# 0.3109850 0.3214062 0.3064085 0.4472882 0.2912683 0.2485323 0.2618182 0.3253542 0.3085686 0.4301018 0.3348534 0.3226758 
# 1998      1999      2000      2001      2002      2003      2004      2005      2006      2007      2008      2009 
# 0.3284133 0.3166360 0.3834489 0.3850475 0.3961010 0.5032614 0.3950942 0.4113322 0.8941849 0.8581560 0.8751234 0.8972858 
# 2010      2011      2012      2013      2014      2015      2016      2017      2018      2019      2020      2021 
# 0.8965922 0.9891892 0.9971231 0.9973133 0.9978881 0.9970603 0.9995112 0.9945504 1.0000000 1.0000000 1.0000000 1.0000000   

all_y  # what happened in 2006?
# 1974  1975  1976  1977  1978  1979  1980  1981  1982  1983  1984  1985  1986  1987  1988  1989  1990  1991  1992  1993 
# 13104 33376 38780 57624 62552 54908 50904 46452 58884 50204 48720 56392 54292 44604 41944 45948 43932 42924 38500 57316 
# 1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004  2005  2006  2007  2008  2009  2010  2011  2012  2013 
# 56532 52276 42980 56924 53116 60928 64624 64792 63196 55804 63924 64736 29372 43428 56728 52612 47656 46620 48664 52108 
# 2014  2015  2016  2017  2018  2019  2020  2021 
# 53032 57148 57288 61656 46592 52612 67928 49252 

n_country_y <- iss_m %>% 
  select(program,year,checklist_id) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(program,year) %>% 
  summarise(n = n(),
            .groups = "drop") %>% 
  arrange(year)

tmpp <- ggplot(data = n_country_y,
               aes(x = year, y = n,colour = program))+
  geom_line(alpha = 0.3)+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(subtitle = "Number of ISS-protocol checklists in eBird, during fall (July 01 - November 30)")+
  ylab("Number of checklists in eBird")+
  scale_y_continuous(limits = c(0,NA))

pdf("Number of unique checklist_ids from ISS protocols by program and year.pdf",
    width = 7,
    height = 6)
print(tmpp)
dev.off()


# Not Approved ------------------------------------------------------------

not_appr <- iss %>% filter(approved == FALSE)

# ISS data to bind --------------------------------------------------------


write_csv(iss,"full_iss_data.csv")


ssData <- iss_m[,c("checklist_id",
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

#renaming to match AKN headers
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


save(list = c("ssData",
              "sps",
              "map"),file = "data/allShorebirdPrismFallCounts.RData")







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











