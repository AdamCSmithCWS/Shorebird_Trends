### compiling ISS data from eBird with OSS and ACSS data from naturecounts


# species list from the last analysis - useful to filter down to the species likely to provide sufficient info to estimate trends
sps1 = read.csv("data/species_list.csv")
sps = unique(sps1$Species)


### extracting the ISS, OSS, and ACSS data from eBird


### only need running once
# library(auk)
# library(archive)
#
# archive_extract("data/eBird/tar/ebd_sampling_relJul-2022.tar",dir = "data/eBird")
#  archive_extract("data/eBird/tar/ebd_US_relJul-2022.tar",dir = "data/eBird/US")
#  #untar("data/eBird/tar/ebd_CA_relJul-2022.tar",exdir = "data/eBird/CA")
#  archive_extract("data/eBird/tar/ebd_CA_relJul-2022.tar",dir = "data/eBird/CA")
#  gunzip("data/eBird/CA/ebd_CA_relJul-2022.txt.gz")
#  gunzip("data/eBird/US/ebd_US_relJul-2022.txt.gz")
#  gunzip("data/eBird/ebd_sampling_relJul-2022.txt.gz")
#  

# 
# iss <- NULL
# iss_samp <- NULL
# 
# for(rr in c("CA","US")){
#   data_dir <- paste0("data/eBird/",rr)
#   auk::auk_set_ebd_path(data_dir,overwrite = T)
#   out_sampling <- file.path(data_dir, paste0("ebd_",rr,"_ISS_sampling.txt"))
#   out_ebd <- file.path(data_dir, paste0("ebd_",rr,"_ISS.txt"))
#   #  
# ebd_filters <- auk_ebd(file = paste0("data/eBird/",rr,"/ebd_",rr,"_relJul-2022.txt"),
#                        file_sampling = "data/eBird/ebd_sampling_relJul-2022.txt",
#                        sep = "\t") %>% 
#   auk_species(species = sps) %>% 
#   auk_date(date = c("*-07-01", "*-11-30")) %>% # fall surveys only
#   auk_protocol(protocol = c("International Shorebird Survey (ISS)")) %>% # restrict to the ISS protocol
#     auk_filter(file = out_ebd, 
#                file_sampling = out_sampling,
#                overwrite = TRUE)
# 
# 
# 
# 
# iss_tmp = read_ebd(out_ebd)
# 
# iss_samp_tmp = read_sampling(out_sampling)
# 
# iss <- bind_rows(iss,iss_tmp)
# iss_samp <- bind_rows(iss_samp,iss_samp_tmp)
# 
# }
# 
# save(list = c("iss","iss_samp"),
#      file = "data/all_iss_eBird.RData")

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

# fitting to strata - geographic overlay ----------------------------------
map <- read_sf(dsn = "data",
               layer ="region_polygons")

st_crs(map)

iss_sites = unique(iss_samp[,c("locality","locality_id",
                        "latitude",
                        "longitude")])


iss_obs <- unique(iss[,c("locality","locality_id",
                              "latitude",
                              "longitude")])

iss_obs = st_as_sf(iss_obs,coords = c("longitude","latitude"), crs = 4326)




iss_obs_regs <- st_join(iss_obs, map, join = st_nearest_feature)

iss_samp <- inner_join(iss_samp,iss_obs_regs[,c("locality_id","locality","Region","Region_FR")])


# Zero-fill manual --------------------------------------------------------
iss_s2 <- expand_grid(iss_samp,sps) #making complete list of checklists and species
iss_s2 <- mutate(iss_s2,common_name = sps)

iss_m <- full_join(iss[,c("checklist_id","common_name","observation_count")],iss_s2) #merging species observations

iss_m[which(is.na(iss_m$observation_count)),"observation_count"] <- "0"


# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables and further filtering to 1974 -
iss_m1 <- iss_m %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date),
  ) %>% 
  filter(.,year > 1973 & year < 2022,
         !is.na(observation_count))



### effort info in ISS is only common in the last ~10 years.
wEMin = which(!is.na(iss_m1$duration_minutes))
wEArea = which(!is.na(iss_m1$effort_area_ha))
wEdist = which(!is.na(iss_m1$effort_distance_km))

wEffort = unique(c(wEMin,wEArea,wEdist))
eff_y = table(iss_m1[wEffort,"year"])
all_y = table(iss_m1$year)
plot(eff_y/all_y)
eff_y/all_y
# 1974      1975      1976      1977      1978      1979      1980      1981      1982      1983      1984      1985 
# 0.4394251 0.4340391 0.3358885 0.2286501 0.2830269 0.2528172 0.3234516 0.3350725 0.3022303 0.2301539 0.2956811 0.3312833 
# 1986      1987      1988      1989      1990      1991      1992      1993      1994      1995      1996      1997 
# 0.3014130 0.3135569 0.3017187 0.4323589 0.2840839 0.2420660 0.2463394 0.3162309 0.2969783 0.4118961 0.3082070 0.3062616 
# 1998      1999      2000      2001      2002      2003      2004      2005      2006      2007      2008      2009 
# 0.3097801 0.3006575 0.3651666 0.3534024 0.3552733 0.4585016 0.3594416 0.3854373 0.8984423 0.8244957 0.8785090 0.8987844 
# 2010      2011      2012      2013      2014      2015      2016      2017      2018      2019      2020      2021 
# 0.9005911 0.9885098 0.9943579 0.9886439 0.9964861 0.9957109 0.9982961 0.9949690 1.0000000 1.0000000 1.0000000 1.0000000  

all_y  # what happened in 2006?
# 1974   1975   1976   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986   1987   1988   1989   1990 
# 27272  68768  80360 121968 130984 114296 105784  96600 123032 115514 101136 117072 112948  92838  87976  97766  91128 
# 1991   1992   1993   1994   1995   1996   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006   2007 
# 89992  83592 121620 118608 111028  95358 121406 114804 132922 139728 144634 145488 124824 144424 141512  61758  95086 
# 2008   2009   2010   2011   2012   2013   2014   2015   2016   2017   2018   2019   2020   2021 
# 121688 112868 100836 102174 109180 118350 127494 130564 131464 134368 103376 116256 145496 110874 

n_country_y <- iss_samp %>% 
  group_by(country,year) %>% 
  summarise(n = n())



#iss_m1$locality_id #this is the unique site identifier


# ISS data to bind --------------------------------------------------------





iss_full <- iss_m1[,c("checklist_id",
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
iss_full <- rename(iss_full,
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


# ISS data have some sites with no bird observations in some years --------
## , despite multiple surveys and many observations in previous year
## e.g., L125009, L125100
site_yr_sum <- iss_full %>% group_by(YearCollected,SurveyAreaIdentifier) %>% 
  summarise(sum_all_sp = sum(ObservationCount),
            n_visits = n()/28)
write.csv(site_yr_sum,"site_yr_sum_count_ISS.csv")

site_yr_sum <-  filter(site_yr_sum,n_visits > 2 & sum_all_sp == 0)

### this identifies sites where they've had > 2 surveys conducted and no birds have been seen.
### this code drops these site*year combinations - there's something wrong with these data
for(j in 1:nrow(site_yr_sum)){
  ss = as.character(site_yr_sum[j,"SurveyAreaIdentifier"])
  yy = as.integer(site_yr_sum[j,"YearCollected"])
  tmp <- which(iss_full$SurveyAreaIdentifier == ss & iss_full$YearCollected == yy)
  if(j == 1){
    iss_drop = tmp
  }else{
    iss_drop = c(iss_drop,tmp)
  }
}
  # = tapply(iss_full$ObservationCount,iss_full[,c("YearCollected","SurveyAreaIdentifier")],sum,na.rm = T)
  
  
iss_full <- iss_full[-iss_drop,]
  
  
site_yr_sum <- iss_full %>% group_by(YearCollected,SurveyAreaIdentifier) %>% 
  summarise(sum_all_sp = sum(ObservationCount),
            n_visits = n()/28)
write.csv(site_yr_sum,"site_yr_sum_count_ISS_post.csv")

site_yr_sum <-  filter(site_yr_sum,n_visits > 2 & sum_all_sp == 0)

  
# Nature Counts data ------------------------------------------------------

install.packages("naturecounts", 
                 repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
                           CRAN = 'https://cloud.r-project.org'))

# OSS data ---------------------------------------------------------------

# oss <- read.delim("data/OSS_8July2020.txt",stringsAsFactors = F,nrows = 48006)
## not sure why, but the original file won't load properly - 
# Warning message:
#   In scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
#             EOF within quoted string
# after warning, it only loads ~6000 rows and even those rows are not properly loaded
# Solution was to open it in excel (tab delimited), save as csv, then import csv below        
oss <- read.csv("data/OSS_8July2020.csv",stringsAsFactors = F)



# ACSS data ---------------------------------------------------------------

acss <- read.delim("data/ACSS_8July2020.txt",stringsAsFactors = F)


acss <- acss[-which(acss$SamplingEventIdentifier == ""),]


# Filtering columns -------------------------------------------------------


#names(oss)
#c("GlobalUniqueIdentifier","DateLastModified","BasisOfRecord","InstitutionCode","CollectionCode","CatalogNumber","ScientificName","HigherTaxon","Kingdom","Phylum","Class","Order","Family","Genus","SpecificEpithet","InfraspecificRank","InfraspecificEpithet","ScientificNameAuthor","IdentificationQualifier","HigherGeography","Continent","WaterBody","IslandGroup","Island","Country","StateProvince","County","Locality","MinimumElevationInMeters","MaximumElevationInMeters","MinimumDepthInMeters","MaximumDepthInMeters","DecimalLatitude","DecimalLongitude","GeodeticDatum","CoordinateUncertaintyInMeters","YearCollected","MonthCollected","DayCollected","TimeCollected","JulianDay","Collector","Sex","LifeStage","ImageURL","RelatedInformation","CollectorNumber","FieldNumber","FieldNotes","OriginalCoordinatesSystem","LatLongComments","GeoreferenceMethod","GeoreferenceReferences","GeoreferenceVerificationStatus","Remarks","FootprintWKT","FootprintSRS","ProjectCode","ProtocolType","ProtocolCode","ProtocolSpeciesTargeted","ProtocolReference","ProtocolURL","SurveyAreaIdentifier","SurveyAreaSize","SurveyAreaPercentageCovered","SurveyAreaShape","SurveyAreaLongAxisLength","SurveyAreaShortAxisLength","SurveyAreaLongAxisOrientation","CoordinatesScope","SamplingEventIdentifier","SamplingEventStructure","RouteIdentifier","TimeObservationsStarted","TimeObservationsEnded","DurationInHours","TimeIntervalStarted","TimeIntervalEnded","TimeIntervalsAdditive","NumberOfObservers","EffortMeasurement1","EffortUnits1","EffortMeasurement2","EffortUnits2","EffortMeasurement3","EffortUnits3","EffortMeasurement4","EffortUnits4","EffortMeasurement5","EffortUnits5","EffortMeasurement6","EffortUnits6","EffortMeasurement7","EffortUnits7","EffortMeasurement8","EffortUnits8","EffortMeasurement9","EffortUnits9","EffortMeasurement10","EffortUnits10","EffortMeasurement11","EffortUnits11","EffortMeasurement12","EffortUnits12","EffortMeasurement13","EffortUnits13","EffortMeasurement14","EffortUnits14","EffortMeasurement15","EffortUnits15","EffortMeasurement16","EffortUnits16","EffortMeasurement17","EffortUnits17","EffortMeasurement18","EffortUnits18","NoObservations","DistanceFromObserver","DistanceFromObserverMin","DistanceFromObserverMax","DistanceFromStart","BearingInDegrees","SpecimenDecimalLatitude","SpecimenDecimalLongitude","SpecimenGeodeticDatum","SpecimenUTMZone","SpecimenUTMNorthing","SpecimenUTMEasting","ObservationCount","ObservationDescriptor","ObservationCount2","ObservationDescriptor2","ObservationCount3","ObservationDescriptor3","ObservationCount4","ObservationDescriptor4","ObservationCount5","ObservationDescriptor5","ObservationCount6","ObservationDescriptor6","ObsCountAtLeast","ObsCountAtMost","ObservationDate","DateUncertaintyInDays","AllIndividualsReported","AllSpeciesReported","UTMZone","UTMNorthing","UTMEasting","CoordinatesUncertaintyInDecimalDegrees","CommonName","RecordPermissions","MultiScientificName1","MultiScientificName2","MultiScientificName3","MultiScientificName4","MultiScientificName5","MultiScientificName6","TaxonomicAuthorityAuthors","TaxonomicAuthorityVersion","TaxonomicAuthorityYear","SpeciesCode","TaxonConceptID","BreedingBirdAtlasCode","HabitatDescription","Remarks2","LastModifiedAction","RecordReviewStatus")
nc_cols <- c("CatalogNumber","ScientificName","Genus","SpecificEpithet",
  "Country","StateProvince","County","Locality","DecimalLatitude","DecimalLongitude","GeodeticDatum",
  "YearCollected","MonthCollected","DayCollected","JulianDay",
  "CollectorNumber",
  "ProjectCode","ProtocolType","SurveyAreaIdentifier",
  "SurveyAreaShape",
  "SamplingEventIdentifier",
  "DurationInHours","NumberOfObservers",
  "EffortMeasurement1","EffortUnits1",
  "EffortMeasurement2","EffortUnits2",
  "NoObservations",
  "ObservationCount","ObservationDescriptor",
  "ObservationDate",
  "AllIndividualsReported","AllSpeciesReported",
  "UTMZone","UTMNorthing","UTMEasting",
  "CommonName","TaxonomicAuthorityAuthors","TaxonomicAuthorityVersion","TaxonomicAuthorityYear","SpeciesCode")


oss <- oss[,nc_cols]
acss <- acss[,nc_cols]

oss[,"SurveyAreaIdentifier"] <- as.character(oss[,"SurveyAreaIdentifier"])
acss[,"SurveyAreaIdentifier"] <- as.character(acss[,"SurveyAreaIdentifier"])

css <- bind_rows(acss,oss)
# css$SurveyAreaIdentifier <- paste(css$ProjectCode,css$SurveyAreaIdentifier,sep = "_")
#above is unecessary column is unique across both datasets

## drops or fixes the observations with no valid dates and outside of the fall migration period
css <- css[which(!is.na(css$MonthCollected) & css$MonthCollected > 6 & css$MonthCollected < 12),] 
css <- css[which(css$YearCollected > 1973),] 

css[which(is.na(css$DayCollected)),"DayCollected"] <- 15 #this places the observation in teh middle of the month

css$ObservationDate <- lubridate::ymd(paste(css$YearCollected,css$MonthCollected,css$DayCollected,sep = "/"))
css$doy <- lubridate::yday(css$ObservationDate)


## drops all sites with no coordinates (there are ~2000 observations and this is about 1% of the total ACSS and OSS combined) most have no information on province or county as well
css <- css[which(!is.na(css$DecimalLatitude)),]


# identifying unique sites and sampling events ------------------------------------------------

css_samp <- unique(css[,c("SamplingEventIdentifier",
                          "Country",
                          "StateProvince",
                          "SurveyAreaIdentifier",
                          "DecimalLatitude",
                          "DecimalLongitude",
                          "YearCollected",
                          "doy")])


# overlaying with regions -------------------------------------------------

css_sites = unique(css_samp[,c("SurveyAreaIdentifier",
                               "DecimalLatitude",
                               "DecimalLongitude")])

css_sites = st_as_sf(css_sites,coords = c("DecimalLongitude","DecimalLatitude"), crs = 4326)


css_sites_regs <- st_join(css_sites, map, join = st_nearest_feature)

css_samp <- left_join(css_samp,data.frame(css_sites_regs[,c("SurveyAreaIdentifier","Region")]))




#full sampling event by species matrix
css_full <- expand_grid(css_samp,sps)
css_full <- rename(css_full,
                   CommonName = sps)


#confirming common species names are the same
css_sps <- unique(css$CommonName)
#sps[-which(sps %in% css_sps)]
#character(0)


# dropping species not included, extra columns, and pres/abs --------------------------------
css <- css[which(css$CommonName %in% sps &
                   css$ObservationDescriptor != "Presence/Absence"),c("SamplingEventIdentifier",
                                                                 "CommonName",
                                            "ObservationCount")]


# zero fill css data  -----------------------------------------------------


css_full <- left_join(css_full,css,by = c("SamplingEventIdentifier","CommonName"))
css_full[which(is.na(css_full$ObservationCount)),"ObservationCount"] <- 0

css_full <- select(css_full,-geometry)
css_full$Country <- "Canada" #some country values are missing from the ACSS file





# Combining ISS and CSS data ----------------------------------------------
# includes observations, zero-filled, and all sampling events




ssData = bind_rows(css_full,iss_full)



# adding national divisions to regions ------------------------------------


ssData[which(ssData$Region == "East Inland" &
               ssData$StateProvince == "ON"),"Region"] <- "Ontario"
ssData[which(ssData$Region == "Northeast Coastal" &
               ssData$Country == "Canada"),"Region"] <- "Atlantic Canada"
ssData[which(ssData$Region == "Northeast Coastal" &
               ssData$Country == "United States"),"Region"] <- "Northeast US Coastal"




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

events <- ssData %>% select(SamplingEventIdentifier:Region) %>% 
  distinct() %>% 
  group_by(Country,StateProvince,SurveyAreaIdentifier,Region,YearCollected) %>% 
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











