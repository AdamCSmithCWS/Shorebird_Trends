
library(readxl)
library(tidyverse)
source("functions/explore_duplicates.R")

load("data/all_iss_eBird.RData")


iss_samp <- iss_samp %>% 
  filter(!state %in% c("Alaska","Hawaii",
                       "Northwest Territories",
                       "New Brunswick",   # removing all atlantic province data to replace with ACSS
                       "Nova Scotia",
                       "Prince Edward Island",
                       "Newfoundland and Labrador")) %>% 
  ungroup()


bystate <- explore_duplicates(iss_samp,
                             x = "locality",
                             y = "state_code")


tt <- bystate$unique_combinations
# counting the number of unique state values for each Site code
tt2 <- bystate$number_of_duplicates
tt2d <- tt2 %>% filter(n_duplicates > 0)

tt3 <- tt %>% 
  filter(locality %in% tt2d$locality)

paste("There are ",length(which(tt2$n_duplicates > 0)),
      "Site names with greater than 1 state assigned")

# this table shows the unique combinations of Site code (locality_id) and SurveySite (locality)
bylocality <- explore_duplicates(iss_samp,
                                 x = "locality",
                             y = "locality_id")
tt <- bylocality$unique_combinations
tt2 <- bylocality$number_of_duplicates
tt2d <- tt2 %>% filter(n_duplicates > 0)

tt3 <- tt %>% 
  filter(locality %in% tt2d$locality)

paste("There are ",length(which(tt2$n_duplicates > 0)),
      "Site names with greater than 1 SurveySite assigned")

# this table shows the unique combinations of Site code (locality_id) and coordinates 


bycoord <- explore_duplicates(iss,
                                 x = "locality",
                                 y = c("latitude","longitude"))
tt <- bycoord$unique_combinations
tt2 <- bycoord$number_of_duplicates

tt2d <- tt2 %>% filter(n_duplicates > 0)

tt3 <- tt %>% 
  filter(locality %in% tt2d$locality)


paste("There are ",length(which(tt2$n_duplicates > 0)),
      "Site names with greater than 1 set of coordinates")





# this table shows the unique combinations of checklist and species 


sp_checkl <- explore_duplicates(iss,
                              x = "checklist_id",
                              y = c("common_name"))
tt <- bycoord$unique_combinations
tt2 <- bycoord$number_of_duplicates

tt2d <- tt2 %>% filter(n_duplicates > 0)

tt3 <- tt %>% 
  filter(locality %in% tt2d$locality)


paste("There are ",length(which(tt2$n_duplicates > 0)),
      "Site names with greater than 1 set of coordinates")




# if desired, select a Unique list of coordinates for each acss locality -------------------------

iss_unique_site_coord <- iss_samp %>% 
  arrange(observation_date) %>% #sorting by observation date oldest obs first
  select(locality,state_code,latitude,longitude) %>% 
  drop_na() %>% #dropping any rows where one of the selected columns is missing
  slice_tail(n = 1,by = locality) #selects the most recent set of coordinates that come up in the dataset

write_csv(iss_unique_site_coord,
        "first_draft_coord_state_by_sitename.csv")







# fitting to strata - geographic overlay ----------------------------------
  map <- read_sf(dsn = "data",
                 layer ="region_polygons")

st_crs(map)

# iss_sites = unique(iss_samp[,c("locality","locality_id",
#                         "latitude",
#                         "longitude")])


iss_sites <- unique(iss_samp[,c("locality","locality_id",
                                "country_code","state_code",
                                "latitude",
                                "longitude")])




iss_sites = st_as_sf(iss_sites,coords = c("longitude","latitude"), crs = 4326)



# Checking for very close sites that have different names -----------------
iss_sites_dist <- iss_sites %>% 
  st_transform(crs = 4087) # WGS 84 World Equidistant Cylindrical

iss_distmat <- as.matrix(units::drop_units(sf::st_distance(iss_sites_dist)))

for(j in 1:nrow(iss_sites)){
  js <- which(iss_distmat[j,] < 160)
  iss_sites[j,"n_near"] <- length(js)
  iss_sites[j,"locality_id_LT_dist"] <- paste(unname(unlist(st_drop_geometry(iss_sites[js,"locality_id"]))),collapse = "-")
  iss_sites[j,"locality_LT_dist"] <- paste(unname(unlist(st_drop_geometry(iss_sites[js,"locality"]))),collapse = "-")
}





iss_sites_regs <- st_join(iss_sites, map, join = st_nearest_feature)

iss_regs_j <- iss_sites_regs %>% 
  as.data.frame() %>% 
  select(locality,locality_id,Region,Region_FR,n_near,locality_id_LT_dist,locality_LT_dist) %>% 
  distinct()


