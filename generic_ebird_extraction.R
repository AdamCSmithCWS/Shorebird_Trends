



### only need running once
library(R.utils)
library(tidyverse)
laea = sf::st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area

proj_name <- "breeding" # provides a text string to name this particular subse of the data

# unzipping the downloaded EBD datsets for Canada and US ------------------

# only needs to be done once after download


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
  
  ebdat <- NULL
  ebdat_samp <- NULL
  # 
  for(rr in c("CA","US")){
    data_dir <- paste0("data/eBird")
    auk::auk_set_ebd_path(data_dir,overwrite = T)
    out_sampling <- file.path(data_dir, paste0("ebd_",rr,"_breeding_sampling.txt"))
    out_ebd <- file.path(data_dir, paste0("ebd_",rr,"_breeding.txt"))
    #
    ebd_filters <- auk_ebd(file = paste0("data/eBird/ebd_",rr,"_relDec-2024.txt"),
                           file_sampling = paste0("data/eBird/ebd_",rr,"_relDec-2024_sampling.txt"),
                           sep = "\t") %>%
      auk_date(date = c("*-05-01", "*-07-15")) %>% # May,June, early July surveys only
      auk_protocol(protocol = c("Stationary","Traveling")) %>% # restrict to the stationary counts
      auk_distance(distance = c(0,0.5)) %>% # filtering to traveling < 500 m
      auk_duration(duration = c(0,60)) %>% # < 1 hr
      auk_complete() %>% 
        auk_filter(file = out_ebd,
                   file_sampling = out_sampling,
                   overwrite = TRUE)
    
    
    
    
    ebdat_tmp1 <- read_ebd(out_ebd)
    
    ebdat_tmp <- auk_unique(ebdat_tmp1, checklists_only = TRUE)
    
    
    ebdat_samp_tmp <- read_sampling(out_sampling)
    
    ebdat <- bind_rows(ebdat,ebdat_tmp)
    ebdat_samp <- bind_rows(ebdat_samp,ebdat_samp_tmp)
    
  }
  
  save(list = c("ebdat","ebdat_samp"),
       file = paste0("data/all_",proj_name,"_eBird.RData"))
}else{
  
  load(paste0("data/all_",proj_name,"_eBird.RData"))
  
}
