# SPECIES MCMC data-prep -------------------------------------------------------
library(tidyverse)
library(sf)
library(spdep)
library(ggforce)


# library(doParallel)
# library(foreach)

#load data
#load("data/allShorebirdPrismFallCounts.RData")
ssData <- readRDS("Data/ssData.rds")
sps <- readRDS("Data/sps_list.rds")
map <- readRDS("Data/regional_map.rds")
#load spatial function to transform GeoBugs format to Stan
#source("functions/mungeCARdata4stan.R")
source("functions/neighbours_define_alt.r")

sp_groups <- read.csv("data/Species_list.csv")

 dir.create("data/species_stan_data")
 # use the same equal-area grid stratification used in the 2019 analyses --------------------------------------
 # load hexagon grid used in previous analyses -----------------------------
 
 poly_grid <- readRDS("hexagon_grid.rds")
 
 ### someday, site-level model would be useful, but it implies a tremendous number of "trend" parameters that may cause some significant
 ### computational limits
 ### instead, etsablish an equal-area grid as a geographic stratification
 laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area
 
 iss_sites_laea <- readRDS("Data/site_map.rds")#loads site map in Lambert equal area projection
 

 prov_state <- bbsBayes2::load_map("prov_state") %>% 
   st_transform(prov_state,crs = laea)
 
 
 orig_ss_regions <- map %>% 
   select(Region) %>% 
   st_transform(crs = laea)
 
 

 

 
 source("functions/GAM_basis_function_mgcv.R")


 # blank holder objects to fill in species loop
 mean_counbts_doy_out <- vector(mode = "list",length = length(sps))
 names(mean_counbts_doy_out) <- sps
 ggp_out <- vector(mode = "list",length = length(sps))
 names(ggp_out) <- sps
 mean_counbts_year_out <- vector(mode = "list",length = length(sps))
 names(mean_counbts_year_out) <- sps

 
 
 
 
 
 

# SPECIES LOOP ------------------------------------------------------------



 
for(sp in sps){
#sp = sps[11]
FYYYY = 1980
LYYYY = max(ssData$YearCollected)

if(sp_groups[which(sp_groups$Species == sp),"two_seasons"]){
  two_seasons <- TRUE
  regs_alt_season <- as.character(str_split(sp_groups[which(sp_groups$Species == sp),"regions_w_alt_season"],pattern = " - ",simplify = TRUE))
}else{
  two_seasons <- FALSE
}



dts <- filter(ssData,CommonName == sp,
              YearCollected >= FYYYY)
nyrs_study <- length(LYYYY:FYYYY) #length of the time-series being modeled


dts$present <- FALSE

dts[which(dts$ObservationCount > 0),"present"] <- TRUE


#number of non-zero observations by site
nobs_site <- dts %>% 
  filter(present == TRUE) %>% 
  group_by(Region,SurveyAreaIdentifier) %>% 
  summarise(nobs = n())

yspn = function(x){
  diff(range(x))
}
#number of years with non-zero observations, by site
nyrs_site <- dts %>% 
  filter(present == TRUE) %>%  
  group_by(Region,SurveyAreaIdentifier,YearCollected) %>% 
  summarise(nobs = n()) %>% 
  group_by(Region,SurveyAreaIdentifier) %>% 
  summarise(nyears = n(),
            span_years = yspn(YearCollected),
            fyear = min(YearCollected),
            lyear = max(YearCollected))


min_nyears <- 2
minspan <- 10
#sites with > 2 years of non-zero, observations and > 10 year span of observations
sites_keep <- nyrs_site[which(nyrs_site$span_years >= minspan,
                              nyrs_site$nyears >= min_nyears),"SurveyAreaIdentifier"]

# drop sites with <10 year span of data ------------------------------------


dts <- filter(dts,SurveyAreaIdentifier %in% sites_keep$SurveyAreaIdentifier) 


#number of years with non-zero observations, by region
nyrs_region <- dts %>% 
  filter(present == TRUE) %>%  
  group_by(hex_name,YearCollected) %>% 
  summarise(nobs = n()) %>% 
  group_by(hex_name) %>% 
  summarise(nyears = n(),
            span_years = yspn(YearCollected),
            fyear = min(YearCollected),
            lyear = max(YearCollected))

#strats with 20 or more year span of non-zero, observations - 
p_time_series = 0.5 #strata are required to have data that span 50% of the time-series in a region.
regions_keep <- nyrs_region[which(nyrs_region$span_years >= nyrs_study*p_time_series),"hex_name"]



dts <- filter(dts,hex_name %in% regions_keep$hex_name) 

real_grid <- poly_grid %>% filter(hex_name %in% regions_keep$hex_name) 

dom_value <- function(x){
  tt = table(x)
  tt = sort(tt)
  return(names(tt)[1])
}
hex_by_reg <- dts %>% group_by(hex_name) %>% 
  summarise(Region = dom_value(Region))

real_grid_regs <- left_join(real_grid, hex_by_reg,by = "hex_name")

dts <- dts %>% select(-Region) %>% 
  left_join(.,hex_by_reg)

strats_dts <- data.frame(hex_name = real_grid$hex_name,
                         stratn = 1:length(real_grid$hex_name))

dts <- left_join(dts,strats_dts,by = "hex_name")
real_grid <- inner_join(real_grid,strats_dts)
real_grid_regs <- inner_join(real_grid_regs,strats_dts)


# DOY season definition ---------------------------------------------------


fday = min(dts$doy)-1

syear = min(dts$YearCollected)

dts <- dts %>% mutate(count = as.integer(ObservationCount),
                      year = as.integer(YearCollected),
                      yr = as.integer(year-(syear-1)),
                      strat = stratn,
                      date = doy-fday,
                      site = as.integer(factor(SurveyAreaIdentifier))) 

nstrata = max(dts$strat)
nsites = max(dts$site)

# sizes_by_site <- unique(dts[,c("site","size_cent")])
# sizes_by_site <- sizes_by_site[order(sizes_by_site$site),]


## indexing of sites by strata for annual index calculations
sByReg = unique(dts[,c("site","strat")])
sByReg <- arrange(sByReg,strat,site)
# 
nsites_strat <- table(sByReg$strat)
max_sites <- max(nsites_strat)
sites <- matrix(data = 0,nrow = max_sites,ncol = nstrata)
for(j in 1:nstrata){
  sites[1:nsites_strat[j],j] <- as.integer(unlist(sByReg[which(sByReg$strat == j),"site"]))
}



# generate neighbourhoods -------------------------------------------------


neighbours = neighbours_define(real_strata_map = real_grid_regs,
                               add_map = prov_state,
                               strat_link_fill = 10000,
                               species = sp,
                               strat_indicator = "stratn",
                               plot_dir = "FIgures/maps/",
                               plot_file = "_strata_map",
                               voronoi = TRUE)

ggp_out[[sp]] <- neighbours$map






mean_counbts_doy = ggplot(data = dts,aes(x = doy,y = count+1,colour = Region))+
  scale_y_log10()+
  geom_point(position = position_jitter(width = 0.1,height = 0),alpha = 0.3)+
  geom_smooth()+
  labs(title = sp)+
  facet_wrap(facets = ~strat,ncol = 5,scales = "free_y")


mean_counbts_doy_out[[sp]] <- mean_counbts_doy










length(unique(dts$strat))

mean_counbts_year = ggplot(data = dts,aes(x = year,y = count+1,colour = Region))+
  scale_y_log10()+
  geom_point(position = position_jitter(width = 0.1,height = 0),alpha = 0.3)+
  geom_smooth(method = "lm")+
  labs(title = sp)+
  facet_wrap(facets = ~strat,ncol = 5,scales = "free_y")

pdf(paste0("Figures/",sp,"annual_counts_2024.pdf"),
    width = 11,height = 8.5)
print(mean_counbts_year)
dev.off()
mean_counbts_year_out[[sp]] <- mean_counbts_year



# Identifying the strata and region combinations --------------------------
strat_regions <- unique(dts[,c("Region","strat")])
strat_regions <- strat_regions[order(strat_regions$strat),]




ncounts = nrow(dts)

nyears = max(dts$yr)
midyear = floor(nyears/2)

# GAM seasonal basis function ---------------------------------------------

nKnots_season = 10
basis_season <- gam_basis(orig.preds = as.integer(unlist(dts[,"date"])),
                               nknots = nKnots_season,
                               npredpoints = max(dts$date),
                               sm_name = "season")

ndays <- basis_season$npredpoints_season
# 

# GAM year basis function ---------------------------------------------

nKnots_year = ceiling(nyears/4)
basis_year <- gam_basis(orig.preds = as.integer(unlist(dts[,"yr"])),
                               nknots = nKnots_year,
                               npredpoints = max(dts$yr),
                               sm_name = "year")

# 


if(two_seasons){
  
  strat_regions$seas_strat <- 1
  strat_regions$seas_strat[which(strat_regions$Region %in% regs_alt_season)] <- 2
  dts <- left_join(dts,strat_regions,by = c("Region","strat"))

  seasons <- as.matrix(strat_regions[,c("strat","seas_strat")])
  
  stan_data <- list(count = as.integer(unlist(dts$count)),
                    year = as.integer(unlist(dts$yr-midyear)),
                    year_raw = as.integer(unlist(dts$yr)),
                    site = as.integer(unlist(dts$site)),
                    strat = as.integer(unlist(dts$strat)),
                    date = as.integer(unlist(dts$date)),
                    seas_strat = as.integer(unlist(dts$seas_strat)),
                    
                    nyears = nyears,
                    nstrata = nstrata,
                    nsites = nsites,
                    ncounts = ncounts,
                    ndays = ndays,
                    
                    nsites_strat = as.integer(nsites_strat),
                    max_sites = max_sites,
                    sites = sites,
                    seasons = seasons,
                    
                    #site_size = sizes_by_site$size_cent,
                    
                    # season_basis = basis_season$season_basis,
                    season_basispred = basis_season$season_basispred,
                    nknots_season = basis_season$nknots_season,
                    
                    year_basispred = basis_year$year_basispred,
                    nknots_year = basis_year$nknots_year,
                    
                    #midyear = midyear,
                    
                    N_edges = neighbours$N_edges,
                    node1 = neighbours$node1,
                    node2 = neighbours$node2)
  

  mod.file = "models/GAMYE_spatial_shorebird_NB_two_season.stan"
  prior = "gamma"
  noise_dist = "NB"
  

  init_def <- function(){ list(alpha_raw = rnorm(stan_data$nsites,0,0.1),
                               ALPHA1 = 0,
                               year_effect_raw = rnorm(stan_data$nyears,0,0.1),
                               B_season_raw1 = rnorm(stan_data$ndays,0,0.1),
                               B_season_raw2 = rnorm(stan_data$ndays,0,0.1),
                               sdnoise = 0.2,
                               sdalpha = 0.1,
                               sdyear_gam = 1,
                               sdyear_gam_strat = runif(1,0.5,1.5),#runif(stan_data$nknots_year,0.1,0.2),
                               sdseason = c(1,1),
                               sdyear = 0.1,
                               B_raw = rnorm(stan_data$nknots_year,0,0.1),
                               b_raw = matrix(rnorm(stan_data$nknots_year*stan_data$nstrata,0,0.01),
                                              nrow = stan_data$nstrata,ncol = stan_data$nknots_year))}
  
  
  


}else{
  stan_data <- list(count = as.integer(unlist(dts$count)),
                    year = as.integer(unlist(dts$yr-midyear)),
                    year_raw = as.integer(unlist(dts$yr)),
                    site = as.integer(unlist(dts$site)),
                    strat = as.integer(unlist(dts$strat)),
                    date = as.integer(unlist(dts$date)),
                    
                    nyears = nyears,
                    nstrata = nstrata,
                    nsites = nsites,
                    ncounts = ncounts,
                    ndays = ndays,
                    
                    nsites_strat = as.integer(nsites_strat),
                    max_sites = max_sites,
                    sites = sites,
                    
                   season_basispred = basis_season$season_basispred,
                    nknots_season = basis_season$nknots_season,
                    
                    year_basispred = basis_year$year_basispred,
                    nknots_year = basis_year$nknots_year,
                    
                    #midyear = midyear,
                    
                    N_edges = neighbours$N_edges,
                    node1 = neighbours$node1,
                    node2 = neighbours$node2)
  

  mod.file = "models/GAMYE_spatial_shorebird_NB.stan"
  prior = "gamma"
  noise_dist = "NB"
  
  init_def <- function(){ list(alpha_raw = rnorm(stan_data$nsites,0,0.1),
                               ALPHA1 = 0,
                               year_effect_raw = rnorm(stan_data$nyears,0,0.1),
                               B_season_raw = rnorm(stan_data$ndays,0,0.1),
                               sdnoise = 0.2,
                               sdalpha = 0.1,
                               sdyear_gam = 1,
                               sdyear_gam_strat = runif(1,0.5,1.5),#runif(stan_data$nknots_year,0.1,0.2),
                               sdseason = 1,
                               sdyear = 0.1,
                               B_raw = rnorm(stan_data$nknots_year,0,0.1),
                               b_raw = matrix(rnorm(stan_data$nknots_year*stan_data$nstrata,0,0.01),
                                              nrow = stan_data$nstrata,ncol = stan_data$nknots_year))}
  
  
  
 }

# Explore site-level trajectories of observed means ------------------------

# site_means <- dts %>% group_by(year,SurveyAreaIdentifier) %>%
#   summarise(means = log(mean(count,na.rm = T)+1))
# nrow(site_means)/nyears
# smp = ggplot(data = site_means,aes(x = year,y = means,colour = SurveyAreaIdentifier))+
#   geom_point(alpha = 0.05)+
#   geom_smooth(method = "lm",se = FALSE)+
#   #scale_y_continuous(trans = "log10")+
#   theme(legend.position = "none")
# print(smp)#   

# prepare stan data -------------------------------------------------------


save(list = c("stan_data",
              "dts",
              "real_grid",
              "real_grid_regs",
              "strats_dts",
              "strat_regions",
              "mod.file",
              "prior",
              "noise_dist",
              "neighbours",
              "two_seasons"),
     file = paste0("data/species_stan_data/",sp,"_2024_stan_data.RData"))




} ### end species loop


 
 
 
 #print graphs
 
 
 pdf(paste0("Figures/","All_seasonal_counts_2024.pdf"),
     width = 11,height = 8.5)
 for(sp in sps[-c(1:16)]){
   print(mean_counbts_doy_out[[sp]]+
           labs(title = sp))
   
 }
 dev.off()
 
 
 
 # maps
 pdf(file = paste0("Figures/","ALL_Strata_2024.pdf"),
     height = 8.5,width = 11)
 for(sp in sps){
   print(ggp_out[[sp]]+
           labs(title = sp))
 }
 dev.off()
  ## end maps
 
 
 
 
 pdf(paste0("Figures/","All_annual_counts_2024.pdf"),
     width = 11,height = 8.5)
 for(sp in sps){
   print(mean_counbts_year_out[[sp]]+
           labs(title = sp))
   
 }
 dev.off()
 

