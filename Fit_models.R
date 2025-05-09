# SPECIES MCMC data-prep -------------------------------------------------------
library(tidyverse)
library(cmdstanr)
library(foreach)
library(doParallel)

setwd("C:/Users/SmithAC/Documents/GitHub/shorebird_trends")
sp_groups <- read.csv("data/Species_list.csv")
sps <- sp_groups$Species

dir.create("output")
# Fit model ---------------------------------------------------------------

n_cores <- length(6)
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


# load("temp_rerun_list.RData")


fullrun <- foreach(sp = sps[c(11,14)],
                   .packages = c("cmdstanr","tidyverse"),
                   .inorder = FALSE,
                   .errorhandling = "pass") %dopar%
  {
    
    
# for(sp in sps[c(1:4)]){
  library(cmdstanr)
  load(paste0("data/species_stan_data/",sp,"_2024_stan_data.RData"))
  
  species_f <- gsub(pattern = "'",gsub(pattern = " ",sp,replacement = "_"),replacement = "")
  
output_dir <- "output/"
out_base <- paste0(species_f,"_NB")
csv_files <- paste0(out_base,"-",1:3,".csv")

#if(file.exists(paste0(output_dir,csv_files[1]))){next}

print(paste("beginning",sp,Sys.time()))


## compile model
model <- cmdstan_model(mod.file, stanc_options = list("O1"))
use_manual_inits<- FALSE
if(use_manual_inits){
if(two_seasons){
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
}else{
  init_def <- 1
}
stanfit <- model$sample(
  data=stan_data,
  refresh=200,
  chains=3, 
  iter_sampling=100,
  iter_warmup=500,
  parallel_chains = 3,
  #pars = parms,
  adapt_delta = 0.95,
  max_treedepth = 11,
  seed = 123,
  init = init_def,
  output_dir = output_dir,
  output_basename = out_base,
  show_exceptions = FALSE)

summ <- stanfit$summary()

save(list = c("stanfit",
              "stan_data",
              "dts",
              "real_grid",
              "strats_dts",
              "strat_regions",
              "mod.file",
              "summ"),
     file = paste0(output_dir,"/",out_base,"_2024_fit.RData"))


}#end modeling loop

stopCluster(cl = cluster)








