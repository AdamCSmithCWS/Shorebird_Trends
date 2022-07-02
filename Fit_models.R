# SPECIES MCMC data-prep -------------------------------------------------------
library(tidyverse)
library(cmdstanr)

sp_groups <- read.csv("data/Species_list.csv")
sps <- sp_groups$Species

# Fit model ---------------------------------------------------------------

for(sp in sps){
  
  load(paste0("data/species_stan_data/",sp,"_stan_data.RData"))
  species_f <- gsub(pattern = " ",sp,replacement = "_")
  
output_dir <- "output/"
out_base <- paste0(species_f,"_NB")
csv_files <- paste0(out_base,"-",1:3,".csv")


print(paste("beginning",sp,Sys.time()))


## compile model
model <- cmdstan_model(mod.file)


stanfit <- model$sample(
  data=stan_data,
  refresh=200,
  chains=3, iter_sampling=2000,
  iter_warmup=2000,
  parallel_chains = 3,
  #pars = parms,
  adapt_delta = 0.95,
  max_treedepth = 14,
  seed = 123,
  init = init_def,
  output_dir = output_dir,
  output_basename = out_base)


#stanfit1 <- as_cmdstan_fit(files = paste0(output_dir,csv_files))


save(list = c("stanfit","stan_data","csv_files"),
     file = paste0(output_dir,"/",out_base,"_gamye_iCAR.RData"))



save(list = c("stanfit",
              "stan_data",
              "dts",
              "real_grid",
              "strats_dts",
              "strat_regions",
              "mod.file"),
     file = paste0(output_dir,"/",out_base,"_fit.RData"))


}#end modeling loop









 

