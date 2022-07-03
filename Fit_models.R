# SPECIES MCMC data-prep -------------------------------------------------------
library(tidyverse)
library(cmdstanr)

setwd("C:/Users/SmithAC/Documents/GitHub/shorebird_trends")
sp_groups <- read.csv("data/Species_list.csv")
sps <- sp_groups$Species

# Fit model ---------------------------------------------------------------

for(sp in sps[c(1:4)]){
  
  load(paste0("data/species_stan_data/",sp,"_stan_data.RData"))
  species_f <- gsub(pattern = " ",sp,replacement = "_")
  
output_dir <- "output/"
out_base <- paste0(species_f,"_NB")
csv_files <- paste0(out_base,"-",1:3,".csv")

if(file.exists(csv_files[1])){next}

print(paste("beginning",sp,Sys.time()))


## compile model
model <- cmdstan_model(mod.file)


stanfit <- model$sample(
  data=stan_data,
  refresh=200,
  chains=3, iter_sampling=1000,
  iter_warmup=1000,
  parallel_chains = 3,
  #pars = parms,
  adapt_delta = 0.95,
  max_treedepth = 14,
  seed = 123,
  init = init_def,
  output_dir = output_dir,
  output_basename = out_base)


save(list = c("stanfit",
              "stan_data",
              "dts",
              "real_grid",
              "strats_dts",
              "strat_regions",
              "mod.file"),
     file = paste0(output_dir,"/",out_base,"_fit.RData"))


}#end modeling loop









 

