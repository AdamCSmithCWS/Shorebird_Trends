# Convergence Summary



library(tidyverse)
library(cmdstanr)
library(shinystan)
#source("functions/utility_functions.R")
# source("functions/posterior_summary_functions.R")
# source("Functions/palettes.R")

load("data/allShorebirdPrismFallCounts.RData")




sp_groups <- read.csv("data/Species_list.csv")
sps <- sp_groups$Species


# Convergence Summary -----------------------------------------------------
convergence_summary <- NULL
for(sp in sps){
  
  load(paste0("data/species_stan_data/",sp,"_stan_data.RData"))
  species_f <- gsub(pattern = "'",gsub(pattern = " ",sp,replacement = "_"),replacement = "")
  
  output_dir <- "output/"
  out_base <- paste0(species_f,"_NB")
  
  if(file.exists(paste0(output_dir,"/",out_base,"_2021_fit.RData"))){
load(paste0(output_dir,"/",out_base,"_2021_fit.RData"))

summ <- stanfit$summary() %>% 
  mutate(species = sp)

convergence_summary <- bind_rows(convergence_summary,summ)

write.csv(convergence_summary,"output/All_species_convergence_summary.csv")
}

}


Cont_trajs_conv <- convergence_summary %>% filter(grepl("NSmooth",variable))

sp_ess <- Cont_trajs_conv %>% 
  group_by(species) %>% 
  summarise(max_rhat = max(rhat),
            mean_rhat = mean(rhat),
            min_ess = min(ess_bulk),
            mean_ess = mean(ess_bulk))

sd_conv <- convergence_summary %>% filter(grepl("sd",variable))

ALPHA_conv <- convergence_summary %>% filter(grepl("ALPHA",variable))


