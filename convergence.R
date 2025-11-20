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
re_summarise <- FALSE

if(re_summarise){
convergence_summary <- NULL
for(sp in sps){
  
  load(paste0("data/species_stan_data/",sp,"_stan_data.RData"))
  species_f <- gsub(pattern = "'",gsub(pattern = " ",sp,replacement = "_"),replacement = "")
  
  output_dir <- "output/"
  
  for(model in c("NB","OPois")){
  out_base <- paste0(species_f,"_",model)
  
  if(file.exists(paste0(output_dir,"/",out_base,"_2024_fit.RData"))){
load(paste0(output_dir,"/",out_base,"_2024_fit.RData"))

summ <- summ %>% 
  mutate(species = sp,
         model_type = model)

convergence_summary <- bind_rows(convergence_summary,summ)

write.csv(convergence_summary,"output/All_species_convergence_summary.csv")
}
}
}

saveRDS(convergence_summary,"output/convergence_summary_all_sp_models.rds")
}else{
  convergence_summary<- readRDS("output/convergence_summary_all_sp_models.rds")
}


Cont_trajs_conv <- convergence_summary %>% filter(grepl("NSmooth",variable))

sp_ess <- Cont_trajs_conv %>% 
  group_by(species,
           model_type) %>% 
  summarise(max_rhat = max(rhat),
            mean_rhat = mean(rhat),
            min_ess = min(ess_bulk),
            mean_ess = mean(ess_bulk))

sd_conv <- convergence_summary %>% filter(grepl("sd",variable))

ALPHA_conv <- convergence_summary %>% filter(grepl("ALPHA",variable))

sp_ess_all <- convergence_summary %>% 
  group_by(species,
           model_type) %>% 
  summarise(max_rhat = max(rhat),
            mean_rhat = mean(rhat),
            min_ess = min(ess_bulk),
            mean_ess = mean(ess_bulk))


