
# SPECIES MCMC data-prep -------------------------------------------------------
library(tidyverse)
library(sf)
library(spdep)
library(ggforce)
library(cmdstanr)


laea = st_crs("+proj=laea +lat_0=40 +lon_0=-95") # Lambert equal area

iss_sites_laea <- readRDS("Data/site_map.rds")#loads site map in Lambert equal area projection

# load hexagon grid used in previous analyses -----------------------------

poly_grid <- readRDS("hexagon_grid.rds")%>% 
  st_transform(laea)

map <- readRDS("Data/regional_map.rds") %>% 
  st_transform(laea)



# Load the previous published version of trends and indices ---------------
## accessed from the Trends folder of https://zenodo.org/badge/latestdoi/291260452 
pub_trends <- read_csv("data/Smith_et_al/All_Shorebird_migration_survey_wide_trends.csv") %>% 
  mutate(version = "published",
         model = "Poisson")
pub_trends2 <- read_csv("data/Smith_et_al/All_Shorebird_migration_survey_wide_trends.csv") %>% 
  mutate(version = "published",
         model = "negative-binomial")
pub_trends <- bind_rows(pub_trends,pub_trends2)

pub_trends_strat <- read_csv("data/Smith_et_al/All_strata_gamma_t_level_trends.csv") %>% 
  mutate(version = "published",
         model = "Poisson")
pub_trends_strat2 <- read_csv("data/Smith_et_al/All_strata_gamma_t_level_trends.csv") %>% 
  mutate(version = "published",
         model = "negative-binomial")
pub_trends_strat <- bind_rows(pub_trends_strat,pub_trends_strat2)

pub_trajectories <- read_csv("data/Smith_et_al/Survey_wide_smooth_and_full_annual_indices_shorebird.csv") %>% 
  rename(median = predicted_mean_abundance,
         lci = lower_95percent_CL,
         uci = upper_95percent_CL) %>% 
  mutate(parm = ifelse(smooth_or_full_indices == "Full",
                       "N_alt","NSmooth_alt"),
         version = "published",
         model = "Poisson")


# pub_trajectories2 <- read_csv("data/Smith_et_al/Survey_wide_smooth_and_full_annual_indices_shorebird.csv") %>% 
#   rename(median = predicted_mean_abundance,
#          lci = lower_95percent_CL,
#          uci = upper_95percent_CL) %>% 
#   mutate(parm = ifelse(smooth_or_full_indices == "Full",
#                        "N","NSmooth"),
#          version = "published",
#          model = "negative-binomial")
# 
# pub_trajectories <- bind_rows(pub_trajectories,pub_trajectories2)

FYYYY <- 1980
LYYYY_trend <- 2024
LYYYY <- 2024

t1 = Sys.time()


trajs <- NULL
trends <- NULL
trends_strat <- NULL


for(mod in c("_OPois_2024","_NB_2024")){

indices_out <- read_csv(paste0("trends/All_survey_wide_trajectories",FYYYY,mod,".csv")) %>% 
  mutate(model = ifelse(mod == "_OPois_2024",
                        "Poisson","negative-binomial"),
         version = "updated")
trajs <- bind_rows(trajs,indices_out)

  
}

for(modl in c("_OPois","_NB")){
  for(yy in c("_2019","_2024")){
   
    mod <- paste0(modl,yy)
    
    TRENDSout <- read_csv(paste0("trends/All_survey_wide_trends",FYYYY,mod,".csv")) %>% 
      mutate(model = ifelse(modl == "_OPois",
                            "Poisson","negative-binomial"),
             version = "updated")
    trends <- bind_rows(trends,TRENDSout)
    
    
    trendsout_strat <- read_csv(paste0("trends/All_region_level_trends",FYYYY,mod,".csv")) %>% 
      mutate(model = ifelse(modl == "_OPois",
                            "Poisson","negative-binomial"),
             version = "updated")
    trends_strat <- bind_rows(trends_strat,trendsout_strat)
    
    
  }
  
}

trends_comp <- bind_rows(trends,pub_trends)
trends_strat_comp <- bind_rows(trends_strat,pub_trends_strat)

trajs_comp <- bind_rows(trajs,pub_trajectories)



# Compare the trend estimates ---------------------------------------------
#

lt_2019_trends <- trends_comp %>% 
  filter(trend_type == "Long-term") %>% 
  mutate(version_2 = paste(version,end_year,sep = "-"))
  
lt_2019plot <- ggplot(data = lt_2019_trends,
                      aes(x = species,y = trend,colour = version_2))+
  geom_pointrange(aes(ymax = uci,ymin = lci),position = position_dodge(width = 0.5))+
  geom_abline(slope = 0,intercept = 0,alpha = 0.7)+
  ylab("Trend (%/year)")+
  xlab("")+
  scale_colour_viridis_d()+
  theme_classic()+
  theme(legend.position = "bottom")+
  coord_flip()+
  facet_wrap(vars(model))

pdf("figures/trend_comparison_pub_vs_both_trimmed_criteria.pdf",
    width = 11,
    height = 8.5)
print(lt_2019plot)
dev.off()



# Compare the population trajectories on the log-scale --------------------
# they have to be compared on the log-scale to account for the different 
# scaling with and without the overall intercept

 

trajs_plotting <- trajs_comp %>% 
  filter(grepl("Smooth",parm)) %>% 
  mutate(version_2 = paste(version,model,sep = "-"))


traj_plot <- ggplot(data = trajs_plotting,
                    aes(x = year, y = median))+
  geom_ribbon(aes(ymin = lci,ymax = uci,
                  fill = version_2),
              alpha = 0.2)+
  geom_line(aes(colour = version_2))+
  scale_y_continuous(transform = "log10")+
  facet_wrap(vars(species),scales = "free_y")


pdf("figures/trajectory_comparisons_pub_vs_both_trimmed_criteria.pdf",
    width = 11,
    height = 8.5)
print(traj_plot)


trajs_plotting_full <- trajs_comp %>% 
  filter(grepl("N_",parm)) %>% 
  mutate(version_2 = paste(version,model,sep = "-"))


traj_plot_full <- ggplot(data = trajs_plotting_full,
                    aes(x = year, y = median))+
  geom_ribbon(aes(ymin = lci,ymax = uci,
                  fill = version_2),
              alpha = 0.2)+
  geom_line(aes(colour = version_2))+
  scale_y_continuous(transform = "log10")+
  facet_wrap(vars(species),scales = "free_y")


print(traj_plot_full)
dev.off()



# Compare the stratum-level trends among versions and models --------------

# Compare the stratum-level trends among versions and models --------------

# Compare the stratum-level trends among versions and models --------------

# Compare the stratum-level trends among versions and models --------------

# Compare the stratum-level trends among versions and models --------------

# Compare the stratum-level trends among versions and models --------------



lt_2019_trends <- trends_strat_comp %>% 
  filter(trend_type == "Long-term",
         model == "Poisson") %>% 
  mutate(version_2 = paste(version,end_year,sep = "-"))

lt_2019plot <- ggplot(data = lt_2019_trends,
                      aes(x = species,y = trend,colour = version_2))+
  geom_pointrange(aes(ymax = uci,ymin = lci),position = position_dodge(width = 0.5))+
  geom_abline(slope = 0,intercept = 0,alpha = 0.7)+
  ylab("Trend (%/year)")+
  xlab("")+
  scale_colour_viridis_d()+
  theme_classic()+
  theme(legend.position = "bottom")+
  coord_flip()+
  facet_wrap(vars(region))

pdf("figures/trend_comparison_pub_vs_NB_trimmed_criteria.pdf",
    width = 11,
    height = 8.5)

print(lt_2019plot)

dev.off()




lt_2019_trends <- trends_strat_comp %>% 
  filter(trend_type == "Long-term",
         model != "Poisson") %>% 
  mutate(version_2 = paste(version,end_year,sep = "-"))

lt_2019plot <- ggplot(data = lt_2019_trends,
                      aes(x = region,y = trend,colour = version_2))+
  geom_pointrange(aes(ymax = uci,ymin = lci),position = position_dodge(width = 0.5))+
  geom_abline(slope = 0,intercept = 0,alpha = 0.7)+
  ylab("Trend (%/year)")+
  xlab("")+
  scale_colour_viridis_d()+
  theme_bw()+
  theme(legend.position = "bottom")+
  coord_flip(ylim = c(-10,10))+
  facet_wrap(vars(species))

pdf("figures/trend_comparison_pub_vs_OPois_trimmed_criteria.pdf",
    width = 11,
    height = 8.5)
print(lt_2019plot)
dev.off()




