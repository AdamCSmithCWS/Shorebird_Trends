### comparing the eBird derived ISS data with the previous multi-source data in 2019

library(tidyverse)
library(patchwork)
library(sf)

# 2019 data ---------------------------------------------------------------

load("data/allShorebirdPrismFallCounts2019.RData")
dt2019 <- ssData %>% 
  mutate(version = "v_2019")


# 2024 data ---------------------------------------------------------------

load("data/allShorebirdPrismFallCounts.RData")
dt2024 <- ssData %>% 
  mutate(version = "v_2024") %>% 
  filter(YearCollected < 2020)



dt_combined <- bind_rows(dt2024,dt2019) %>% 
  mutate(Region = ifelse(Region %in% c("Atlantic Canada","Ontario"),
                         Region, "ISS"))


ncounts_by_year <- dt_combined %>% 
  group_by(version,YearCollected,Region) %>% 
  summarise(n_counts = n(),
            mean_count = mean(ObservationCount),
            max_count = max(ObservationCount)) %>% 
  arrange(Region,YearCollected)

nc_plot <- ggplot(data = ncounts_by_year,
                  aes(x = YearCollected,
                      y = n_counts,
                      colour = version))+
  geom_point(alpha = 0.5)+
  facet_wrap(facets = vars(Region),
             scales = "free_y",
             ncol = 1)

nc_plot



max_plot <- ggplot(data = ncounts_by_year,
                  aes(x = YearCollected,
                      y = max_count,
                      colour = version))+
  geom_point(alpha = 0.5)+
  facet_wrap(facets = vars(Region),
             scales = "free_y",
             ncol = 1)


max_plot


mean_plot <- ggplot(data = ncounts_by_year,
                   aes(x = YearCollected,
                       y = mean_count,
                       colour = version))+
  geom_point(alpha = 0.5)+
  facet_wrap(facets = vars(Region),
             scales = "free_y",
             ncol = 1)


mean_plot


ncounts_map <- dt_combined %>% 
  group_by(Region,version,SurveyAreaIdentifier,DecimalLatitude,DecimalLongitude) %>% 
  summarise(n_years  = length(unique(YearCollected))) %>% 
  filter(n_years > 10) %>% 
  st_as_sf(coords = c("DecimalLongitude","DecimalLatitude")) 

st_crs(ncounts_map) <- 4326
  
ncounts_spatial <- ggplot()+
  geom_sf(data = bbsBayes2::load_map("prov_state"),
          fill = NA)+
  geom_sf(data = ncounts_map,
          aes(colour =n_years)) +
  facet_wrap(vars(version))+
  scale_colour_viridis_c(direction = -1)

ncounts_spatial

all_plots_combined <- nc_plot + max_plot + mean_plot + plot_layout(guides = "collect")


pdf("comparison_by_region.pdf",
    height = 8.5,width = 11)
print(all_plots_combined)
dev.off()

# by species --------------------------------------------------------------

for(reg in c("Atlantic Canada","Ontario")){
  pdf(paste0("comparison_by_species_",reg,".pdf"),
      width = 11,
      height = 8.5)
ncounts_by_year <- dt_combined %>% 
  filter(Region == reg) %>% 
  group_by(version,YearCollected,Region,CommonName) %>% 
  summarise(n_counts = n(),
            mean_count = mean(ObservationCount),
            max_count = max(ObservationCount),
            quantile_95_count = unname(quantile(ObservationCount,0.95))) %>% 
  arrange(Region,YearCollected,CommonName)

max_plot <- ggplot(data = ncounts_by_year,
                   aes(x = YearCollected,
                       y = max_count,
                       colour = version))+
  geom_point(alpha = 0.5)+
  facet_wrap(vars(CommonName),
             scales = "free_y")


print(max_plot)


mean_plot <- ggplot(data = ncounts_by_year,
                    aes(x = YearCollected,
                        y = mean_count,
                        colour = version))+
  geom_point(alpha = 0.5)+
  facet_wrap(vars(CommonName),
             scales = "free_y")


print(mean_plot)

q_plot <- ggplot(data = ncounts_by_year,
                    aes(x = YearCollected,
                        y = quantile_95_count,
                        colour = version))+
  geom_point(alpha = 0.5)+
  facet_wrap(vars(CommonName),
             scales = "free_y")


print(q_plot)


dev.off()

}


for(reg in c("Atlantic Canada","Ontario")){
 dt1 <- dt_combined %>% 
   filter(Region == reg) %>% 
   mutate(lat = round(DecimalLatitude,2),
          long = round(DecimalLongitude,2)) %>% 
   group_by(YearCollected,lat,long,doy,
            CommonName) %>% 
   summarise(n_vers = length(unique(version)),
             sd_count = sd(ObservationCount))
  
 
 matches <- dt1 %>% 
   filter(n_vers > 1)
  
 
 dta <- dt2019%>% 
   filter(YearCollected < 2020 & YearCollected >1979,
          Region == reg) %>% 
   mutate(lat = round(DecimalLatitude,2),
          long = round(DecimalLongitude,2)) %>% 
   select(-c(Country,StateProvince,version,DecimalLatitude,DecimalLongitude,Region)) %>% 
   rename_with(.cols = c(SamplingEventIdentifier,
                         SurveyAreaIdentifier,
                         ObservationCount,
                         SiteName),
               .fn = ~paste0(.x,"_2019")) 

 
 
 dtb <- dt2024 %>% 
   filter(YearCollected < 2020 & YearCollected >1979,
          Region == reg) %>% 
   mutate(lat = round(DecimalLatitude,2),
          long = round(DecimalLongitude,2))%>% 
   select(-c(Country,StateProvince,version,DecimalLatitude,DecimalLongitude,Region)) %>% 
   rename_with(.cols = c(SamplingEventIdentifier,
                         SurveyAreaIdentifier,
                         ObservationCount,
                         SiteName),
               .fn = ~paste0(.x,"_2024")) 
 
 dt_bind <- full_join(dta,dtb) %>% 
   mutate(dif_count = ObservationCount_2024 - ObservationCount_2019) %>% 
   arrange(YearCollected,doy,lat,CommonName)
 
 dif_count <- dt_bind %>% 
   filter(dif_count != 0)
 
 miss <- dt_bind %>% 
   filter(is.na(dif_count))
 
 
 }


# Accessing data from NatureCounts ------------------------------------------------------

library(naturecounts)








