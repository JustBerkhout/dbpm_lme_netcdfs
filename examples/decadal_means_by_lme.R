library(tidyverse)

#loads the lme scale gcm data 
#load the LME shape file
#isolates lme id and name from the shapefile dataframe (discard rest)
#selects 1 random variable from all available variables in gcm data (sst, sbt, lhpy or sphy)
#wrangles and plots data
#  join gcm data by lme id to LME names from shapefile
#  filter for the randomly selected variable
#  creates decades from the month-data (from gcm data)
#  generate mean, group by lme, decade, scenario
#  plot in facetted (by lme) line plots


dat <- readRDS("/rd/gem/private/fishmip_inputs/gcm_lmescale/lme_scale_gcm_inputs.rds")

lmes <- rgdal::readOGR(dsn ="/rd/gem/private/fishmip_inputs/misc/LMEs66/", layer = "LMEs66", verbose = FALSE)
lme_dat <- lmes@data %>% 
  select(LME_NUMBER, LME_NAME) %>% 
  mutate(LME_NAME = as.character(LME_NAME))
rm(lmes)

available_variables <- as.character(unique(dat$variable))
selected_variable <- sample(available_variables,1)

dat %>% 
  inner_join(lme_dat, by=c("lme" = "LME_NUMBER")) %>% 
  filter(variable == selected_variable) %>% 
  mutate(decade = 10 * as.numeric(substr(month, 1, 3))  ) %>% 
  group_by (LME_NAME, decade, scenario) %>% 
  summarise(value = mean(value)) %>% 
  ggplot(aes(decade, value, color=scenario)) +
    geom_line()+
    facet_wrap(vars(LME_NAME)) +
    ggtitle(sprintf("Decadal mean %s by LME (1950-2100)", selected_variable)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    


  
  