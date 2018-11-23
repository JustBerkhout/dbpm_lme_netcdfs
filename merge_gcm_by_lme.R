#merge files into one rds
library(tidyverse)
library(lubridate)

my_working_path <- "/rd/gem/private/users/justb/gcm_lmescale_v2/"

lme_depths <- readRDS(paste0(my_working_path, "/lme_depths.rds"))


df <- list.files(my_working_path, "^hist|^rcp", full.names = TRUE) %>% 
  map_dfr(readRDS) %>% 
  mutate(
    scenario = factor(scenario),
    variable = factor(variable)
  ) %>% 
  inner_join(lme_depths) %>% 
  select(-min, -max, -mean) %>% 
  rename(aw_mean_depth = aw_mean) %>% 
  mutate(ts_abs = ifelse(scenario=="historical", ts, ts + 672)) %>% 
  mutate(month = substr(as.character(ymd("1950-01-15") %m+% months(ts_abs-1 )), 1, 7)) %>% 
  select(-ts_abs)

saveRDS(df, paste0(my_working_path, "/lme_scale_gcm_inputs.rds"))
