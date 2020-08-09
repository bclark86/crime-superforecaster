library(tidyverse)
library(lubridate)
library(config)

# 0. SETUP ----
# Sys.setenv(R_CONFIG_ACTIVE = "default")

config <- config::get()
felony_categories <- config$felony_categories

# 1. DATA ----
crimes <- readRDS(
  file = fs::path(
    here::here(),
    config$local_data$dir,
    config$local_data$raw_dir,
    config$local_data$full_data_file,
    ext = "rds"
  )
)

# 1.1 Profile Data ----

crimes %>% glimpse()

crimes %>% skimr::skim()

crimes %>% pull(ofns_desc) %>% unique()

# 1.2 Daily Aggregation ----
crimes_daily <- crimes %>%
  group_by(rpt_dt, boro_nm, ofns_desc) %>%
  summarize(n = n(), .groups = "keep") %>%
  ungroup() %>%
  mutate(
    quarter  = quarter(rpt_dt, with_year = T),
    category = case_when(
      ofns_desc %in% felony_categories$violent ~ "violent",
      ofns_desc %in% felony_categories$sprecial_victims ~ "special victims",
      ofns_desc %in% felony_categories$property_financial ~ "property/financial",
      TRUE ~ "other"
    )
  )

rm(crimes)

# 1.3 Save Daily Data (RDS) ----
saveRDS(
  crimes_daily, 
  file = fs::path(
    here::here(),
    config$local_data$dir,
    config$local_data$app_dir,
    config$local_data$daily_data_file,
    ext = "rds"
  )
)

