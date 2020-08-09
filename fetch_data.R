library(RSocrata)
library(tidyverse)
library(config)
library(tictoc)

# load config file
config <- config::get()

query_nyc_felony_crime <- function(dataset_id) {
  url <- str_glue(
    "https://data.cityofnewyork.us/resource/{dataset_id}.json?law_cat_cd=FELONY"
    )
  
  df <- RSocrata::read.socrata(url) %>% as_tibble()
  
  return(df)
}

# takes about 15 seconds
tic()
crime_ty <- query_nyc_felony_crime(config$crime_data$current_year_id)
toc()

# takes about 20 minutes
tic()
crime_historic <- query_nyc_felony_crime(config$crime_data$historical_id)
toc()

# combine to single dataset
all_crimes <- bind_rows(crime_ty, crime_historic)

rm(crime_ty, crime_historic)

# save data as RDS file
saveRDS(
  all_crimes, 
  file = fs::path(
    here::here(),
    config$local_data$dir,
    config$local_data$raw_dir,
    config$local_data$full_data_file,
    ext = "rds"
    )
)
