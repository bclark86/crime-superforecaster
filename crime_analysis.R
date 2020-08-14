library(leaflet)
library(leaflet.extras)
library(tidyverse)

# 0. SETUP ----
# Sys.setenv(R_CONFIG_ACTIVE = "default")

config <- config::get()
felony_categories <- config$felony_categories

# 1. DATA ---- 

# 1.1 Load Data ----
crimes <- readRDS(
  file = fs::path(
    here::here(),
    config$local_data$dir,
    config$local_data$raw_dir,
    config$local_data$full_data_file,
    ext = "rds"
  )
)

# 1.2 Filter Data ----
selected_borough <- c("BROOKLYN")

crimes_filtered_tbl <- crimes %>%
  filter(
    # str_to_lower(category) %in% str_to_lower(selected_categories),
    str_to_lower(boro_nm) == str_to_lower(selected_borough)
  ) %>%
  mutate(
    quarter = rpt_dt %>% lubridate::quarter(with_year = T)
  )

# 2. TRENDS ----

# 2.1 YoY Comparison ----
quarter_ty <- crimes_filtered_tbl$quarter %>% max()
quarter_ly <- quarter_ty - 1

quarter_crimes_ty <- crimes_filtered_tbl %>%
  filter(quarter == quarter_ty) %>%
  group_by(ofns_desc) %>%
  summarise(n = n(), .groups = "keep") %>%
  arrange(desc(n))

quarter_crimes_ly <- crimes_filtered_tbl %>%
  filter(quarter == quarter_ly) %>%
  group_by(ofns_desc) %>%
  summarise(n = n(), .groups = "keep") %>%
  arrange(desc(n))

ofns_desc_pct_change_tbl <- quarter_crimes_ty %>%
  left_join(
    quarter_crimes_ly,
    by = "ofns_desc", 
    suffix = c("_ty", "_ly")
  ) %>%
  mutate(
    pct_change_yoy = n_ty / n_ly - 1
  )

# 3. MAP ----
crimes_map_tbl <- crimes_filtered_tbl %>%
  filter(quarter == quarter_ty) %>%
  mutate_at(vars(c("latitude", "longitude")), as.numeric)


m <- leaflet() %>% 
  setView(
    lng = -73.935242,
    lat = 40.730610, 
    zoom = 12
  ) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap)

m %>% addHeatmap(map, lng = crimes_map_tbl$longitude, lat = crimes_map_tbl$latitude, intensity = NULL,
             layerId = NULL, group = NULL, minOpacity = 0.5, max = 1,
             radius = 15, blur = 50, gradient = NULL, cellSize = NULL)

# 4. FORECAST ----