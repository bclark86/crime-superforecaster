library(leaflet)
library(leaflet.extras)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(lubridate)

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


m <- leaflet(data = crimes_map_tbl) %>% 
  setView(
    lng = mean(crimes_map_tbl$longitude),
    lat = mean(crimes_map_tbl$latitude), 
    zoom = 11
  ) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap)

m %>% 
  addHeatmap(
    map, 
    lng = crimes_map_tbl$longitude,
    lat = crimes_map_tbl$latitude,
    intensity = NULL,
    layerId = NULL, 
    group = NULL, 
    minOpacity = 0.85,
    max = 1,
    radius = 15,
    blur = 50,
    gradient = NULL,
    cellSize = NULL
  )

# 4. FORECAST ----
# * Data Preparation ----
crimes_daily_tbl <- crimes_filtered_tbl %>%
  group_by(rpt_dt) %>%
  summarize(n = n()) %>%
  ungroup()

crimes_daily_tbl %>%
  plot_time_series(rpt_dt, n)

data_prepared_tbl <- crimes_daily_tbl %>%
  mutate(n = ifelse(n == 0, NA, n)) %>%
  mutate(n = ts_impute_vec(n, period = 12)) %>%
  rename(date = rpt_dt, value = n)

# * Train/Test ----
splits <- time_series_split(data_prepared_tbl,
                            assess = "3 months",
                            cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value) 

# * ARIMA ----
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(
    value ~ date, 
    data = training(splits)
  )

# * LINEAR REGRESSION ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * LINEAR REGRESSION - NO TREND ----
model_fit_lm_no_trend <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    value ~ month(date, label = TRUE), 
    data = training(splits)
  )

# * PROPHET ----
model_fit_prophet <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(
    value ~ date, 
    data = training(splits)
  )

# * RANDOM FOREST ----
model_fit_rf <- rand_forest(mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * XGBOOST ----
model_fit_xgboost <- boost_tree(mode = "regression") %>%
  set_engine("xgboost") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * SVM - Polynomial ----
model_fit_svm_poly <- svm_poly(mode = "regression") %>%
  set_engine("kernlab") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * SVM - RBF ----
model_fit_svm_rbf <- svm_rbf(mode = "regression") %>%
  set_engine("kernlab") %>%
  fit(
    value ~ as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * PROPHET BOOST ----
model_fit_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost") %>%
  fit(
    value ~ date + as.numeric(date) + month(date, label = TRUE), 
    data = training(splits)
  )

# * ARIMA BOOST ----
# model_fit_arima_boost <- arima_boost() %>%
#   set_engine("auto_arima_xgboost") %>%
#   fit(
#     value ~ date + as.numeric(date) + month(date, label = TRUE), 
#     data = training(splits)
#   )

# 3.0 MODELTIME FORECAST WORKFLOW ----

# * Modeltime Table ----
model_tbl <- modeltime_table(
  model_fit_arima,
  model_fit_lm,
  model_fit_lm_no_trend,
  model_fit_prophet,
  model_fit_rf,
  model_fit_xgboost,
  model_fit_svm_poly,
  model_fit_svm_rbf,
  model_fit_prophet_boost
  # model_fit_arima_boost
)

# * Calibrate ----
calibration_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(resizable = TRUE, bordered = TRUE)

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits), 
    actual_data = data_prepared_tbl,
    conf_interval = 0.80
  ) %>%
  plot_modeltime_forecast(.legend_show = TRUE, 
                          .legend_max_width = 25)

# * Refit ----
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data_prepared_tbl) 

forecast_tbl <- refit_tbl %>%
  modeltime_forecast(
    h = "3 months",
    actual_data = data_prepared_tbl,
    conf_interval = 0.80
  ) 

forecast_tbl %>%
  plot_modeltime_forecast(.interactive = TRUE)

# 4.0 MODEL AVERAGING ----

# * Mean Forecast ----
mean_forecast_tbl <- forecast_tbl %>%
  filter(.key != "actual") %>%
  group_by(.key, .index) %>%
  summarise(across(.value:.conf_hi, mean)) %>%
  mutate(
    .model_id   = 10,
    .model_desc = "AVERAGE OF MODELS"
  )

# * Visualize Mean Forecast ----
forecast_tbl %>%
  filter(.key == "actual") %>%
  bind_rows(mean_forecast_tbl) %>%
  plot_modeltime_forecast()
