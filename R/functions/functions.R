#### Functions for preparing data ----

prep_weight_data <- function(file) {
  weight_data <- read_csv(file) |>
    janitor::clean_names() |>
    mutate(
      date = round_date(ymd_hms(date), unit = "day")
    ) |>
    select(date, weight_kg)
  
  return(weight_data)
}

prep_withings_step_data <- function(file) {
  withings_step_data <- read_csv(file) |>
    mutate(
      date = ymd(date),
      steps_withings = value
    ) |>
    select(-value)
  
  return(withings_step_data)
}

prep_google_step_data <- function(file) {
  google_step_data <- read_csv(file) |>
    janitor::clean_names() |>
    mutate(
      date = ymd(date),
      steps_google = step_count
    ) |>
    select(date, steps_google)
  
  return(google_step_data)
}

prep_diet_data <- function(file) {
  diet_data <- readxl::read_xlsx(file, sheet = 1) |>
    janitor::clean_names() |>
    mutate(
      date = ymd(date)
    )
  
  return(diet_data)
}
  
prep_tee_data <- function(file) {
  tee_data <- readxl::read_xlsx(file, sheet = 6) |>
    janitor::clean_names() |>
    mutate(
      date = ymd(date)
    )
  
  return(tee_data)
}

prep_macrofactor_step_data <- function(file) {
  macrofactor_step_data <- readxl::read_xlsx(file, sheet = 7) |>
    janitor::clean_names() |>
    mutate(
      date = ymd(date)
    )
  
  return(macrofactor_step_data)
}

collapse_and_steps <- function(data) {
  
  # collapse all to one row and average across step sources when multiple
  data <- data |>
    group_by(date) |>
    summarise(
      weight_kg = {
        w <- c(weight_kg.x, weight_kg.y)
        if (all(is.na(w))) NA_real_ else mean(w, na.rm = TRUE)
      },
      weight_kg = first(weight_kg),
      steps = {
        s <- c(steps, steps_google, steps_withings)
        if (all(is.na(s))) NA_real_ else mean(s, na.rm = TRUE)
      },
      across(everything(), ~ first(.x)),
      .groups = "drop"
    ) |>
    select(-weight_kg.x, -weight_kg.y,
           -steps_google, -steps_withings)
  
  return(data)
}

calculate_trend_weight <- function(data) {
  
  # alpha for 20-day exponentially weighted moving average
  alpha = 0.095238
  
  data <- data |>
    arrange(date) |>
    mutate(
      trend_weight = accumulate(
        weight_kg,
        ~ ifelse(is.na(.y), .x, alpha * .y + (1 - alpha) * .x),
        .init = data |>
          # only calculate from 2020 onwards due to data availability
          filter(date >= "2020-01-01") |>
          filter(!is.na(weight_kg)) |>
          slice(1) |>
          pull(weight_kg)
      )[-1]
    )
  
}

#### Functions for analysis and plots ----

estimate_ffm <- function(data, bf_loess) {
  data <- data |>
    bind_cols(as_tibble(predict(bf_loess, newdata = data$date, se=TRUE))) |>
    select(-residual.scale) |>
    rename(
      magee_loess_fit = "fit",
      magee_loess_se = "se.fit"
    ) |>
    mutate(
      magee_loess_lower = magee_loess_fit - qt(0.975,df)*magee_loess_se,
      magee_loess_upper = magee_loess_fit + qt(0.975,df)*magee_loess_se
    ) |>
    
    # calculate ffm based on scale and trend weight
    mutate(
      magee_ffm = (1-(magee_loess_fit/100)) * weight_kg,
      trend_magee_ffm = (1-(magee_loess_fit/100)) * trend_weight,
      
      magee_ffm_lower = (1-(magee_loess_lower/100)) * weight_kg,
      trend_magee_ffm_lower = ((1-(magee_loess_lower/100)) * trend_weight),
      
      magee_ffm_upper = (1-(magee_loess_upper/100)) * weight_kg,
      trend_magee_ffm_upper = ((1-(magee_loess_upper/100)) * trend_weight),
    ) 
}