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

# Cut phase
calculate_weight_loss <- function(data) {
  weight_loss <- data |>
    arrange(date) |>
    summarise(
      start_weight = first(trend_weight),
      end_weight = last(trend_weight)
    ) |>
    mutate(
      loss = start_weight - end_weight
    )
  
  return(weight_loss)
}

plot_weight_loss <- function(data, weight_loss) {
  weight_plot <- data |>
    ggplot(aes(x=date)) +
    geom_line(
      aes(y = weight_kg, colour = "Raw Scale Weight"),
    ) +
    geom_line(
      aes(y = trend_weight, colour = "Trend Weight"),
    ) +
    scale_colour_manual(
      name = NULL,
      values = c("Raw Scale Weight" = "grey70",
                 "Trend Weight" = "black")
    ) +
    annotate("text",
             x = as.numeric(ymd("2025-11-15")),
             y = 75,
             label = glue::glue("Start weight = {round(weight_loss$start_weight,2)} kg\nEnd weight = {round(weight_loss$end_weight,2)} kg\nWeight loss = {round(weight_loss$loss,2)} kg")) +
    scale_x_date(limits = ymd(c("2025-09-03", "2025-12-18"))) +
    labs(
      y = "Weight (kg)",
      x = "Time",
      title = "Scale and trend weight during cut"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  return(weight_plot)
}

calculate_energy_deficit <- function(data) {
  energy_deficit <- data |>
    mutate(
      energy_diff = calories_kcal - expenditure
    ) |>
    summarise(
      average_diff = mean(energy_diff, na.rm=TRUE),
      sd_diff = sd(energy_diff, na.rm=TRUE)
    )
  
  return(energy_deficit)
}

plot_kcal <- function(data, energy_deficit) {
  kcal_plot <- data |>
    ggplot(aes(x=date, y=calories_kcal)) +
    geom_col(
      aes(y = calories_kcal, fill = "Intake"),
      color = "black"
    ) +
    geom_line(
      aes(y = expenditure, colour = "Expenditure"),
      linewidth = 1
    ) +
    scale_fill_manual(
      name = NULL,
      values = c("Intake" = "grey70")
    ) +
    scale_colour_manual(
      name = NULL,
      values = c("Expenditure" = "red")
    ) +
    annotate("text",
             x = as.numeric(ymd("2025-10-01")),
             y = 7000,
             label = glue::glue("Mean (SD) energy deficit = {round(energy_deficit$average_diff)} ({round(energy_deficit$sd_diff)}) kcal")) +
    scale_x_date(limits = ymd(c("2025-09-03", "2025-12-18"))) +
    labs(
      y = "Energy (kcal)",
      x = "Time",
      title = "Total energy intake and expenditure during cut"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  return(kcal_plot)
}

calculate_steps <- function(data) {
  steps <- data |>
    summarise(
      mean_steps = mean(steps, na.rm=TRUE),
      sd_steps = sd(steps, na.rm=TRUE)
    )
  
  return(steps)
}

plot_steps <- function(data, steps) {
  steps_plot <- data |>
    ggplot(aes(x=date, y=steps)) +
    geom_col(color = "black", fill = "grey70") +
    geom_text(
      data = steps,
      aes(
        x = as.numeric(ymd("2025-10-30")),
        y = 22500,
        label = glue::glue("Mean (SD) = {round(mean_steps)} ({round(sd_steps)}) steps")
      )
    ) +
    scale_x_date(limits = ymd(c("2025-09-03", "2025-12-18"))) +
    labs(
      y = "Total steps",
      x = "Time",
      title = "Total daily steps during cut"
    ) +
    theme_bw()
  
  return(steps_plot)
}

prep_macros_data <- function(data) {
  macros_data <- data |>
    select(date, fat_g, carbs_g, protein_g) |>
    pivot_longer(2:4,
                 names_to = "macro",
                 values_to = "grams") |>
    mutate(
      macro = case_when(
        macro == "carbs_g" ~ "Carbohydrates",
        macro == "fat_g" ~ "Fats",
        macro == "protein_g" ~ "Proteins"
      )
    )
  
  return(macros_data)
}

calculate_macros <- function(data) {
  macros_averages <- data |>
    group_by(macro) |>
    summarise(
      mean_g = mean(grams, na.rm=TRUE),
      sd_g = sd(grams, na.rm=TRUE)
    )
  
  return(macros_averages)
}

plot_macros <- function(data, macros_averages) {
  macros_plot <- data |>
    ggplot(aes(x=date, y=grams)) +
    geom_col(aes(fill=macro)) +
    ggh4x::facet_wrap2("macro", scales = "free_y") +
    ggh4x::facetted_pos_scales(
      y = list(
        NULL,
        NULL,
        scale_y_continuous(expand = expansion(mult = c(0, 0.25)))
      )
    ) +
    geom_text(
      data = macros_averages,
      aes(
        x = Inf,
        y = Inf,
        label = glue::glue("Mean (SD) = {round(mean_g)} ({round(sd_g)}) grams")
      ),
      vjust = 3, hjust = 1.5,
      size = 2
    ) +
    scale_x_date(limits = ymd(c("2025-09-03", "2025-12-18"))) +
    scale_fill_manual(values = c("#08C343", "#FFD15F", "#CE5400")) +
    labs(
      y = "Intake (grams)",
      x = "Time",
      title = "Total macronutrient intake during cut",
      fill = "Macronutrient"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  return(macros_plot)
}

# Longer term reflection
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