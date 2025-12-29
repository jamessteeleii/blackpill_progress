library(tidyverse)
library(patchwork)

#### all data ----
weight_data <- read_csv("withins_weight.csv") |>
  janitor::clean_names() |>
  mutate(
    date = round_date(ymd_hms(date), unit = "day")
  ) |>
  select(date, weight_kg)

withins_step_data <- read_csv("withins_aggregates_steps.csv") |>
  mutate(
    date = ymd(date),
    steps_withins = value
  ) |>
  select(-value)

google_step_data <- read_csv("google_fit_data.csv") |>
  janitor::clean_names() |>
  mutate(
    date = ymd(date),
    steps_google = step_count
  ) |>
  select(date, steps_google)

diet_data <- readxl::read_xlsx("macrofactor.xlsx", sheet = 1) |>
  janitor::clean_names() |>
  mutate(
    date = ymd(date)
  )

tee_data <- readxl::read_xlsx("macrofactor.xlsx", sheet = 6) |>
  janitor::clean_names() |>
  mutate(
    date = ymd(date)
  )

macrofactor_step_data <- readxl::read_xlsx("macrofactor.xlsx", sheet = 7) |>
  janitor::clean_names() |>
  mutate(
    date = ymd(date)
  )

density_data <- tibble(
  date = ydm(
    c(
      "2010-07-01",
      "2011-10-02",
      "2014-09-06",
      "2015-08-05",
      "2015-12-06",
      "2017-01-11",
      "2023-01-03",
      "2025-05-12"
    )
  ),
  density = c(
    1.0887,
    1.0782,
    1.0770,
    1.0802,
    1.0866,
    1.0872,
    1.0790,
    1.0932
  )
  ) |>
  mutate(
    siri_bf = ((4.95/density)-4.50)*100,
    brozek_bf = ((4.57/density)-4.142)*100,
    magee_bf = 21.125 - (280.823 * (density-1.058))
  )

# join all data together
data <- full_join(weight_data, withins_step_data, by = "date") |>
  full_join(google_step_data, by = "date") |>
  full_join(macrofactor_step_data, by = "date") |>
  full_join(density_data, by = "date") |>
  full_join(diet_data, by = "date") |>
  full_join(tee_data, by = "date") 

# calculate trend weight
alpha = 0.095238

# collapse all to one row
data <- data |>
  group_by(date) |>
  summarise(
    weight_kg = first(weight_kg),
    steps = {
      s <- c(steps, steps_google, steps_withins)
      if (all(is.na(s))) NA_real_ else max(s, na.rm = TRUE)
    },
    across(everything(), ~ first(.x)),
    .groups = "drop"
  )


data <- data |>
  arrange(date) |>
  mutate(
    trend_weight = accumulate(
      weight_kg,
      ~ ifelse(is.na(.y), .x, alpha * .y + (1 - alpha) * .x),
      .init = data |>
        filter(!is.na(weight_kg)) |>
        slice(1) |>
        pull(weight_kg)
    )[-1]
  )

data <- data |>
  filter(date <= "2025-12-18")
  

#### FFM estimates

fit_magee <- loess(
  magee_bf ~ as.numeric(date),
  data = data,
  span = 0.75,
  na.action = na.exclude
)


data <- data |>
  bind_cols(as_tibble(predict(fit_magee, newdata = data$date, se=TRUE))) |>
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

weight_ffm <- data |>
  filter(date >= "2020-01-01") |>
  ggplot(aes(x=date)) +
  # weight
  geom_line(aes(y=weight_kg), colour = "gray") +
  geom_line(aes(y=trend_weight), colour = "black") +
  
  # ffm
  geom_hline(yintercept = mean(data$trend_magee_ffm, na.rm=TRUE), linetype = "dashed", alpha = 0.5, color = "black") +
  geom_ribbon(aes(ymin=trend_magee_ffm_lower, ymax=trend_magee_ffm_upper), 
              alpha = 0, color = "red", linewidth = 0.25) +
  geom_line(aes(y=magee_ffm), color = "red", alpha = 0.25) +
  geom_line(aes(y=trend_magee_ffm), color = "red") +
  scale_x_date(limits = ymd(c("2010-01-01", "2025-12-18"))) +
  labs(
    y = "Weight (kg)",
    x = "Time"
  )

ffm_loess <- tibble(
    date = seq(min(data$date), max(data$date), by = "day")
    ) |>
  bind_cols(as_tibble(predict(fit_magee, newdata = seq(min(data$date), max(data$date), by = "day"), se=TRUE))) |>
  ggplot(aes(x=date)) +
  geom_hline(yintercept = mean(100 - data$magee_bf, na.rm=TRUE), linetype = "dashed", alpha = 0.5, color = "black") +
  geom_ribbon(aes(ymin=100-(fit-qt(0.975,df)*se.fit), ymax=100-(fit+qt(0.975,df)*se.fit)),
              alpha = 0.25, color = "red", fill = "red") +
  geom_line(aes(y=100-fit), color = "red") +
  geom_point(data = data, aes(y=100-magee_bf), color = "black", size = 2) +
  geom_point(data = data, aes(y=100-magee_bf), color = "red", size = 1) +
  labs(
    y = "FFM (%)",
    x = "Time"
  )




weight_ffm / ffm_loess






#### weight ----

data <- read.csv("weight.csv") |>
  janitor::clean_names() |>
  mutate(
    date = ymd_hms(date)
  )

alpha = 0.095238

data <- data |> 
  mutate(
    trend_weight = accumulate(
      weight_kg,
      ~ alpha * .y + (1 - alpha) * .x
    )
    ) |>
  select(-comments) |>
  pivot_longer(2:7,
               names_to = "variable",
               values_to = "value")


data |>
  filter(variable == "weight_kg" |
           variable == "trend_weight") |>
  mutate(variable = factor(variable, levels = c("weight_kg", "trend_weight"))) |>
  ggplot(aes(x=date, y=value, colour = variable)) +
  geom_line() +
  scale_colour_manual(values = c("grey", "black")) +
  labs(
    y = "Weight (kg)",
    x = "Time",
    color = "Variable"
  ) +
  theme(
    legend.position = "bottom"
  )



#### bodpod ----
density_data <- tibble(
  date = ydm(
    c(
      "2010-07-01",
      "2011-10-02",
      "2014-09-06",
      "2015-08-05",
      "2015-12-06",
      "2017-01-11",
      "2023-01-03",
      "2025-05-12"
    )
  ),
  density = c(
    1.0887,
    1.0782,
    1.0770,
    1.0802,
    1.0866,
    1.0872,
    1.0790,
    1.0932
  )
) |>
  mutate(
    siri_bf = ((4.95/density)-4.50)*100,
    brozek_bf = ((4.57/density)-4.142)*100,
    magee_bf = 21.125 - (280.823 * (density-1.058))
  ) |>
  pivot_longer(2:5,
               names_to = "variable",
               values_to = "value")

density_data |>
  filter(variable != "density") |>
  mutate(
   variable = case_when(
     variable == "siri_bf" ~ "Siri (1961)",
     variable == "brozek_bf" ~ "Brozek (1963)",
     variable == "magee_bf" ~ "Magee et al (2025)", # https://doi.org/10.1080/15502783.2025.2504578 
     
   ) 
  ) |>
  ggplot(aes(x=date, y=value, colour = variable)) +
  geom_point() +
  # scale_colour_manual(values = c("grey", "black")) +
  geom_smooth(se = FALSE) +
  labs(
    y = "Body Fat (%)",
    x = "Time",
    color = "Model"
  ) +
  theme(
    legend.position = "bottom"
  )

#### fat free mass

predict(loess(value ~ as.numeric(date),
      data = density_data |> filter(variable == "magee_bf")),
      newdata = 1:100)


?predict
