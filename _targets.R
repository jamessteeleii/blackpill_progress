# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "here",
    "patchwork",
    "ggtext"
  ),
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker"
)


tar_source("R/functions/.")
# tar_source("R/other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(

  # Read in and prepare each data source ----
  tar_target(
    weight_data_file,
    here("data", "withings_weight.csv"),
    format = "file"
  ),
  
  tar_target(
    weight_data,
    prep_weight_data(weight_data_file)
  ),
  
  tar_target(
    withings_step_data_file,
    here("data", "withings_aggregates_steps.csv"),
    format = "file"
  ),
  
  tar_target(
    withings_step_data,
    prep_withings_step_data(withings_step_data_file)
  ),
  
  tar_target(
    google_step_data_file,
    here("data", "google_fit_data.csv"),
    format = "file"
  ),
  
  tar_target(
    google_step_data,
    prep_google_step_data(google_step_data_file)
  ),
  
  tar_target(
    macrofactor_data_file,
    here("data", "macrofactor.xlsx"),
    format = "file"
  ),
  
  tar_target(
    diet_data,
    prep_diet_data(macrofactor_data_file)
  ),
  
  tar_target(
    tee_data,
    prep_tee_data(macrofactor_data_file)
  ),
  
  tar_target(
    macrofactor_step_data,
    prep_macrofactor_step_data(macrofactor_data_file)
  ),
  
  tar_target(
    bodpod_data,
    # create bodpod data
    tibble(
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
      weight_kg = c(
        68.336,
        67.800,
        70.774,
        72.066,
        NA_real_,
        NA_real_,
        NA_real_,
        70.561
      ),
      volume_L_1 = c(
        61.984,
        64.857,
        60.372,
        62.115,
        64.282,
        63.373,
        60.393,
        60.328
      ),
      volume_L_2 = c(
        62.1,
        64.688,
        60.661,
        62.185,
        64.277,
        63.206,
        60.571,
        60.422
      ),
      density_kg_L = c(
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
        weight_kg = if_else(is.na(weight_kg), mean(c(volume_L_1, volume_L_2)) * density_kg_L, weight_kg),
        siri_bf = ((4.95/density_kg_L)-4.50)*100,
        brozek_bf = ((4.57/density_kg_L)-4.142)*100,
        magee_bf = 21.125 - (280.823 * (density_kg_L-1.058))
      )
  ),
  
  tar_target(
    all_data_raw,
    data <- full_join(weight_data, withings_step_data, by = "date") |>
      full_join(google_step_data, by = "date") |>
      full_join(macrofactor_step_data, by = "date") |>
      full_join(bodpod_data, by = "date") |>
      full_join(diet_data, by = "date") |>
      full_join(tee_data, by = "date")
  ),
  
  # collapse to single row per date and handle multiple step sources
  tar_target(
    all_data_collapsed,
    collapse_and_steps(all_data_raw)
  ),
  
  # calculate trend weight and add to data
  tar_target(
    all_data_trend_weight,
    calculate_trend_weight(all_data_collapsed)
  ),
  
  # filter to date of DEXA post cut
  tar_target(
    all_data_prepared,
    all_data_trend_weight |>
      filter(date <= "2025-12-18")
  ),
  
  # Cut phase analysis/plots ----
  
  tar_target(
    cut_data,
    all_data_prepared |>
      filter(date >= "2025-09-03")
  ),
  
  tar_target(
    weight_loss,
    calculate_weight_loss(cut_data)
  ),
  
  tar_target(
    weight_loss_plot,
    plot_weight_loss(cut_data, weight_loss)
  ),
  
  tar_target(
    energy_deficit,
    calculate_energy_deficit(cut_data)
  ),
  
  tar_target(
    kcal_plot,
    plot_kcal(cut_data, energy_deficit)
  ),
  
  tar_target(
    weight_energy_plot,
    (weight_loss_plot / kcal_plot) +
      plot_annotation(caption = "*This is the same as used in MacroFactors trended weight")
  ),
  
  tar_target(
    steps,
    calculate_steps(cut_data)
  ),
  
  tar_target(
    steps_plot,
    plot_steps(cut_data, steps)
  ),
  
  tar_target(
    macros_data,
    prep_macros_data(cut_data)
  ),
  
  tar_target(
    macros_averages,
    calculate_macros(macros_data)
  ),
  
  tar_target(
    macros_plot,
    plot_macros(cut_data, macros_averages)
  ),
  
  
  # Longer term reflection ----
  
  # Weight
  tar_target(
    weight_plot,
    plot_weight(all_data_prepared)
  ),
  
  # Fat free mass
  tar_target(
    bf_loess,
    fit_magee <- loess(
      magee_bf ~ as.numeric(date), # use the Magee model for bodpod density data
      data = all_data_prepared,
      na.action = na.exclude
    )
  ),
  
  tar_target(
    ffm_estimate_data,
    estimate_ffm(all_data_prepared, bf_loess)
  ),
  
  tar_target(
    ffm_percent_plot,
    plot_ffm_percent(ffm_estimate_data, bf_loess)
  ),
  
  tar_target(
    ffm_kgs_plot,
    plot_ffm_kgs(ffm_estimate_data)
  ),
  
  # Are my biceps bigger? lol
  
  tar_target(
    image_dates,
    get_image_dates()
  ),
  
  tar_target(
    image_date_data,
    image_dates |>
      left_join(all_data_prepared, by = "date")
  )
  
)
