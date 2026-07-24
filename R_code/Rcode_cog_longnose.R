#Installing from GitHub indexwc

pak::pkg_install("pfmc-assessments/indexwc")
#pak::pkg_install("pfmc-assessments/indexwc@main")

setwd("C:/Users/vladlena.gertseva/Desktop/Center gravity")

library(dplyr)
library(indexwc)
library(purrr)
library(sdmTMB)
library(tibble)


configuration_ytk <- configuration |>
  dplyr::filter(species == "longnose skate")

configuration_ytk_wcgbt <- configuration_ytk |>
  dplyr::filter(source == "NWFSC.Combo" & family == "sdmTMB::delta_gamma()")

# Claire code, slightly modified

  pulled_data <- nwfscSurvey::pull_catch(
  common_name = "longnose skate",
survey = "NWFSC.Combo")


data_filtered <- format_data(pulled_data) |>
  dplyr::filter(depth <= configuration_ytk_wcgbt$min_depth[1], depth >= configuration_ytk_wcgbt$max_depth[1],
                latitude >= configuration_ytk_wcgbt$min_latitude[1], latitude <= configuration_ytk_wcgbt$max_latitude[1],
                year >= configuration_ytk_wcgbt$min_year[1], year <= configuration_ytk_wcgbt$max_year[1])



#back to vignette

#Statistical model

configuration_ytk_wcgbt[, "formula"]

configuration_to_run <- configuration_ytk_wcgbt |>
  dplyr::filter(
    formula == "catch_weight ~ 0 + fyear + pass_scaled"
  )

  #Preparing the data

#  data("yellowtail")

data <- data_filtered |>
  dplyr::filter(
    depth <= configuration_to_run$min_depth,
    depth >= configuration_to_run$max_depth,
    latitude >= configuration_to_run$min_latitude,
    latitude <= configuration_to_run$max_latitude,
    year >= configuration_to_run$min_year,
    year <= configuration_to_run$max_year
  )

  #Fitting the model with indexwc and sdmTMB

family_obj <- eval(rlang::parse_expr(configuration_to_run$family))

fit <- run_sdmtmb(
  dir_main = NULL,
  data,
  family = family_obj,
  formula = configuration_to_run$formula,
  n_knots = configuration_to_run$knots,
  share_range = configuration_to_run$share_range,
  sdmtmb_control = sdmTMB::sdmTMBcontrol(newton_loops = 3)
)

  #Examining diagnostics

  family_obj <- eval(rlang::parse_expr(configuration_to_run$family))

fit <- run_sdmtmb(
  dir_main = NULL,
  data,
  family = family_obj,
  formula = configuration_to_run$formula,
  n_knots = configuration_to_run$knots,
  share_range = configuration_to_run$share_range,
  spatial = "on",
  spatiotemporal = list("iid","iid"),
  sdmtmb_control = sdmTMB::sdmTMBcontrol(newton_loops = 1)
)

#Next, we can run the diagnostics with the California Current prediction grid

pred_grid <- sdmTMB::replicate_df(california_current_grid,
  time_name = "year",
  time_values = unique(data$year)
)
pred_grid$fyear <- as.factor(pred_grid$year)

diagnostics <- diagnose(
  dir = NULL,
  fit = fit,
  prediction_grid = pred_grid
)

diagnostics$sanity

diagnostics$model

diagnostics$formula

diagnostics$loglike
#> 'log Lik.' -123.9427 (df=43)
diagnostics$aic
#> [1] 333.8853

diagnostics$effects

diagnostics$mesh_plot
diagnostics$qq_plot
diagnostics$anisotropy_plot
diagnostics$fixed_effects_plot
diagnostics$density_plots[[10]]

plot1 <- diagnostics$residual_maps_by_year[[1]]

ggforce::n_pages(plot1) # check number of pages
#> [1] 10
# View page 2
print(plot1 + ggforce::facet_wrap_paginate("year", nrow = 1, ncol = 2, page = 2))

diagnostics$date
diagnostics$session_info
diagnostics$data_with_residuals
diagnostics$predictions

# Calculating indices

available_areas()

index <- calc_index_areas(
  data = fit$data,
  fit = fit,
  prediction_grid = pred_grid,
  boundaries = c("Coastwide", "WA", "OR", "CA", "North of Cape Mendocino","South of Cape Mendocino","North of Monterey Bay", "South of Monterey Bay", "North of Point Conception", "South of Point Conception"),
  cog = TRUE, # added
  bias_correct = FALSE, # added
  dir = NULL
)

index$plot_indices
index$indices
index$cogs

#Saving output
save_index_outputs(
  fit = fit,
  diagnostics = diagnostics,
  indices = index,
  dir_main = paste0(getwd(), "/longnose_cog")
)


write.csv(index$cogs,"longnose_cog.csv")
