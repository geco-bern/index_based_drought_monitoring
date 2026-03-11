# Basic xgboost model with limited
# hyperparameter tuning

# load the ecosystem
library(tidymodels)
library(ranger)
library(dplyr)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(tidyterra)
set.seed(0)

# read in precompiled model
regression_model <- readRDS(
  here::here("data/model_rf.rds")
)

files <- data.frame(
  file = list.files("data-raw/modis_data_spatial/", "*.tif", full.names = TRUE)
)

doys <- 180:300

lapply(doys, function(doy){

  files <- files |>
    dplyr::filter(
      grepl(sprintf("doy2018%s",as.character(doy)),file)
    )

  r <- terra::rast(files$file)

  # the model only works when variable names
  # are consistent we therefore rename them
  band_names <- data.frame(
    name = names(r)
  ) |>
    mutate(
      date = as.Date(substr(name, 40, 46), format = "%Y%j"),
      name = substr(name, 13, 35)
    )

  # reassign the names of the terra image stack
  names(r) <- band_names$name

  # return probabilities, where each class is
  # associated with a layer in an image stack
  # and the probabilities reflect the probabilities
  # of the classification for said layer
  p <- terra::predict(
    r,
    regression_model
  )

  # grab country polygons from world map
  # restrict to selected country
  country <- ne_countries(
    scale = 50,
    returnclass = "sf"
  ) |>
    dplyr::filter(
      sovereignt %in%  c("Switzerland", "Germany", "Austria")
    ) |>
    sf::st_union() |>
    sf::st_as_sf()

  p <- ggplot() +
    geom_spatraster(
      data = p
    ) +
    scale_fill_viridis_c(
      limits = c(0, 1.5)
    ) +
    geom_sf(data = country, colour = "white", fill = NA) +
    labs(
      title = doy
    )

  ggsave(filename = sprintf("manuscript/regression/%s.png", as.character(doy)), p, width = 5, height = 5)
})
