#extract pcwd and t2m data for centrale europe
#for june, july, and august of 2018 and 2020

#load packages
library(terra)
library(dplyr)
library(patchwork)
library(scico)
library(raster)
library(tidymodels)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(purrr)
library(lubridate)


#read pcwd and t2m data
#only data that covers central europe
#inspect pcwd
pcwd <- readRDS("/home/yousra/pcwd/pcwd/ERA5Land_pcwd_LON_+000.100.rds")
str(pcwd$data[[1]])
head(pcwd)

#Central Europe bounding box

country <- sf::st_bbox(
  c(xmin = 2, ymin = 45, xmax = 17, ymax = 53),
  crs = sf::st_crs(4326)
) |> sf::st_as_sfc() |> sf::st_sf()

# Convert to terra extent
bbox_ext <- terra::ext(terra::vect(country))

# List RDS files

path <- "/home/yousra/pcwd/pcwd/"
files <- list.files(path, pattern = "^ERA5Land_pcwd_LON_.*\\.rds$", full.names = TRUE)

#Read and crop each longitude tile
rasters_data <- map(files, function(f) {
  rds <- readRDS(f)
  # Crop to bounding box
  rds_subset <- rds %>%
    filter(lon >= bbox_ext$xmin & lon <= bbox_ext$xmax,
           lat >= bbox_ext$ymin & lat <= bbox_ext$ymax)

  return(rds_subset)
})

# Combine all tiles into one dataframe
pcwd <- bind_rows(rasters_data)


# Extract JJA monthly deficits for 2018 and 2020

pcwd_jja <- pcwd %>%
  mutate(
    # Extract daily data (df) from the list column
    df = map(data, "df"),
    # Summarize by month for JJA 2018 and 2020
    monthly = map(df, ~ .x %>%
                    filter(year(date) %in% c(2018, 2020),
                           month(date) %in% 6:8) %>%
                    mutate(year = year(date),
                           month = month(date)) %>%
                    group_by(year, month) %>%
                    summarise(pcwd_era5 = mean(deficit, na.rm = TRUE), .groups = "drop") #rename parameter to match the model
    )
  ) %>%
  select(lon, lat, monthly)


pcwd_jja$monthly[[1]]

# save rasters
out_dir <- "data-raw/pcwd_JJA"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Extract all year–month combinations present in your data
year_months <- pcwd_jja$monthly %>%
  map_df(~ .x) %>%
  distinct(year, month)

# Loop over each combination and create raster
walk2(year_months$year, year_months$month, function(y, m) {

  # Extract deficit for that year & month from each cell
  df <- pcwd_jja %>%
    mutate(value = map_dbl(monthly, function(x) {
      val <- x %>%
        filter(year == y, month == m) %>%
        pull(pcwd_era5)
      if (length(val) == 0) NA_real_ else val
    })) %>%
    select(lon, lat, value)

  # Convert to terra raster
  r <- terra::rast(df, type = "xyz", crs = "EPSG:4326")

  # Name and write the raster
  fname <- sprintf("%s/pcwd_%d_%02d.tif", out_dir, y, m)
  writeRaster(r, fname, overwrite = TRUE)

  message("saved: ", fname)
})

#do the same for t2m
# List RDS files
path <- "/home/yousra/pcwd/t2m/"
files <- list.files(path, pattern = "^ERA5Land_UTCDaily_t2m_LON_.*\\.rds$", full.names = TRUE)

#Read and crop each longitude tile
rasters_data <- map(files, function(f) {
  rds <- readRDS(f)
  # Crop to bounding box
  rds_subset <- rds %>%
    filter(lon >= bbox_ext$xmin & lon <= bbox_ext$xmax,
           lat >= bbox_ext$ymin & lat <= bbox_ext$ymax)

  return(rds_subset)
})

# Combine all tiles into one dataframe
t2m <- bind_rows(rasters_data)

str(t2m$data[[1]])

# Extract JJA monthly deficits for 2018 and 2020
t2m_jja <- t2m %>%
  mutate(
    df = map(data, ~ {
      if (is.null(.x)) return(NULL)
      .x %>%
        mutate(date = as.Date(datetime))  # convert from character
    }),
    monthly = map(df, ~ {
      if (is.null(.x)) return(tibble(year = NA, month = NA, mean_t2m = NA))

      .x %>%
        filter(year(date) %in% c(2018, 2020),
               month(date) %in% 6:8) %>%
        mutate(year = year(date), month = month(date)) %>%
        group_by(year, month) %>%
        summarise(t2m_era5 = mean(mean_t2m, na.rm = TRUE), .groups = "drop")
    })
  ) %>%
  select(lon, lat, monthly)


print(t2m_jja$monthly[[1]])


# save rasters
out_dir <- "data-raw/t2m_JJA"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Extract all year–month combinations
year_months <- t2m_jja$monthly %>%
  map_df(~ .x) %>%
  distinct(year, month)

# Loop over each combination and create raster
walk2(year_months$year, year_months$month, function(y, m) {

  # Extract t2m for that year & month from each cell
  df <- t2m_jja %>%
    mutate(value = map_dbl(monthly, function(x) {
      val <- x %>%
        filter(year == y, month == m) %>%
        pull(t2m_era5)
      if (length(val) == 0) NA_real_ else val
    })) %>%
    select(lon, lat, value)

  # Convert to terra raster
  r <- terra::rast(df, type = "xyz", crs = "EPSG:4326")

  # Name and write the raster
  fname <- sprintf("%s/t2m_%d_%02d.tif", out_dir, y, m)
  writeRaster(r, fname, overwrite = TRUE)

  message("saved: ", fname)
})


#do same for ssrd

#do the same for t2m
# List RDS files

path <- "/home/yousra/pcwd/ssrd/"
files <- list.files(path, pattern = "^ERA5Land_UTCDaily_ssrd_LON_.*\\.rds$", full.names = TRUE)

#Read and crop each longitude tile
rasters_data <- map(files, function(f) {
  rds <- readRDS(f)
  # Crop to bounding box
  rds_subset <- rds %>%
    filter(lon >= bbox_ext$xmin & lon <= bbox_ext$xmax,
           lat >= bbox_ext$ymin & lat <= bbox_ext$ymax)

  return(rds_subset)
})

# Combine all tiles into one dataframe
ssrd <- bind_rows(rasters_data)

str(ssrd$data[[1]])

# Extract JJA monthly deficits for 2018 and 2020
ssrd_jja <- ssrd %>%
  mutate(
    df = map(data, ~ {
      if (is.null(.x)) return(NULL)
      .x %>%
        mutate(date = as.Date(datetime))  # convert from character
    }),
    monthly = map(df, ~ {
      if (is.null(.x)) return(tibble(year = NA, month = NA, tot_ssrd = NA))

      .x %>%
        filter(year(date) %in% c(2018, 2020),
               month(date) %in% 6:8) %>%
        mutate(year = year(date), month = month(date)) %>%
        group_by(year, month) %>%
        summarise(ssrd_era5 = mean(tot_ssrd, na.rm = TRUE), .groups = "drop")
    })
  ) %>%
  dplyr::select(lon, lat, monthly)


print(ssrd_jja$monthly[[1]])


# save rasters
out_dir <- "data-raw/ssrd_JJA"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Extract all year–month combinations
year_months <- ssrd_jja$monthly %>%
  map_df(~ .x) %>%
  distinct(year, month)

# Loop over each combination and create raster
walk2(year_months$year, year_months$month, function(y, m) {

  # Extract ssrd for that year & month from each cell
  df <- ssrd_jja %>%
    mutate(value = map_dbl(monthly, function(x) {
      val <- x %>%
        filter(year == y, month == m) %>%
        pull(ssrd_era5)
      if (length(val) == 0) NA_real_ else val
    })) %>%
    dplyr::select(lon, lat, value)

  # Convert to terra raster
  r <- terra::rast(df, type = "xyz", crs = "EPSG:4326")

  # Name and write the raster
  fname <- sprintf("%s/ssrd_%d_%02d.tif", out_dir, y, m)
  writeRaster(r, fname, overwrite = TRUE)

  message("saved: ", fname)
})
