
#load libraries 
library(appeears)
library(dplyr)
library(sf)

# Central Europe bounding box (WGS84)
country <- sf::st_bbox(
  c(xmin = 2, ymin = 45, xmax = 17, ymax = 53),
  crs = sf::st_crs(4326)
) |> sf::st_as_sfc() |> sf::st_sf()

#Select MODIS NDVI product (MOD13A1.061 — 500 m)
product_subset <- c("MOD13A1.061")

# Retrieve all layers of the product
layers <- rs_layers(product_subset)

# Filter NDVI layer (non-QA)
bands <- layers |>
  filter(IsQA == FALSE) |>
  filter(grepl("NDVI", Layer, ignore.case = TRUE)) |>
  select(Layer)

#Define temporal range (June, July, August of 2018 & 2020)
years <- c(2018, 2020)
months <- c("06", "07", "08")

# Build all month–year combinations
time_periods <- expand.grid(year = years, month = months)

#Build query dataframe
full_queries <- lapply(
  1:nrow(time_periods),
  function(i) {
    year <- time_periods$year[i]
    month <- time_periods$month[i]
    
    start_date <- sprintf("%s-%s-01", year, month)
    end_date <- as.character(as.Date(start_date) + 31) # covers full month
    
    bands |>
      rowwise() |>
      do({
        data.frame(
          subtask = product_subset,
          task = paste0("NDVI_", year, "_", month),
          start = start_date,
          end = end_date,
          product = product_subset,
          layer = as.character(.$Layer)
        )
      })
  }
) |> bind_rows()

#Build AppEEARS tasks
tasks <- full_queries |>
  dplyr::group_by(task, subtask) |>
  group_split()

tasks <- lapply(
  tasks,
  function(task) {
    appeears::rs_build_task(
      task,
      roi = country  # box 
    )
  }
)

#  Authenticate (login)
token <- rs_login(user = "ymejjaouy")  # replace with your AppEEARS username

# Submit requests
requests <- rs_request_batch(
  request = tasks,
  user = "ymejjaouy",
  workers = 3,
  path = "data-raw/NDVI_JJA_500m"
)

# appeears::task_download(task_id = "<insert task ID>", path = "data-raw/NDVI_JJA_500m/")


###calculate mean of each month 

base_dir <- "data-raw/NDVI_JJA_500m/"  # folder with NDVI-2018-06, NDVI-2018-07, etc.

# Helper function to decode VI_Quality band (simplified)
# Only keep "good" quality pixels (bit 0–1 == 00)
mask_quality <- function(quality_raster) {
  # Convert raster values to bits and find pixels with low bits == 00
  good <- (quality_raster %% 4) == 0
  return(good)
}

#Process each folder (month)
folders <- list.dirs(base_dir, recursive = FALSE)

for (folder in folders) {
  # Skip empty folders
  if (length(list.files(folder, pattern = "NDVI.*\\.tif$")) == 0) next
  
  cat("\n📦 Processing folder:", folder, "\n")
  
  # List NDVI and Quality files
  ndvi_files <- list.files(folder, pattern = "NDVI.*\\.tif$", full.names = TRUE)
  qual_files <- list.files(folder, pattern = "VI_Quality.*\\.tif$", full.names = TRUE)
  
  if (length(ndvi_files) != length(qual_files)) {
    warning("⚠️ NDVI and Quality file count mismatch in ", folder)
  }
  
  # Sort to ensure pairing
  ndvi_files <- sort(ndvi_files)
  qual_files <- sort(qual_files)
  
  # Container for masked NDVI rasters
  masked_list <- list()
  
  for (i in seq_along(ndvi_files)) {
    ndvi <- rast(ndvi_files[i])
    qual <- rast(qual_files[i])
    
    # Compute quality mask
    good <- mask_quality(qual)
    
    # Apply mask: set bad pixels to NA
    ndvi_masked <- mask(ndvi, good, maskvalues = FALSE)
    
    masked_list[[i]] <- ndvi_masked
  }
  
  # Stack all masked rasters for this month
  ndvi_stack <- rast(masked_list)
  
  # Compute mean NDVI (ignoring NA)
  ndvi_month_mean <- mean(ndvi_stack, na.rm = TRUE)
  
  # Extract year & month from folder name
  month_label <- str_extract(basename(folder), "\\d{4}-\\d{2}")
  
  # Save result
  out_name <- paste0("NDVI_", month_label, "_monthly_mean_QAmasked.tif")
  writeRaster(
    ndvi_month_mean,
    filename = file.path(folder, out_name),
    overwrite = TRUE
  )
  
  cat("saved:", out_name, "\n")
}
#check plot
# ndvi_june <- rast("data-raw/NDVI_JJA_500m/NDVI_2020_06/NDVI_NA_monthly_mean_QAmasked.tif")
# plot(ndvi_june, main = "NDVI June 2018 (500 m, QA masked)")
