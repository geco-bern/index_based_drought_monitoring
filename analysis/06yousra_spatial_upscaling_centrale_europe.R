#Spatial prediction of fLUE in summer 2018 and 2020
#Difference to NDVI

#load packages
library(terra)
library(dplyr)
library(patchwork)
# library(scico)
library(raster)
library(tidymodels)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidymodels)
library(caret)
library(ranger)
```

#read NDVI rasters
#read rasters of the 3 months nadir reflectance and LST/ 2018
#LST was reampled to 500 m : bilinear interpolation
ndvi_june <- terra::rast(here::here("data-raw/NDVI_JJA_500m/NDVI_QAmasked_mean/NDVI_NA_monthly_mean_QAmasked_201806.tif"))
ndvi_july <- terra::rast(here::here("data-raw/NDVI_JJA_500m/NDVI_QAmasked_mean/NDVI_NA_monthly_mean_QAmasked_201807.tif"))
ndvi_aug <- terra::rast(here::here("data-raw/NDVI_JJA_500m/NDVI_QAmasked_mean/NDVI_NA_monthly_mean_QAmasked_201808.tif"))

#LST 2018
lst_june <- terra::rast(here::here("data-raw/LST_JJA_500m/lst_june_avg.tif"))
lst_july <- terra::rast(here::here("data-raw/LST_JJA_500m/lst_july_avg.tif"))
lst_aug <- terra::rast(here::here("data-raw/LST_JJA_500m/lst_aug_avg.tif"))

#nadir reflectance 2018
nrb_june <- terra::rast(here::here("data-raw/nrb_JJA/nrb_june_avg.tif"))
nrb_july <- terra::rast(here::here("data-raw/nrb_JJA/nrb_july_avg.tif"))
nrb_aug <- terra::rast(here::here("data-raw/nrb_JJA/nrb_aug_avg.tif"))

#2020
ndvi_june_20 <- terra::rast(here::here("data-raw/NDVI_JJA_500m/NDVI_QAmasked_mean/NDVI_NA_monthly_mean_QAmasked_202006.tif"))
ndvi_july_20 <- terra::rast(here::here("data-raw/NDVI_JJA_500m/NDVI_QAmasked_mean/NDVI_NA_monthly_mean_QAmasked_202007.tif"))
ndvi_aug_20 <- terra::rast(here::here("data-raw/NDVI_JJA_500m/NDVI_QAmasked_mean/NDVI_NA_monthly_mean_QAmasked_202008.tif"))

#LST
lst_june_20 <- terra::rast(here::here("data-raw/LST_JJA_500m/lst_june_avg_2020.tif"))
lst_july_20 <- terra::rast(here::here("data-raw/LST_JJA_500m/lst_july_avg_2020.tif"))
lst_aug_20 <- terra::rast(here::here("data-raw/LST_JJA_500m/lst_aug_avg_2020.tif"))

#nrb
nrb_june_20 <- terra::rast(here::here("data-raw/nrb_JJA/nrb_june_avg_2020.tif"))
nrb_july_20 <- terra::rast(here::here("data-raw/nrb_JJA/nrb_july_avg_2020.tif"))
nrb_aug_20 <- terra::rast(here::here("data-raw/nrb_JJA/nrb_aug_avg_2020.tif"))

#disaggregateion : Every fine-resolution (500 m) pixel
#inside a coarse-resolution (0.1°) cell gets exactly the same value
pcwd_june <- terra::rast(here::here("data-raw/pcwd_JJA/pcwd_2018_06.tif")
factor <- res(pcwd_june) / res(nrb_june)
#read pcwd and t2m rasters
pcwd_june <- terra::rast(here::here("data-raw/pcwd_JJA/pcwd_2018_06.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")

pcwd_july <- terra::rast(here::here("data-raw/pcwd_JJA/pcwd_2018_07.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
pcwd_aug <- terra::rast(here::here("data-raw/pcwd_JJA/pcwd_2018_08.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")

#2020
pcwd_june_20 <- terra::rast(here::here("data-raw/pcwd_JJA/pcwd_2020_06.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
pcwd_july_20 <- terra::rast(here::here("data-raw/pcwd_JJA/pcwd_2020_07.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
pcwd_aug_20 <- terra::rast(here::here("data-raw/pcwd_JJA/pcwd_2020_08.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")

#t2m 2018
t2m_june <- terra::rast(here::here("data-raw/t2m_JJA/t2m_2018_06.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
t2m_july <- terra::rast(here::here("data-raw/t2m_JJA/t2m_2018_07.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
t2m_aug <- terra::rast(here::here("data-raw/t2m_JJA/t2m_2018_08.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")

#2020
t2m_june_20 <- terra::rast(here::here("data-raw/t2m_JJA/t2m_2020_06.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
t2m_july_20 <- terra::rast(here::here("data-raw/t2m_JJA/t2m_2020_07.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
t2m_aug_20 <- terra::rast(here::here("data-raw/t2m_JJA/t2m_2020_08.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")

#ssrd 2018
ssrd_june <- terra::rast(here::here("data-raw/ssrd_JJA/ssrd_2018_06.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
ssrd_july <- terra::rast(here::here("data-raw/ssrd_JJA/ssrd_2018_07.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
ssrd_aug <- terra::rast(here::here("data-raw/ssrd_JJA/ssrd_2018_08.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")

#ssrd 2020
ssrd_june_20 <- terra::rast(here::here("data-raw/ssrd_JJA/ssrd_2020_06.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
ssrd_july_20 <- terra::rast(here::here("data-raw/ssrd_JJA/ssrd_2020_07.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")
ssrd_aug_20 <- terra::rast(here::here("data-raw/ssrd_JJA/ssrd_2020_08.tif"))|>
  disagg(fact = factor, method = "near") |>
  resample(y = nrb_june, method = "near")

#check
all.equal(ext(pcwd_june), ext(nrb_june))
all.equal(res(pcwd_june), res(nrb_june))
all.equal(origin(pcwd_june), origin(nrb_june))
```

#spatial fLUE 20018
june_rast <- c(nrb_june, lst_june, pcwd_june, t2m_june, ssrd_june)
names(june_rast)

new_names <- c("NR_B1", "NR_B2", "NR_B3", "NR_B4", "NR_B5", "NR_B6", "NR_B7", "LST", "pcwd_era5", "t2m_era5", "ssrd_era5")

names(june_rast) <- new_names

#Vegetation cover as factor
#read the land cover band
land_cover <- list.files("data-raw/LC/", pattern = "MCD12Q1.*LC_Type1.*\\.tif$", full.names = TRUE)
Cover_rast <- rast(land_cover)

# read the class types and legend from the lookup CSV file
lookup <- read.csv("data-raw/LC/MCD12Q1-061-LC-Type1-Statistics.csv")
landcover <- as.factor(Cover_rast)

#predict flue june 2018
regression_model <- readRDS(here::here("data/model_rf.rds"))

# set variables as ID
rec <- regression_model$recipe
rec <- rec |> update_role_requirements(role = "ID", bake = FALSE)
regression_model$recipe <- rec

# Keep the model classes as before
vegtype_mapping <- c(
  ENF = 1,
  EBF = 2,
  DBF = 3,
  MF  = 4,
  GRA = 5,
  SAV = 6,
  WSA = 7,
  WET = 8
)

# Reclassify MODIS LC_Type1 >> model vegtypes
landcover <- classify(Cover_rast, rcl = matrix(c(
  1, 1,   # ENF
  2, 2,   # EBF
  4, 3,   # DBF
  5, 4,   # MF
  10, 5,  # GRA
  9, 6,   # SAV
  8, 7,   # WSA
  11, 8   # WET
), ncol = 2, byrow = TRUE))

# Assign NA to all other classes
landcover[!(landcover[] %in% 1:8)] <- NA

# Assign readable labels for only model classes
levels(landcover) <- data.frame(
  value = 1:8,
  label = names(vegtype_mapping)
)

#add landcover to vegetationtype predictor
june_rast$vegtype <- landcover

#apply model
flue_june <- terra::predict(
  june_rast,
  regression_model,
  na.rm = TRUE,
  filename = "flue_predicted_june.tif",
  overwrite = TRUE,
  wopt = list(datatype = "FLT4S")
)

##fLUE july
july_rast <- c(nrb_july, lst_july, pcwd_july, t2m_july, ssrd_july)
names(july_rast)

new_names <- c("NR_B1", "NR_B2", "NR_B3", "NR_B4", "NR_B5", "NR_B6", "NR_B7", "LST", "pcwd_era5", "t2m_era5", "ssrd_era5")
names(july_rast) <- new_names

july_rast$vegtype <- landcover

#flue july 2018
flue_july <- terra::predict(
  july_rast,
  regression_model,
  na.rm = TRUE,
  filename = "flue_predicted_july.tif",
  overwrite = TRUE,
  wopt = list(datatype = "FLT4S")
)

## fLUE august

#raster stuck
aug_rast <- c(nrb_aug, lst_aug, pcwd_aug, t2m_aug, ssrd_aug)
names(aug_rast)
new_names <- c("NR_B1", "NR_B2", "NR_B3", "NR_B4", "NR_B5", "NR_B6", "NR_B7", "LST", "pcwd_era5", "t2m_era5", "ssrd_era5")
names(aug_rast) <- new_names

#add landcover as predictor
aug_rast$vegtype <- landcover

#flue aug 2018
flue_aug <- terra::predict(
  aug_rast,
  regression_model,
  na.rm = TRUE,
  filename = "flue_predicted_aug.tif",
  overwrite = TRUE,
  wopt = list(datatype = "FLT4S")
)

#repeat same for 2020 rasters
#fLUE for june, july, august 2020
##2020 rasters

#resampling to match res
# ref <- nrb_june_20
#raster stack
june_rast_20 <- c(nrb_june_20, lst_june_20, pcwd_june_20, t2m_june_20, ssrd_june_20)
names(june_rast_20)

#rename
new_names <- c("NR_B1", "NR_B2", "NR_B3", "NR_B4", "NR_B5", "NR_B6", "NR_B7", "LST", "pcwd_era5", "t2m_era5", "ssrd_era5")
names(june_rast_20) <- new_names

#add landcover to vegetationtype predictor
june_rast_20$vegtype <- landcover

#predict flue june 2018
flue_june_20 <- terra::predict(
  june_rast_20,
  regression_model,
  na.rm = TRUE,
  filename = "flue_predicted_june_20.tif",
  overwrite = TRUE,
  wopt = list(datatype = "FLT4S")
)

##july 2020

#raster stack
july_rast_20 <- c(nrb_july_20, lst_july_20, pcwd_july_20, t2m_july_20, ssrd_july_20)
names(july_rast_20)
#rename
new_names <- c("NR_B1", "NR_B2", "NR_B3", "NR_B4", "NR_B5", "NR_B6", "NR_B7", "LST", "pcwd_era5", "t2m_era5", "ssrd_era5")
names(july_rast_20) <- new_names

#landcover as predictor
july_rast_20$vegtype <- landcover

#flue july 2020
flue_july_20 <- terra::predict(
  july_rast_20,
  regression_model,
  na.rm = TRUE,
  filename = "flue_predicted_july_20.tif",
  overwrite = TRUE,
  wopt = list(datatype = "FLT4S")
)

## fLUE august 2020

#raster stack
aug_rast_20 <- c(nrb_aug_20, lst_aug_20, pcwd_aug_20, t2m_aug_20, ssrd_aug_20)
names(aug_rast_20)
#rename
new_names <- c("NR_B1", "NR_B2", "NR_B3", "NR_B4", "NR_B5", "NR_B6", "NR_B7", "LST", "pcwd_era5", "t2m_era5", "ssrd_era5")
names(aug_rast_20) <- new_names
#landcover as predictor
aug_rast_20$vegtype <- landcover

#flue july 2018
flue_aug_20 <- terra::predict(
  aug_rast_20,
  regression_model,
  na.rm = TRUE,
  filename = "flue_predicted_aug_20.tif",
  overwrite = TRUE,
  wopt = list(datatype = "FLT4S")
)

#Calculate the differences
#flue
delta_flue_july <- flue_july - flue_june
delta_flue_aug <- flue_aug - flue_june
#flue 2020
delta_flue_july_20 <- flue_july_20 - flue_june_20
delta_flue_aug_20 <- flue_aug_20 - flue_june_20

#appply mask for ndvi
landcover_masked <- classify(Cover_rast, rcl = matrix(c(
  1, 1,   # ENF
  2, 2,   # EBF
  4, 3,   # DBF
  5, 4,   # MF
  10, 5,  # GRA
  9, 6,   # SAV
  8, 7,   # WSA
  11, 8   # WET
), ncol = 2, byrow = TRUE))

# Assign NA to all other classes
landcover_masked[!(landcover_masked[] %in% 1:8)] <- NA

# mask non represented landcover
ndvi_june <- mask(ndvi_june, landcover_masked)
ndvi_july <- mask(ndvi_july, landcover_masked)
ndvi_aug <- mask(ndvi_aug, landcover_masked)
#ndvi 2020
ndvi_june_20 <- mask(ndvi_june_20, landcover_masked)
ndvi_july_20 <- mask(ndvi_july_20, landcover_masked)
ndvi_aug_20 <- mask(ndvi_aug_20, landcover_masked)

#difference
#ndvi
delta_ndvi_july <- ndvi_july - ndvi_june
delta_ndvi_aug <- ndvi_aug - ndvi_june
#2020
delta_ndvi_july_20 <- ndvi_july_20 - ndvi_june_20
delta_ndvi_aug_20 <- ndvi_aug_20 - ndvi_june_20

# retain negative values for calculting delta_ndvi_flue_month
delta_ndvi_july[delta_ndvi_july > 0] <- NA
delta_ndvi_aug[delta_ndvi_aug > 0] <- NA
delta_ndvi_july_20[delta_ndvi_july_20 > 0] <- NA
delta_ndvi_aug_20[delta_ndvi_aug_20 > 0] <- NA

#calculate difference of difference (ndvi - flue)
delta_ndvi_flue_july <- delta_ndvi_july - delta_flue_july
delta_ndvi_flue_aug <- delta_ndvi_aug - delta_flue_aug

delta_ndvi_flue_july_20 <- delta_ndvi_july_20 - delta_flue_july_20
delta_ndvi_flue_aug_20 <- delta_ndvi_aug_20 - delta_flue_aug_20

#again ndvi difference ndvi for saving (positive values included in ndvi raster)
delta_ndvi_july <- ndvi_july - ndvi_june
delta_ndvi_aug <- ndvi_aug - ndvi_june
#2020
delta_ndvi_july_20 <- ndvi_july_20 - ndvi_june_20
delta_ndvi_aug_20 <- ndvi_aug_20 - ndvi_june_20

#save all ouput rasters
out_dir <- here::here("/home/yousra/index_based_physiological_drought/data-raw/fLUE_pred_rast/")

delta_rasters <- list(
  delta_flue_july_2018 = delta_flue_july,
  delta_flue_aug_2018 = delta_flue_aug,
  delta_flue_july_2020 = delta_flue_july_20,
  delta_flue_aug_2020 = delta_flue_aug_20,
  delta_ndvi_july_2018 = delta_ndvi_july,
  delta_ndvi_aug_2018 = delta_ndvi_aug,
  delta_ndvi_july_2020 = delta_ndvi_july_20,
  delta_ndvi_aug_2020 = delta_ndvi_aug,
  delta_ndvi_flue_july_2018 = delta_ndvi_flue_july,
  delta_ndvi_flue_aug_2018 = delta_ndvi_flue_aug,
  delta_ndvi_flue_july_2020 = delta_ndvi_flue_july_20,
  delta_ndvi_flue_aug_2020 = delta_ndvi_flue_aug_20
)

# Loop and write each raster
for (name in names(delta_rasters)) {
  writeRaster(
    delta_rasters[[name]],
    filename = file.path(out_dir, paste0(name, ".tif")),
    datatype = "FLT4S",  # 32-bit float
    overwrite = TRUE
  )
