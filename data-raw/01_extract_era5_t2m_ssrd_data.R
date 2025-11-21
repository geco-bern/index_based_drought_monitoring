install.packages("ncdf4")
library(ncdf4)
library(dplyr)

#read netcdf file
nc <- nc_open(here::here("data-raw/era5/ERA5Land_UTCDaily.tot_ssrd.2006.nc"))
print(nc)

# load fLUE data from previous paper
df <- read.csv("data/flue_stocker18nphyt.csv")

# load site info for downloading data
# only retain relevant site locations
site_info <- read.csv("data/site_info.csv") |>
  filter(
    sitename %in% unique(df$site)
  )

# create a base query / locations + time frame
site <- site_info |>
  select(
    sitename,
    lat,
    lon,
    date_start,
    date_end
  ) |>
  rename(
    "site" = "sitename",
    "latitude"= "lat",
    "longitude" = "lon",
    "start" = "date_start",
    "end" = "date_end",
  ) |>
  mutate(
    start = ifelse(start < "2000-01-01", "2000-01-01", start)
  )
head(site)

#Extract t2m and ssrd srom these sites locations and dates.

min(df$date)
library(ncdf4)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

#function to extract data
extract_era5_for_sites <- function(sites, nc_folder) {
  all_data <- list()

  # Map of file and variable names
  var_map <- list(
    t2m = list(varname = "mean_t2m", file_prefix = "ERA5Land_UTCDaily.mean_t2m"),
    ssrd = list(varname = "tot_ssrd", file_prefix = "ERA5Land_UTCDaily.tot_ssrd")
  )

  for (i in seq_len(nrow(sites))) {
    site_row <- sites[i, ]
    site_name <- site_row$site
    lat <- site_row$latitude
    lon <- site_row$longitude
    start_date <- as.Date(site_row$start)
    end_date <- as.Date(site_row$end)
    lon_adj <- ifelse(lon < 0, lon + 360, lon)
    years <- seq(lubridate::year(start_date), lubridate::year(end_date))

    for (yr in years) {
      yearly_data <- list()
      dates_reference <- NULL

      for (var_short in names(var_map)) {
        var_info <- var_map[[var_short]]
        nc_path <- file.path(nc_folder, sprintf("%s.%d.nc", var_info$file_prefix, yr))

        if (!file.exists(nc_path)) {
          message("File missing: ", nc_path)
          next
        }

        nc <- nc_open(nc_path)
        lats <- ncvar_get(nc, "latitude")
        lons <- ncvar_get(nc, "longitude")
        time_vals <- ncvar_get(nc, "valid_time")
        time_units <- ncatt_get(nc, "valid_time", "units")$value
        origin <- as.Date(sub("days since ", "", time_units))
        dates <- origin + time_vals

        lat_idx <- which.min(abs(lats - lat))
        lon_idx <- which.min(abs(lons - lon_adj))
        date_mask <- which(dates >= start_date & dates <= end_date)

        if (length(date_mask) == 0) {
          nc_close(nc)
          next
        }

        vals <- ncvar_get(nc, var_info$varname,
                          start = c(lon_idx, lat_idx, min(date_mask)),
                          count = c(1, 1, length(date_mask)))

        if (is.null(dates_reference)) {
          dates_reference <- dates[date_mask]
        }

        yearly_data[[var_short]] <- vals
        nc_close(nc)
      }

      if (!is.null(yearly_data$t2m) && !is.null(yearly_data$ssrd)) {
        df <- data.frame(
          site = site_name,
          date = dates_reference,
          latitude = lat,
          longitude = lon,
          t2m = yearly_data$t2m,
          ssrd = yearly_data$ssrd
        )
        all_data[[length(all_data) + 1]] <- df
      }
    }
  }

  dplyr::bind_rows(all_data)
}



nc_folder  <- "/home/yousra/index_based_physiological_drought/data-raw/era5"

era5_data_tm_ssrd <- extract_era5_for_sites(site, nc_folder)

#save data in csv file

write.csv(era5_data_tm_ssrd,"/home/yousra/index_based_physiological_drought/data/era5_data_tm_ssrd_sites.csv", row.names = FALSE)


