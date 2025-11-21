# xgboost and random forest with leave-group of sites-out CV, no initial split, comprehensive hp tuning, alternatively testing Random Forest

# load the ecosystem
library(caret)
library(ranger)
library(rlang)  # ← required for := and sym()
library(purrr)
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(readr)
library(tictoc)
library(recipes)
library(vip)
library(themis)
# remotes::install_github("geco-bern/rgeco")
library(rgeco)
# remotes::install_github("geco-bern/FluxDataKit")
library(FluxDataKit)

source("R/read_ml_data.R")

## Read data -------------------------------------------------------------------
# created with data-raw/02_compose_machine_learning_data.R
df <- read_rds(here("data/machine_learning_training_data.rds"))

# vis_miss(df, warn_large_data = FALSE)

df <- df |>
  # not needed
  select(-cluster) |>
  # correct
  mutate(is_flue_drought = as.factor(is_flue_drought)) |>
  # drop rows with missing data is needed variables
  drop_na("flue", "is_flue_drought", starts_with("NR_"), "LST", ends_with("_era5"), "vegtype")

## Common training setup -------------------------------------------------------
# KNN imputation of missing values with the following predictors
# note: this is used if `df` contains missing values and if air temperature and
# shortwave radiation (tair, r_sw) are added as predictors from local measurements.
# construct normalised difference indices
band_names <- df |>
  select(starts_with("NR_B")) |>
  names()
band_pairs <- combn(band_names, 2, simplify = FALSE)

# Build named list of expressions using set_names()
nd_exprs <- band_pairs |>
  map(~ expr((!!sym(.x[1]) - !!sym(.x[2])) / (!!sym(.x[1]) + !!sym(.x[2]) + 1e-8))) |>
  set_names(map_chr(band_pairs, ~ paste0("nd_", .x[1], "_", .x[2])))

nd_names <- names(nd_exprs)

# Define recipe
rec <- recipe(
  flue ~ .,
  data = df
) |>

  # Role handling
  update_role(site, date, is_flue_drought, new_role = "ID") |>

  # Feature engineering
  step_mutate(!!!nd_exprs) |>

  # Preprocessing (only model-predictors!)
  step_normalize(all_numeric_predictors()) |>
  step_novel() |>
  step_dummy(vegtype) |>

  # upsample cases when flue is < 1. flue-droughts are now overemphasised and
  # make up (over_ratio) times the cases of cases for which is_flue_drought is
  # false.
  step_upsample(is_flue_drought, over_ratio = 1) |>
  step_rm(is_flue_drought)

# check roles
summary(rec)

# Cross-validation by site (number of folds corresponds to number of sites) or group of sites
folds <- caret::groupKFold(
  df$site,
  k = length(unique(df$site))
  )

traincotrlParams <- caret::trainControl(
  index = folds,
  method = "cv",
  savePredictions = "final"   # predictions on each validation resample are then available as modl$pred$Resample
)

tune_grid <- expand.grid(
  .mtry = 7, # c(3, 5, 7),
  .min.node.size = 15, # c(5, 15, 25),
  .splitrule = "variance"
)

model <- train(
  rec,
  data            = df,
  metric          = "RMSE",
  method          = "ranger",
  tuneGrid        = tune_grid,
  trControl       = traincotrlParams,
  replace         = TRUE,
  sample.fraction = 0.5,
  num.trees       = 500,         # to be boosted to 2000 for the final model
  importance      = "impurity",  # for variable importance analysis, alternative: "permutation"
  ranger.num.threads = 5
)

saveRDS(model, file = here("data/model_rf.rds"))

# inspect out-of-sample validation results visually
model <- readRDS(file = here("data/model_rf.rds"))
preds <- model$pred
preds$site <- df$site[preds$rowIndex]

write_csv(preds, file = here("data/preds_rf.csv"))

# evaluate overall skill on pooled data from all sites
out <- rgeco::analyse_modobs2(
  preds,
  mod = "pred",
  obs = "obs",
  type = "hex",
  pal = "magma",
  shortsubtitle = TRUE
)

out$gg

# variable importance plot
vip(model)

# evaluate by site
my_analyse_modobs2 <- function(df, use_sitename, ...){

  out <- rgeco::analyse_modobs2(
    df,
    mod = "pred",
    obs = "obs",
    type = "hex",
    pal = "magma",
    shortsubtitle = TRUE
  )

  out$gg <- out$gg +
    labs(title = use_sitename)

  return(out)
}

preds_nested <- preds |>
  group_by(site) |>
  nest() |>
  mutate(modobs = purrr::map2(
    data,
    site,
    ~my_analyse_modobs2(.x, .y))) |>
  mutate(gg = purrr::map(modobs, "gg"))

cowplot::plot_grid(
  plotlist = preds_nested$gg[1:20],
  ncol = 4
)
ggsave(here("fig/plot_modobs_bysite_A.pdf"), width = 20, height = 16)

cowplot::plot_grid(
  plotlist = preds_nested$gg[21:40],
  ncol = 4
)
ggsave(here("fig/plot_modobs_bysite_B.pdf"), width = 20, height = 16)

cowplot::plot_grid(
  plotlist = preds_nested$gg[41:60],
  ncol = 4
)
ggsave(here("fig/plot_modobs_bysite_C.pdf"), width = 20, height = 16)

cowplot::plot_grid(
  plotlist = preds_nested$gg[61:69],
  ncol = 4
)
ggsave(here("fig/plot_modobs_bysite_D.pdf"), width = 20, height = 8)
