rm(list=ls())

pacman::p_load(tidyverse, data.table, xgboost,caret,SHAPforxgboost,
               ggpubr, shapr, SHAPforxgboost, parallel)

# some useful functions
rsq <- function(x,y) summary(lm(y~x))$r.squared
shap.values.cal <- function(explanation){
  shapley_values <- explanation$dt[, -1, drop = FALSE]
  shap.values_cal <- data.frame(Tair = sum(abs(explanation$dt$Tair))/nrow(explanation$dt),
                                SW = sum(abs(explanation$dt$SW))/nrow(explanation$dt),
                                VPD = sum(abs(explanation$dt$VPD))/nrow(explanation$dt),
                                SWC = sum(abs(explanation$dt$SWC))/nrow(explanation$dt))
  return(shap.values_cal)
}

InputFolder <- ".../file/"

# import the organized spatial data in a file called spatial_main.csv
df_raw <- read.csv(paste0(InputFolder,"spatial_main.csv")) %>% 
  mutate(land_cover = ifelse(land_cover == "OSH", "SH", land_cover),
         land_cover = ifelse(land_cover == "CSH", "SH", land_cover)) # few are "CSH"

# Humid: AI ≥0.65
# Dry sub-humid: 0.50 <AI ≤0.65
# Semi-arid: 0.20 <AI ≤0.50
# Arid: 0.05 < AI ≤0.20
# Hyper-arid: AI <0.05

df_dry <- df_raw %>% drop_na(sif) %>% filter(sif > 0, aridity <= 0.65) %>% 
  rename(Tair = t2m, SW = ssrd, VPD = vpd, SWC = sm, SIF = sif, Land_Cover = land_type) %>% 
  filter(Land_Cover != "BAR", Land_Cover != "CRO", Land_Cover != "Water",
         Land_Cover != "URB", Land_Cover != "MIX", Land_Cover != "WET",
         Land_Cover != "SNO") %>% 
  filter(Land_Cover != "DBF", Land_Cover != "DNF", Land_Cover != "EBF",
         Land_Cover != "ENF", Land_Cover != "MF")
  
df_wet <- df_raw %>% drop_na(sif) %>% filter(sif > 0, aridity > 0.65) %>% 
  rename(Tair = t2m, SW = ssrd, VPD = vpd, SWC = sm, SIF = sif, Land_Cover = land_type)%>% 
  filter(Land_Cover != "BAR", Land_Cover != "CRO", Land_Cover != "Water",
         Land_Cover != "URB", Land_Cover != "MIX", Land_Cover != "WET",
         Land_Cover != "SNO") %>% 
  filter(Land_Cover != "SH", Land_Cover != "DNF")


df_dry <- df_raw %>% drop_na(sif) %>% filter(sif > 0, aridity <= 0.65) %>% 
  rename(Tair = t2m, SW = ssrd, VPD = vpd, SWC = sm, SIF = sif, Land_Cover = land_type) %>% 
  filter(Land_Cover != "BAR", Land_Cover != "CRO", Land_Cover != "Water",
         Land_Cover != "URB", Land_Cover != "MIX", Land_Cover != "WET",
         Land_Cover != "SNO")

df_wet <- df_raw %>% drop_na(sif) %>% filter(sif > 0, aridity > 0.65) %>% 
  rename(Tair = t2m, SW = ssrd, VPD = vpd, SWC = sm, SIF = sif, Land_Cover = land_type)%>% 
  filter(Land_Cover != "BAR", Land_Cover != "CRO", Land_Cover != "Water",
         Land_Cover != "URB", Land_Cover != "MIX", Land_Cover != "WET",
         Land_Cover != "SNO")

# 1 - Model training for both dry and wet biomes--------------------------------------
x_var <- c("Tair", "SW", "VPD", "SWC"); y_var <- "SIF"
grid <- expand.grid(
  nrounds = c(500, 800, 1200),
  eta = c(0.02, 0.05, 0.1, 0.5),
  max_depth = c(4, 6, 9, 12),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)

control <- trainControl(method = "cv", number = 5)

## 1.1 - Training: dry biomes
for (k in 1:3) {
  smp_size_dry <- floor(0.8 * nrow(df_dry))
  train_ind_dry <- sample(seq_len(nrow(df_dry)), size = smp_size_dry)
  #save the index for training for reproduction
  saveRDS(train_ind_dry, paste0(InputFolder,"model_training/train_ind_dry_",k,".rds"))
  x_train_dry <- as.matrix(df_dry[train_ind_dry, x_var])
  y_train_dry <- df_dry[train_ind_dry, y_var]
  x_test_dry <- as.matrix(df_dry[-train_ind_dry, x_var])
  y_test_dry <- df_dry[-train_ind_dry, y_var]
  
  model_dry <- train(
    x = x_train,
    y = y_train,
    method = "xgbTree",
    trControl = control,
    tuneGrid = grid,
    metric = "RMSE"
  )
  saveRDS(model_dry, paste0(InputFolder,"model_training/xgboost_model_dry_",k,".rds"))
  # Model performance
  pred_y_dry <- predict(model_dry, x_test_dry)
  RMSE <- caret::RMSE(pred_y_dry, y_test_dry) 
  RSQ <- rsq(pred_y_dry, y_test_dry) 
  rBias <- (mean(y_test_dry, na.rm = T)-mean(pred_y_dry, na.rm = T))*100/mean(y_test_dry, na.rm = T)
}

## 1.2 - Training: non-dry biomes
for (k in 1:3) {
  smp_size_wet <- floor(0.8 * nrow(df_wet))
  train_ind_wet <- sample(seq_len(nrow(df_wet)), size = smp_size_wet)
  #save the index for training for reproduction
  saveRDS(train_ind_wet, paste0(InputFolder,"model_training/train_ind_wet_",k,".rds"))
  x_train_wet <- as.matrix(df_wet[train_ind_wet, x_var])
  y_train_wet <- df_wet[train_ind_wet, y_var]
  x_test_wet <- as.matrix(df_wet[-train_ind_wet, x_var])
  y_test_wet <- df_wet[-train_ind_wet, y_var]
  
  model_wet <- train(
    x = x_train,
    y = y_train,
    method = "xgbTree",
    trControl = control,
    tuneGrid = grid,
    metric = "RMSE"
  )
  saveRDS(model_wet, paste0(InputFolder,"model_training/xgboost_model_wet_",k,".rds"))
  # Model performance
  pred_y_wet <- predict(model_wet, x_test_wet)
  RMSE <- caret::RMSE(pred_y_wet, y_test_wet) 
  RSQ <- rsq(pred_y_wet, y_test_wet) 
  rBias <- (mean(y_test_wet, na.rm = T)-mean(pred_y_wet, na.rm = T))*100/mean(y_test_wet, na.rm = T)
}

# 2 - Compute Causal Shapley Values ----------------------------------------------
for (k in 1:3) {
  p_dry <- mean(df_dry$SIF)
  p_wet <- mean(df_wet$SIF)
  
  dataX_dry <- as.matrix(subset(df_dry, select = -c(lon,lat,lon_adj,SIF,aridity,land_cover,Land_Cover, Group))[,c("Tair", "SW", "VPD", "SWC")])
  dataX_wet <- as.matrix(subset(df_wet, select = -c(lon,lat,lon_adj,SIF,aridity,land_cover,Land_Cover, Group))[,c("Tair", "SW", "VPD", "SWC")])
  
  model_dry <- readRDS(paste0(InputFolder,"model_training/xgboost_model_dry_",k,".rds"))
  model_wet <- readRDS(paste0(InputFolder,"model_training/xgboost_model_wet_",k,".rds"))
  
  explainer_symmetric_dry <- shapr(dataX_dry, model_dry) 
  explainer_symmetric_wet <- shapr(dataX_wet, model_wet)
  
  # 2.1. We compute the causal Shapley values on a given partial order for dry lands
  partial_order_dry <- list(2, c(1, 3, 4))
  explanation_causal_dry <- explain(
    dataX_dry,
    approach = "causal",
    explainer = explainer_symmetric_dry,
    prediction_zero = p_dry,
    ordering = partial_order_dry,
    confounding = FALSE,
    seed = 2023
  )
  #save the explanationer for reproduction
  saveRDS(explanation_causal_dry, paste0(InputFolder,"explanation_causal_TROPOMI_dry_",k,".rds"))
  
  # 2.2. We compute the causal Shapley values on a given partial order for non-dry lands
  partial_order_wet <- list(2, 1, 3, 4)
  explanation_causal_wet <- explain(
    dataX_wet,
    approach = "causal",
    explainer = explainer_symmetric_wet,
    prediction_zero = p_wet,
    ordering = partial_order_wet,
    confounding = FALSE,
    seed = 2023
  )
  #save the explanationer for reproduction
  saveRDS(explanation_causal_wet, paste0(InputFolder,"explanation_causal_TROPOMI_wet_",k,".rds"))
  
  all.shap.causal_dry <- explanation_causal_dry$dt
  all.shap.causal_dry$lon <- df_dry$lon
  all.shap.causal_dry$lat <- df_dry$lat
  all.shap.causal_dry$lon_adj <- df_dry$lon_adj
  all.shap.causal_dry$SIF <- df_dry$SIF
  all.shap.causal_dry<- all.shap.causal_dry %>% mutate(Tair_ori = df_dry$Tair,
                                                       SW_ori = df_dry$SW,
                                                       VPD_ori = df_dry$VPD,
                                                       SWC_ori = df_dry$SWC,
                                                       WI = df_dry$aridity,
                                                       Land_Cover = df_dry$Land_Cover)
  all.shap.causal_wet <- explanation_causal_wet$dt
  all.shap.causal_wet$lon <- df_wet$lon
  all.shap.causal_wet$lat <- df_wet$lat
  all.shap.causal_wet$lon_adj <- df_wet$lon_adj
  all.shap.causal_wet$SIF <- df_wet$SIF
  all.shap.causal_wet<- all.shap.causal_wet %>% mutate(Tair_ori = df_wet$Tair,
                                                       SW_ori = df_wet$SW,
                                                       VPD_ori = df_wet$VPD,
                                                       SWC_ori = df_wet$SWC,
                                                       WI = df_wet$aridity,
                                                       Land_Cover = df_wet$Land_Cover)
  
  lonlat_raw <- df_raw %>% select(lon, lat, lon_adj)
  
  all.cshap.ori_dry <- merge(lonlat_raw, all.shap.causal_dry, by = c("lat", "lon"), all.x = TRUE) %>% 
    select(-lon_adj.y)
  all.cshap.ori_wet <- merge(lonlat_raw, all.shap.causal_wet, by = c("lat", "lon"), all.x = TRUE) %>% 
    select(-lon_adj.y)
  
  write.csv(all.shap.causal_dry, file = paste0(InputFolder, "all_causalSHAP_dry_",k,".csv"), row.names = F)
  write.csv(all.cshap.ori_dry, file = paste0(InputFolder, "all_causalSHAP_origin_dry_",k,".csv"), row.names = F)
  shap.causal_dry <- shap.values.cal(explanation_causal_dry)
  write.csv(shap.causal_dry, file = paste0(InputFolder, "FI_causalSHAP_dry_",k,".csv"), row.names = F)
  
  write.csv(all.shap.causal_wet, file = paste0(InputFolder, "all_causalSHAP_wet_",k,".csv"), row.names = F)
  write.csv(all.cshap.ori_wet, file = paste0(InputFolder, "all_causalSHAP_origin_wet_",k,".csv"), row.names = F)
  shap.causal_wet <- shap.values.cal(explanation_causal_wet)
  write.csv(shap.causal_wet, file = paste0(InputFolder, "FI_causalSHAP_wet_",k,".csv"), row.names = F)
}



