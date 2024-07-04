rm(list=ls())
pacman::p_load(tidyverse, data.table, xgboost, caret, e1071,
               ggpubr, shapr, SHAPforxgboost, parallel)

rsq <- function(x,y) summary(lm(y~x))$r.squared
shap.values.cal <- function(explanation){
  shapley_values <- explanation$dt[, -1, drop = FALSE]
  shap.values_cal <- data.frame(Tair = sum(abs(explanation$dt$Tair))/nrow(explanation$dt),
                                SW = sum(abs(explanation$dt$SW))/nrow(explanation$dt),
                                VPD = sum(abs(explanation$dt$VPD))/nrow(explanation$dt),
                                SWC = sum(abs(explanation$dt$SWC))/nrow(explanation$dt))
  return(shap.values_cal)
}
# import the organized hourly eddy covariance data in a file called FLUXNET2015_main.csv
InputFolder <- ".../file/"
df_hourly <- read.csv(paste0(InputFolder, "FLUXNET2015_main.csv"))
# SWC_threshold is the threshold of SM to identify water- and energy-limited conditions
# The values for each site are shown in Table S1
# SWC1 is the surface SM. SWC is the surface SM for grasslands, savannahs and shrublands
# SWC is the average value of SM from soil layers 1-3 for forests
df_total <- df_hourly %>% 
  select(Site, Landcover, TA, SW, VPD, SWC1, SWC, Site_ID, GPP_NT, SWC_threshold) %>% 
  rename(Tair = TA,GPP = GPP_NT) %>% 
  mutate(Type = case_when(Landcover == "ENF"| Landcover == "EBF" | Landcover == "DBF" | Landcover == "MF" ~ "Forest",
                          Landcover == "WSA" | Landcover == "OSH" | Landcover == "CSH" | Landcover == "SAV" ~ "Dryland",
                          Landcover == "GRA" ~ "Grassland"),
         Dry_flag = case_when(SWC1 < SWC_threshold ~ "dry",
                              TRUE ~ "wet")) %>% 
  select(-SWC_threshold, -SWC1)

x_var <- c("Tair", "SW", "VPD", "SWC", "Site_ID"); y_var <- "GPP"

grid <- expand.grid(
  nrounds = c(500, 800, 1200),
  eta = c(0.02, 0.05, 0.1, 0.5),
  max_depth = c(4, 6, 9, 12),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)

control <- trainControl(method = "cv", number = 5)

Type <- c("Forest", "Dryland", "Grassland")
DryWet <- c("dry", "wet") # dry vs. wet

for (i in Type){
  for (j in DryWet){
    for (k in 1:3) {
      # we remove outliers based on 1th and 99th percentiles
      df <- df_total %>% filter(Type == i, Dry_flag == j,
                                GPP < quantile(GPP, 0.99),
                                GPP > quantile(GPP, 0.01))
      
      smp_size <- floor(0.8 * nrow(df))
      train_ind <- sample(seq_len(nrow(df)), size = smp_size)
      #save the index for training for reproduction
      saveRDS(train_ind, paste0(InputFolder,"model_training/train_ind_", i,"_",j,"_",k,".rds"))
      
      x_train <- as.matrix(df[train_ind, x_var])
      y_train <- df[train_ind, y_var]
      
      x_test <- as.matrix(df[-train_ind, x_var])
      y_test <- df[-train_ind, y_var]
      
      model <- train(
        x = x_train,
        y = y_train,
        method = "xgbTree",
        trControl = control,
        tuneGrid = grid,
        metric = "RMSE"
      )
      
      saveRDS(model, paste0(InputFolder,"model_training/xgboost_model_", i,"_",j,"_",k,".rds"))
      
      pred_y <- predict(model, x_test)
      RMSE <- caret::RMSE(pred_y, y_test) 
      RSQ <- rsq(pred_y, y_test) 
      rBias <- (mean(y_test, na.rm = T)-mean(pred_y, na.rm = T))*100/mean(y_test, na.rm = T)
    }
  }
}

Type <- c("Forest", "Dryland", "Grassland")
DryWet <- c("dry", "wet") # dry vs. wet

for (i in Type){
  for(j in DryWet){
    for (k in 1:3) {
      message(paste0("Compute causal Shapley values for type: ", i, ", periods: ", j, ", repeat: ", k))
      model <- readRDS(paste0(InputFolder,"model_training/xgboost_model_", i,"_",j,"_",k,".rds"))
      
      # 1 - Compute Shapley Values ----------------------------------------------
      FileSampling <-list.files(paste0(InputFolder,"representative_sampling/"), 
                                pattern=paste0("representative_sampling_", i,"_Hourly_",j))
      # change the order of columns in dataX. Otherwise there will be error in calculating shap values
      df_sampling <- read.csv(paste0(InputFolder, "representative_sampling/",FileSampling))
      p <- mean(df_sampling$GPP)
      dataX <- as.matrix(subset(df_sampling, 
                                select = -c(GPP,Landcover))[,c("Tair", "SW", "VPD", "SWC", "Site_ID")])
      
      explainer_symmetric <- shapr(dataX, model)                    
      # a. We compute the causal Shapley values on a given partial order
      if (j == "dry") {
        partial_order <- list(2, c(1, 3, 4), 5)
      } else {
        partial_order <- list(2, 1, 3, 4, 5)
      }
      explanation_causal <- explain(
        dataX,
        approach = "causal",
        explainer = explainer_symmetric,
        prediction_zero = p,
        ordering = partial_order,
        confounding = FALSE,
        seed = 2023
      )
      #save the explanationer for reproduction
      saveRDS(explanation_causal, paste0(InputFolder,"causalSHAP_result/explanation_causal_", i,"_",j,"_",k,".rds"))
      all.shap.causal <- explanation_causal$dt
      
      all.shap.causal$Landcover <- df_sampling$Landcover
      write.csv(all.shap.causal, file = paste0(InputFolder, "causalSHAP_result/all_causalSHAP_", i,"_",j,"_",k,".csv"), row.names = F)
      
      shap.causal <- shap.values.cal(explanation_causal) %>% mutate(Type = i, DryWet = j)
      write.csv(shap.causal, file = paste0(InputFolder, "causalSHAP_result/FI_causalSHAP_", i,"_",j,"_",k,".csv"), row.names = F)
    }
  }
}








