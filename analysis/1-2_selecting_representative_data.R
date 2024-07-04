# this code scripts generate the most representative samples for Causal Shapley value calculation
# the selection is based on convex hulls
# the final results are stored at ".../file/representative_sampling/"

rm(list=ls())
pacman::p_load(tidyverse, PBSmapping, sf, units)
InputFolder <- "/Users/jiangongliu/Desktop/1_cshap_update/file/submit/"

# import the organized hourly eddy covariance data in a file called FLUXNET2015_main.csv
df <- read.csv(paste0(InputFolder, "FLUXNET2015_main.csv"))

# SWC_threshold is the threshold of SM to identify water- and energy-limited conditions
# The values for each site are shown in Table S1
df_total_filter <- df %>% select(TA, SW, VPD, SWC1, SWC, Landcover, Site_ID, GPP_NT, SWC_threshold) %>% 
  rename(Tair = TA,GPP = GPP_NT) %>% 
  mutate(Type = case_when(Landcover == "ENF"| Landcover == "EBF" | Landcover == "DBF" | Landcover == "MF" ~ "Forest",
                          Landcover == "WSA" | Landcover == "OSH" | Landcover == "CSH" | Landcover == "SAV" ~ "Dryland",
                          Landcover == "GRA" ~ "Grassland"),
         Dry_flag = case_when(SWC1 < SWC_threshold ~ 1,
                              TRUE ~ 0))

df_dry <- df_total_filter %>% filter(Dry_flag == 1) %>% select(-SWC1, -SWC_threshold)
df_wet <- df_total_filter %>% filter(Dry_flag == 0) %>% select(-SWC1, -SWC_threshold)

# criteria
# >10k sampling number
# representativeness can reach >99.5%
################################Forest-Wet##########################################
df_forest_total_total <- df_wet %>% filter(Type == "Forest") %>% select(-Type, -Dry_flag)
df_forest_total <- df_wet %>% filter(Type == "Forest") %>% select(-Type, -GPP, -Dry_flag, -Landcover, -Site_ID)

combn_forest <- combn(df_forest_total, 2, simplify=FALSE)

list.frames.forest <- replicate(20, data.frame())
df.main_forest <- data.frame()

for (i in 1:20) {
  
  df_sample <- sample_n(df_forest_total_total, 4000)
  list.frames.forest[[i]] <- df_sample
  df_sample <- df_sample %>% select(-GPP, -Site_ID, -Landcover)
  combn_sample <- combn(df_sample, 2, simplify=FALSE)

  for (j in 1:length(combn_forest)) {
    
    df_forest <- combn_forest[[j]] 
    df_sample <- combn_sample[[j]]
    
    colnames(df_forest) <- c('X','Y')
    df_forest <- df_forest %>% 
      filter(X > quantile(X, 0.01) & X < quantile(X, 0.99) 
             & Y > quantile(Y, 0.01) & Y < quantile(Y, 0.99))
    
    # calculate the total area of this combination
    full_chull_forest <- df_forest[chull(df_forest[,1], df_forest[,2]),]
    
    full_chull_forest <- full_chull_forest %>% 
      mutate(PID = 1,
             POS = 1:nrow(full_chull_forest)) 
    
    full_polygon_forest <- st_sfc(st_polygon(list(cbind(c(full_chull_forest$X, full_chull_forest$X[1]),
                                                        c(full_chull_forest$Y, full_chull_forest$Y[1]))))) %>%
      st_set_crs(32615)
    
    full_area_forest <- st_area(full_polygon_forest)
      
  colnames(df_sample) <- c('X','Y')
      
  sample_chull <- df_sample[chull(df_sample[,1], df_sample[,2]),]
  sample_chull <- sample_chull %>% 
    mutate(PID = 2,
           POS = 1:nrow(sample_chull)) 
      
  joint_chull <- joinPolys(full_chull_forest,sample_chull)
      
      
  joint_polygon <- st_sfc(st_polygon(list(cbind(c(joint_chull$X, joint_chull$X[1]),
                                                    c(joint_chull$Y, joint_chull$Y[1]))))) %>%
  st_set_crs(32615)
      
  representativeness <- st_area(joint_polygon)/full_area_forest
      
      df1 <- data.frame(sampling = i, combination = j, representativeness = representativeness)
      
      df.main_forest <- rbind(df.main_forest, df1)
      
    }
}

summary_forest <- df.main_forest %>% 
  group_by(sampling) %>% 
  summarise(mean = mean(representativeness),
            sd = sd(representativeness)) %>% 
  mutate(mean = as.numeric(mean)*100,
         sd = as.numeric(sd)*100)

inx_forest <- which(summary_forest$mean == max(summary_forest$mean))
max_forest <- round(max(summary_forest$mean) * 100, 0) 
################################Forest-Dry##########################################
df_forest_total_total <- df_dry %>% filter(Type == "Forest") %>% select(-Type, -Dry_flag)
df_forest_total <- df_dry %>% filter(Type == "Forest") %>% select(-Type, -Landcover, -GPP,-Dry_flag, -Site_ID)

combn_forest <- combn(df_forest_total, 2, simplify=FALSE)

list.frames.forest <- replicate(20, data.frame())
df.main_forest <- data.frame()

for (i in 1:20) {
  
  df_sample <- sample_n(df_forest_total_total, 4000)
  list.frames.forest[[i]] <- df_sample
  df_sample <- df_sample %>% select(-Site_ID, -GPP, -Landcover)
  combn_sample <- combn(df_sample, 2, simplify=FALSE)
  
  for (j in 1:length(combn_forest)) {
    
    df_forest <- combn_forest[[j]] 
    df_sample <- combn_sample[[j]]
    
    colnames(df_forest) <- c('X','Y')
    df_forest <- df_forest %>% 
      filter(X > quantile(X, 0.01) & X < quantile(X, 0.99) 
             & Y > quantile(Y, 0.01) & Y < quantile(Y, 0.99))
    
    # calculate the total area of this combination
    full_chull_forest <- df_forest[chull(df_forest[,1], df_forest[,2]),]
    
    full_chull_forest <- full_chull_forest %>% 
      mutate(PID = 1,
             POS = 1:nrow(full_chull_forest)) 
    
    full_polygon_forest <- st_sfc(st_polygon(list(cbind(c(full_chull_forest$X, full_chull_forest$X[1]),
                                                        c(full_chull_forest$Y, full_chull_forest$Y[1]))))) %>%
      st_set_crs(32615)
    
    full_area_forest <- st_area(full_polygon_forest)
    
    colnames(df_sample) <- c('X','Y')
    
    sample_chull <- df_sample[chull(df_sample[,1], df_sample[,2]),]
    sample_chull <- sample_chull %>% 
      mutate(PID = 2,
             POS = 1:nrow(sample_chull)) 
    
    joint_chull <- joinPolys(full_chull_forest,sample_chull)
    
    
    joint_polygon <- st_sfc(st_polygon(list(cbind(c(joint_chull$X, joint_chull$X[1]),
                                                  c(joint_chull$Y, joint_chull$Y[1]))))) %>%
      st_set_crs(32615)
    
    representativeness <- st_area(joint_polygon)/full_area_forest
    
    df1 <- data.frame(sampling = i, combination = j, representativeness = representativeness)
    
    df.main_forest <- rbind(df.main_forest, df1)
    
  }
}

summary_forest <- df.main_forest %>% 
  group_by(sampling) %>% 
  summarise(mean = mean(representativeness),
            sd = sd(representativeness)) %>% 
  mutate(mean = as.numeric(mean)*100,
         sd = as.numeric(sd)*100)

inx_forest <- which(summary_forest$mean == max(summary_forest$mean))
max_forest <- round(max(summary_forest$mean) * 100, 0) 
################################Dryland-Wet##########################################
df_shrub_total_total <- df_wet %>% filter(Type == "Dryland") %>% select(-Type,-Dry_flag)
df_shrub_total <- df_wet %>% filter(Type == "Dryland") %>% select(-Type, -Landcover, -GPP, -Dry_flag, -Site_ID)

combn_shrub <- combn(df_shrub_total, 2, simplify=FALSE)

list.frames.shrub <- replicate(20, data.frame())
df.main_shrub <- data.frame()

for (i in 1:20) {
  
  df_sample <- sample_n(df_shrub_total_total, 4000)
  list.frames.shrub[[i]] <- df_sample
  df_sample <- df_sample %>% select(-Site_ID, -GPP, -Landcover)
  combn_sample <- combn(df_sample, 2, simplify=FALSE)
  
  for (j in 1:length(combn_shrub)) {
    
    df_shrub <- combn_shrub[[j]] 
    df_sample <- combn_sample[[j]]
    
    colnames(df_shrub) <- c('X','Y')
    df_shrub <- df_shrub %>% 
      filter(X > quantile(X, 0.01) & X < quantile(X, 0.99) 
             & Y > quantile(Y, 0.01) & Y < quantile(Y, 0.99))
    
    # calculate the total area of this combination
    full_chull_shrub <- df_shrub[chull(df_shrub[,1], df_shrub[,2]),]
    
    full_chull_shrub <- full_chull_shrub %>% 
      mutate(PID = 1,
             POS = 1:nrow(full_chull_shrub)) 
    
    full_polygon_shrub <- st_sfc(st_polygon(list(cbind(c(full_chull_shrub$X, full_chull_shrub$X[1]),
                                                        c(full_chull_shrub$Y, full_chull_shrub$Y[1]))))) %>%
      st_set_crs(32615)
    
    full_area_shrub <- st_area(full_polygon_shrub)

    colnames(df_sample) <- c('X','Y')
    
    sample_chull <- df_sample[chull(df_sample[,1], df_sample[,2]),]
    sample_chull <- sample_chull %>% 
      mutate(PID = 2,
             POS = 1:nrow(sample_chull)) 
    
    joint_chull <- joinPolys(full_chull_shrub,sample_chull)
    
    
    joint_polygon <- st_sfc(st_polygon(list(cbind(c(joint_chull$X, joint_chull$X[1]),
                                                  c(joint_chull$Y, joint_chull$Y[1]))))) %>%
      st_set_crs(32615)
    
    representativeness <- st_area(joint_polygon)/full_area_shrub
    
    df1 <- data.frame(sampling = i, combination = j, representativeness = representativeness)
    
    df.main_shrub <- rbind(df.main_shrub, df1)
    
  }
}

summary_shrub <- df.main_shrub %>% 
  group_by(sampling) %>% 
  summarise(mean = mean(representativeness),
            sd = sd(representativeness)) %>% 
  mutate(mean = as.numeric(mean)*100,
         sd = as.numeric(sd)*100)

inx_shrub <- which(summary_shrub$mean == max(summary_shrub$mean))
max_shrub <- round(max(summary_shrub$mean) * 100, 0) 
################################Dryland-Dry##########################################
df_shrub_total_total <- df_dry %>% filter(Type == "Dryland") %>% select(-Type,-Dry_flag)
df_shrub_total <- df_dry %>% filter(Type == "Dryland") %>% select(-Type, -Landcover, -GPP, -Dry_flag, -Site_ID)

combn_shrub <- combn(df_shrub_total, 2, simplify=FALSE)

list.frames.shrub <- replicate(20, data.frame())
df.main_shrub <- data.frame()

for (i in 1:20) {
  
  df_sample <- sample_n(df_shrub_total_total, 4000)
  list.frames.shrub[[i]] <- df_sample
  df_sample <- df_sample %>% select(-Site_ID, -GPP, -Landcover)
  combn_sample <- combn(df_sample, 2, simplify=FALSE)
  
  for (j in 1:length(combn_shrub)) {
    
    df_shrub <- combn_shrub[[j]] 
    df_sample <- combn_sample[[j]]
    
    colnames(df_shrub) <- c('X','Y')
    df_shrub <- df_shrub %>% 
      filter(X > quantile(X, 0.01) & X < quantile(X, 0.99) 
             & Y > quantile(Y, 0.01) & Y < quantile(Y, 0.99))
    
    # calculate the total area of this combination
    full_chull_shrub <- df_shrub[chull(df_shrub[,1], df_shrub[,2]),]
    
    full_chull_shrub <- full_chull_shrub %>% 
      mutate(PID = 1,
             POS = 1:nrow(full_chull_shrub)) 
    
    full_polygon_shrub <- st_sfc(st_polygon(list(cbind(c(full_chull_shrub$X, full_chull_shrub$X[1]),
                                                       c(full_chull_shrub$Y, full_chull_shrub$Y[1]))))) %>%
      st_set_crs(32615)
    
    full_area_shrub <- st_area(full_polygon_shrub)
    
    colnames(df_sample) <- c('X','Y')
    
    sample_chull <- df_sample[chull(df_sample[,1], df_sample[,2]),]
    sample_chull <- sample_chull %>% 
      mutate(PID = 2,
             POS = 1:nrow(sample_chull)) 
    
    joint_chull <- joinPolys(full_chull_shrub,sample_chull)
    
    
    joint_polygon <- st_sfc(st_polygon(list(cbind(c(joint_chull$X, joint_chull$X[1]),
                                                  c(joint_chull$Y, joint_chull$Y[1]))))) %>%
      st_set_crs(32615)
    
    representativeness <- st_area(joint_polygon)/full_area_shrub
    
    df1 <- data.frame(sampling = i, combination = j, representativeness = representativeness)
    
    df.main_shrub <- rbind(df.main_shrub, df1)
    
  }
}

summary_shrub <- df.main_shrub %>% 
  group_by(sampling) %>% 
  summarise(mean = mean(representativeness),
            sd = sd(representativeness)) %>% 
  mutate(mean = as.numeric(mean)*100,
         sd = as.numeric(sd)*100)

inx_shrub <- which(summary_shrub$mean == max(summary_shrub$mean))
max_shrub <- round(max(summary_shrub$mean) * 100, 0) 
################################Grassland-Wet##########################################
df_grass_total_total <- df_wet %>% filter(Type == "Grassland") %>% select(-Type,-Dry_flag)
df_grass_total <- df_wet %>% filter(Type == "Grassland") %>% select(-Type, -Landcover, -GPP, -Dry_flag, -Site_ID)

combn_grass <- combn(df_grass_total, 2, simplify=FALSE)

list.frames.grass <- replicate(20, data.frame())
df.main_grass <- data.frame()

for (i in 1:20) {
  
  df_sample <- sample_n(df_grass_total_total, 4000)
  list.frames.grass[[i]] <- df_sample
  df_sample <- df_sample %>% select(-Site_ID, -GPP, -Landcover)
  combn_sample <- combn(df_sample, 2, simplify=FALSE)
  
  for (j in 1:length(combn_grass)) {
    
    df_grass <- combn_grass[[j]] 
    df_sample <- combn_sample[[j]]
    
    colnames(df_grass) <- c('X','Y')
    df_grass <- df_grass %>% 
      filter(X > quantile(X, 0.01) & X < quantile(X, 0.99) 
             & Y > quantile(Y, 0.01) & Y < quantile(Y, 0.99))
    
    # calculate the total area of this combination
    full_chull_grass <- df_grass[chull(df_grass[,1], df_grass[,2]),]
    
    full_chull_grass <- full_chull_grass %>% 
      mutate(PID = 1,
             POS = 1:nrow(full_chull_grass)) 
    
    full_polygon_grass <- st_sfc(st_polygon(list(cbind(c(full_chull_grass$X, full_chull_grass$X[1]),
                                                        c(full_chull_grass$Y, full_chull_grass$Y[1]))))) %>%
      st_set_crs(32615)
    
    full_area_grass <- st_area(full_polygon_grass)
  
    colnames(df_sample) <- c('X','Y')
    
    sample_chull <- df_sample[chull(df_sample[,1], df_sample[,2]),]
    sample_chull <- sample_chull %>% 
      mutate(PID = 2,
             POS = 1:nrow(sample_chull)) 
    
    joint_chull <- joinPolys(full_chull_grass,sample_chull)
    
    
    joint_polygon <- st_sfc(st_polygon(list(cbind(c(joint_chull$X, joint_chull$X[1]),
                                                  c(joint_chull$Y, joint_chull$Y[1]))))) %>%
      st_set_crs(32615)
    
    representativeness <- st_area(joint_polygon)/full_area_grass
    
    df1 <- data.frame(sampling = i, combination = j, representativeness = representativeness)
    
    df.main_grass <- rbind(df.main_grass, df1)
    
  }
}

summary_grass <- df.main_grass %>% 
  group_by(sampling) %>% 
  summarise(mean = mean(representativeness),
            sd = sd(representativeness)) %>% 
  mutate(mean = as.numeric(mean)*100,
         sd = as.numeric(sd)*100)

inx_grass <- which(summary_grass$mean == max(summary_grass$mean))
max_grass <- round(max(summary_grass$mean) * 100, 0) 
################################Grassland-Dry##########################################
df_grass_total_total <- df_dry %>% filter(Type == "Grassland") %>% select(-Type,-Dry_flag)
df_grass_total <- df_dry %>% filter(Type == "Grassland") %>% select(-Type, -Landcover, -GPP, -Dry_flag, -Site_ID)

combn_grass <- combn(df_grass_total, 2, simplify=FALSE)

list.frames.grass <- replicate(20, data.frame())
df.main_grass <- data.frame()

for (i in 1:20) {
  
  df_sample <- sample_n(df_grass_total_total, 4000)
  list.frames.grass[[i]] <- df_sample
  df_sample <- df_sample %>% select(-Site_ID, -GPP, -Landcover)
  combn_sample <- combn(df_sample, 2, simplify=FALSE)
  
  for (j in 1:length(combn_grass)) {
    
    df_grass <- combn_grass[[j]] 
    df_sample <- combn_sample[[j]]
    
    colnames(df_grass) <- c('X','Y')
    df_grass <- df_grass %>% 
      filter(X > quantile(X, 0.01) & X < quantile(X, 0.99) 
             & Y > quantile(Y, 0.01) & Y < quantile(Y, 0.99))
    
    # calculate the total area of this combination
    full_chull_grass <- df_grass[chull(df_grass[,1], df_grass[,2]),]
    
    full_chull_grass <- full_chull_grass %>% 
      mutate(PID = 1,
             POS = 1:nrow(full_chull_grass)) 
    
    full_polygon_grass <- st_sfc(st_polygon(list(cbind(c(full_chull_grass$X, full_chull_grass$X[1]),
                                                       c(full_chull_grass$Y, full_chull_grass$Y[1]))))) %>%
      st_set_crs(32615)
    
    full_area_grass <- st_area(full_polygon_grass)
    
    colnames(df_sample) <- c('X','Y')
    
    sample_chull <- df_sample[chull(df_sample[,1], df_sample[,2]),]
    sample_chull <- sample_chull %>% 
      mutate(PID = 2,
             POS = 1:nrow(sample_chull)) 
    
    joint_chull <- joinPolys(full_chull_grass,sample_chull)
    
    
    joint_polygon <- st_sfc(st_polygon(list(cbind(c(joint_chull$X, joint_chull$X[1]),
                                                  c(joint_chull$Y, joint_chull$Y[1]))))) %>%
      st_set_crs(32615)
    
    representativeness <- st_area(joint_polygon)/full_area_grass
    
    df1 <- data.frame(sampling = i, combination = j, representativeness = representativeness)
    
    df.main_grass <- rbind(df.main_grass, df1)
    
  }
}

summary_grass <- df.main_grass %>% 
  group_by(sampling) %>% 
  summarise(mean = mean(representativeness),
            sd = sd(representativeness)) %>% 
  mutate(mean = as.numeric(mean)*100,
         sd = as.numeric(sd)*100)

inx_grass <- which(summary_grass$mean == max(summary_grass$mean))
max_grass <- round(max(summary_grass$mean) * 100, 0) 