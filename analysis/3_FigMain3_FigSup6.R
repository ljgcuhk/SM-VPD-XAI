rm(list=ls())
pacman::p_load(tidyverse, ncdf4, fields, RColorBrewer,rnaturalearth,scales,ggpubr,patchwork,ggsci,rstatix,
               maps,rnaturalearth,sf,lubridate, agricolae,cowplot,reshape2,viridis,grid,gridExtra)

input_folder <- ".../file/"

# functionns to extract the dominant factors for determining pixel-level SIF
getMaxVarName <- function(row) {
  max_vars <- names(which(row == max(row, na.rm = TRUE)))
  return(paste(max_vars, collapse = ", "))
}

getWaterVar <- function(row) {
  if (any(is.na(row))) {
    return(NA)}
  var1 <- row[1]
  var2 <- row[2]
  if(var1 == 0 || var2 == 0) {
    return(NA)}
  percentage_diff <- abs(var1 - var2) / min(var1, var2, na.rm = TRUE)
  if (percentage_diff >= 0.20) {
    max_var_name <- names(which.max(row))
    return(max_var_name)
  } else {return(NA)}
}

# 0 - prepare data --------------------------------------
# import the organized spatial data in a file called spatial_main.csv
df_raw <- read.csv(paste0(input_folder, "spatial_main.csv"))
# these two files storing Causal Shapley values calculated from the XGBoost models with the best performance among three
df_cshap_dry <- read.csv(paste0(input_folder, "causalSHAP_result/all_causalSHAP_dry_065_1.csv"))
df_cshap_wet <- read.csv(paste0(input_folder, "causalSHAP_result/all_causalSHAP_wet_065_2.csv"))

# Define the breaks and labels
breaks <- c(0, 0.03, 0.2, 0.5, 0.65, 50)
labels <- c("Hyper Arid", "Arid", "Semi-Arid", "Dry sub-humid", "Humid")
df_raw$aridity_factor <- cut(df_raw$aridity, breaks = breaks, labels = labels, include.lowest = TRUE)

df_dry <- merge(df_raw, df_cshap_dry, by = c("lat", "lon"), all.x = TRUE) %>% 
  select(-lon_adj.y) %>% 
  mutate(AbsTair = abs(Tair),
         AbsSW = abs(SW),
         AbsVPD = abs(VPD),
         AbsSWC = abs(SWC))
df_wet <- merge(df_raw, df_cshap_wet, by = c("lat", "lon"), all.x = TRUE) %>% 
  select(-lon_adj.y) %>% 
  mutate(AbsTair = abs(Tair),
         AbsSW = abs(SW),
         AbsVPD = abs(VPD),
         AbsSWC = abs(SWC))

df_dry$max_var_all = apply(df_dry[,c("AbsTair", "AbsSW", "AbsVPD", "AbsSWC")], 1, getMaxVarName)
df_dry$max_var_water = apply(df_dry[,c("AbsVPD", "AbsSWC")], 1, getWaterVar)
df_dry <- df_dry %>% mutate(max_var_all = ifelse(max_var_all == "", NA, max_var_all),
                     max_var_water = ifelse(max_var_water == "", NA, max_var_water))

split_vars_all_dry <- unlist(strsplit(as.character(df_dry$max_var_all), ", "))
split_vars_water_dry <- unlist(strsplit(as.character(df_dry$max_var_water), ", "))
var_all_counts_dry <- table(split_vars_all_dry)
var_water_counts_dry <- table(split_vars_water_dry)

df_dry_plot <- df_dry %>% select(lat,lon_adj.x,max_var_all,max_var_water,AbsVPD,AbsSWC,aridity, aridity_factor, land_type,Group) %>% 
  rename(lon = lon_adj.x, dominant_var = max_var_all, dominant_water = max_var_water) %>% 
  mutate(dominant_var = case_when(dominant_var == "AbsTair" ~ "TA",
                                  dominant_var == "AbsSW" ~ "SW",
                                  dominant_var == "AbsVPD" ~ "VPD",
                                  dominant_var == "AbsSWC" ~ "SM"),
         dominant_water = case_when(dominant_water == "AbsVPD" ~ "VPD",
                                  dominant_water == "AbsSWC" ~ "SM")) %>% 
  mutate(dominant_var = ifelse(Group == "Other", NA, dominant_var),
         dominant_water = ifelse(Group == "Other", NA, dominant_water))

df_wet$max_var_all = apply(df_wet[,c("AbsTair", "AbsSW", "AbsVPD", "AbsSWC")], 1, getMaxVarName)
df_wet$max_var_water = apply(df_wet[,c("AbsVPD", "AbsSWC")], 1, getWaterVar)
df_wet <- df_wet %>% mutate(max_var_all = ifelse(max_var_all == "", NA, max_var_all),
                            max_var_water = ifelse(max_var_water == "", NA, max_var_water))
split_vars_all_wet <- unlist(strsplit(as.character(df_wet$max_var_all), ", "))
split_vars_water_wet <- unlist(strsplit(as.character(df_wet$max_var_water), ", "))
var_all_counts_wet <- table(split_vars_all_wet)
var_water_counts_wet <- table(split_vars_water_wet)

df_wet_plot <- df_wet %>% select(lat,lon_adj.x,max_var_all,max_var_water,AbsVPD,AbsSWC,land_type, Group) %>% 
  rename(lon = lon_adj.x, dominant_var = max_var_all, dominant_water = max_var_water) %>% 
  mutate(dominant_var = case_when(dominant_var == "AbsTair" ~ "TA",
                                  dominant_var == "AbsSW" ~ "SW",
                                  dominant_var == "AbsVPD" ~ "VPD",
                                  dominant_var == "AbsSWC" ~ "SM"),
         dominant_water = case_when(dominant_water == "AbsVPD" ~ "VPD",
                                    dominant_water == "AbsSWC" ~ "SM")) %>% 
  mutate(dominant_var = ifelse(Group == "Other", NA, dominant_var),
         dominant_water = ifelse(Group == "Other", NA, dominant_water))

# 1 - dominant water factor for the dry periods --------------------------------------
# Filter out NA values from the data
df_dry_plot_filtered <- df_dry_plot %>%
  filter(!is.na(dominant_water))

p3a <- ggplot(data = df_dry_plot_filtered) + 
  geom_tile(aes(x = lon, y = lat, fill = dominant_water)) + 
  scale_fill_manual(values = c("VPD" = "#FB9C2A",  # Sky Blue for VPD
                               "SM" = "#137DC5"),  # Saddle Brown for Soil Moisture
                    na.value = "transparent", 
                    name = "") +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  # labs(tag = "a") +
  theme_minimal() +
  theme(legend.position = c(.12, .55),
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) + 
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_legend(title.position = "right",
                             title.hjust = 0.5))

# Split the 'max_var' column by comma and unlist it
split_vars_ <- unlist(strsplit(as.character(df_dry_plot$dominant_water), ", "))
# Count the frequency of each variable name
var_counts_ <- table(split_vars_)
# Convert the table to a dataframe for better readability
count_df_ <- as.data.frame(var_counts_)
count_df_$prop <- round(count_df_$Freq/(sum(count_df_$Freq)) * 100,0)

count.data_ <- data.frame(class = c("1SM", "2VPD"),
                         prop = c(count_df_[1,3], count_df_[2,3])) %>% 
  arrange(desc(class)) %>% 
  mutate(lab.pos = cumsum(prop) - 0.5*prop)

mycols_ <- c("#137DC5", "#FB9C2A")
p3a_1 <- ggplot(count.data_, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.pos, label = prop), color = "white", size = 3)+
  scale_fill_manual(name = "",values = mycols_,
                    labels=c('VPD','SM')) +
  theme_void()+
  xlim(0.5, 2.5)+
  theme(legend.position="none")

# Convert p3a_1 to a grob
p3a1_grob <- ggplotGrob(p3a_1)
p3a <- p3a + annotation_custom(grob = p3a1_grob, xmin = -180, xmax = -120, ymin = -60, ymax = 0)

# 2 - dominant water factor for each PFT during the dry periods --------------------------------------
# Calculate the percentages
dominant_water_counts_dry <- df_dry_plot %>%
  mutate(land_type = ifelse(land_type == "OSH", "SH", land_type),
         land_type = ifelse(land_type == "CSH", "SH", land_type)) %>% 
  count(land_type, dominant_water) %>%
  drop_na() %>% 
  group_by(land_type) %>%
  mutate(Percentage = n / sum(n) * 100)

# Since the data is already in long format, just rename the columns appropriately
dominant_water_long_dry <- dominant_water_counts_dry %>%
  rename(Dominant = dominant_water) 

# Reorder 'land_type' according to the specified order
land_type_order <- c('SH', 'GRA', 'SAV', 'WSA', 'DBF','EBF','MF')
dominant_water_long_dry$land_type <- factor(dominant_water_long_dry$land_type, levels = land_type_order)


p3c <- ggplot(dominant_water_long_dry, aes(x = land_type, y = Percentage, fill = Dominant, label = round(Percentage))) +
  geom_bar(stat = "identity", alpha=0.6) +
  scale_fill_manual(values = c("VPD" = "#FB9C2A", "SM" = "#137DC5")) +
  geom_text(size = 3, position = position_stack(vjust = 0.6)) +
  scale_x_discrete(limits = c('SH', 'GRA', 'SAV', 'WSA', 'DBF','EBF', 'MF')) +
  # labs(tag = "c") +
  theme_minimal() +
  labs(x = NULL, y = "Percentage (%)", fill = "Dominant Water") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.position = "none",
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))


# 3 - dominant water factor for the wet periods --------------------------------------
df_wet_plot_filtered <- df_wet_plot %>%
  filter(!is.na(dominant_water))

p3b <- ggplot(data = df_wet_plot_filtered) + 
  geom_tile(aes(x = lon, y = lat, fill = dominant_water)) + 
  scale_fill_manual(values = c("VPD" = "#FB9C2A",  # Sky Blue for VPD
                               "SM" = "#137DC5"),  # Saddle Brown for Soil Moisture
                    na.value = "transparent", 
                    name = "") +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  # labs(tag = "b") +
  theme_minimal() +
  theme(legend.position = c(.12, .55),
        legend.direction = "vertical",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        #axis.title.y = element_text(size = 12),
        axis.title.y = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) + 
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_legend(title.position = "right",
                             title.hjust = 0.5))

# Split the 'max_var' column by comma and unlist it
split_vars_ <- unlist(strsplit(as.character(df_wet_plot$dominant_water), ", "))
# Count the frequency of each variable name
var_counts_ <- table(split_vars_)
# Convert the table to a dataframe for better readability
count_df_ <- as.data.frame(var_counts_)
count_df_$prop <- round(count_df_$Freq/(sum(count_df_$Freq)) * 100,0)

count.data_ <- data.frame(class = c("1SM", "2VPD"),
                          prop = c(count_df_[1,3], count_df_[2,3])) %>% 
  arrange(desc(class)) %>% 
  mutate(lab.pos = cumsum(prop) - 0.5*prop)

mycols_ <- c("#137DC5","#FB9C2A")
p3b_1 <- ggplot(count.data_, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.pos, label = prop), color = "white", size = 3)+
  scale_fill_manual(name = "",values = mycols_,
                    labels=c('VPD', 'SM')) +
  theme_void()+
  xlim(0.5, 2.5)+
  theme(legend.position="none")
# Convert p2 to a grob
p3b1_grob <- ggplotGrob(p3b_1)
# Print the combined plot
p3b <- p3b + annotation_custom(grob = p3b1_grob, xmin = -180, xmax = -120, ymin = -60, ymax = 0)


# 4 - dominant water factor for each PFT during the wet periods --------------------------------------
# Calculate the percentages
dominant_water_counts_wet <- df_wet_plot %>%
  mutate(land_type = ifelse(land_type == "OSH", "SH", land_type),
         land_type = ifelse(land_type == "CSH", "SH", land_type)) %>% 
  count(land_type, dominant_water) %>%
  drop_na() %>% 
  group_by(land_type) %>%
  mutate(Percentage = n / sum(n) * 100)

# Since the data is already in long format, just rename the columns appropriately
dominant_water_long_wet <- dominant_water_counts_wet %>%
  rename(Dominant = dominant_water)

# Reorder 'land_type' according to the specified order
land_type_order <- c('DNF','SH','MF','DBF','GRA', 'SAV', 'WSA', 'EBF','ENF')
dominant_water_long_wet$land_type <- factor(dominant_water_long_wet$land_type, levels = land_type_order)

p3d <- ggplot(dominant_water_long_wet, aes(x = land_type, y = Percentage, fill = Dominant, label = round(Percentage))) +
  geom_bar(stat = "identity", alpha=0.6) +
  scale_fill_manual(values = c("VPD" = "#FB9C2A", "SM" = "#137DC5")) +
  geom_text(size = 3, position = position_stack(vjust = 0.6)) +
  scale_x_discrete(limits = c('DNF','SH','MF','DBF','GRA', 'SAV', 'WSA', 'EBF','ENF')) +
  # labs(tag = "d") +
  theme_minimal() +
  labs(x = NULL, y = "Percentage (%)", fill = "Dominant Water") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.position = "none",
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))


# 5 - dominant water factor along aridity classes --------------------------------------
# Calculate the percentages
dominant_water_counts_arid_dry <- df_dry_plot %>%
  count(aridity_factor, dominant_water) %>%
  drop_na() %>% 
  group_by(aridity_factor) %>%
  mutate(Percentage = n / sum(n) * 100)

# Since the data is already in long format, just rename the columns appropriately
dominant_water_long_arid_dry <- dominant_water_counts_arid_dry %>%
  rename(Dominant = dominant_water)

# Reorder 'land_type' according to the specified order
arid_order <- c('Hyper Arid', 'Arid', 'Semi-Arid', 'Dry sub-humid')
dominant_water_long_arid_dry$aridity_factor <- factor(dominant_water_long_arid_dry$aridity_factor, levels = arid_order)

dominant_water_counts_arid_dry <- df_dry_plot %>%
  count(aridity_factor, dominant_water) %>%
  drop_na() %>% 
  group_by(aridity_factor) %>%
  mutate(Percentage = n / sum(n) * 100)

dominant_water_counts_arid_wet <- df_wet_plot %>%
  count(dominant_water) %>%
  drop_na() %>%
  mutate(Percentage = n / sum(n) * 100)

dominant_water_counts_arid_wet <- data.frame(aridity_factor = c("Humid", "Humid"),
                                             dominant_water = c("SM", "VPD"),
                                             n = c(3456, 12514),
                                             Percentage = c(21.64058, 78.35942))

dominant_water_counts_arid_dry <- rbind(dominant_water_counts_arid_dry, dominant_water_counts_arid_wet)
# Since the data is already in long format, just rename the columns appropriately
dominant_water_long_arid_dry <- dominant_water_counts_arid_dry %>%
  rename(Dominant = dominant_water)
# Reorder 'land_type' according to the specified order
arid_order <- c('Hyper Arid', 'Arid', 'Semi-Arid', 'Dry sub-humid', 'Humid')
dominant_water_long_arid_dry$aridity_factor <- factor(dominant_water_long_arid_dry$aridity_factor, levels = rev(arid_order))

p3e <- ggplot(dominant_water_long_arid_dry, aes(x = aridity_factor, y = Percentage, fill = Dominant, label = round(Percentage))) +
  geom_bar(stat = "identity", alpha=0.6, position = position_stack()) +  # Ensure stacking
  scale_fill_manual(values = c("VPD" = "#FB9C2A", "SM" = "#137DC5")) +
  geom_text(size = 4, position = position_stack(vjust = 0.5)) +  # Adjust text position for stacked bars
  scale_x_discrete(limits = c('Hyper Arid', 'Arid', 'Semi-Arid', 'Dry sub-humid', 'Humid')) +
  # labs(tag = "e") +
  theme_minimal() +
  labs(x = NULL, y = "Percentage (%)", fill = "Dominant Water") +
  coord_flip() +  # Rotate the plot to make bars horizontal
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2))


# 6 - S0, Zr, elevation, canopy height for each PFT  --------------------------------------
df_dry_root <- df_dry %>% 
  select(max_var_water, Land_Cover, WI, S0, Zroot, elevation, canopy_height, P50, drought_coupling, isohydricity) %>% 
  mutate(Land_Cover = ifelse(Land_Cover == "OSH", "SH", Land_Cover),
         Land_Cover = ifelse(Land_Cover == "CSH", "SH", Land_Cover)) %>% 
  mutate(max_var_water = ifelse(max_var_water == "AbsSWC", "VWC", max_var_water),
         max_var_water = ifelse(max_var_water == "AbsVPD", "VPD", max_var_water),
         max_var_water = as.factor(max_var_water),
         Land_Cover = as.factor(Land_Cover)) %>% 
  drop_na() %>% 
  filter(Land_Cover %in% c('SH', 'GRA', 'SAV', 'WSA', 'DBF', 'EBF','MF'))

df_wet_root <- df_wet %>% 
  select(max_var_water, Land_Cover, WI, S0, Zroot, elevation, canopy_height, P50, drought_coupling, isohydricity) %>% 
  mutate(Land_Cover = ifelse(Land_Cover == "OSH", "SH", Land_Cover),
         Land_Cover = ifelse(Land_Cover == "CSH", "SH", Land_Cover)) %>% 
  mutate(max_var_water = ifelse(max_var_water == "AbsSWC", "VWC", max_var_water),
         max_var_water = ifelse(max_var_water == "AbsVPD", "VPD", max_var_water),
         max_var_water = as.factor(max_var_water),
         Land_Cover = as.factor(Land_Cover)) %>% 
  drop_na() %>% 
  filter(Land_Cover %in% c('SH', 'GRA', 'SAV', 'WSA', 'DBF', 'DNF','MF', 'EBF', 'ENF'))


lm.dry.Zroot <- lm(Zroot ~ Land_Cover, df_dry_root)
av.dry.Zroot <- aov(lm.dry.Zroot)
tukey.test.dry.Zroot <- HSD.test(av.dry.Zroot, trt = 'Land_Cover')
Tukey_label.dry.Zroot <- data.frame(tukey.test.dry.Zroot$groups)
Tukey_label.dry.Zroot$Land_Cover <- rownames(tukey.test.dry.Zroot$groups)
df_dry_root$Zroot_order <- factor(df_dry_root$Land_Cover, 
                               levels = Tukey_label.dry.Zroot$Land_Cover, ordered = TRUE)

ggplot(df_dry_root, aes(x= Zroot_order, y=Zroot/1000)) + 
  geom_boxplot(size = 1, outlier.color= NA, color = "khaki4") + 
  geom_jitter(position=position_jitter(0.15), size = 0.4, alpha = 0.4, color = "khaki2") + 
  geom_text(data = Tukey_label.dry.Zroot, aes(x = Land_Cover, label = groups, y = 24), 
            vjust=0, size = 5, color = "black") + 
  scale_y_continuous(limits = c(0,26)) +
  labs(y= expression("Z"["r"]~'(m)'))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

lm.dry.P50 <- lm(P50 ~ Land_Cover, df_dry_root)
av.dry.P50 <- aov(lm.dry.P50)
tukey.test.dry.P50 <- HSD.test(av.dry.P50, trt = 'Land_Cover')
Tukey_label.dry.P50 <- data.frame(tukey.test.dry.P50$groups)
Tukey_label.dry.P50$Land_Cover <- rownames(tukey.test.dry.P50$groups)
df_dry_root$P50_order <- factor(df_dry_root$Land_Cover, 
                                levels = Tukey_label.dry.P50$Land_Cover, ordered = TRUE)

ggplot(df_dry_root, aes(x= P50_order, y=P50)) + 
  geom_boxplot(size = 1, outlier.color= NA, color = "khaki4") + 
  geom_jitter(position=position_jitter(0.15), size = 0.4, alpha = 0.4, color = "khaki2") + 
  geom_text(data = Tukey_label.dry.P50, aes(x = Land_Cover, label = groups, y = 0.5), 
            vjust=0, size = 5, color = "black") + 
  scale_y_continuous(limits = c(-10,1.8)) +
  labs(y= expression(psi["50"]~'(MPa)'))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

lm.dry.isohydricity <- lm(isohydricity ~ Land_Cover, df_dry_root)
av.dry.isohydricity <- aov(lm.dry.isohydricity)
tukey.test.dry.isohydricity <- HSD.test(av.dry.isohydricity, trt = 'Land_Cover')
Tukey_label.dry.isohydricity <- data.frame(tukey.test.dry.isohydricity$groups)
Tukey_label.dry.isohydricity$Land_Cover <- rownames(tukey.test.dry.isohydricity$groups)
df_dry_root$isohydricity_order <- factor(df_dry_root$Land_Cover, 
                                             levels = Tukey_label.dry.isohydricity$Land_Cover, ordered = TRUE)

ggplot(df_dry_root, aes(x= isohydricity_order, y=isohydricity)) + 
  geom_boxplot(size = 1, outlier.color= NA, color = "khaki4") + 
  geom_jitter(position=position_jitter(0.15), size = 0.4, alpha = 0.4, color = "khaki2") + 
  geom_text(data = Tukey_label.dry.isohydricity, aes(x = Land_Cover, label = groups, y = 1.2), 
            vjust=0, size = 5, color = "black") + 
  scale_y_continuous(limits = c(0,1.3)) +
  labs(y= "Anisohydricity")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

lm.dry.canopy_height <- lm(canopy_height ~ Land_Cover, df_dry_root)
av.dry.canopy_height <- aov(lm.dry.canopy_height)
tukey.test.dry.canopy_height <- HSD.test(av.dry.canopy_height, trt = 'Land_Cover')
Tukey_label.dry.canopy_height <- data.frame(tukey.test.dry.canopy_height$groups)
Tukey_label.dry.canopy_height$Land_Cover <- rownames(tukey.test.dry.canopy_height$groups)
df_dry_root$canopy_height_order <- factor(df_dry_root$Land_Cover, 
                                  levels = Tukey_label.dry.canopy_height$Land_Cover, ordered = TRUE)

ggplot(df_dry_root, aes(x= canopy_height_order, y=canopy_height)) + 
  geom_boxplot(size = 1, outlier.color= NA, color = "khaki4") + 
  geom_jitter(position=position_jitter(0.15), size = 0.4, alpha = 0.4, color = "khaki2") + 
  geom_text(data = Tukey_label.dry.canopy_height, aes(x = Land_Cover, label = groups, y = 45), 
            vjust=0, size = 5, color = "black") + 
  scale_y_continuous(limits = c(0,50)) +
  labs(y= expression("Canopy height"~'(m)'))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)


lm.wet.Zroot <- lm(Zroot ~ Land_Cover, df_wet_root)
av.wet.Zroot <- aov(lm.wet.Zroot)
tukey.test.wet.Zroot <- HSD.test(av.wet.Zroot, trt = 'Land_Cover')
Tukey_label.wet.Zroot <- data.frame(tukey.test.wet.Zroot$groups)
Tukey_label.wet.Zroot$Land_Cover <- rownames(tukey.test.wet.Zroot$groups)
df_wet_root$Zroot_order <- factor(df_wet_root$Land_Cover, 
                                  levels = Tukey_label.wet.Zroot$Land_Cover, ordered = TRUE)

ggplot(df_wet_root, aes(x= Zroot_order, y=Zroot/1000)) + 
  geom_boxplot(size = 1, outlier.color= NA, color = "lightblue3") + 
  geom_jitter(position=position_jitter(0.15), size = 0.4, alpha = 0.4, color = "lightblue1") + 
  geom_text(data = Tukey_label.wet.Zroot, aes(x = Land_Cover, label = groups, y = 24), 
            vjust=0, size = 5, color = "black") + 
  scale_y_continuous(limits = c(0,26)) +
  labs(y= expression("Z"["r"]~'(m)'))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

lm.wet.P50 <- lm(P50 ~ Land_Cover, df_wet_root)
av.wet.P50 <- aov(lm.wet.P50)
tukey.test.wet.P50 <- HSD.test(av.wet.P50, trt = 'Land_Cover')
Tukey_label.wet.P50 <- data.frame(tukey.test.wet.P50$groups)
Tukey_label.wet.P50$Land_Cover <- rownames(tukey.test.wet.P50$groups)
df_wet_root$P50_order <- factor(df_wet_root$Land_Cover, 
                                levels = Tukey_label.wet.P50$Land_Cover, ordered = TRUE)

ggplot(df_wet_root, aes(x= P50_order, y=P50)) + 
  geom_boxplot(size = 1, outlier.color= NA, color = "lightblue3") + 
  geom_jitter(position=position_jitter(0.15), size = 0.4, alpha = 0.4, color = "lightblue1") + 
  geom_text(data = Tukey_label.wet.P50, aes(x = Land_Cover, label = groups, y = 0.5), 
            vjust=0, size = 5, color = "black") + 
  scale_y_continuous(limits = c(-10,1.8)) +
  labs(y= expression(psi["50"]~'(MPa)'))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

lm.wet.isohydricity <- lm(isohydricity ~ Land_Cover, df_wet_root)
av.wet.isohydricity <- aov(lm.wet.isohydricity)
tukey.test.wet.isohydricity <- HSD.test(av.wet.isohydricity, trt = 'Land_Cover')
Tukey_label.wet.isohydricity <- data.frame(tukey.test.wet.isohydricity$groups)
Tukey_label.wet.isohydricity$Land_Cover <- rownames(tukey.test.wet.isohydricity$groups)
df_wet_root$isohydricity_order <- factor(df_wet_root$Land_Cover, 
                                         levels = Tukey_label.wet.isohydricity$Land_Cover, ordered = TRUE)

ggplot(df_wet_root, aes(x= isohydricity_order, y=isohydricity)) + 
  geom_boxplot(size = 1, outlier.color= NA, color = "lightblue3") + 
  geom_jitter(position=position_jitter(0.15), size = 0.4, alpha = 0.4, color = "lightblue1") + 
  geom_text(data = Tukey_label.wet.isohydricity, aes(x = Land_Cover, label = groups, y = 1.2), 
            vjust=0, size = 5, color = "black") + 
  scale_y_continuous(limits = c(0,1.3)) +
  labs(y= "Anisohydricity")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

lm.wet.canopy_height <- lm(canopy_height ~ Land_Cover, df_wet_root)
av.wet.canopy_height <- aov(lm.wet.canopy_height)
tukey.test.wet.canopy_height <- HSD.test(av.wet.canopy_height, trt = 'Land_Cover')
Tukey_label.wet.canopy_height <- data.frame(tukey.test.wet.canopy_height$groups)
Tukey_label.wet.canopy_height$Land_Cover <- rownames(tukey.test.wet.canopy_height$groups)
df_wet_root$canopy_height_order <- factor(df_wet_root$Land_Cover, 
                                          levels = Tukey_label.wet.canopy_height$Land_Cover, ordered = TRUE)

ggplot(df_wet_root, aes(x= canopy_height_order, y=canopy_height)) + 
  geom_boxplot(size = 1, outlier.color= NA, color = "lightblue3") + 
  geom_jitter(position=position_jitter(0.15), size = 0.4, alpha = 0.4, color = "lightblue1") + 
  geom_text(data = Tukey_label.wet.canopy_height, aes(x = Land_Cover, label = groups, y = 41), 
            vjust=0, size = 5, color = "black") + 
  scale_y_continuous(limits = c(0,45)) +
  labs(y= expression("Canopy height"~'(m)'))+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)


