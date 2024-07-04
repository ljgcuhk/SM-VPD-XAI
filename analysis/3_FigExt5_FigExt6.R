rm(list=ls())
pacman::p_load(tidyverse, ncdf4, fields, RColorBrewer,rnaturalearth,scales,ggpubr,patchwork,ggsci,
               maps,rnaturalearth,sf,lubridate, agricolae,cowplot,reshape2,viridis,grid,gridExtra, rstatix)
input_folder <- "...file/"
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

df_raw <- read.csv(paste0(input_folder, "spatial_main.csv"))
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

df_dry_stat<- df_dry %>% filter(max_var_water != "NA") %>% 
  filter(land_type == "DBF" | land_type == "GRA" | land_type == "OSH" |
             land_type == "SAV" | land_type == "WSA") %>% 
  rename(Zr = Zroot, Hc = canopy_height, Water = max_var_water) %>% 
  mutate(Zr = Zr/1000, S0 = S0/1000,
         isohydricity = ifelse(isohydricity < 0, 0, isohydricity)) %>% 
  gather(key = "Var", value = "Value", WI, S0, Zr, elevation, Hc, P50, drought_coupling, isohydricity) %>%
  mutate(Water = case_when(Water == "AbsVPD" ~ "VPD",
                           Water == "AbsSWC" ~ "SM")) %>% 
  convert_as_factor(Var, Water)

df_wet_stat<- df_wet %>% filter(max_var_water != "NA") %>% 
  filter(land_type == "DBF" | land_type == "GRA" | land_type == "ENF" | land_type == "MF" |
         land_type == "EBF" | land_type == "SAV" | land_type == "WSA") %>% 
  rename(Zr = Zroot, Hc = canopy_height, Water = max_var_water) %>% 
  mutate(Zr = Zr/1000, S0 = S0/1000,
         isohydricity = ifelse(isohydricity < 0, 0, isohydricity)) %>% 
  gather(key = "Var", value = "Value", WI, S0, Zr, elevation, Hc, P50, drought_coupling, isohydricity) %>%
  mutate(Water = case_when(Water == "AbsVPD" ~ "VPD",
                           Water == "AbsSWC" ~ "SM")) %>% 
  convert_as_factor(Var, Water)

stat.test_dry <- df_dry_stat %>%
  group_by(Var, land_type) %>%
  t_test(Value ~ Water) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  add_xy_position(x = "Water")

stat.test_wet <- df_wet_stat %>%
  group_by(Var, land_type) %>%
  t_test(Value ~ Water) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  add_xy_position(x = "Water")

WI_dry <- ggboxplot(df_dry_stat %>% filter(Var == "WI"), x = "Water", y = "Value", fill = "#E7B800",
                     facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_dry %>% filter(Var == "WI"), y.position = 0.75, hide.ns = TRUE, size = 6) +
  ylim(0,0.8) +
  labs(y= expression("Wetness index")) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

Zr_dry <- ggboxplot(df_dry_stat %>% filter(Var == "Zr"), x = "Water", y = "Value", fill = "#E7B800",
                    facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_dry %>% filter(Var == "Zr"), y.position = 23, hide.ns = TRUE, size = 6) +
  ylim(0,25) +
  labs(y= expression("Z"["r"]~'(m)'))+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

Hc_dry <- ggboxplot(df_dry_stat %>% filter(Var == "Hc"), x = "Water", y = "Value", fill = "#E7B800",
                    facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_dry %>% filter(Var == "Hc"), y.position = 33, hide.ns = TRUE, size = 6) +
  ylim(0,35) +
  labs(y= expression('Canopy height'~'(m)'))+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

P50_dry <- ggboxplot(df_dry_stat %>% filter(Var == "P50"), x = "Water", y = "Value", fill = "#E7B800",
                    facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_dry %>% filter(Var == "P50"), y.position = 0, hide.ns = TRUE, size = 6) +
  ylim(-8,0.5) +
  labs(y= expression(psi["50"]~'(MPa)'))+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

isohydricity_dry <- ggboxplot(df_dry_stat %>% filter(Var == "isohydricity"), x = "Water", y = "Value", fill = "#E7B800",
                          facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_dry %>% filter(Var == "isohydricity"), y.position = 1.3, hide.ns = TRUE, size = 6) +
  ylim(0,1.5) +
  labs(y= expression("Anisohydricity"))+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

WI_wet <- ggboxplot(df_wet_stat %>% filter(Var == "WI"), x = "Water", y = "Value", fill = "#00AFBB",
                    facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_wet %>% filter(Var == "WI"), y.position = 4.1, hide.ns = TRUE, size = 6) +
  ylim(0, 4.5) +
  labs(y= expression("Wetness index")) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

Zr_wet <- ggboxplot(df_wet_stat %>% filter(Var == "Zr"), x = "Water", y = "Value", fill = "#00AFBB",
                    facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_wet %>% filter(Var == "Zr"), y.position = 28, hide.ns = TRUE, size = 6) +
  ylim(0,30) +
  labs(y= expression("Z"["r"]~'(m)'))+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

P50_wet <- ggboxplot(df_wet_stat %>% filter(Var == "P50"), x = "Water", y = "Value", fill = "#00AFBB",
                     facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_wet %>% filter(Var == "P50"), y.position = 0, hide.ns = TRUE, size = 6) +
  ylim(-8,0.5) +
  labs(y= expression(psi["50"]~'(MPa)'))+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

isohydricity_wet <- ggboxplot(df_wet_stat %>% filter(Var == "isohydricity"), x = "Water", y = "Value", fill = "#00AFBB",
                              facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_wet %>% filter(Var == "isohydricity"), y.position = 1.3, hide.ns = TRUE, size = 6) +
  ylim(0,1.5) +
  labs(y= expression("Anisohydricity"))+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)

Hc_wet <- ggboxplot(df_wet_stat %>% filter(Var == "Hc"), x = "Water", y = "Value", fill = "#00AFBB",
                    facet = "land_type", nrow = 1, bxp.errorbar = TRUE, outlier.shape = NA) +
  stat_pvalue_manual(stat.test_wet %>% filter(Var == "Hc"), y.position = 48, hide.ns = TRUE, size = 6) +
  ylim(0,50) +
  labs(y= expression("Canopy height"~'(m)'))+
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank()) +
  guides(color = FALSE)






