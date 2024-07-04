rm(list=ls())
pacman::p_load(tidyverse,cowplot,scales,RColorBrewer)
InputFolder <- ".../file/"

# 0-0 - read original files for dry----------------------------------------------
df.hourly.forest.dry.ori <- read.csv(paste0(InputFolder,"representative_sampling/",
                                            list.files(paste0(InputFolder,"representative_sampling/"), 
                                                       pattern="_Forest_Hourly_dry")))
df.hourly.grass.dry.ori <- read.csv(paste0(InputFolder,"representative_sampling/",
                                           list.files(paste0(InputFolder,"representative_sampling/"), 
                                                      pattern="_Grassland_Hourly_dry")))
df.hourly.dry.dry.ori <- read.csv(paste0(InputFolder,"representative_sampling/",
                                         list.files(paste0(InputFolder,"representative_sampling/"), 
                                                    pattern="_Dryland_Hourly_dry")))

df.year.dry <- read.csv(paste0(InputFolder,"causalSHAP_result/all_causalSHAP_origin_dry_065_1.csv")) %>% 
  mutate(absVPD = abs(VPD),
         absVWC = abs(SWC)) %>% 
  drop_na(absVPD, absVWC)

# 0-1 - read original shapley value files ----------------------------------------------
df.hourly.forest.shap_dry <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Forest_dry_3.csv")) %>% 
  rename(Tair_shap = Tair, SW_shap = SW, VPD_shap = VPD, VWC_shap = SWC, Site_ID_shap = Site_ID) %>% 
  select(-Landcover) %>% cbind(df.hourly.forest.dry.ori) %>% 
  mutate(Landcover = as.factor(Landcover), Group = "Forest",
         absVPD = abs(VPD_shap),
         absVWC = abs(VWC_shap)) %>% 
  rename(VWC = SWC, Forest = Landcover)

df.hourly.dryland.shap_dry <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Dryland_dry_1.csv")) %>% 
  rename(Tair_shap = Tair, SW_shap = SW, VPD_shap = VPD, VWC_shap = SWC, Site_ID_shap = Site_ID) %>% 
  select(-Landcover) %>% cbind(df.hourly.dry.dry.ori) %>% 
  mutate(Landcover = ifelse(Landcover == "WSH", "WSA", Landcover)) %>% 
  mutate(Landcover = as.factor(Landcover), Group = "Dryland",
         absVPD = abs(VPD_shap),
         absVWC = abs(VWC_shap)) %>% 
  rename(VWC = SWC, Dryland = Landcover)

df.hourly.grassland.shap_dry <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Grassland_dry_2.csv")) %>% 
  rename(Tair_shap = Tair, SW_shap = SW, VPD_shap = VPD, VWC_shap = SWC, Site_ID_shap = Site_ID) %>% 
  select(-Landcover) %>% cbind(df.hourly.grass.dry.ori) %>% 
  mutate(Landcover = as.factor(Landcover), Group = "Grassland",
         absVPD = abs(VPD_shap),
         absVWC = abs(VWC_shap)) %>% 
  rename(VWC = SWC, Grassland = Landcover)

# 1 - dry FLUXNET (90% percentile)----------------------------------------------
df.main.forest <- data.frame()
df.main.dryland <- data.frame()
df.main.grassland <- data.frame()
df.main.global <- data.frame()

for (i in seq(0.1, 1, by = 0.1)) {
  dry.forest.quantile <- df.hourly.forest.shap_dry %>% filter(VWC < quantile(VWC, i))
  dry.dryland.quantile <- df.hourly.dryland.shap_dry %>% filter(VWC < quantile(VWC, i))
  dry.grassland.quantile <- df.hourly.grassland.shap_dry %>% filter(VWC < quantile(VWC, i))
  dry.global.quantile <- df.year.dry %>% filter(SWC_ori < quantile(SWC_ori, i))
  
  df.main.f <- data.frame(Percentile = as.character(i * 100), Type = "Forest", 
                          Ratio = mean(dry.forest.quantile$absVWC)/mean(dry.forest.quantile$absVPD),
                          pvalue = t.test(dry.forest.quantile$absVPD, dry.forest.quantile$absVWC)$p.value)
  df.main.d <- data.frame(Percentile = as.character(i * 100), Type = "Dryland", 
                          Ratio = mean(dry.dryland.quantile$absVWC)/mean(dry.dryland.quantile$absVPD),
                          pvalue = t.test(dry.dryland.quantile$absVPD, dry.dryland.quantile$absVWC)$p.value)
  df.main.g <- data.frame(Percentile = as.character(i * 100), Type = "Grassland", 
                          Ratio = mean(dry.grassland.quantile$absVWC)/mean(dry.grassland.quantile$absVPD),
                          pvalue = t.test(dry.grassland.quantile$absVPD, dry.grassland.quantile$absVWC)$p.value)
  df.main.gl <- data.frame(Percentile = as.character(i * 100), Type = "Global", 
                          Ratio = mean(dry.global.quantile$absVWC)/mean(dry.global.quantile$absVPD),
                          pvalue = t.test(dry.global.quantile$absVPD, dry.global.quantile$absVWC)$p.value)
  
  df.main.forest <- rbind(df.main.forest, df.main.f)
  df.main.dryland <- rbind(df.main.dryland, df.main.d)
  df.main.grassland <- rbind(df.main.grassland, df.main.g)
  df.main.global <- rbind(df.main.global, df.main.gl)
}

df.summary <- rbind(df.main.forest, df.main.dryland, df.main.grassland,df.main.global) %>% 
  mutate(Sig = ifelse(pvalue < 0.05, "*", ""))

df.summary.plot <- df.summary %>% 
  mutate(Percentile = factor(Percentile, levels = paste0(seq(10, 100, by = 10))),
         Type = ifelse(Type == "Dryland","Savannah and Shrubland",Type),
         Type = factor(Type, levels = c("Forest", "Savannah and Shrubland", "Grassland", "Global")))

full_palette <- colorRampPalette(brewer.pal(11, "RdYlBu"))(999)
color_3 <- c("#384C3B", full_palette[1], full_palette[300], "darkgrey")

ggplot(df.summary.plot , aes(x = Percentile, y = Ratio, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  scale_fill_manual(values = setNames(color_3, levels(df.summary.plot$Type))) +
  geom_text(aes(label = Sig, y = 4.6), 
            position = position_dodge(width = 0.6), vjust = -0.5, size = 5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) + 
  labs(x = "Percentile (%)", y = "Ratio") +
  theme_bw() + 
  theme(panel.grid.major.x = element_line(color = "lightgrey", size = 0.5),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(color = "lightgrey", size = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +
  labs(fill = "")
