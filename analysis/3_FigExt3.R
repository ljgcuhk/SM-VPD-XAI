rm(list=ls())
pacman::p_load(tidyverse, ggpubr, ggsci,cowplot)

InputFolder <- ".../file/"

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
# 0-0 - read original files for dry----------------------------------------------
df.hourly.forest.wet.ori <- read.csv(paste0(InputFolder,"representative_sampling/",
                                        list.files(paste0(InputFolder,"representative_sampling/"), 
                                        pattern="_Forest_Hourly_wet")))
df.hourly.grass.wet.ori <- read.csv(paste0(InputFolder,"representative_sampling/",
                                       list.files(paste0(InputFolder,"representative_sampling/"), 
                                                    pattern="_Grassland_Hourly_wet")))
df.hourly.dry.wet.ori <- read.csv(paste0(InputFolder,"representative_sampling/",
                                     list.files(paste0(InputFolder,"representative_sampling/"), 
                                                pattern="_Dryland_Hourly_wet")))

df.year.wet <- read.csv(paste0(InputFolder,"causalSHAP_result/all_causalSHAP_origin_wet_065_2.csv"))
# 0-1 - read original shapley value files ----------------------------------------------
df.hourly.forest.shap <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Forest_wet_2.csv")) %>% 
  rename(Tair_shap = Tair, SW_shap = SW, VPD_shap = VPD, VWC_shap = SWC, Site_ID_shap = Site_ID) %>% 
  select(-Landcover) %>% cbind(df.hourly.forest.wet.ori) %>% 
  mutate(Landcover = as.factor(Landcover), Group = "Forest") %>% 
  rename(VWC = SWC, Forest = Landcover)

df.hourly.dryland.shap <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_dryland_wet_1.csv")) %>% 
  rename(Tair_shap = Tair, SW_shap = SW, VPD_shap = VPD, VWC_shap = SWC, Site_ID_shap = Site_ID) %>% 
  select(-Landcover) %>% cbind(df.hourly.dry.wet.ori) %>% 
  mutate(Landcover = ifelse(Landcover == "WSH", "WSA", Landcover)) %>% 
  mutate(Landcover = as.factor(Landcover), Group = "Dryland") %>% 
  rename(VWC = SWC, Dryland = Landcover)
 
df.hourly.grassland.shap <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Grassland_wet_3.csv")) %>% 
  rename(Tair_shap = Tair, SW_shap = SW, VPD_shap = VPD, VWC_shap = SWC, Site_ID_shap = Site_ID) %>% 
  select(-Landcover) %>% cbind(df.hourly.grass.wet.ori) %>% 
  mutate(Landcover = as.factor(Landcover), Group = "Grassland") %>% 
  rename(VWC = SWC, Grassland = Landcover)
my_colors <- c("EBF" = "#E64B35B2", "DBF" = "#00A087B2", "ENF" = "#4DBBD5B2", 
               "MF" = "#3C5488B2", "DNF" = "#F39B7FB2", "GRA" = "#91D1C2B2",
               "WSH" = "#978D7F", "SAV" = "#C7675D", "OSH" = "darkgoldenrod3",
               "CSH" = "#8491B4B2", "WSA" = "darkgoldenrod1") 
# 1 - Dependence plots for flux forest sites----------------------------------------------
p11 <- ggplot(df.hourly.forest.shap, aes(SW, SW_shap)) +
  geom_point(aes(color = Forest), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression('SW (W'~'m'^-2*')'),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0(df.hourly.forest.shap$Group[1], ", SW")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.position = "none",plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p12 <- ggplot(df.hourly.forest.shap, aes(Tair, Tair_shap)) +
  geom_point(aes(color = Forest), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression("T"[air]~"\u00B0C"),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0(df.hourly.forest.shap$Group[1], ", TA")) +
  scale_y_continuous(limits = c(-8, 6)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p13 <- ggplot(df.hourly.forest.shap, aes(VPD, VPD_shap)) +
  geom_point(aes(color = Forest), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression("VPD (hPa)"),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0(df.hourly.forest.shap$Group[1], ", VPD")) +
  scale_x_continuous(limits = c(5, 30)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p14 <- ggplot(df.hourly.forest.shap, aes(VWC, VWC_shap)) +
  geom_point(aes(color = Forest), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression('VWC (m'^3~'m'^-3*')'),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0(df.hourly.forest.shap$Group[1], ", VWC")) +
  scale_x_continuous(limits = c(10, 50)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# 1 - Dependence plots for flux shrubland and savannah sites ----------------------------------------------
p21 <- ggplot(df.hourly.dryland.shap, aes(SW, SW_shap)) +
  geom_point(aes(color = Dryland), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression('SW (W'~'m'^-2*')'),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0("Savannah and Shrubland", ", SW")) +
  scale_x_continuous(limits = c(0, 1200)) +
  scale_y_continuous(limits = c(-10, 6)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p22 <- ggplot(df.hourly.dryland.shap, aes(Tair, Tair_shap)) +
  geom_point(aes(color = Dryland), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression("T"[air]~"\u00B0C"),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0("Savannah and Shrubland", ", TA")) +
  scale_x_continuous(limits = c(5, 40)) +
  scale_y_continuous(limits = c(-3, 5)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p23 <- ggplot(df.hourly.dryland.shap, aes(VPD, VPD_shap)) +
  geom_point(aes(color = Dryland), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression("VPD (hPa)"),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0("Savannah and Shrubland", ", VPD")) +
  scale_x_continuous(limits = c(5, 40)) +
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p24 <- ggplot(df.hourly.dryland.shap, aes(VWC, VWC_shap)) +
  geom_point(aes(color = Dryland), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression('SWC (m'^3~'m'^-3*')'),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0("Savannah and Shrubland", ", VWC")) +
  scale_x_continuous(limits = c(5, 50)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# 3 - Dependence plots for flux grassland sites----------------------------------------------
p31 <- ggplot(df.hourly.grassland.shap, aes(SW, SW_shap)) +
  geom_point(aes(color = Grassland), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression('SW (W'~'m'^-2*')'),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0(df.hourly.grassland.shap$Group[1], ", SW")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p32 <- ggplot(df.hourly.grassland.shap, aes(Tair, Tair_shap)) +
  geom_point(aes(color = Grassland), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression("T"[air]~"\u00B0C"),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0(df.hourly.grassland.shap$Group[1], ", TA")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p33 <- ggplot(df.hourly.grassland.shap, aes(VPD, VPD_shap)) +
  geom_point(aes(color = Grassland), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression("VPD (hPa)"),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0(df.hourly.grassland.shap$Group[1], ", VPD")) +
  scale_x_continuous(limits = c(5, 40)) +
  scale_y_continuous(limits = c(-6, 5)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p34 <- ggplot(df.hourly.grassland.shap, aes(VWC, VWC_shap)) +
  geom_point(aes(color = Grassland), alpha = 0.35, size = 0.3) +
  scale_color_manual(values = my_colors) +
  labs(x = expression('SWC (m'^3~'m'^-3*')'),
       y = expression('Causal Shapley values')) +
  ggtitle(paste0(df.hourly.grassland.shap$Group[1], ", VWC")) +
  scale_x_continuous(limits = c(10, 60)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# 4 - Dependence plots for global wet biomes ----------------------------------------------
p41 <- ggplot(df.year.wet, aes(SW_ori, SW)) +
  geom_point(aes(color = Land_Cover), alpha = 0.35, size = 0.2) +
  scale_color_manual(values = my_colors) +
  labs(x = expression('SW (W'~'m'^-2*')'),
       y = expression('Causal Shapley values')) +
  ggtitle("Global, SW") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", 
        plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p42 <- ggplot(df.year.wet, aes(Tair_ori, Tair)) +
  geom_point(aes(color = Land_Cover), alpha = 0.35, size = 0.2) + 
  scale_color_manual(values = my_colors) +
  scale_x_continuous(limits = c(0, 30)) +
  labs(x = expression("TA"~"(\u00B0C)"),
       y = expression('Causal Shapley values')) +
  ggtitle("Global, TA") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", 
        plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p43 <- ggplot(df.year.wet, aes(VPD_ori, VPD)) +
  geom_point(aes(color = Land_Cover), alpha = 0.35, size = 0.2) + 
  labs(x = expression("VPD (hPa)"),
       y = expression('Causal Shapley values')) +
  scale_color_manual(values = my_colors) +
  scale_x_continuous(limits = c(1, 22)) +
  ggtitle("Global, VPD") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "none", 
        plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

p44 <- ggplot(df.year.wet, aes(SWC_ori, SWC)) +
  geom_point(aes(color = Land_Cover), alpha = 0.35, size = 0.2) +
  scale_color_manual(values = my_colors) +
  labs(x = expression('VWC (%)'),
       y = expression('Causal Shapley values')) +
  scale_x_continuous(limits = c(10, 70)) +
  scale_y_continuous(limits = c(-0.3, 0.3)) +
  ggtitle("Global, VWC") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none",
        plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) + 
        guides(color = guide_legend(title = "Land cover", nrow = 1, byrow = TRUE))


