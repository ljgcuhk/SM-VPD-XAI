rm(list=ls())
pacman::p_load(tidyverse,cowplot)
InputFolder <- ".../file/"

# 0 - dry FLUXNET (all)----------------------------------------------
df.hourly.forest.shap_dry <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Forest_dry_2.csv")) %>% 
  mutate(absVPD = abs(VPD),
         absVWC = abs(SWC),
         comparision = ifelse(absVPD < absVWC, 1, 0))
sum(df.hourly.forest.shap_dry$comparision) / 4000 * 100 #58.125
t.test(df.hourly.forest.shap_dry$absVPD, df.hourly.forest.shap_dry$absVWC) #p-value < 2.2e-16

df.hourly.dryland.shap_dry <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Dryland_dry_1.csv")) %>% 
  mutate(absVPD = abs(VPD),
         absVWC = abs(SWC),
         comparision = ifelse(absVPD < absVWC, 1, 0))
sum(df.hourly.dryland.shap_dry$comparision) / 4000 * 100 #57.275
t.test(df.hourly.dryland.shap_dry$absVPD, df.hourly.dryland.shap_dry$absVWC) #p-value < 2.2e-16

df.hourly.grassland.shap_dry <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Grassland_dry_2.csv")) %>% 
  mutate(absVPD = abs(VPD),
         absVWC = abs(SWC),
         comparision = ifelse(absVPD < absVWC, 1, 0))

sum(df.hourly.grassland.shap_dry$comparision) / 4000 * 100 #65.925
t.test(df.hourly.grassland.shap_dry$absVPD, df.hourly.grassland.shap_dry$absVWC) #p-value < 2.2e-16

# 1 - wet FLUXNET (all)----------------------------------------------
df.hourly.forest.shap_wet <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Forest_wet_2.csv")) %>% 
  mutate(absVPD = abs(VPD),
         absVWC = abs(SWC),
         comparision = ifelse(absVPD < absVWC, 0, 1))
sum(df.hourly.forest.shap_wet$comparision) / 4000 * 100 #65.975
t.test(df.hourly.forest.shap_wet$absVPD, df.hourly.forest.shap_wet$absVWC) #p-value < 2.2e-16

df.hourly.dryland.shap_wet <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Dryland_wet_1.csv")) %>% 
  mutate(absVPD = abs(VPD),
         absVWC = abs(SWC),
         comparision = ifelse(absVPD < absVWC, 0, 1))

sum(df.hourly.dryland.shap_wet$comparision) / 4000 * 100 #46.925
t.test(df.hourly.dryland.shap_wet$absVPD, df.hourly.dryland.shap_wet$absVWC) #p-value = 0.9136

df.hourly.grassland.shap_wet <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_Grassland_wet_3.csv")) %>% 
  mutate(absVPD = abs(VPD),
         absVWC = abs(SWC),
         comparision = ifelse(absVPD < absVWC, 0, 1))

sum(df.hourly.grassland.shap_wet$comparision) / 4000 * 100 #51.075
t.test(df.hourly.grassland.shap_wet$absVPD, df.hourly.grassland.shap_wet$absVWC) #p-value = p-value = 0.4688

# 2 - Plot for dry periods (all)----------------------------------------------
df.forest.dry <- df.hourly.forest.shap_dry %>% select(absVPD, absVWC) %>% 
  rename(VPD = absVPD, SM = absVWC) %>% 
  pivot_longer(cols = 1:2, names_to = "Variable", values_to = "CSHAP")

t_test_forest_dry <- t.test(CSHAP ~ Variable, data = df.forest.dry) #p-value < 2.2e-16
Ave_forest_dry <- aggregate(CSHAP ~ Variable, data = df.forest.dry, mean)
Ave_forest_dry$Sta <- c("a", "b")

figS1 <- ggplot(df.forest.dry, aes(x = Variable, y = CSHAP, color = Variable)) + 
  geom_boxplot(size = 1, outlier.color = NA, width = 0.6) + 
  geom_text(data = Ave_forest_dry, aes(x = Variable, y = 6, label = Sta), 
            vjust = 0, color = "black", size = 6) +
  labs(y= expression('Absolute causal Shapley values ('*mu*'mol'~'m'^-2~'s'^-1*')'))+
  scale_color_manual(values = c("VPD" = "#FB9C2A", "SM" = "#FB9C2A")) +
  scale_y_continuous(limits = c(0, 7)) +
  ggtitle("Forest")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  guides(color = FALSE)

df.dryland.dry <- df.hourly.dryland.shap_dry %>% select(absVPD, absVWC) %>% 
  rename(VPD = absVPD, SM = absVWC) %>% 
  pivot_longer(cols = 1:2, names_to = "Variable", values_to = "CSHAP")

t_test_dryland_dry <- t.test(CSHAP ~ Variable, data = df.dryland.dry) #p-value < 2.2e-16
Ave_dryland_dry <- aggregate(CSHAP ~ Variable, data = df.dryland.dry, mean)
Ave_dryland_dry$Sta <- c("a", "b")

figS2 <- ggplot(df.dryland.dry, aes(x = Variable, y = CSHAP, color = Variable)) + 
  geom_boxplot(size = 1, outlier.color = NA, width = 0.6) + 
  geom_text(data = Ave_dryland_dry, aes(x = Variable, y = 4, label = Sta), 
            vjust = 0, color = "black", size = 6) +
  labs(y= expression('Absolute causal Shapley values ('*mu*'mol'~'m'^-2~'s'^-1*')'))+
  scale_color_manual(values = c("VPD" = "#FB9C2A", "SM" = "#FB9C2A")) +
  scale_y_continuous(limits = c(0, 4.5)) +
  ggtitle("Savannah and Shrubland")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),         
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  guides(color = FALSE)

df.grassland.dry <- df.hourly.grassland.shap_dry %>% select(absVPD, absVWC) %>% 
  rename(VPD = absVPD, SM = absVWC) %>% 
  pivot_longer(cols = 1:2, names_to = "Variable", values_to = "CSHAP")

t_test_grassland_dry <- t.test(CSHAP ~ Variable, data = df.grassland.dry) #p-value < 2.2e-16
Ave_grassland_dry <- aggregate(CSHAP ~ Variable, data = df.grassland.dry, mean)
Ave_grassland_dry$Sta <- c("a", "b")

figS3 <- ggplot(df.grassland.dry, aes(x = Variable, y = CSHAP, color = Variable)) + 
  geom_boxplot(size = 1, outlier.color = NA, width = 0.6) + 
  geom_text(data = Ave_grassland_dry, aes(x = Variable, y = 5, label = Sta), 
            vjust = 0, color = "black", size = 6) +
  labs(y= expression('Absolute causal Shapley values ('*mu*'mol'~'m'^-2~'s'^-1*')'))+
  scale_color_manual(values = c("VPD" = "#FB9C2A", "SM" = "#FB9C2A")) +
  scale_y_continuous(limits = c(0, 6)) +
  ggtitle("Grassland")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),         
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  guides(color = FALSE)


# 4 - Plot for wet periods----------------------------------------------
df.forest.wet <- df.hourly.forest.shap_wet %>% select(absVPD, absVWC) %>% 
  rename(VPD = absVPD, SM = absVWC) %>% 
  pivot_longer(cols = 1:2, names_to = "Variable", values_to = "CSHAP")

t_test_forest_wet <- t.test(CSHAP ~ Variable, data = df.forest.wet) #p-value < 2.2e-16
Ave_forest_wet <- aggregate(CSHAP ~ Variable, data = df.forest.wet, mean)
Ave_forest_wet$Sta <- c("b","a")

figS4 <- ggplot(df.forest.wet, aes(x = Variable, y = CSHAP, color = Variable)) + 
  geom_boxplot(size = 1, outlier.color = NA, width = 0.6) + 
  geom_text(data = Ave_forest_wet, aes(x = Variable, y = 6, label = Sta), 
            vjust = 0, color = "black", size = 6) +
  labs(y= expression('Absolute causal Shapley values ('*mu*'mol'~'m'^-2~'s'^-1*')'))+
  scale_color_manual(values = c("VPD" = "#137DC5", "SM" = "#137DC5")) +
  scale_y_continuous(limits = c(0, 7)) +
  ggtitle("Forest")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),         
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  guides(color = FALSE)

df.dryland.wet <- df.hourly.dryland.shap_wet %>% select(absVPD, absVWC) %>% 
  rename(VPD = absVPD, SM = absVWC) %>% 
  pivot_longer(cols = 1:2, names_to = "Variable", values_to = "CSHAP")

t_test_wetland_wet <- t.test(CSHAP ~ Variable, data = df.dryland.wet) #p-value = 0.9136
Ave_wetland_wet <- aggregate(CSHAP ~ Variable, data = df.dryland.wet, mean)
Ave_wetland_wet$Sta <- c("a", "a")

figS5 <- ggplot(df.dryland.wet, aes(x = Variable, y = CSHAP, color = Variable)) + 
  geom_boxplot(size = 1, outlier.color = NA, width = 0.6) + 
  geom_text(data = Ave_wetland_wet, aes(x = Variable, y = 4, label = Sta), 
            vjust = 0, color = "black", size = 6) +
  labs(y= expression('Absolute causal Shapley values ('*mu*'mol'~'m'^-2~'s'^-1*')'))+
  scale_color_manual(values = c("VPD" = "#137DC5", "SM" = "#137DC5")) +
  scale_y_continuous(limits = c(0, 4.5)) +
  ggtitle("Savannah and Shrubland")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),         
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  guides(color = FALSE)

df.grassland.wet <- df.hourly.grassland.shap_wet %>% select(absVPD, absVWC) %>% 
  rename(VPD = absVPD, SM = absVWC) %>% 
  pivot_longer(cols = 1:2, names_to = "Variable", values_to = "CSHAP")

t_test_grassland_wet <- t.test(CSHAP ~ Variable, data = df.grassland.wet) #p-value = 0.4688
Ave_grassland_wet <- aggregate(CSHAP ~ Variable, data = df.grassland.wet, mean)
Ave_grassland_wet$Sta <- c("a", "a")

figS6 <- ggplot(df.grassland.wet, aes(x = Variable, y = CSHAP, color = Variable)) + 
  geom_boxplot(size = 1, outlier.color = NA, width = 0.6) + 
  geom_text(data = Ave_grassland_wet, aes(x = Variable, y = 5, label = Sta), 
            vjust = 0, color = "black", size = 6) +
  labs(y= expression('Absolute causal Shapley values ('*mu*'mol'~'m'^-2~'s'^-1*')'))+
  scale_color_manual(values = c("VPD" = "#137DC5", "SM" = "#137DC5")) +
  scale_y_continuous(limits = c(0, 6)) +
  ggtitle("Grassland")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),         
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  guides(color = FALSE)

# 5 - Plot for spatial wet and dry----------------------------------------------
df_cshap_dry <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_dry_065_3_upLandCover.csv")) %>% 
  mutate(absTair = abs(Tair),
         absSW = abs(SW),
         absVPD = abs(VPD),
         absVWC = abs(SWC))
df_cshap_wet <- read.csv(paste0(InputFolder, "causalSHAP_result/all_causalSHAP_wet_065_1_upLandCover.csv")) %>% 
  mutate(absTair = abs(Tair),
         absSW = abs(SW),
         absVPD = abs(VPD),
         absVWC = abs(SWC))

df.dry <- df_cshap_dry %>% select(absVPD, absVWC) %>% 
  rename(VPD = absVPD, SM = absVWC) %>% 
  pivot_longer(cols = 1:2, names_to = "Variable", values_to = "CSHAP")

t_test_dry <- t.test(CSHAP ~ Variable, data = df.dry) #p-value < 2.2e-16
Ave_dry <- aggregate(CSHAP ~ Variable, data = df.dry, mean)
Ave_dry$Sta <- c("a", "b")

figS7 <- ggplot(df.dry, aes(x = Variable, y = CSHAP, color = Variable)) + 
  geom_boxplot(size = 1, outlier.color = NA, width = 0.6) + 
  geom_text(data = Ave_dry, aes(x = Variable, y = 0.25, label = Sta), 
            vjust = 0, color = "black", size = 6) +
  labs(y= expression('Absolute causal Shapley values ('*mu*'mol'~'m'^-2~'s'^-1*')'))+
  scale_color_manual(values = c("VPD" = "#FB9C2A", "SM" = "#FB9C2A")) +
  scale_y_continuous(limits = c(0, 0.3)) +
  ggtitle("Global")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),         
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  guides(color = FALSE)

df.wet <- df_cshap_wet %>% select(absVPD, absVWC) %>% 
  rename(VPD = absVPD, SM = absVWC) %>% 
  pivot_longer(cols = 1:2, names_to = "Variable", values_to = "CSHAP")

t_test_wet <- t.test(CSHAP ~ Variable, data = df.wet) #p-value = 0.4688
Ave_wet <- aggregate(CSHAP ~ Variable, data = df.wet, mean)
Ave_wet$Sta <- c("b","a")

figS8 <- ggplot(df.wet, aes(x = Variable, y = CSHAP, color = Variable)) + 
  geom_boxplot(size = 1, outlier.color = NA, width = 0.6) + 
  geom_text(data = Ave_wet, aes(x = Variable, y = 0.25, label = Sta), 
            vjust = 0, color = "black", size = 6) +
  labs(y= expression('Absolute causal Shapley values ('*mu*'mol'~'m'^-2~'s'^-1*')'))+
  scale_color_manual(values = c("VPD" = "#137DC5", "SM" = "#137DC5")) +
  scale_y_continuous(limits = c(0, 0.3)) +
  ggtitle("Global")+
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),         
        plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm")) +
  guides(color = FALSE)


