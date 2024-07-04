rm(list=ls())
pacman::p_load(tidyverse, data.table, ggpubr, cowplot)
InputFolder <- ".../file/"
# round a number to the closest 0.5 for plot readibility
ceiling_to_half <- function(x) {
  rounded <- ceiling(x * 2) / 2
  return(rounded)
}

# 1 - Read data for flux sites----------------------------------------------
Models <- c("causalSHAP", "marginalSHAP")
DryWet <- c("dry", "wet")
Groups <- c("Forest","Dryland","Grassland")

for (i in Models){
  for (ii in Groups){
    for (iii in DryWet){
      df_name <- paste0("df_", ii, "_", iii, "_", i)
      df_FI <- data.frame()
      for (k in 1:3) {
      df_FI_ <- read.csv(paste0(InputFolder, "feature_importance/FI_",i,"_",ii,"_",iii,"_",k,".csv")) %>%
        rename(TA = Tair, SM = SWC) 
      df_FI <- rbind(df_FI, df_FI_)
      }
      df_FI_sum <- data.frame(TA = mean(df_FI$TA),
                              SW = mean(df_FI$SW),
                              VPD = mean(df_FI$VPD),
                              SM = mean(df_FI$SM),
                              Type = ii, DryWet = iii, Model = i) %>% 
        pivot_longer(cols = 1:4, names_to = "Variable", values_to = "Mean")
      df_SD_sum <- data.frame(TA = sd(df_FI$TA),
                              SW = sd(df_FI$SW),
                              VPD = sd(df_FI$VPD),
                              SM = sd(df_FI$SM)) %>% 
        pivot_longer(cols = 1:4, names_to = "Variable", values_to = "SD")
      df_FI_merge <- merge(df_FI_sum, df_SD_sum, by = "Variable")
      assign(df_name, df_FI_merge)
    }
  }
}

# 2 - Plot dry periods for FigMain2 FI from Causal Shapley values----------------------------------------------
Models <- c("causalSHAP")
DryWet <- c("dry")
Groups <- c("Forest","Dryland","Grassland")

for (i in Models){
  for (ii in Groups){
    for (iii in DryWet){
        fig_name <- paste0("fig_", ii, "_", iii, "_", i)
        xMax <- ceiling(max(get(paste0("df_", ii, "_", iii, "_", i))$Mean))
        if(ii == "Dryland"){
          Type <- "Savannah and Shrubland"
        } else {
          Type <- ii
        }
        fig <- ggdotchart(get(paste0("df_", ii, "_", iii, "_", i)), x = "Variable", y = "Mean",                            # Color by groups
                        sorting = "descending",                       # Sort value in descending order
                        add = "segments",                             # Add segments from y = 0 to dots
                        add.params = list(color = "#FB9C2A", size = 3, alpha = 0.1), # Change segment color and size
                        dot.size = 1,                                 # Large dot size
                        label = sprintf("%.2f",round(get(paste0("df_", ii, "_", iii, "_", i))$Mean,2)),           # Add mpg values as dot labels
                        font.label = list(color = "black", size = 12,   # Adjust label parameters
                                          vjust = 0.5, hjust = -0.2),
                        legend.title = "",
                        rotate = TRUE,
                        xlab = "",
                        ylab = "",
                        ggtheme = theme_bw()) +
                ggtitle(Type)+
                scale_y_continuous(limits = c(0, xMax)) +
                theme(plot.title = element_text(hjust = 0.5, size = 12),
                      panel.grid.major = element_line(linetype = "dashed", color = "lightgrey"),
                      panel.grid.minor = element_blank(),
                      axis.text.x = element_text(size = 12),
                      axis.text.y = element_text(size = 12),
                      legend.title = element_text(size = 12),
                      panel.border = element_rect(color = "black", fill = NA, size = 1),
                      plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
                      axis.ticks.length = unit(0.2, "cm")) +
                geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                        width = 0.4, color = "black", size = 0.4)
      assign(fig_name, fig)}
    }
}

# 3 - Plot wet periods for FigMain2 FI from Causal Shapley value----------------------------------------------
Models <- c("causalSHAP")
DryWet <- c("wet")
Groups <- c("Forest","Dryland","Grassland")

for (i in Models){
  for (ii in Groups){
    for (iii in DryWet){
      fig_name <- paste0("fig_", ii, "_", iii, "_", i)
      xMax <- ceiling(max(get(paste0("df_", ii, "_", iii, "_", i))$Mean))
      if(ii == "Dryland"){
        Type <- "Savannah and Shrubland"
      } else {
        Type <- ii
      }
      fig <- ggdotchart(get(paste0("df_", ii, "_", iii, "_", i)), x = "Variable", y = "Mean",                            # Color by groups
                        sorting = "descending",                       # Sort value in descending order
                        add = "segments",                             # Add segments from y = 0 to dots
                        add.params = list(color = "#137DC5", size = 3, alpha = 0.1), # Change segment color and size
                        dot.size = 1,                                 # Large dot size
                        label = sprintf("%.2f",round(get(paste0("df_", ii, "_", iii, "_", i))$Mean,2)),           # Add mpg values as dot labels
                        font.label = list(color = "black", size = 12,   # Adjust label parameters
                                          vjust = 0.5, hjust = -0.2),
                        legend.title = "",
                        rotate = TRUE,
                        xlab = "",
                        ylab = "",
                        ggtheme = theme_bw()) +
        ggtitle(Type)+
        scale_y_continuous(limits = c(0, xMax)) +
        theme(plot.title = element_text(hjust = 0.5, size = 12),
              panel.grid.major = element_line(linetype = "dashed", color = "lightgrey"),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              legend.title = element_text(size = 12),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
              axis.ticks.length = unit(0.2, "cm")) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                      width = 0.4, color = "black", size = 0.4)
      assign(fig_name, fig)}
  }
}


# 4 - Read data for the global analysis----------------------------------------------
Models <- c("causalSHAP")
DryWet <- c("dry", "wet")
Groups <- c("Global")

for (i in Models){
  for (ii in Groups){
    for (iii in DryWet){
      df_name <- paste0("df_", ii, "_", iii, "_", i)
      df_FI <- data.frame()
      for (k in 1:3) {
        df_FI_ <- read.csv(paste0(InputFolder, "Feature_Importance/FI_",i,"_",iii,"_065_",k,".csv")) %>%
          rename(TA = Tair, SM = SWC) 
        df_FI <- rbind(df_FI, df_FI_)
      }
      df_FI_sum <- data.frame(TA = mean(df_FI$TA),
                              SW = mean(df_FI$SW),
                              VPD = mean(df_FI$VPD),
                              SM = mean(df_FI$SM),
                              Type = ii, DryWet = iii, Model = i) %>% 
        pivot_longer(cols = 1:4, names_to = "Variable", values_to = "Mean")
      df_SD_sum <- data.frame(TA = sd(df_FI$TA),
                              SW = sd(df_FI$SW),
                              VPD = sd(df_FI$VPD),
                              SM = sd(df_FI$SM)) %>% 
        pivot_longer(cols = 1:4, names_to = "Variable", values_to = "SD")
      df_FI_merge <- merge(df_FI_sum, df_SD_sum, by = "Variable")
      assign(df_name, df_FI_merge)
    }
  }
}

fig_Global_dry_causalSHAP <- ggdotchart(df_Global_dry_causalSHAP, x = "Variable", y = "Mean",   
                                        sorting = "descending",                       
                                        add = "segments",                             
                                        add.params = list(color = "#FB9C2A", size = 3, alpha = 0.1), 
                                        dot.size = 1,                             
                                        label = sprintf("%.2f",round(df_Global_dry_causalSHAP$Mean,2)),    
                                        font.label = list(color = "black", size = 12, 
                                                          vjust = 0.5, hjust = -0.2),
                                        legend.title = "",
                                        rotate = TRUE,
                                        xlab = "",
                                        ylab = "",
                                        ggtheme = theme_bw()) +
                              ggtitle("Global")+
                              scale_y_continuous(limits = c(0, 0.2)) +
                              theme(plot.title = element_text(hjust = 0.5, size = 12),
                                    panel.grid.major = element_line(linetype = "dashed", color = "lightgrey"),
                                    panel.grid.minor = element_blank(),
                                    axis.text.x = element_text(size = 12),
                                    axis.text.y = element_text(size = 12),
                                    legend.title = element_text(size = 12),
                                    panel.border = element_rect(color = "black", fill = NA, size = 1),
                                    plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
                                    axis.ticks.length = unit(0.2, "cm")) +
                              geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                                            width = 0.4, color = "black", size = 0.4)

fig_Global_wet_causalSHAP <- ggdotchart(df_Global_wet_causalSHAP, x = "Variable", y = "Mean",   
                                        sorting = "descending",                       
                                        add = "segments",                             
                                        add.params = list(color = "#137DC5", size = 3, alpha = 0.1), 
                                        dot.size = 1,                             
                                        label = sprintf("%.2f",round(df_Global_wet_causalSHAP$Mean,2)),    
                                        font.label = list(color = "black", size = 12, 
                                                          vjust = 0.5, hjust = -0.2),
                                        legend.title = "",
                                        rotate = TRUE,
                                        xlab = "",
                                        ylab = "",
                                        ggtheme = theme_bw()) +
  ggtitle("Global")+
  scale_y_continuous(limits = c(0, 0.4)) +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        panel.grid.major = element_line(linetype = "dashed", color = "lightgrey"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
        axis.ticks.length = unit(0.2, "cm")) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.4, color = "black", size = 0.4)


# 5 - Plot for FigS5 FI from SHAP----------------------------------------------
Models <- c("marginalSHAP")
DryWet <- c("dry")
Groups <- c("Forest","Dryland","Grassland")
for (i in Models){
  for (ii in Groups){
    for (iii in DryWet){
      fig_name <- paste0("fig_", ii, "_", iii, "_", i)
      xMax <- ceiling(max(get(paste0("df_", ii, "_", iii, "_", i))$Mean))
      if(ii == "Dryland"){
        Type <- "Savannah and Shrubland"
      } else {
        Type <- ii
      }
      fig <- ggdotchart(get(paste0("df_", ii, "_", iii, "_", i)), x = "Variable", y = "Mean",                            # Color by groups
                        sorting = "descending",                       # Sort value in descending order
                        add = "segments",                             # Add segments from y = 0 to dots
                        add.params = list(color = "#FB9C2A", size = 3, alpha = 0.1), # Change segment color and size
                        dot.size = 1,                                 # Large dot size
                        label = sprintf("%.2f",round(get(paste0("df_", ii, "_", iii, "_", i))$Mean,2)),           # Add mpg values as dot labels
                        font.label = list(color = "black", size = 12,   # Adjust label parameters
                                          vjust = 0.5, hjust = -0.2),
                        legend.title = "",
                        rotate = TRUE,
                        xlab = "",
                        ylab = "",
                        ggtheme = theme_bw()) +
        ggtitle(Type)+
        # ggtitle(paste0(ii, " (", iii,")")) +
        scale_y_continuous(limits = c(0, xMax)) +
        theme(plot.title = element_text(hjust = 0.5, size = 12),
              panel.grid.major = element_line(linetype = "dashed", color = "lightgrey"),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              legend.title = element_text(size = 12),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
              axis.ticks.length = unit(0.2, "cm")) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                      width = 0.4, color = "black", size = 0.4)
      assign(fig_name, fig)}
  }
}
Models <- c("marginalSHAP")
DryWet <- c("wet")
Groups <- c("Forest","Dryland","Grassland")
for (i in Models){
  for (ii in Groups){
    for (iii in DryWet){
      fig_name <- paste0("fig_", ii, "_", iii, "_", i)
      xMax <- ceiling(max(get(paste0("df_", ii, "_", iii, "_", i))$Mean))
      if(ii == "Dryland"){
        Type <- "Savannah and Shrubland"
      } else {
        Type <- ii
      }
      fig <- ggdotchart(get(paste0("df_", ii, "_", iii, "_", i)), x = "Variable", y = "Mean",                            # Color by groups
                        sorting = "descending",                       # Sort value in descending order
                        add = "segments",                             # Add segments from y = 0 to dots
                        add.params = list(color = "#137DC5", size = 3, alpha = 0.1), # Change segment color and size
                        dot.size = 1,                                 # Large dot size
                        label = sprintf("%.2f",round(get(paste0("df_", ii, "_", iii, "_", i))$Mean,2)),           # Add mpg values as dot labels
                        font.label = list(color = "black", size = 12,   # Adjust label parameters
                                          vjust = 0.5, hjust = -0.2),
                        legend.title = "",
                        rotate = TRUE,
                        xlab = "",
                        ylab = "",
                        ggtheme = theme_bw()) +
        ggtitle(Type)+
        scale_y_continuous(limits = c(0, xMax)) +
        theme(plot.title = element_text(hjust = 0.5, size = 12),
              panel.grid.major = element_line(linetype = "dashed", color = "lightgrey"),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              legend.title = element_text(size = 12),
              panel.border = element_rect(color = "black", fill = NA, size = 1),
              plot.margin = margin(t = 0.3, r = 0.5, b = 0.3, l = 0.5, unit = "cm"),
              axis.ticks.length = unit(0.2, "cm")) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                      width = 0.4, color = "black", size = 0.4)
      assign(fig_name, fig)}
  }
}

