rm(list=ls())
pacman::p_load(tidyverse, ncdf4, fields, RColorBrewer,rnaturalearth,scales,
               maps,rnaturalearth,sf,lubridate, cowplot, reshape2,viridis)

input_folder <- ".../file/"
lonlat <- read.csv(paste0(input_folder, "spatial_main.csv"))
color_palette <- rev(RColorBrewer::brewer.pal(999, "RdYlBu"))
# 0 - SIF  --------------------------------------
p0 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = sif, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(0,2), space = "Lab", oob=squish,
                       name = expression('SIF (mW'~'m'^-2~'sr'^-1~'nm'^-1*')')) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        #plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))
# 1 - SW  --------------------------------------
p1 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = ssrd, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(100, 300), space = "Lab", oob=squish,
                       name = expression('SW (W'~'m'^-2*')')) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
       # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))
# 2 - TA  --------------------------------------
p2 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = t2m, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(-10, 35.0001), space = "Lab", oob=squish,
                       name = expression('TA (\u00B0C)')) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        #plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))
# 3 - VPD  --------------------------------------
p3 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = vpd, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(0, 35.00001), space = "Lab", oob=squish,
                       name = expression('VPD (hPa)')) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        #plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))

# 4 - VWC  --------------------------------------
color_palette_ <- RColorBrewer::brewer.pal(999, "RdYlBu")
p4 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = sm*100, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette_, na.value = "transparent",
                       limit = c(0, 65), space = "Lab", oob=squish,
                       name = expression('VWC (%)')) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))

# 5 - Zr  --------------------------------------
p5 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = Zroot/1000, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(0, 20), space = "Lab", oob=squish,
                       name = expression("Z"["r"]~'(m)')) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))

# 6 - P50  --------------------------------------
p6 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = P50, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(-7, 0), space = "Lab", oob=squish,
                       name = expression(psi["50"]~'(MPa)')) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))

# 7 - drought coupling  --------------------------------------
p7 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = drought_coupling, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(0,1), space = "Lab", oob=squish,
                       name = expression("Drought coupling")) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))

# 8 - isohydricity --------------------------------------
p8 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = isohydricity, color = NA), colour = NA) + 
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(0,1.5), space = "Lab", oob=squish,
                       name = expression("Anisohydricity")) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8, 
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))

# 9 - Hc  --------------------------------------
p9 <- ggplot(data = lonlat) +
  geom_tile(aes(x = lon_adj, y = lat, fill = canopy_height, color = NA), colour = NA) +
  scale_fill_gradientn(colors = color_palette, na.value = "transparent",
                       limit = c(0, 40), space = "Lab", oob=squish,
                       name = expression("H"["c"]~'(m)')) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        legend.title = element_text(angle = -90),
        legend.margin = margin(l = -1, unit = "pt"),
        # plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_colourbar(barwidth = 0.8,
                                barheight = 7,
                                title.position = "right",
                                title.hjust = 0.5,
                                frame.colour = "black",
                                ticks.colour = "black"))

# 10 - WI  --------------------------------------
breaks <- c(0, 0.03, 0.2, 0.5, 0.65, 50)
labels <- c("Hyper Arid", "Arid", "Semi-Arid", "Dry sub-humid", "Humid")
lonlat$aridity_factor <- cut(lonlat$aridity, breaks = breaks, labels = labels, include.lowest = TRUE)

full_palette <- colorRampPalette(brewer.pal(11, "RdYlBu"))(999)
color_palette_2 <- c(full_palette[1],    # Red, for the lowest range
                     full_palette[150],  # Transition from red to orange
                     full_palette[300],  # Yellow
                     full_palette[450],  # Light greenish-blue
                     full_palette[800])  # Blue
# Plotting
p10 <- ggplot(data = lonlat) + 
  geom_tile(aes(x = lon_adj, y = lat, fill = aridity_factor, color = NA),colour = NA) + 
  scale_fill_manual(values = color_palette_2, name = expression('WI'), 
                    na.value = "transparent", na.translate = FALSE, labels = labels) +
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 60)) +  
  borders("world", xlim = c(-180, 180), ylim = c(-60, 60), colour = "grey50", size = 0.4) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_blank(),  
        legend.key.size = unit(1.5, "lines"),  
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2)) +  # Bold the map frame
  xlab("Longitude") + ylab("Latitude") +
  guides(fill = guide_legend(title.position = "top", 
                             title.vjust = 0.5,
                             keywidth = 1.5,
                             keyheight = 1,
                             label.position = "right"))
