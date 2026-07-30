# ==============================================================================
# BASEBALL STATCAST DATA VISUALIZATION - MAIN PLOT
# ==============================================================================

# 1. LOAD REQUIRED LIBRARIES
library(ggplot2)
library(tidyverse)
library(gghexsize)
library(cowplot)
library(magick)
library(png)
library(grid)

# 2. LOAD AND COMBINE DATASETS
statcast_apr <- read.csv(file = "./savant_data mar apr.csv")
statcast_may <- read.csv(file = "./savant_data may.csv")
statcast_jun <- read.csv(file = "./savant_data jun.csv")
statcast_jul <- read.csv(file = "./savant_data jul.csv")
statcast_aug <- read.csv(file = "./savant_data aug.csv")
statcast_sep <- read.csv(file = "./savant_data sep oct.csv")

statcast_big <- rbind(statcast_apr, statcast_may, statcast_jun, statcast_jul, statcast_aug, statcast_sep)

# 3. PREPARE HIT/OUT VARIABLE
# Keep only relevant columns and create a binary hit/out indicator
statcast_big_p <- statcast_big |> 
  filter(type == "X") |> 
  select(launch_speed, launch_angle, events) |> 
  mutate("hit_or_out" = if_else(events %in% c("single", 
                                              "double", 
                                              "triple", 
                                              "home_run"), 
                                1, 0)) 

# 4. BINNING AND FILTERING
# Create bins for launch velocity and angle, then filter out hexes with < 80 observations
statcast_final <- statcast_big_p |> 
  mutate(
    velo_bin = cut(launch_speed, 
                   breaks = seq(floor(min(launch_speed, na.rm=TRUE)), 
                                ceiling(max(launch_speed, na.rm=TRUE)), 
                                by = 5), 
                   include.lowest = TRUE,
                   right = FALSE),
    angle_bin = cut(launch_angle, 
                    breaks = seq(floor(min(launch_angle, na.rm=TRUE)), 
                                 ceiling(max(launch_angle, na.rm=TRUE)), 
                                 by = 5),
                    include.lowest = TRUE,
                    right = FALSE)
  ) |> group_by(velo_bin, angle_bin) |> 
  mutate(
    N_in_bin = n() 
  ) |> 
  ungroup()

statcast_filtered <- statcast_final |> 
  filter(N_in_bin > 80)

# Display the size reduction metrics in the console
message(paste("Original number of rows:", nrow(statcast_final)))
message(paste("Rows remaining after filtering:", nrow(statcast_filtered)))
message(paste("Number of rows removed:", nrow(statcast_final) - nrow(statcast_filtered)))

# 5. LOAD EXTERNAL ASSETS (ICONS)
batter_img <- image_read("batter.PNG")
img_raster <- readPNG("batter.PNG") 
g <- rasterGrob(img_raster, interpolate = TRUE)

# Calculate average angle (for reference)
angle_avg <- statcast_big_p |> 
  summarise("angle average" = mean(launch_angle))

# 6. BUILD MAIN PLOT
p <- ggplot(statcast_filtered, aes(launch_speed, launch_angle, z = hit_or_out)) +
  geom_hextile(fun = "mean", na.rm = TRUE, bins = 50) +
  scale_size_tile(limits = c(40, 100), max_size = 0.89) +  
  scale_fill_gradientn(
    colors = c(
      "#fffed4",
      "#f1ae64",
      "#d64639",
      "#b32d30",
      "#7a1b2a"
    ),
    values = scales::rescale(c(0, 50, 70, 85, 100))
  ) +
  lims(
    x = c(45, 120),
    y = c(-85, 75)) +
  theme_minimal() + 
  theme(
    plot.margin = margin(t = 1.5, r = 1, b = 1, l = 0.6, unit = "cm"),
    axis.title.y = element_text(angle = 0, vjust = 0.9, face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.text = element_text(colour = "grey60"),
    legend.title = element_text(face = "bold"),
    legend.position = c(0.03, 1),
    legend.justification = "left",
    legend.margin = margin(b = 10, t = 10)
  ) +
  guides(
    size = "none",
    fill = guide_colorbar(
      title = "Hit probability", 
      title.position = "top",      
      title.hjust = 0,           
      barwidth = unit(4.5, "cm"),   
      barheight = unit(0.2, "cm"), 
      label.position = "bottom",   
      ticks = FALSE,
      label = FALSE,
      direction = "horizontal"
    )) +
  scale_x_continuous(name = "Hit speed", breaks=c(50, 80, 110),
                     labels = c("50", "80", "110 mph")) +
  scale_y_continuous(name = "Launch\n angle", breaks=c(-70, -35, 0, 35, 70),
                     labels = c("-70º", "-35º", "0º", "35º", "70º")) +
  coord_fixed(ratio = 0.75) +
  
  annotate("segment",       
           x = 45, xend = Inf,       
           y = 11.26, yend = 11.26,      
           color = "gray30") +
  annotate("text", 
           x = 58,             
           y = 8.5,      
           label = "2016 average 11.26º",   
           color = "gray50",   
           size = 3.1)

# 7. ADD ICONS, TEXT, AND ARROWS USING COWPLOT
final_plot <- ggdraw(p) +
  draw_image(
    batter_img,
    x = -0.325, 
    y = 0,   
    scale = 0.09 
  ) +
  # Top Arrow (Fly balls)
  draw_line(
    x = c(0.165, 0.165),    
    y = c(0.7, 0.74), 
    color = "grey60",
    arrow = arrow(
      length = unit(0.13, "cm"),
      angle = 45
    )
  ) +
  draw_label(
    "Fly-\nballs",  
    x = 0.18,            
    y = 0.65,           
    hjust = 1,      
    size = 10, color = "grey60", lineheight = 0.9
  ) +
  # Bottom Arrow (Ground balls)
  draw_line(
    x = c(0.165, 0.165),   
    y = c(0.29, 0.25),    
    color = "grey60",
    arrow = arrow(
      length = unit(0.13, "cm"),
      angle = 45
    )
  ) +
  draw_label(
    "Ground-\nballs",
    x = 0.18, 
    y = 0.35, 
    hjust = 1,
    size = 10, color = "grey60", lineheight = 0.9
  ) +
  # Legend subtext
  draw_label(    
    "More likely to result in a hit",
    x = 0.46, 
    y = 0.88, 
    hjust = 1,
    size = 9, color = "grey60", lineheight = 0.9
  ) +
  draw_line(     
    x = c(0.475, 0.51),   
    y = c(0.88, 0.88),
    color = "grey60",
    arrow = arrow(
      length = unit(0.1, "cm"),
      type = "closed")
  )

# 8. EXPORT FINAL PLOT
ggsave("my_plot.pdf", 
       plot = final_plot, # Explicitly calling the cowplot assembly
       width = 7,
       height = 7,
       units = "in",
       dpi = 300
)
