# ==============================================================================
# BASEBALL STATCAST - RADIAL IMPROVED GRAPH INFOGRAPHIC
# ==============================================================================

# 1. LOAD REQUIRED LIBRARIES
library(ggplot2)
library(tidyverse)
library(cowplot)
library(magick)
library(png)
library(grid)

# 2. LOAD AND PREPARE DATA (Pulled from statcast.R dependencies)[cite: 1]
statcast_apr <- read.csv(file = "./savant_data mar apr.csv")
statcast_may <- read.csv(file = "./savant_data may.csv")
statcast_jun <- read.csv(file = "./savant_data jun.csv")
statcast_jul <- read.csv(file = "./savant_data jul.csv")
statcast_aug <- read.csv(file = "./savant_data aug.csv")
statcast_sep <- read.csv(file = "./savant_data sep oct.csv")

statcast_big <- rbind(statcast_apr, statcast_may, statcast_jun, statcast_jul, statcast_aug, statcast_sep)

# Filter valid batted ball events and create binary hit/out indicator
statcast_big_p <- statcast_big |> 
  filter(type == "X") |> 
  select(launch_speed, launch_angle, events) |> 
  mutate("hit_or_out" = if_else(events %in% c("single", 
                                              "double", 
                                              "triple", 
                                              "home_run"), 
                                1, 0)) 

# Augment data for radial projection frame and convert to factor format
statcast_aug <- statcast_big_p |> 
  add_row(launch_angle = 180) |>  
  add_row(launch_angle = -180) 

statcast_aug_factor <- statcast_aug |> 
  mutate(hit_or_out = factor(hit_or_out, 
                             levels = c(1, 0), 
                             labels = c("Hit", "Out")))

# 3. LOAD EXTERNAL GRAPHIC ASSETS
batter_img <- image_read("batter.PNG")

# ==============================================================================
# 4. CREATE AUXILIARY ARC COMPONENT
# ==============================================================================
radial_df <- data.frame(
  point_id = 1:8,
  theta = seq(-180, 180, length.out = 9)[1:8],
  r = 120
)

arc <- ggplot(radial_df) +
  geom_point(aes(r, theta), color = "#00000000") +
  coord_radial(
    theta = "y", 
    start = 1.5707,
    direction = -1,
    clip = "off",
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  ) +
  annotate("segment", 
           x = 160, xend = 160,      
           y = -80, yend = -45,      
           color = "gray10", linetype = "dotdash",
           arrow = arrow(
             length = unit(0.1, "cm"),
             type = "closed"))

# ==============================================================================
# 5. BUILD MAIN RADIAL PLOT (p2)
# ==============================================================================
p2 <- statcast_aug_factor |> 
  ggplot(aes(x = launch_speed, y = launch_angle, colour = hit_or_out)) +
  coord_radial(
    theta = "y", 
    start = 1.5707,
    direction = -1,
    clip = "off",
    expand = FALSE
  ) +
  geom_point(size = 1.2) +
  labs(
    x = "",                                                 
    y = ""                                                  
  ) +
  scale_color_manual(values = c("#00939b03", "#9b000003"),  # Nearly transparent colours for overlap[cite: 2]
                     labels = c("Hit", "Out"),              
                     na.translate = FALSE
  ) +
  guides(
    color = guide_legend(
      override.aes = list(alpha = 1, size = 2.7)            
    )) +                                                    
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(0, 125),
    expand = c(0, 0)
  ) +
  theme_void() +                                            
  theme(
    legend.background = element_rect(fill = "transparent"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.position = c(0.2, 0.8),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    plot.background = element_rect(fill = "gray92"),
    panel.background = element_rect(fill = "gray92"),        # Fixed typo from panel.backgroun[cite: 2]
    panel.spacing = unit(0, "pt"),
    legend.margin = margin(t = 8, r = 8, b = 8, l = 8, unit = "pt")
  ) +
  annotate("segment", 
           x = c(40, 80, 120), xend = c(40, 80, 120),      # Circular axes[cite: 2]
           y = -90, yend = 90,
           color = "gray80", linetype = "dashed") +
  annotate("text",                                         # Angle labels[cite: 2]
           x = 125,              
           y = c(-70, -35, 0, 35, 70),      
           label = c("-70º", "-35º", "0º", "35º", "70º"),  
           color = "gray50",  
           size = 3) +
  annotate("segment",                                      # Radial axes[cite: 2]
           x = 0, xend = 120,
           y = c(-70, -35, 0, 35, 70), yend = c(-70, -35, 0, 35, 70),   
           color = "gray30", linetype = "dotdash") +
  annotate("segment",                                      # Average line[cite: 2]
           x = 20, xend = Inf,       
           y = 11.26, yend = 11.26,      
           color = "gray60",
           linetype = "dashed") +
  annotate("text",                                         # Average text label[cite: 2]
           x = 41,             
           y = 18.5,      
           label = "2016 average",   
           color = "gray40",   
           size = 3.5,
           angle = 11.26) +
  annotate("text",                                         # Caption text[cite: 2]
           x = 105,             
           y = -135,      
           label = str_wrap("2016 US Baseball: 113k batted balls. 'Out' vs. 'Hit'.", width = 40),   
           color = "gray50",   
           size = 4.5,
           lineheight = 0.9,
           face = "italic")

# ==============================================================================
# 6. ASSEMBLE FINAL INFOGRAPHIC WITH COWPLOT
# ==============================================================================
final_infographic <- ggdraw(p2) +
  draw_image(
    batter_img,
    x = -0.05,   
    y = 0.015,  
    scale = 0.09 
  ) +
  draw_plot(arc,
            hjust = -0.06,
            vjust = 0.03) +
  draw_text(
    "Launch Angle",
    x = 0.61,
    y = 0.09,
    color = "gray20"
  ) +
  draw_text(
    "Launch Speed",
    x = 0.4,
    y = 0.74,
    angle = 90,
    color = "gray20"
  ) +
  draw_line(
    x = c(0.435, 0.435),    
    y = c(0.6, 0.85), 
    color = "gray80",
    linetype = "dashed",
    arrow = arrow(
      length = unit(0.13, "cm"),
      type = "closed"
    )
  ) +
  draw_label(          
    "40 -",
    x = 0.47, 
    y = 0.6365,
    size = 10, color = "grey60", lineheight = 0.9
  ) +
  draw_label(          
    "80 -",
    x = 0.47, 
    y = 0.761,
    size = 10, color = "grey60", lineheight = 0.9
  ) +
  draw_label(          
    "120 -",
    x = 0.47, 
    y = 0.887,
    size = 10, color = "grey60", lineheight = 0.9
  )