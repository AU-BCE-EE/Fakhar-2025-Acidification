rm(list = ls())

library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(ggbreak)
library(patchwork)

# Read and format data
daily <- read.csv("//Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Weather/Daily.csv")
hourly <- read.csv("/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Weather/weatherhour.csv")
hourly <- hourly[!duplicated(hourly$elapsed.time), ]

#change date format
daily$date <- gsub("(\\d{1,2}/\\d{1,2}/)(\\d{2})$", "\\120\\2", daily$date)
daily$date <- as.Date(daily$date, format = "%d/%m/%Y")

# Define NH3 exp starting and ending point
x_line <- as.Date("2025-07-04")
nh3_start <- as.Date("2025-06-30")
nh3_end <- as.Date("2025-07-04")

# Plot
dailyplot <- ggplot(daily, aes(x = date)) +
  geom_col(aes(y = prec, fill = "Precipitation"), alpha = 0.7, width = 0.6) +
  geom_line(aes(y = temp, color = "Temperature"), linewidth = 0.8) +
  geom_point(aes(y = temp, color = "Temperature"), size = 2) +
  annotate("text", x = nh3_start + 1, y = max(daily$temp, na.rm = TRUE), 
           label = "NH3 Period", hjust = 0.5, vjust = -2, size = 3, fontface = "bold", color = "gray30") +
  geom_vline(xintercept = x_line, color = "#BC544B", linetype = "solid", linewidth = 0.5, alpha = 0.7) +
  scale_y_continuous(
    name = "Temperature (°C)",
    breaks = seq(0, 30, by = 5)
  ) +
  scale_x_date(date_breaks = "5 days", date_labels = "%d-%m") +
  scale_color_manual(name = NULL, values = c("Temperature" = "#f28e2b")) +
  scale_fill_manual(name = NULL, values = c("Precipitation" = "#4e79a7")) +
  labs(x = "Date") +
  theme_bw() +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
    axis.title.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2, override.aes = list(alpha = 0.7))
  ); dailyplot

#Highlight NH3 period on daily plot:
dailyplot2 <- dailyplot +
  annotate("rect",
           xmin = nh3_start, xmax = nh3_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  geom_vline(xintercept = x_line, color = "#BC544B", linetype = "solid", size = 0.5, alpha = 0.7)

#Create hourly plot:
hourlyplot <- ggplot(hourly, aes(x = elapsed.time)) +
  geom_col(aes(y = prec, fill = "Precipitation"), alpha = 0.7, width = 1) +
  geom_line(aes(y = temp, color = "Temperature"), linewidth = 0.8) +
  geom_point(aes(y = temp, color = "Temperature"), size = 2) +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    sec.axis = sec_axis(~ ., name = "Precipitation (mm)", breaks = seq(0, 30, by = 5))
  ) +
  scale_color_manual(name = NULL, values = c("Temperature" = "#f28e2b")) +
  scale_fill_manual(name = NULL, values = c("Precipitation" = "#4e79a7")) +
  labs(x = "Hours") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 12),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = c(1, 1),
    legend.background = element_rect(fill = NA, color = NA),
    legend.justification = c("right", "top"),  
    legend.spacing.y = unit(0.1, "cm"),    # reduce vertical space between legends
    legend.key.height = unit(0.4, "cm"),   # reduce key height
    legend.key.width = unit(0.4, "cm"),
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2, override.aes = list(alpha = 0.7))
  );hourlyplot

dailyplot <- dailyplot +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 4, fontface = "bold")

hourlyplot  <- hourlyplot  +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 4, fontface = "bold")


# Combine plots and collect legends into one centered below
combined_plot <- dailyplot2 + hourlyplot +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 10, face = "bold", hjust = 1)); combined_plot


# Save plot
#ggsave("/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Figures/weather daily and hourly.jpg", 
       plot = combined_plot, 
       width = 11, 
       height = 5, 
       dpi = 400, 
       bg = "white")
