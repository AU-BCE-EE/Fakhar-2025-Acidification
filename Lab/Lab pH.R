rm(list = ls())

#library
library(ggplot2)

#Importing data
acid <- read.csv('/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Lab/Acid level requirement.csv')

#Defining the pH selected point
x_line <- acid$mean[acid$s.no == 8] 

#Making graph
lab1 <- ggplot(acid, aes(x = mean, y = acid)) +
  geom_ribbon(aes(xmin = mean - std, xmax = mean + std), 
              fill = "#4e79a7", alpha = 0.5) +  
  geom_line(linewidth = 0.5, color = "#4e79a7") +
  geom_point(size = 2.5, shape = 16, alpha = 1, color = "#4e79a7") +  
  geom_vline(xintercept = x_line, color = "#BC544B", linetype = "solid", linewidth = 0.5, alpha = 0.7) +  # Red vertical line
  scale_x_continuous(breaks = seq(0, 11, by = 1)) +
  scale_y_continuous(breaks = seq(0, 15, by = 2)) +
  labs(
    y = expression("Acid (kg ton"^{-1}*")"),
    x = "Slurry pH"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 12),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1)); lab1



#Saving graph#
#ggsave("/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Figures/pH.jpg", 
       plot = lab1, 
       width = 5, 
       height = 3.5, 
       dpi = 400, 
       bg = "white")

