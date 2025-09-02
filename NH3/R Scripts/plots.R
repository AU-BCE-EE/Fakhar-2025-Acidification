########################################################################################
#-----Plot NH3 Flux Over Time ------------
########################################################################################

#Defining the order of treatment groups#
dat$group <- factor(dat$group, levels = c("control", "acid treatment"))
cum.last$group <- factor(cum.last$group, levels = c("control", "acid treatment"))

# Defining colors for each group#
category_colors <- c(
  "acid treatment" = "#4e79a7",
  "control" = "#f28e2b"
)

#Plot NH3 flux#
Fluxes <- ggplot(dat_summary, aes(x = elapsed.time, y = mean_flux, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), alpha = 0.4, color = NA) +  
  geom_line(size = 1) +  # Mean flux line
  scale_color_manual(values = category_colors) +  
  scale_fill_manual(values = category_colors)  +
  # Axis labels and title
  labs(
    y = expression(paste(NH[3], " Flux (mg NH"[3]-N, " * m"^-2, " * min"^-1, ")")),
    x = "Time after slurry application (hours)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  
  # Theme settings
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.text = element_text(size = 14),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "bottom"        
  ) +
  
  guides(color = guide_legend(nrow = 1)); Fluxes 

########################################################################################


########################################################################################
#-----Plot Tan Over Time ------------
########################################################################################

#Ploting Tan
Tan <- ggplot(dat_tan_summary, aes(x = elapsed.time, y = mean, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.3, color = NA) +  
  geom_line(linewidth = 1) +  
  geom_point(size = 2, shape = 16, alpha = 0.7) +  
  scale_color_manual(values = category_colors, labels = c("Acid treatment", "Control")) +  
  scale_fill_manual(values = category_colors, labels = c("Acid treatment", "Control")) + 
  coord_cartesian(ylim = c(-0.2, 3)) +  # to show the red line at -0.15
  labs(
    y = expression("NH"[3]*"-N flux (% of applied TAN "*h^{-1}*")"),
    x = "Time after slurry application (hours)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.text = element_text(size = 14),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "bottom",
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1)); Tan
Tan <- Tan +
  geom_segment(aes(x = 1, xend = 47, y = -0.15, yend = -0.15), 
               color = "#BC544B", linetype = "solid", size = 0.5, alpha = 0.7); Tan


#Cumulative emissions
cumplot <- ggplot(cum.last, aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  scale_fill_manual(values = category_colors, labels = c("control", "acid treatment")) +
  scale_y_continuous(breaks = seq(0, 31, by = 5), limits = c(0, 31), expand = c(0, 0)) +
  labs(
    y = expression(
      "Cumulative NH"[3]*" loss (% of applied TAN)"
    )) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 12),       
    legend.text = element_text(size = 12),      
    legend.position = "bottom",
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
  );cumplot

########################################################################################
#----- End of experiment ------------
########################################################################################


