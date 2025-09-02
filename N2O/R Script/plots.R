#Reformatting date to ddmmyy
flux.dat <- flux.dat %>%
  mutate(
    date = format(as.Date(date, format="%d%m%y"), "%d-%m-%y")
  )

#Defining the order of treatment groups#
flux.dat$group <- factor(flux.dat$treatment, levels = c("Control", "Acid treatment"))

# Defining colors for each group#
category_colors <- c(
  "Acid treatment" = "#4e79a7",
  "Control" = "#f28e2b"
)


#Plot N2O flux#
Fluxes <- ggplot(flux.dat.summary, aes(x = elapsed.time, y = mean, color = treatment, fill = treatment)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.3, color = NA) +  
  geom_line(linewidth = 1) +  
  geom_point(size = 2, shape = 16, alpha = 0.7) +  
  scale_color_manual(values = category_colors, labels = c("Acid treatment", "Control")) +  
  scale_fill_manual(values = category_colors, labels = c("Acid treatment", "Control")) + 
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  scale_y_continuous(breaks = seq(-0.3, 90, by = 20), expand = expansion(mult = c(0, 0.3))) +
  labs(
    y = expression(paste(mu, "g N" [2], "O-N ", m^{-2}, " ", h^{-1})),
    x = "Time after slurry application (days)",
    color = "Treatment"
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
  guides(color = guide_legend(nrow = 1)); Fluxes 


#########################################################################

#Cumulative emissions
cumplot <- ggplot(cum.last, aes(x = treatment, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  scale_fill_manual(values = category_colors, labels = c("Control", "Acid treatment")) +
  scale_y_continuous(breaks = seq(0, 510, by = 100), limits = c(0, 510), expand = c(0, 0)) +
  labs(
    y = expression("Cumulative N"[2]*"O loss ("*mu*"g "*m^{-2}*")")
  ) +
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
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2));cumplot

