#Plots

#Saving plot 1#
ggsave("/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Figures/NH3 flux.jpg", 
       plot = Tan, 
       width = 8, 
       height = 6, 
       dpi = 300, 
       bg = "white")

#Saving plot 2#
ggsave("/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Figures/NH3 cumulative.jpg", 
       plot = cumplot, 
       width = 5, 
       height = 4, 
       dpi = 400, 
       bg = "white")


#Data file
write.csv(dat_tan, '/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/NH3/Output/NH3 flux.csv', row.names = F)
write.csv(cum.summary, '/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/NH3/Output/cumulative emissions.csv', row.names = F)
