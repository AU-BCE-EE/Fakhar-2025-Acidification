#Saving plot 1#
ggsave("/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Figures/N2O flux.jpg", 
       plot = Fluxes, 
       width = 8, 
       height = 6, 
       dpi = 300, 
       bg = "white")

#Saving plot 2#
ggsave("/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Figures/N2O cumulative.jpg", 
       plot = cumplot, 
       width = 5, 
       height = 4, 
       dpi = 400, 
       bg = "white")

#Data file
write.csv(res, '/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/N2O/Output/HMR Results.csv', row.names = F)
write.csv(cum.summary, '/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/N2O/Output/cumulative emissions.csv', row.names = F)
