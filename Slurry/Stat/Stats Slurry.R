rm(list = ls())

#Library
library(dplyr)
library(agricolae)
library(multcompView)


#loading data
data <- read.csv('/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Slurry/Stat/Stat data.csv')
#Get slurry parameters
paramtr <- unique(data$parameter)

#Create an empty list to store results
results <- list()

#ANOVA Loop over each parameter
for (param in paramtr) {
  sub_data <- subset(data, parameter == param)
  # Run ANOVA
  model <- aov(value ~ treatment, data = sub_data)
  # Tukey HSD
  tukey <- TukeyHSD(model)
  # Get letter groupings
  letters <- multcompLetters4(model, tukey)
  # Get means
  means <- aggregate(value ~ treatment, data = sub_data, mean)
  means$letters <- letters$treatment$Letters
  means$parameter <- param  # Add parameter label
  
  # Store
  results[[param]] <- means
}

#Combine all results into one data frame
Anova_result <- do.call(rbind, results)
rownames(Anova_result) <- NULL

#Printing results
print(Anova_result)

