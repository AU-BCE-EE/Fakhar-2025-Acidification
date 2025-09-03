rm(list = ls())

#Setting working directory
setwd('/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/N2O/R Script')

# Load required libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(agricolae)

data <- read.csv('Data/N2Ostat.csv')
#fix data by make tittle row
fix_colnames <- function(df) {
  colnames(df) <- df[1, ]
  df <- df[-1, ]
  return(df)
}

data <- fix_colnames(data)

#Convert to numeric 
data$cum.micro <- as.numeric(data$cum.micro)
# Filtering the data to get the last time point for each treatment 
data.last <- subset(data, elapsed.time == max(elapsed.time))

################################################################################################
################################################################################################
#Does acidification had any effect?
################################################################################################

#Check normality
shapiro <- shapiro.test(data.last$cum.micro)
shapiro # Data is normally distributed

#Check homogeneity log transformed
levene <- leveneTest(cum.micro ~ treatment, data = data.last)
levene

#Since assumption are normally distributed we used anova
anova_model <- aov(cum.micro ~ treatment, data = data.last)
summary(anova_model) #Acidification do not have any statistically significant effec
TukeyHSD(anova_model)

#HSD.test for alphbets
tukey <- HSD.test(anova_model, "treatment", group = TRUE)
tukey$groups





################################################################################################
################################################################################################
#Does emission varies by time and acidification treatment?
################################################################################################

# Convert relevant columns to factors (if not already)
data$replicate <- as.factor(data$replicate)
data$flux.ug <- as.numeric(data$flux.ug)

#Fitting model
model1 <- lmer(flux.ug ~ treatment * elapsed.time + (1 | replicate), data = data)

#Get estimated marginal means for treatment at each time point
emm <- emmeans(model1, ~ treatment | elapsed.time)

#Pairwise comparisons between treatments at each time point
contrast_results <- contrast(emm, method = "pairwise", adjust = "tukey")

#View summary of contrasts with significance
summary(contrast_results)


