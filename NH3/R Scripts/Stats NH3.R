# Load required libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)

data <- read.csv('/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/NH3/Raw data/NH3stat.csv')

#fix data by make tittle row
fix_colnames <- function(df) {
  colnames(df) <- df[1, ]
  df <- df[-1, ]
  return(df)
}

data <- fix_colnames(data)

#Convert to numeric 
data$cum.tan.per <- as.numeric(data$cum.tan.per)
# Filtering the data to get the last time point for each treatment 
data.last <- subset(data, elapsed.time == max(elapsed.time))

################################################################################################
################################################################################################
#Does acidification had any effect on total emissions?
################################################################################################
#Check normality
shapiro <- shapiro.test(data.last$cum.tan.per)
shapiro # Data not normally distributed

#Log-transformed 
# Define a small constant to avoid log(0)
k <- 0.001
data.last <- data.last %>%
  mutate(log_cum = log(cum.tan.per + k))

#Check normality og log transformed
shapiro <- shapiro.test(data.last$log_cum)
shapiro # Data still not normally distributed
#Check homogeneity log transformed
levene <- leveneTest(log_cum ~ treatment, data = data.last)
levene

#Since assumption are normally distributed we used anova
anova_model <- aov(log_cum ~ treatment, data = data.last)
summary(anova_model) #Acidification had strong effect and statistically significant
TukeyHSD(anova_model)

#HSD.test for alphbets
tukey <- HSD.test(anova_model, "treatment", group = TRUE)
tukey$groups

################################################################################################
################################################################################################
#Does emission varies by time and acidification treatment?
################################################################################################

data$rep <- as.factor(data$rep)

# Convert columns to numeric
data$tanloss.per <- as.numeric(data$tanloss.per)
data$elapsed.time <- as.numeric(data$elapsed.time)

#Fitting model
model <- lmer(tanloss.per ~ treatment * factor(elapsed.time) + (1 | rep), data = data)

#Get estimated marginal means for treatment at each time point
emm <- emmeans(model, ~ treatment | elapsed.time)

#Pairwise comparisons between treatments at each time point
contrast_results <- contrast(emm, method = "pairwise", adjust = "tukey")

#View summary of contrasts with significance
summary(contrast_results)

