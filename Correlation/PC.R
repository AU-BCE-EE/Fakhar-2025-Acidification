rm(list = ls())

#Setting working directory
setwd('/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Correlation')

#Loading data
pc <- read.csv('PC data.csv')

#Selecting variables
vars <- c("pH", "TAN", "TS", "EC", "NO3")

# Pearson correlation for NH3
nh3.res <- lapply(vars, function(v) {
  cor.test(pc[[v]], pc$NH3,
           alternative = "two.sided",
           method = "pearson",
           conf.level = 0.95)
})

names(nh3.res) <- vars

# Summarize results
nh3_summary <- data.frame(
  Variable    = vars,
  Correlation = sapply(nh3.res, function(x) x$estimate),
  P_value     = sapply(nh3.res, function(x) x$p.value),
  CI_lower    = sapply(nh3.res, function(x) x$conf.int[1]),
  CI_upper    = sapply(nh3.res, function(x) x$conf.int[2])
)

print(nh3_summary)


# Pearson correlation for N2O
n2o.res <- lapply(vars, function(v) {
  cor.test(pc[[v]], pc$N2O,
           alternative = "two.sided",
           method = "pearson",
           conf.level = 0.95)
})

names(n2o.res) <- vars

# Summarize results
n2o_summary <- data.frame(
  Variable    = vars,
  Correlation = sapply(n2o.res, function(x) x$estimate),
  P_value     = sapply(n2o.res, function(x) x$p.value),
  CI_lower    = sapply(n2o.res, function(x) x$conf.int[1]),
  CI_upper    = sapply(n2o.res, function(x) x$conf.int[2])
)

print(n2o_summary)
