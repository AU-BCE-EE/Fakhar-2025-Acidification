########################################################################################
#----- Calculating flux ------------ 
########################################################################################

#NH3 flux prerequisite components#
#Convert temperature from C to F#
dat$temp <- as.numeric(dat$temp)
dat$air.temp.K <- dat$temp + 273.15

#Air flow Calculation#
dat$air.flow <- 2.25 * 1000 # L min^-1 

#Chamber Area Calculation#
dat$dfc.area <- (0.7/2)**2 * 3.14 #m^2

#Atmospheric constant#
atm.con <- 1 #atm

#Gas constant#
g.con <- 0.082057338 #L * atm * K^-1 * mol^-1

#Mass of nitrogen#
M.N <- 14.0067 #g * mol^-1


#Calculation of NH3 flux#
#Convert NH3.corr from ppb to ppm
dat$ppm <- dat$NH3_corr / 1000

#Convert NH3.corr from ppm to mol (mol L^-1)#
dat$n <- atm.con / (g.con * dat$air.temp.K) * dat$ppm * 10^-6  # mol * L^-1   


#Calculation of flux, from mol * L^-1 to mg.NH3 m^-2 min^-1 *#
dat$NH3.flux <- ((dat$n * M.N * dat$air.flow) / dat$dfc.area) * 1000
dat$NH3.flux.hour <- dat$NH3.flux * 60 # mg m^-2 hour^-1 


#Filtering data#
dat <- dat %>% filter(elapsed.time >= 0 & elapsed.time <= 120)

#Aggregate NH3 flux data by elapsed time and treatment group for plots#
dat_summary <- aggregate(
  NH3.flux ~ elapsed.time + group, 
  data = dat, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

# Convert the list columns to separate columns for plots#
dat_summary <- do.call(data.frame, dat_summary)
colnames(dat_summary)[3:4] <- c("mean_flux", "sd_flux")  # Rename columns

########################################################################################


########################################################################################
#----- Calculating TAN loss ------------
########################################################################################
#Creating new dataset for TAN#

#Calculating mean#
tan.mean <- aggregate(Tan$`N.NH4.mg.L`, by = list(treatment = Tan$treatment), FUN = function(x) mean(x, na.rm = TRUE))  # mg/L
names(tan.mean)[2] <- "mean"

#Calculating applied Tan#
tan.mean$volume.applied <- 1.188 /((0.7/2)**2 * 3.14) #L/m^2
tan.mean$totaltan <- (tan.mean$mean* tan.mean$volume.applied) #mg/m^2

#Merging Tan data with cumulative emissions#
dat_tan <- dat %>%
  left_join(tan.mean %>% select(treatment, totaltan), by = "treatment")


# Calculate TAN fractional loss#
dat_tan <- dat_tan %>%
  mutate(
    tanloss = (NH3.flux.hour/ totaltan) # (Fraction of applied TAN/h)
  )

dat_tan <- dat_tan %>%
  mutate(
    tanloss.per = (NH3.flux.hour/ totaltan) *100 # (%/h)
  )

# Summarize data using for box plot#
dat_tan_summary <- aggregate(
  tanloss.per ~ elapsed.time + group, 
  data = dat_tan, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

# The NH3.flux column is a matrix; flatten it into separate columns
dat_tan_summary <- cbind(dat_tan_summary[, 1:2], as.data.frame(dat_tan_summary$tanloss.per))

# Rename columns if needed
colnames(dat_tan_summary)[3:4] <- c("mean", "sd")

########################################################################################


########################################################################################
#----- Calculating cumlative emission ------------
########################################################################################
#Calculating cumulative emissions#
dat_tan$cum.hour <- mintegrate((dat_tan$elapsed.time), dat_tan$NH3.flux.hour, by = dat_tan$id, method = 'trap') #(NH3-N mg m^-2)
dat_tan$cum.tan.per <- mintegrate((dat_tan$elapsed.time), dat_tan$tanloss.per, by = dat_tan$id, method = 'trap') #(% of applied TAN)


########################################################################################
#----- Cummulative summary ------------
########################################################################################
cum.summary <- aggregate(
  cum.tan.per ~ elapsed.time + group,
  data = dat_tan,
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                      sd = sd(x, na.rm = TRUE))
)

# Split matrix columns into separate variables
cum.summary <- do.call(data.frame, cum.summary)
colnames(cum.summary)[3:4] <- c("mean", "sd")

#Filtering last elapsed time
cum.summary$elapsed.time <- as.numeric(as.character(cum.summary$elapsed.time))
cum.last <- subset(cum.summary, elapsed.time == max(elapsed.time))
