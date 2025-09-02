#Convert to numeric
flux.dat <- flux.dat %>% mutate(flux = as.numeric(flux))
flux.dat <- flux.dat %>% mutate(flux_g_ha_day = as.numeric(flux_g_ha_day))


#Calculating cumulative emissions#
flux.dat$cum.micro <- mintegrate((flux.dat$elapsed.time), flux.dat$flux, by = flux.dat$replicate, method = 'trap') #(N2O-N ug m^-2)
flux.dat$cum.ha <- mintegrate((flux.dat$elapsed.time), flux.dat$flux_g_ha_day, by = flux.dat$replicate, method = 'trap') #(N2O-N g ha^-1)

#Calculating avg and sd
flux.dat.summary <- aggregate(
  flux ~ elapsed.time + treatment, 
  data = flux.dat, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

# Convert the list columns to separate columns for plots#
flux.dat.summary <- do.call(data.frame, flux.dat.summary)
colnames(flux.dat.summary)[3:4] <- c("mean", "sd")  # Rename columns

#Calculating avg and sd for cumulative
cum.summary <- aggregate(
  cum.micro ~ elapsed.time + treatment, 
  data = flux.dat, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

# Convert the list columns to separate columns for plots#
cum.summary <- do.call(data.frame, cum.summary)
colnames(cum.summary)[3:4] <- c("mean", "sd")  # Rename columns

#Filtering last elapsed time
cum.summary$elapsed.time <- as.numeric(as.character(cum.summary$elapsed.time))
cum.last <- subset(cum.summary, elapsed.time == max(elapsed.time))

