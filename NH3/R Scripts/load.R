########################################################################################
#----- Loading picarro data ------------
########################################################################################

da <- readCRDS(('/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/NH3/Raw data'), From = '30.06.2025 07:12:00', To = '07.07.2025 07:04:42', mult = F, tz = "UTC", rm = F)

#Making date time stamp#

da$date.time <- paste(da$DATE, da$TIME)
da$date.time <- ymd_hms(da$date.time)
da$st <- da$date.time                                       
da$DATE <- as.Date(da$st)                                 
da$TIME <- format(da$st, format = "%H:%M:%S") 
########################################################################################


########################################################################################
#----- Loading Weather data ------------
########################################################################################
#Import Weather Data and filter data#
weather <- read.csv('/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/Weather/weather data.csv', fill = T, stringsAsFactors = F)
weather <- weather[, -c(4, 5)]

########################################################################################


########################################################################################
#----- Loading TAN data ------------
########################################################################################
#Import Tan Data#
header <- c('Id', 'Treatment', 'g Slurry', 'Dilution Factor', 'N-NH4', 'N-NH4 mg/L')
Tan <- read.csv('/Users/AU775281/Documents/GitHub/Fakhar-2025-Acidification/NH3/Raw data/TAN analysis.csv', fill = T, stringsAsFactors = F)
Tan <- Tan [, -c(1, 3:5)]
Tan$treatment <- as.factor(Tan$treatment)
