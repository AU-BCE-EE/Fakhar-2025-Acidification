########################################################################################
#----- Cleaning and ordering data ------------
########################################################################################

#Removing unnecessary data#
da <- da[, -c(1:15, 17:19, 21:27)]

#Renaming the column MPVPosition to id#
names(da)[names(da) == "MPVPosition"]  <- "id"

#Cropping data and taking the last point of each measurement from each vavle#
da <- filter(da, !(da$id == lead(da$id)))

#Remove id changing ids from 1-19#
da <- da[da$id == '1' | da$id == '2' | da$id == '3' | da$id == '4' | da$id == '5' | da$id == '6' | da$id == '7' | 
           da$id == '8' | da$id == '9' | da$id == '10' | da$id == '11' | da$id == '12' | da$id == '13' | da$id == '14' |
           da$id == '15' | da$id == '16' | da$id == '17' | da$id == '18' | da$id == '19', ]

#Ordering data according to id#
split_id <- split(da, f = da$id)
id <- paste0("V", unique(da$id))
new_da <- NULL

########################################################################################
#----- Calculating elapsed time ------------ 
########################################################################################

for (i in seq_along(split_id)) {
  subset_data <- split_id[[i]]
  subset_data$elapsed.time <- difftime(subset_data$date.time, min(subset_data$date.time), units = 'hours')
  new_da <- rbind(new_da, subset_data)
}
dat<- new_da

#Rounding elapsed time to days#
dat$elapsed.time <- round(as.numeric(dat$elapsed.time))
dat$days <- dat$elapsed.time / 24

#Removing duplicates#
dat <- dat %>%
  group_by(id, elapsed.time) %>%
  slice(1) %>%
  ungroup()
