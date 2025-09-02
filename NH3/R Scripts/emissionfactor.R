#Filter last time point 
dat_last <- dat_tan %>%
  group_by(id, group) %>%
  filter(row_number() == n()) %>%  
  ungroup()

#Select relevant columns 
average.last <- dat_last %>%
  select(group, totaltan, cum.hour) %>%
  distinct()

#Calculate average cum.hour and totaltan 
avg <- aggregate(cum.hour ~ group, data = average.last, FUN = mean, na.rm = TRUE)
totaltan_avg <- aggregate(totaltan ~ group, data = average.last, FUN = mean, na.rm = TRUE)

#Merge avg cumulative and total TAN averages
avg_summary <- merge(avg, totaltan_avg, by = "group")

#Convert group names to safe column names and pivot to wide format
emisfac <- avg_summary %>%
  mutate(group = str_replace_all(group, " ", "_")) %>%
  pivot_wider(names_from = group, values_from = c(cum.hour, totaltan))

#Calculate N applied
N_applied <- mean(c(emisfac$totaltan_acid_treatment, emisfac$totaltan_control), na.rm = TRUE)

#Calculate Emission Factor
emisfac <- emisfac %>%
  mutate(emission_factor = ((cum.hour_control - cum.hour_acid_treatment) / N_applied) * 100)
