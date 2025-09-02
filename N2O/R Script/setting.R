#Filter necessary data and renaming
flux.dat <- res[, c("Series", "f0", "Method")]
names(flux.dat)[names(flux.dat) == "f0"] <- "flux"

#Convert from #ug m^-2 h^-2 to #g ha^-1 day^-1
flux.dat <- flux.dat %>%
  mutate(
    flux = as.numeric(as.character(flux)),  # convert safely
    flux_g_ha_day = flux * 24 * 10000 / 1e6
  )

#Making new column for treatment and date
flux.dat <- flux.dat %>%
  mutate(
    treatment = case_when(
      str_detect(Series, "cont") ~ "Control",
      str_detect(Series, "acid") ~ "Acid treatment",
      TRUE ~ "unknown"
    ),
    date_str = str_sub(Series, -6, -1),
    date = dmy(date_str),
    replicate = str_sub(Series, 1, 3)
  ) %>%
  select(-date_str) %>%
  ungroup()%>%
  arrange(treatment)

#Elapsed time
flux.dat <- flux.dat %>%
  mutate(
    elapsed.time = as.numeric(difftime(date, as.Date("2025-06-30"), units = "days"))
  )



