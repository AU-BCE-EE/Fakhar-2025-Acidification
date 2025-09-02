#Runing HRM
res = HMR(
  filename = 'Flux R Raw.csv',
  sep = ',',
  FollowHMR = TRUE,
  pfvar = 0.0001,
  IfNoSignal = 'LR',
  SatPct = 90,
  SatTimeMin = 2
) #ug m^-2 h^-1


