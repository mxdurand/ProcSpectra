# Calculate stats for every wavelength between spectra in a list
specStat <- function(irr_list = irr, norm = F, w.length = 1)
{
  nb.spectra = length(irr_list)
  
  if(norm){
    for(i in 1:nb.spectra)
    {
      irr_list[[i]] <- normalize(irr_list[[i]])
    }
  }
  
  df = multIrrDf(irr_list = irr_list)
  
  MEAN <- vector()
  MED <- vector()
  MOD <- vector()
  SD <- vector()
  CV <- vector()
  
  for (i in 1:nrow(df))
  {
    MEAN <- append(MEAN, values = mean(as.numeric(df[i,colnames(df)[-w.length]]), na.rm = T))
    MED <- append(MED, values = median(as.numeric(df[i,colnames(df)[-w.length]]), na.rm = T))
    MOD <- append(MOD, values = Mode(as.numeric(df[i,colnames(df)[-w.length]])))
    SD <- append(SD, values = sd(as.numeric(df[i,colnames(df)[-w.length]]), na.rm = T))
    CV <- append(CV, values = sd(as.numeric(df[i,colnames(df)[-w.length]]), na.rm = T) / mean(as.numeric(df[i,colnames(df)[-w.length]]), na.rm = T))
  }
  
  CV[is.na(CV)] <- 0
  RES <- data.frame("w.length" = df[,w.length], "Mean" = MEAN, "Med" = MED, "Mode" = MOD, "SD" = SD, "CV" = CV)
  return(RES)
}
