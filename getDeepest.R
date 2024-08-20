# Get deepest curve from list of N spectra (optional, return the fdata object for further functional analyses)
getDeepest <- function(list = Opn1, return.fdata = F)
{
  df <- multIrrDf(list)
  M <- t(as.matrix(df[,2:ncol(df)]))
  fdf <- fdata(M, argvals = df[,1], names = list(main = "", xlab = "Wavelength (nm)", ylab = expression(paste("Spectral energy irradiance E(", lambda, ") (W m"^-2, " nm"^-1, ")", sep = ""))))
  if(return.fdata){
    return(fdf)
  } else {
    return(list[[depth.FM(fdf)$lmed]])
  }
}
