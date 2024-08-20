# Raw to Irrad for multispectras
multIrr <- function(pattern = "open", path, date = "2019-07-02", CorMeth = MAYP112785_ylianttila.mthd, smoothing = FALSE, norm = FALSE)
{
  O <- getwd()
  setwd(path)
  nb.spectra = length(dir(path)[grep(pattern = paste(pattern, "00", sep = ""), dir(path))]) # How many light spectra were taken
  irr <- vector("list", nb.spectra)
  
  lmat <- matrix(c(dir(path)[grep(pattern = paste(pattern, "00", sep = ""), dir(path))], 
                   rep(paste(pattern, "PC.txt", sep = ""), nb.spectra), 
                   rep(paste(pattern, "dark.txt", sep = ""), nb.spectra)), 
                 nrow = nb.spectra, ncol = 3, byrow = F)
  
  chainS_Irrad_Corrected <- function(i, descriptor = descriptor, correction.method = correction.method)
  {
    l <- list(light = lmat[i,1], filter = lmat[i,2], dark =lmat[i,3] )
    s_irrad_corrected(x = l, descriptor = descriptor, correction.method = correction.method)
  }
  irr <- lapply(1:nb.spectra, chainS_Irrad_Corrected, descriptor = which_descriptor(date, verbose = F), correction.method = CorMeth)
  
  if(norm){
    irr <- lapply(irr, normalize)
  }
  
  suppressMessages(
    {
      if(smoothing){
        irr <- lapply(irr,smooth_spct, na.rm = T)
      }
    })
  
  setwd(O)
  return(irr)
}

# Put list of NB spectra into one data frame
multIrrDf <- function(irr_list = irr)
{
  nb.spectra = length(irr_list)
  df = data.frame("w.length" = irr_list[[1]][,"w.length"])
  for(i in 1:nb.spectra)
  {
    df[,i+1] <- irr_list[[i]][,"s.e.irrad"]
    colnames(df)[i+1] <- paste("s.e.irrad", i ,sep = "")
  }
  return(df)
}


