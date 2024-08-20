### Simple ploting of a spectrum
plotS <- function(XYlines = T, plot = FALSE, ...)
{
  if(plot == TRUE){
    plot(xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "l", ...)
    
    if(XYlines){
      xL <- c(axTicks(side=1)[1] - 100, axTicks(side=1), tail(axTicks(side=1),1) + 100)
      for(iX in xL)
      {
        abline(v = iX, col = addTrans("gray70", 150))
      }
      yL <- c(axTicks(side=2)[1] - 0.025, axTicks(side=2), tail(axTicks(side=2),1) + 0.025)
      for(iY in yL)
      {
        abline(h = iY, col = addTrans("gray70", 150))
      }
    }
    
    axis(side = 1, cex = 1, font = 2)
    axis(side = 2, cex = 1, font = 2, las = 2)
    mtext(side = 1, line = 2.5, text = "Wavelength (nm)", cex = 1.4)
    mtext(side = 2, line = 3, text = expression(paste("Spectral energy irradiance E(", lambda, ") (W m"^-2, " nm"^-1, ")", sep = "")), cex = 1.4)
    
  } else {
    points(type = "l", ...)
  }
}  
