# Plot distribution of data for every wavelength (median and main quantiles)
plotSpecDens <- function(df)
{
  # Calculate quantile at every wavelength
  mat <- as.matrix(df[,-1])
  calQuant <- function(x){quantile(x, probs = c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))}
  dfSTATS <- data.frame(df[,1], t(apply(mat, 1, calQuant))) # 1 or 2 for row or column
  colnames(dfSTATS) <- c("w.length", "p1", "p10", "p25", "p50", "p75", "p90", "p99")
  
  # plots
  par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(4,5,1,1))
  plot(-500, bty = "L", type = "l", xlim = c(250,900), ylim = c(0, max(dfSTATS$p99, na.rm = T)), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  
  xL <- c(axTicks(side=1)[1] - 100, seq(axTicks(side=1)[1], tail(axTicks(side=1),1), 100), tail(axTicks(side=1),1) + 100)
  for(iX in xL)
  {
    abline(v = iX, col = addTrans("gray70", 150))
  }
  
  yL <- c(axTicks(side=2)[1] - 0.025, seq(axTicks(side=2)[1], tail(axTicks(side=2),1), 0.025), tail(axTicks(side=2),1) + 0.025)
  for(iY in yL)
  {
    abline(h = iY, col = addTrans("gray70", 150))
  }
  polygon(x = c(dfSTATS$w.length,rev(dfSTATS$w.length)), y = c(dfSTATS$p1,rev(dfSTATS$p99)), border = NA, col = addTrans("gray70", 150))
  polygon(x = c(dfSTATS$w.length,rev(dfSTATS$w.length)), y = c(dfSTATS$p10,rev(dfSTATS$p90)), border = NA, col = addTrans("gray50", 150))
  polygon(x = c(dfSTATS$w.length,rev(dfSTATS$w.length)), y = c(dfSTATS$p25,rev(dfSTATS$p75)), border = NA, col = addTrans("gray30", 150))
  
  points(p25~w.length, data = dfSTATS, type = "l", col = "gray30", lwd = 1)
  points(p75~w.length, data = dfSTATS, type = "l", col = "gray30", lwd = 1)
  points(p10~w.length, data = dfSTATS, type = "l", col = "gray50", lwd = 1)
  points(p90~w.length, data = dfSTATS, type = "l", col = "gray50", lwd = 1)
  points(p1~w.length, data = dfSTATS, type = "l", col = "gray70", lwd = 1)
  points(p99~w.length, data = dfSTATS, type = "l", col = "gray70", lwd = 1)
  points(p50~w.length, data = dfSTATS, type = "l", col = "firebrick3", lwd = 2)
  
  axis(side = 1, cex = 1, font = 2)
  axis(side = 2, cex = 1, font = 2, las = 2)
  mtext(side = 1, line = 2.5, text = "Wavelength (nm)", cex = 1.4)
  mtext(side = 2, line = 3, text = expression(paste("Spectral energy irradiance E(", lambda, ") (W m"^-2, " nm"^-1, ")", sep = "")), cex = 1.4)
}
