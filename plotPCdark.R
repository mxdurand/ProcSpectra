library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ooacquire)
library(ggplot2)
library(reshape)
library(fda)
library(data.table)

myUsername = ""

### Plot PC and dark files for check-up
plotPCdark <- function(subFolder, Folder, User = myUsername, Year = 2020, desc = MAYP112785_descriptors$cal_2021xa, corMeth = MAYP112785_sun.mthd)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  namePattern <- substring(darkFiles, 1, nchar(darkFiles)-8)
  filterFiles <- paste0(namePattern, "PC.txt")
  
  for (i in 1:length(darkFiles))
  {
    namePattern <- substring(darkFiles, 1, nchar(darkFiles)-8)
    namePattern <- namePattern[i]
    if(length(grep("long00", namePattern, fixed = T)) > 0){
      measFiles <- list.files(path = paste0(inFolder, Folder, "/", subFolder, "/"), pattern = paste0(namePattern, "00"))[1]
    } else {
      measFiles <- list.files(path = paste0(inFolder, Folder, "/", subFolder, "/"), pattern = paste0(namePattern, "00"))
    }
    # If only one spectrum, there is no "00"
    if(length(measFiles) == 0){
      measFiles <- list.files(path = paste0(inFolder, Folder, "/", subFolder, "/"), paste0(namePattern, ".txt"))
      # If only one scan but sevreral are found (becasue of same namepatter
      if(length(measFiles) > 1){
        measFiles <- measFiles[which(nchar(measFiles) == nchar(paste0(namePattern, ".txt")))]
      }
      if(length(measFiles) > 1){
        stop("Found multiple scans with the same name pattern...")
      }
    }
    
    irrad <- list(light = paste0(inFolder, Folder, "/", subFolder, "/", measFiles[1]),
                  filter = paste0(inFolder, Folder, "/", subFolder, "/", filterFiles[i]),
                  dark = paste0(inFolder, Folder, "/", subFolder, "/", darkFiles[i]))
    
    raw.mspct <- ooacquire::read_files2mspct(files = irrad, descriptor = desc, verbose = FALSE)
    cps.mspct <- raw2corr_cps(raw.mspct, ref.pixs.range = corMeth[["inst.dark.pixs"]])
    
    par(mfrow = c(1,2), oma = c(0,0,1,0), mar = c(4,5,1,1), bty = "L")
    plot(-500, xaxt = "n", yaxt = "n", xlab = "", ylab = "", xlim = c(190, 1100), ylim = c(0,max(c(cps.mspct$light$cps,cps.mspct$filter$cps))))
    points(cps~w.length, data = cps.mspct$filter, type = "l", lwd = 3, col = "black")
    points(cps~w.length, data = cps.mspct$dark, type = "l", lwd = 1, col = "gray60")
    points(cps~w.length, data = cps.mspct$light, type = "l", lwd = 1, col = "red")
    axis(side = 1, font = 2, cex.axis = 0.9)
    axis(side = 2, font = 2, cex.axis = 0.9, las = 2, labels = formatC(axTicks(2), format = "g"), at = axTicks(2))
    mtext(side = 1, line = 2, text = "Wavelength (nm)", cex = 1.2)
    mtext(side = 2, line = 3.5, text = "Counts per sec", cex = 1.2)
    abline(v = 390, col = "red")
    abline(v = 400, col = "red")
    title(outer = T, paste(Folder, subFolder, substring(darkFiles[i], 1, nchar(darkFiles[i])-8), sep = " | "), cex.main = 0.9)
    
    maxVal <- max(c(cps.mspct$filter[cps.mspct$filter$w.length < 400,"cps"],cps.mspct$light[cps.mspct$light$w.length < 400,"cps"])) * 1.1
    minVal <- min(c(cps.mspct$filter[cps.mspct$filter$w.length < 400,"cps"],cps.mspct$light[cps.mspct$light$w.length < 400,"cps"])) * 0.9
    
    plot(-500, xaxt = "n", yaxt = "n", xlab = "", ylab = "", xlim = c(190, 400), ylim = c(minVal, maxVal))
    points(cps~w.length, data = cps.mspct$filter, type = "l", lwd = 3, col = "black")
    points(cps~w.length, data = cps.mspct$dark, type = "l", lwd = 1, col = "gray60")
    points(cps~w.length, data = cps.mspct$light, type = "l", lwd = 1, col = "red")
    axis(side = 1, font = 2, cex.axis = 0.9)
    axis(side = 2, font = 2, cex.axis = 0.9, las = 2, labels = formatC(axTicks(2), format = "g"), at = axTicks(2))
    mtext(side = 1, line = 2, text = "Wavelength (nm)", cex = 1.2)
    
    readline(prompt="Press [enter] for next plot...")
  }
}
