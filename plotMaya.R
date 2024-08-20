library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ooacquire)
library(ggplot2)
library(reshape)
library(fda)
library(data.table)

myUsername = ""

### figure only
plotMaya <- function(subFolder, Folder, User = myUsername)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya2020/Scans2020/")
  outFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya2020/ScansOut2020/")
  
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  filterFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("PC.txt"))
  
  outPath <- paste0(outFolder, Folder, "/", subFolder, "/")
  figPath <- paste0(outFolder, Folder, "/", subFolder, "/Figures/")
  dir.create(figPath, recursive = TRUE)
  
  for (iSet in 1:length(darkFiles))
  {
    # If long spectrum, only use 1st one
    namePattern <- substr(darkFiles[iSet], start=1, stop=nchar(darkFiles[iSet])-8)
    if(length(grep("long", namePattern, fixed = T)) > 0){
      measFiles <- list.files(path = paste0(inFolder, Folder, "/", subFolder, "/"), pattern = paste0(namePattern, "00"))[1]
    } else {
      measFiles <- list.files(path = paste0(inFolder, Folder, "/", subFolder, "/"), pattern = paste0(namePattern, "00"))
    }
    # If only one spectrum, there is no "00"
    if(length(measFiles) == 0){
      measFiles <- list.files(path = paste0(inFolder, Folder, "/", subFolder, "/"), paste0(namePattern, ".txt"))
    }
    
    colNames <- substring(measFiles, 1, nchar(measFiles) - 4)
    irrad.out <- read.table(paste0(outFolder, Folder, "/", subFolder, "/", colNames[1],"_irrad.txt"), header = T)
    irrad.out.ggplot <- melt(irrad.out[,2:ncol(irrad.out)]) 
    irrad.out.ggplot$wavelength <- rep(irrad.out$wavelength, ncol(irrad.out) - 1)
    
    if(is.null(irrad.out.ggplot$variable)){
      irrad.out.ggplot$variable<-"A"
    } 
    
    pp <- ggplot(irrad.out.ggplot, aes(x=wavelength, y = value, col = variable))
    pp <- pp + geom_line() + coord_cartesian(xlim = c(290,900)) + theme(legend.position = "none")
    pp <- pp + scale_y_continuous(name = expression("Spectral energy irradiance "~E(lambda)~" ("~W*m^-2*nm^-1~")"))
    pp <- pp + scale_x_continuous(name = "Wavelength (nm)")
    #pp <- pp + geom_line(data = irrad.out.ggplot[irrad.out.ggplot$variable == names(tempDeepestIrrad)[2],], aes(x = wavelength, y = value), col = "black")
    #pp <- pp + geom_line(data = tempAvgIrrad, aes_string(x = names(tempAvgIrrad)[1], y = names(tempAvgIrrad)[2]), col = "black", lwd = 1)
    pp <- pp + theme_bw(base_size = 20, base_family = "") + theme(legend.position = "none")
    
    jpegName <- paste0(figPath, Folder, subFolder, colNames[1], ".jpg")
    graphics.off()
    jpeg(filename = jpegName, width = 800*1.5, height = 800)
    print(pp)
    dev.off()
  }
}
