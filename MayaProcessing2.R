library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ooacquire)
library(ggplot2)
library(reshape)
library(fda)
library(data.table)

myUsername = ""

### Plots figures and compute averages
MayaProcessing2 <- function(subFolder, Folder, User = myUsername, Year = 2020)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  outFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/ScansOut", Year, "/")
  
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  filterFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("PC.txt"))
  
  intruder <- which(darkFiles == "temp-dark.txt")
  if(sum(intruder) > 0){darkFiles <- darkFiles[-intruder]}
  
  outPath <- paste0(outFolder, Folder, "/", subFolder, "/")
  figPath <- paste0(outFolder, Folder, "/", subFolder, "/Figures/")
  dir.create(figPath, recursive = TRUE)
  
  allDeepestNames = allDeepestIrrad = allDeepestBSWF = NULL
  allAvgNames = allAvgIrrad = allAvgBSWF = NULL
  
  iSet = 1
  for (iSet in 1:length(darkFiles))
  {
    # If long spectrum, only use 1st one
    namePattern <- substr(darkFiles[iSet], start=1, stop=nchar(darkFiles[iSet])-8)
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
    
    # Read data
    colNames <- substring(measFiles, 1, nchar(measFiles) - 4)
    if(sum(grep(paste0(colNames[1],"_irrad.txt"), list.files(paste0(outFolder, Folder, "/", subFolder, "/")))) == 0){
      cat("Skipped ", paste0(colNames[1],"_irrad.txt"), "\n")
      next()
    }
    irrad.out <- fread(paste0(outFolder, Folder, "/", subFolder, "/", colNames[1],"_irrad.txt"), header = T)
    BSWF.out <- fread(paste0(outFolder, Folder, "/", subFolder, "/", colNames[1],"_BSWF.txt"), header = T)
    irrad.out <- as.data.frame(irrad.out)
    BSWF.out <- as.data.frame(BSWF.out)
    
    avgIrradName <- paste0(colNames[1], "_averageirrad.txt")
    avgBSWFName.xls <- paste0(colNames[1], "_averageBSWF.xls")
    avgBSWFName.txt <- paste0(colNames[1], "_averageBSWF.txt")
    deepestIrradName <- paste0(colNames[1], "_deepestirrad.txt")
    deepestBSWFName.xls <- paste0(colNames[1], "_deepestBSWF.xls")
    deepestBSWFName.txt <- paste0(colNames[1], "_deepestBSWF.txt")
    depthIrradName.xls <- paste0(colNames[1], "_depthirrad.xls")
    depthIrradName.txt <- paste0(colNames[1], "_depthirrad.txt")
    
    # Find deepest curve only for data with more than one measurement
    if(ncol(irrad.out) > 2){
      
      # fda won't run of only 2 measurments
      if(ncol(irrad.out) == 3){
        fda.out <- data.frame(depth = c(0,0))
      } else {
        fda.out <- fbplot(fit = irrad.out[,-1], x = irrad.out[,1], plot = FALSE)
      }
      dfDepth <- data.frame(indices = 0:(ncol(irrad.out[,-1]) - 1), depth = fda.out$depth, ave = apply(irrad.out[,-1], 2, mean, na.rm = T))
      
      dfDepth.ordered <- dfDepth[order(dfDepth$depth, decreasing = TRUE),]
      dfDepth.ordered2 <- dfDepth[order(dfDepth$ave, decreasing = TRUE),]
      
      # We add 2 to index because measurements start from 0 and we insert 'wavelength'
      # rank <- quantile(1:ncol(irrad.out[,-1]), probs = percentile)
      # indexpercentile <- dfDepth.ordered2$indices[rank] + 2
      indexpercentile <- dfDepth.ordered$indices[1] + 2
      
      ave.irrad <- data.frame(wavelength = irrad.out[,1], average = apply(irrad.out[,-1], 1, mean, na.rm = T))
      ave.BSWF <- data.frame(spectra = BSWF.out[,1], average = apply(BSWF.out[,-1], 1, mean, na.rm = T))
      
      write.table(BSWF.out[,c(1,indexpercentile)], paste0(outPath, deepestBSWFName.txt), row.names = FALSE)
      write.table(irrad.out[,c(1,indexpercentile)], paste0(outPath, deepestIrradName), row.names = FALSE)
      write.table(dfDepth.ordered2, paste0(outPath, depthIrradName.txt), row.names = FALSE)
      write.table(ave.BSWF, paste0(outPath, avgBSWFName.txt), row.names = FALSE)
      write.table(ave.irrad, paste0(outPath, avgIrradName), row.names = FALSE)
      
    } else if(ncol(irrad.out) == 2){
      
      write.table(BSWF.out, paste0(outPath, deepestBSWFName.txt), row.names = FALSE)
      write.table(irrad.out, paste0(outPath, deepestIrradName), row.names = FALSE)
      write.table(BSWF.out, paste0(outPath, avgBSWFName.txt), row.names = FALSE)
      write.table(irrad.out, paste0(outPath, avgIrradName), row.names = FALSE)
      
    }
    
    # Reading deepest .txt file   
    tempDeepestIrrad <- read.table(paste0(outPath, deepestIrradName), header = TRUE)
    allDeepestIrrad <- cbind(allDeepestIrrad, tempDeepestIrrad[,2])
    allDeepestNames<-c(allDeepestNames,colnames(tempDeepestIrrad)[2])
    
    # Reading deepest BSWF .xls file     
    tempDeepestBSWF <- read.table(paste0(outPath, deepestBSWFName.txt), header = TRUE)
    allDeepestBSWF <- cbind(allDeepestBSWF, tempDeepestBSWF[,2])
    
    # Reading average file
    tempAvgIrrad <- read.table(paste0(outPath, avgIrradName), header = TRUE)
    allAvgIrrad <- cbind(allAvgIrrad, tempAvgIrrad[,2])
    allAvgNames <- c(allAvgNames, colnames(tempAvgIrrad)[2])
    
    # Reading average BSWF file     
    tempAvgBSWF <- read.table(paste0(outPath, avgBSWFName.txt), header = TRUE)
    allAvgBSWF <- cbind(allAvgBSWF, tempAvgBSWF[,2])
    
    # Figs
    if(ncol(irrad.out) > 2){
      irrad.out.ggplot <- melt(setDT(irrad.out[,2:ncol(irrad.out)])) 
      irrad.out.ggplot$wavelength <- rep(irrad.out$wavelength, ncol(irrad.out) - 1)
    } else {
      irrad.out.ggplot <- irrad.out
      irrad.out.ggplot$value <- irrad.out.ggplot[,2]
    }
    
    if(is.null(irrad.out.ggplot$variable)){
      irrad.out.ggplot$variable<-"A"
    } 
    
    pp <- ggplot(irrad.out.ggplot, aes(x = wavelength, y = value, col = variable))
    pp <- pp + geom_line() + coord_cartesian(xlim = c(290,900)) + theme(legend.position = "none")
    pp <- pp + ylab(expression("Spectral energy irradiance "~E(lambda)~" ("~W*m^-2*nm^-1~")"))
    pp <- pp + xlab("Wavelength (nm)")
    pp <- pp + geom_line(data = irrad.out.ggplot[irrad.out.ggplot$variable == names(tempDeepestIrrad)[2],], aes(x = wavelength, y = value), col = "black", lwd = 1)
    pp <- pp + geom_line(data = tempAvgIrrad, aes_string(x = names(tempAvgIrrad)[1], y = names(tempAvgIrrad)[2]), col = "black")
    pp <- pp + theme_bw(base_size = 20, base_family = "") + theme(legend.position = "none")
    
    jpegName <- paste0(figPath, Folder, gsub("/", ".", subFolder), colNames[1], ".jpg")
    graphics.off()
    jpeg(filename = jpegName, width = 800*1.5, height = 800)
    print(pp)
    dev.off()
  }
  
  # allDeepestIrrad <- cbind(tempDeepestIrrad[,1], allDeepestIrrad)
  # colnames(allDeepestIrrad)<-c("wavelength", allDeepestNames)
  # fileName <- paste0(outPath, Folder, subFolder, "_alldeepestirrad.txt")
  # write.table(allDeepestIrrad, file = fileName ,row.names = FALSE)
  # 
  # allDeepestBSWF <- data.frame(spectra = tempDeepestBSWF[,1], allDeepestBSWF)
  # names(allDeepestBSWF) <- c("spectra", allDeepestNames)
  # fileName.xls <- paste0(outPath, Folder, subFolder, "_alldeepestBSWF.xls")
  # fileName.txt <- paste0(outPath, Folder, subFolder, "_alldeepestBSWF.txt")
  # write.table(allDeepestBSWF, file = fileName.txt, row.names = FALSE)
  # 
  # allAvgIrrad <- cbind(tempAvgIrrad[,1], allAvgIrrad)
  # colnames(allAvgIrrad) <- c("wavelength", allDeepestNames)
  # fileName <- paste0(outPath, Folder, subFolder, "_allaverageirrad.txt")
  # write.table(allAvgIrrad, file = fileName, row.names = FALSE)
  # 
  # allAvgBSWF <- data.frame(spectra = tempAvgBSWF[,1], allAvgBSWF)
  # names(allAvgBSWF) <- c("spectra", allDeepestNames)
  # fileName.xls <- paste0(outPath, Folder, subFolder, "_allaverageBSWF.xls")
  # fileName.txt <- paste0(outPath, Folder, subFolder, "_allaverageBSWF.txt")
  # write.table(allAvgBSWF, file = fileName.txt, row.names = FALSE)
}
