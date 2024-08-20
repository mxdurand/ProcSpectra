library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ooacquire)
library(ggplot2)
library(reshape)
library(fda)
library(data.table)

myUsername = ""

###
MayaProcessing <- function(subFolder, Folder, User = myUsername, Year = 2020, verbose = TRUE)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  outFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/ScansOut", Year, "/")
  
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  filterFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("PC.txt"))
  
  if(length(darkFiles) != length(filterFiles)){
    print(darkFiles) ; print(filterFiles)
    stop("The number of dark files differs from the number of filter files")
  }
  
  iOut <- paste0(outFolder, Folder, "/", subFolder, "/")
  dir.create(iOut, recursive = T)
  
  iSet <- 1
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
    
    BSWF.out <- NULL
    irrad.out <- NULL
    
    iMeas <- measFiles[1]
    for(iMeas in measFiles)
    {
      if(verbose == TRUE)
      {
        cat(namePattern, " | ", grep(iMeas, measFiles), "/", length(measFiles), "\n")
      }
      
      irrad.spct <- NULL
      irrad <- list(light = paste0(inFolder, Folder, "/", subFolder, "/", iMeas),
                    filter = paste0(inFolder, Folder, "/", subFolder, "/", filterFiles[iSet]),
                    dark = paste0(inFolder, Folder, "/", subFolder, "/", darkFiles[iSet]))
      
      # This data point had bad filter file
      if(Folder == "2020.05.25.FI.Lammi" | subFolder == "Picea" | namePattern == "MP3ovc"){
        irrad <- list(light = paste0(inFolder, Folder, "/", subFolder, "/", iMeas),
                      #filter = paste0(inFolder, Folder, "/", subFolder, "/", filterFiles[iSet]),
                      dark = paste0(inFolder, Folder, "/", subFolder, "/", darkFiles[iSet]))
      }
      
      # Find which spectrometer
      SPCM <- readChar(paste0(inFolder, Folder, "/", subFolder, "/", iMeas), 5e2)
      SPCM <- strsplit(SPCM, "\r\n", fixed = T)[[1]][8]
      SPCM <- trimws(strsplit(SPCM, ":", fixed = T)[[1]][2])
      
      # Find descriptor based on date
      date <- paste(strsplit(Folder, ".", fixed = T)[[1]][1:3], collapse = "-")
      
      if(SPCM == "MAYP11278"){
        if(verbose){cat("Using MAYP11278 spectrometer...", "\n")}
        
        if(substring(date,1,4) == "2016" | substring(date,1,4) == "2017"){
          desc <- MAYP11278_descriptors$cal_2016a
        } else if(substring(date,1,4) == "2018" & as.numeric(substring(date,6,7)) < 4){
          desc <- MAYP11278_descriptors$cal_2016a
        } else if(substring(date,1,4) == "2018" & as.numeric(substring(date,6,7)) >= 4){
          desc <- MAYP11278_descriptors$cal_2019a
        } else if(substring(date,1,4) == "2019" | substring(date,1,4) == "2020"){
          desc <- MAYP11278_descriptors$cal_2019a
        } else if(substring(date,1,4) == "2021" & as.numeric(substring(date,6,7)) < 3){
          desc <- MAYP11278_descriptors$cal_2019a
        } else if(substring(date,1,4) == "2021" & as.numeric(substring(date,6,7)) >= 3){
          desc <- MAYP11278_descriptors$cal_2021a
        } else if(substring(date,1,4) == "2022" | (substring(date,1,4) == "2023" & as.numeric(substring(date,6,7)) < 3)){
          desc <- MAYP11278_descriptors$cal_2021a
        } else if((substring(date,1,4) == "2023" & as.numeric(substring(date,6,7)) >= 3) | substring(date,1,4) == "2024"){
          desc <- MAYP11278_descriptors$cal_2023a
        } else {
          message("Calibration for spectrometer could not be found, using latest calibration")
          desc <- MAYP11278_descriptors$cal_2023a
        }
        
        if(verbose){print(desc$inst.calib$start.date)}
        
        # Assign correction method
        corMeth <- MAYP11278_sun.mthd
        
      } else if(SPCM == "MAYP112785"){
        if(verbose){cat("Using MAYP112785 spectrometer...", "\n")}
        
        if(substring(date,1,4) == "2016" | substring(date,1,4) == "2017" | substring(date,1,4) == "2018"){
          desc <- MAYP112785_descriptors$cal_2016xa
        } else if(substring(date,1,4) == "2019" | substring(date,1,4) == "2020"){
          desc <- MAYP112785_descriptors$cal_2019xa
        } else if(substring(date,1,4) == "2021" & as.numeric(substring(date,6,7)) < 3){
          desc <- MAYP112785_descriptors$cal_2019xa
        } else if(substring(date,1,4) == "2021" & as.numeric(substring(date,6,7)) >= 3){
          desc <- MAYP112785_descriptors$cal_2021xa
        } else if(substring(date,1,4) == "2022" | (substring(date,1,4) == "2023" & as.numeric(substring(date,6,7)) < 3)){
          desc <- MAYP112785_descriptors$cal_2021xa
        } else if((substring(date,1,4) == "2023" & as.numeric(substring(date,6,7)) >= 3) | substring(date,1,4) == "2024"){
          desc <- MAYP112785_descriptors$cal_2023xa
        } else {
          message("Calibration for spectrometer could not be found, using latest calibration")
          desc <- MAYP112785_descriptors$cal_2023xa
        }
        
        if(verbose){print(desc$inst.calib$start.date)}
        
        # Assign correction method
        corMeth <- MAYP112785_sun.mthd
        
      } else {
        stop("Error: Spectrometer not found")
      }
      
      # Run Pedro's routine
      irrad.spct <- s_irrad_corrected(x = irrad,
                                      descriptor = desc,
                                      correction.method = corMeth)
      
      # Approximation of NA
      if(sum(is.na(irrad.spct$s.e.irrad)) > 0){
        y<-irrad.spct$s.e.irrad
        x<-irrad.spct$w.length
        where.na<-is.na(y)
        xout<-x[where.na]
        out<-approx(x=x,y=y,xout=xout,f=0,method="constant")
        y[where.na]<-out$y
        irrad.spct$s.e.irrad<-y
      }
      
      irrad.spct <- trim_wl(irrad.spct, range = c(290, 900), use.hinges = FALSE, fill = 0)
      tryCatch({suppressMessages({irrad.spct <- smooth_spct(irrad.spct, method = "custom")})}, error=function(e){})
      
      #take 2 columns from out and then remove it
      dfMeas <- data.frame(wavelength = irrad.spct$w.length, irradiance=irrad.spct$s.e.irrad)
      MeasBSWF <- suppressWarnings(BSWF.irrad.calc(irrad.spct))
      
      BSWF.out <- cbind(BSWF.out, MeasBSWF$value)
      irrad.out <- cbind(irrad.out, dfMeas$irradiance)
    }
    
    colNames <- substring(measFiles, 1, nchar(measFiles) - 4)
    irrad.out <- data.frame(dfMeas$wavelength, irrad.out)
    names(irrad.out) <- c("wavelength", colNames)
    BSWF.out <- data.frame(MeasBSWF$spectra, BSWF.out)
    names(BSWF.out) <- c("spectra", colNames)
    
    write.table(BSWF.out, paste0(iOut, colNames[1], "_BSWF.txt"), row.names = FALSE)
    write.table(irrad.out, paste0(iOut, colNames[1], "_irrad.txt"), row.names = FALSE)
  }
}
