library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ooacquire)
library(ggplot2)
library(reshape)
library(fda)
library(data.table)

myUsername = ""

#### Maya processing with temp dark and filter files
MayaProcessingTemp <- function(subFolder, Folder, User = myUsername, Year = 2020, verbose = TRUE)
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
        } else if(substring(date,1,4) == "2022" | (substring(date,1,4) == "2023" & as.numeric(substring(date,6,7)) < 4)){
          desc <- MAYP11278_descriptors$cal_2021a
        } else if((substring(date,1,4) == "2023" & as.numeric(substring(date,6,7)) > 4) | substring(date,1,4) == "2024"){
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
      
      # Base scan file path
      irrad <- list(light = paste0(inFolder, Folder, "/", subFolder, "/", iMeas),
                    filter = paste0(inFolder, Folder, "/", subFolder, "/", filterFiles[iSet]),
                    dark = paste0(inFolder, Folder, "/", subFolder, "/", darkFiles[iSet]))
      
      ### correct dark and filter data for each iMeas scan
      raw.mspct <- ooacquire::read_files2mspct(files = irrad, descriptor = desc, verbose = FALSE)
      cps.mspct <- raw2corr_cps(raw.mspct, ref.pixs.range = corMeth[["inst.dark.pixs"]])
      
      # Calculate noise
      Dnoise <- abs(median(cps.mspct$light[5:155,"cps"]) - median(cps.mspct$dark[5:155,"cps"]))
      Fnoise <- abs(median(cps.mspct$light[5:155,"cps"]) - median(cps.mspct$filter[5:155,"cps"]))
      
      ref.wls.range <- unlist(raw.mspct$dark[range(2:4), "w.length"])
      valD <- average_spct(trim_spct(spct = raw.mspct$dark, range = ref.wls.range, byref = FALSE))
      valF <- average_spct(trim_spct(spct = raw.mspct$filter, range = ref.wls.range, byref = FALSE))
      
      # Import dark scan
      dfD <- read.table(paste0(inFolder, Folder, "/", subFolder, "/", darkFiles[iSet]), colClasses = "character", fill = TRUE)
      dfF <- read.table(paste0(inFolder, Folder, "/", subFolder, "/", filterFiles[iSet]), colClasses = "character", fill = TRUE)
      
      Dcounts <- as.numeric(dfD[18:2085,2])
      Fcounts <- as.numeric(dfF[18:2085,2])
      
      itD <- as.numeric(dfD[9,4]) / 1e6
      itF <- as.numeric(dfF[9,4]) / 1e6
      
      # Calculate new counts
      Dnew <- ((Dcounts[5:length(Dcounts)] - valD) / itD) + Dnoise
      Fnew <- ((Fcounts[5:length(Fcounts)] - valF) / itF) + Fnoise
      Dnew <- (Dnew * itD) + valD
      Fnew <- (Fnew * itF) + valF
      
      Dcounts <- c(Dcounts[1:4], Dnew)
      Fcounts <- c(Fcounts[1:4], Fnew)
      
      dfD[18:2085,2] <- round(Dcounts,0)
      dfF[18:2085,2] <- round(Fcounts,0)
      
      # Re-create and format dark and filter files
      SSD <- apply(dfD, 1, paste, collapse = " ", recycle0 = TRUE)
      SSF <- apply(dfF, 1, paste, collapse = " ", recycle0 = TRUE)
      SSD <- trimws(SSD, which = "right")
      SSF <- trimws(SSF, which = "right")
      SSD[18:2085] <- gsub(x = SSD[18:2085], pattern = " ", replacement = "\t")
      SSF[18:2085] <- gsub(x = SSF[18:2085], pattern = " ", replacement = "\t")
      
      # Write new dark and filter as temp files
      write.table(SSD, paste0(inFolder, Folder, "/", subFolder, "/", "temp-dark.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
      write.table(SSF, paste0(inFolder, Folder, "/", subFolder, "/", "temp-PC.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
      
      # New path for actual data to be processed
      irrad <- list(light = paste0(inFolder, Folder, "/", subFolder, "/", iMeas),
                    filter = paste0(inFolder, Folder, "/", subFolder, "/", "temp-PC.txt"),
                    dark = paste0(inFolder, Folder, "/", subFolder, "/", "temp-dark.txt"))
      
      # Run Pedro's routine
      irrad.spct <- NULL
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
