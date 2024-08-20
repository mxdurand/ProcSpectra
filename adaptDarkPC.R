library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ooacquire)
library(ggplot2)
library(reshape)
library(fda)
library(data.table)

myUsername = ""

### Correct dark and light measurements when not taken at same time/integrating times
adaptDarkPC <- function(subFolder, Folder, User = myUsername, Year = 2020, verbose = TRUE, desc = MAYP112785_descriptors$cal_2021xa, corMeth = MAYP112785_sun.mthd)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  outFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/ScansOut", Year, "/")
  
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  filterFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("PC.txt"))
  
  iSet <- 3
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
      # If only one scan but several are found (because of same name pattern)
      if(length(measFiles) > 1){
        measFiles <- measFiles[which(nchar(measFiles) == nchar(paste0(namePattern, ".txt")))]
      }
      if(length(measFiles) > 1){
        stop("Found multiple scans with the same name pattern...")
      }
    }
    
    # Use only first file
    iMeas <- measFiles[1]
    irrad <- list(light = paste0(inFolder, Folder, "/", subFolder, "/", iMeas),
                  filter = paste0(inFolder, Folder, "/", subFolder, "/", filterFiles[iSet]),
                  dark = paste0(inFolder, Folder, "/", subFolder, "/", darkFiles[iSet]))
    
    raw.mspct <- ooacquire::read_files2mspct(files = irrad, descriptor = desc, verbose = FALSE)
    cps.mspct <- raw2corr_cps(raw.mspct, ref.pixs.range = corMeth[["inst.dark.pixs"]])
    
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
    
    Dnew <- ((Dcounts[5:length(Dcounts)] - valD) / itD) + Dnoise
    Fnew <- ((Fcounts[5:length(Fcounts)] - valF) / itF) + Fnoise
    Dnew <- (Dnew * itD) + valD
    Fnew <- (Fnew * itF) + valF
    
    Dcounts <- c(Dcounts[1:4], Dnew)
    Fcounts <- c(Fcounts[1:4], Fnew)
    
    dfD[18:2085,2] <- round(Dcounts,0)
    dfF[18:2085,2] <- round(Fcounts,0)
    
    SSD <- apply(dfD, 1, paste, collapse = " ", recycle0 = TRUE)
    SSF <- apply(dfF, 1, paste, collapse = " ", recycle0 = TRUE)
    SSD <- trimws(SSD, which = "right")
    SSF <- trimws(SSF, which = "right")
    SSD[18:2085] <- gsub(x = SSD[18:2085], pattern = " ", replacement = "\t")
    SSF[18:2085] <- gsub(x = SSF[18:2085], pattern = " ", replacement = "\t")
    
    write.table(SSD, paste0(inFolder, Folder, "/", subFolder, "/", darkFiles[iSet]), quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(SSF, paste0(inFolder, Folder, "/", subFolder, "/", filterFiles[iSet]), quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
