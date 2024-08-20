library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ooacquire)
library(ggplot2)
library(reshape)
library(fda)
library(data.table)

myUsername = ""

### Transform High-Speed Acquisition into standard scans form processing using open file of any other template
HSAprocessing <- function(subFolder, Folder, Template = NULL, User = myUsername, Year = 2020, verbose = TRUE)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  
  # Find dark and filter files
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  filterFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("PC.txt"))
  
  if(length(darkFiles) != length(filterFiles)){
    print(darkFiles) ; print(filterFiles)
    stop("The number of dark files differs from the number of filter files")
  }
  
  # Create output folder
  iOut <- paste0(inFolder, Folder, "/", strsplit(subFolder, "_", fixed = T)[[1]][1], "/")
  dir.create(iOut, recursive = T)
  
  # Get template
  if(is.null(Template))
  {
    if(sum(grep("open", dir(paste0(inFolder, Folder)))) > 0){
      noTemplate <- min(grep("00000", dir(paste0(inFolder, Folder, "/open/"))))
      if(is.infinite(noTemplate)){noTemplate <- 1}
      Template <- dir(paste0(inFolder, Folder, "/open/"), full.names = T)[noTemplate]
      if(verbose == T){message("Cannot find template file, using open scan as template...")}
    } else if(sum(grep("Open", dir(paste0(inFolder, Folder)))) > 0){
      noTemplate <- min(grep("00000", dir(paste0(inFolder, Folder, "/Open/"))))
      if(is.infinite(noTemplate)){noTemplate <- 1}
      Template <- dir(paste0(inFolder, Folder, "/Open/"), full.names = T)[noTemplate]
      if(verbose == T){message("Cannot find template file, using open scan as template...")}
    } else {
      DIR <- dir(paste0(inFolder, "/", Folder, "/", subFolder, "/"), full.names = T)
      Template <- DIR[grep("dark", DIR)][1]
      if(verbose == T){message("Cannot find template file, using dark scan as template...")}
    } 
  }
  dfT <- read.table(Template, colClasses = "character", fill = TRUE)
  
  iSet <- 9
  for (iSet in 1:length(darkFiles))
  {
    namePattern <- substr(darkFiles[iSet], start=1, stop=nchar(darkFiles[iSet])-8)
    measFiles <- list.files(path = paste0(inFolder, Folder, "/", subFolder, "/"), paste0(namePattern, ".txt"))
    
    if(verbose == TRUE)
    {
      cat("Reading", measFiles)
    }
    
    dx <- read.table(paste0(inFolder, Folder, "/", subFolder, "/", measFiles), fill = TRUE, skipNul = TRUE)
    dx[is.na(dx)] <- 0
    
    if(verbose == TRUE)
    {
      cat(" |  Writing", measFiles, "\n")
    }
    
    for(i in 1:ncol(dx))
    {
      if(nrow(dx) == length(18:2085))
      {
        dfT[18:2085,2] <- dx[,i]
      } else {
        NROW = nrow(dx)
        dfT[18:(NROW+17),2] <- dx[,i]
        dfT[(NROW+18):2085,2] <- 0
      }
      
      SS <- apply(dfT, 1, paste, collapse = " ", recycle0 = TRUE)
      SS <- trimws(SS, which = "right")
      SS[18:2085] <- gsub(x = SS[18:2085], pattern = " ", replacement = "\t")
      
      inTime <- round(as.numeric(gsub("X", "", colnames(dx)[2])) -  as.numeric(gsub("X", "", colnames(dx)[1])), 0)
      if(inTime == 7){inTime <- 7.2}
      inTime <- inTime * 1e3
      SS[9] <- paste0("Integration Time (usec): ", inTime, " (MAYP112785)")
      
      write.table(SS, paste0(iOut, namePattern, formatC(i, width = nchar(ncol(dx))+2, format = "d", flag = "0"), ".txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
  }
}
