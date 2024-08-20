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
ScanAssess <- function(Folder, User = myUsername, Year = 2020)
{
  df0 <- data.frame()
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  
  iSubFolder <- list.files(path = paste0(inFolder, "/", Folder))[1]
  for(iSubFolder in  list.files(path = paste0(inFolder, "/", Folder)))
  {
    darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", iSubFolder, "/"), pattern = paste0("dark.txt"))
    if(length(darkFiles) == 0){next()}
    
    namePattern <- substr(darkFiles, start = 1, stop = nchar(darkFiles)-8)
    nbFiles <- vector()
    for(iNamePattern in 1:length(namePattern))
    {
      nb <- length(list.files(path = paste0(inFolder, Folder, "/", iSubFolder, "/"), pattern = paste0(namePattern[iNamePattern], "00")))
      if(nb == 0){
        nb <- length(list.files(path = paste0(inFolder, Folder, "/", iSubFolder, "/"), paste0(namePattern[iNamePattern], ".txt")))
      }
      nbFiles <- append(nbFiles, values = nb)
      row <- c(Folder, iSubFolder, namePattern[iNamePattern], nbFiles[iNamePattern])
      df0 <- rbind(df0, row)
    }
  }
  if(length(df0) == 0){stop("No scan to assess")}
  colnames(df0) <- c("Folder", "subFolder", "ScanName", "ScanNumber")
  
  estTime <- sum(as.numeric(df0$ScanNumber))*33/50
  cat("Estimated time is", estTime, "sec or", estTime/60, "min or", estTime/3600, "hours", "\n")
  
  return(df0)
}
