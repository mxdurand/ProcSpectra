library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ooacquire)
library(ggplot2)
library(reshape)
library(fda)
library(data.table)

myUsername = ""

### Copy dark and PC files for long scan if not taken
copyPCdark <- function(subFolder, Folder, User = myUsername, pattern = "long", Year = 2020)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  root <- substring(darkFiles, 1, nchar(darkFiles)-8)
  Files  <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0(pattern))
  
  for(iFile in Files)
  {
    for(iRoot in root)
    {
      if(sum(grep(gsub("open", "", iRoot), iFile)) > 0){
        cat("Using", iRoot, "for", iFile, "\n")
        
        namedark <- paste0(substring(iFile, 1, nchar(iFile)-4), "dark.txt")
        namePC <- paste0(substring(iFile, 1, nchar(iFile)-4), "PC.txt")
        
        file.copy(from = paste0(inFolder, Folder, "/", subFolder, "/", iRoot, "dark.txt"), to = paste0(inFolder, Folder, "/", subFolder, "/", namedark), overwrite = F)
        file.copy(from = paste0(inFolder, Folder, "/", subFolder, "/", iRoot, "PC.txt"), to = paste0(inFolder, Folder, "/", subFolder, "/", namePC), overwrite = F)
      }
    }
  }
}
