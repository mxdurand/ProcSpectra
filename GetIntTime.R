### Get integration time of spectra
GetIntTime <- function(subFolder, Folder, User = "Localadmin_durandma", Year = 2020)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  
  df <- data.frame()
  for (iDark in darkFiles)
  {
    namePattern <- substring(iDark, 1, nchar(iDark)-8)
    pathToFiles <- paste0(inFolder, Folder, "/", subFolder, "/")
    firstFile <- list.files(pathToFiles)[grep(namePattern, list.files(pathToFiles))[1]]
    data <- ooacquire::read_oo_data(file = paste0(pathToFiles, firstFile))
    
    row <- c(Folder, subFolder, substring(iDark,1,nchar(iDark)-8), attributes(data)$instr.settings$integ.time)
    df <- rbind(df, row)
  }
  colnames(df) <- c("Folder", "subFolder", "spectrum", "integrationTime_us")
  return(df)
}
