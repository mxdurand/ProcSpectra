### Get time for spectra measurement
getMayaTime <- function(subFolder, Folder, User = "Localadmin_durandma", Year = 2020)
{
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", Year, "/Scans", Year, "/")
  darkFiles <- list.files(path = paste0(inFolder, "/", Folder, "/", subFolder, "/"), pattern = paste0("dark.txt"))
  
  df <- data.frame()
  for (iDark in darkFiles)
  {
    namePattern <- substring(iDark, 1, nchar(iDark)-8)
    pathToFiles <- paste0(inFolder, Folder, "/", subFolder, "/")
    allFiles <- list.files(pathToFiles)[grep(namePattern, list.files(pathToFiles))]
    if(length(allFiles) > 1){firstFile <- allFiles[2]} else {firstFile <- allFiles[1]}
    data <- ooacquire::read_oo_data(file = paste0(pathToFiles, firstFile))
    
    row <- c(substring(subFolder,1,1), substring(iDark,1,nchar(iDark)-8), strsplit(as.character(attributes(data)$when.measured), " ", fixed = T)[[1]])
    df <- rbind(df, row)
  }
  colnames(df) <- c("stand", "spectrum", "date", "time")
  return(df)
}
