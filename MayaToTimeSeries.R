### Transform a multiscan measure into dataframe for Time Series analysis (uses "GetIntTime")
MayaToTimeSeries <- function(df, Folder, subFolder, integTime_ms = NULL, User = "Localadmin_durandma", Year = 2020)
{
  # Get spectrum number
  spcNames <- colnames(df)[2:ncol(df)]
  
  # If first character is number, R adds an X to col name (so we remove it here)
  for(iName in 1:length(spcNames))
  {
    if(substring(spcNames[iName], 1, 1) == "X")
    {
      spcNames[iName] <- substring(spcNames[iName], 2, nchar(spcNames[iName]))
    }
  }
  
  splitName <- strsplit(spcNames[1], "")[[1]]
  lastLetter <- suppressWarnings(max(grep(TRUE, is.na(as.numeric(splitName)))))
  spcNum <- substring(spcNames, lastLetter + 1, nchar(spcNames))
  #spcNum <- gsub("[^[:digit:]]", "", spcNames)
  while(as.numeric(substring(spcNum[1], 1, 1)) != 0)
  {
    spcNum <- substring(spcNum, 2, nchar(spcNum))
  }
  spcNum <- as.numeric(spcNum)
  
  # Get integration time if no given
  if(is.null(integTime_ms))
  {
    namePattern <- strsplit(spcNames[1], "00")[[1]][1]
    dfT <- GetIntTime(Folder = Folder, subFolder = subFolder, User = User, Year = Year)
    iTime <- dfT[dfT$spectrum == namePattern,"integrationTime_us"]
    if(length(iTime) == 0){iTime <- dfT[dfT$spectrum == paste0(namePattern,"0"),"integrationTime_us"]}
    iTime <- as.numeric(iTime) / 1e6
  } else {
    iTime <- integTime_ms  / 1e3
  }
  
  # Calculate time of spectrum based on integration time and spectrum number
  Times <- iTime * spcNum
  
  # Build new data frane in long form
  newWavelengthCol <- rep(unlist(df[,1]), times = ncol(df) - 1)
  newIrradCol <- unlist(df)[(nrow(df)+1):(ncol(df)*nrow(df))]
  newNamesCol <- rep(spcNames, each = nrow(df))
  newSpcNumCol <-  rep(spcNum, each = nrow(df))
  newTimeCol <- rep(Times, each = nrow(df))
  
  newdf <- data.frame(newWavelengthCol, newTimeCol, newIrradCol, newSpcNumCol, newNamesCol)
  rownames(newdf) <- NULL
  if(is.numeric(newWavelengthCol))
  {
    colnames(newdf) <- c("Wavelength", "Time", "Irrad", "SpcNum", "Spectrum")
  } else {
    colnames(newdf) <- c("BSWF", "Time", "Irrad", "SpcNum", "Spectrum")
    require(tidyr)
    newdf <- tidyr::spread(newdf, BSWF, Irrad)
    for(j in 4:44)
    {
      newdf[,j] <- as.numeric(newdf[,j])
    }
    
  }
  return(newdf)
}