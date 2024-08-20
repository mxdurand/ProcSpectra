### Rename files in batch
renameBatch <- function(path, oldPattern, newPattern, digit = 5)
{
  #readline("Warning! The function will try to change EVERY files with the oldPattern. Type [Enter] to proceed or [Esc]x2 to abort")
  
  if (substring(path, nchar(path), nchar(path)) != "/"){
    path = paste0(path, "/")
  }
  
  filenames = list.files(path = path, pattern=oldPattern)
  if(sum(grep("dark", filenames)) != 0)
  {
    filenames <- filenames[-grep("dark", filenames)]
  }
  if(sum(grep("PC", filenames)) != 0)
  {
    filenames <- filenames[-grep("PC", filenames)]
  }
  
  nb <- length(filenames)
  if(nb > 10000){stop("function does not support number of files higher than 10000")}
  
  
  if(nb < 11){
    newFilenames = paste0(newPattern, "0000", 0:(nb-1), ".txt")
  } else {
    newFilenames = paste0(newPattern, "0000", 0:9, ".txt")
  }
  if(nb > 10 & nb < 101){
    newFilenames = append(newFilenames, values = paste0(newPattern, "000", 10:(nb-1), ".txt"))
  } else if (nb > 10) {
    newFilenames = append(newFilenames, values = paste0(newPattern, "000", 10:99, ".txt"))
  }
  if(nb > 100 & nb < 1001){
    newFilenames = append(newFilenames, values = paste0(newPattern, "00", 100:(nb-1), ".txt"))
  } else if (nb > 100) {
    newFilenames = append(newFilenames, values = paste0(newPattern, "00", 100:999, ".txt"))
  }
  if(nb > 1000 & nb < 10001){
    newFilenames = append(newFilenames, values = paste0(newPattern, "0", 1000:(nb-1), ".txt"))
  } else if (nb > 1000)  {
    newFilenames = append(newFilenames, values = paste0(newPattern, "00", 1000:9999, ".txt"))
  }
  
  file.rename(paste0(path, filenames), paste0(path, newFilenames))
}
