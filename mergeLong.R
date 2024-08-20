# Merges long and short scans (high and low resolution) to get better UV measurement.
mergeLong <- function(short, long, Folder, subFolder, User, plotit = F)
{
  # Load calibrations files
  library(ooacquire)
  library(minpack.lm)
  
  ### Define paths
  year <- substring(Folder, 1, 4)
  date <- substring(Folder, 1,10)
  inFolder <- paste0("C:/Users/", User, "/Dropbox/SpecS/Maya", year, "/Scans", year, "/")
  short_path <- paste0(inFolder, Folder, "/", subFolder, "/", short)
  long_path <- paste0(inFolder, Folder, "/", subFolder, "/", long)
  
  # Name the output short+long file
  short_name <- strsplit(short, ".txt", fixed = T)[[1]][1]
  short_namepattern <- strsplit(short_name, "00")[[1]][1]
  short_numbers <- gsub(pattern = short_namepattern, replacement = "", x = short_name)
  out_path <- paste0(inFolder, Folder, "/", subFolder, "/", short_namepattern, "_merged_", short_numbers, ".txt")
  
  ### Find which spectrometer and get decriptor and correction method
  SPCM <- readChar(short_path, 5e2)
  SPCM <- strsplit(SPCM, "\r\n", fixed = T)[[1]][8]
  SPCM <- trimws(strsplit(SPCM, ":", fixed = T)[[1]][2])
  
  if(SPCM == "MAYP11278"){
    
    # Assign correct calibration settings
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
    } else {
      message("Calibration for spectrometer could not be found, using latest calibration")
      desc <- MAYP11278_descriptors$cal_2021a
    }
    
    # Assign correction method
    corMeth <- MAYP11278_sun.mthd
  
  } else if(SPCM == "MAYP112785"){
    if(substring(date,1,4) == "2016" | substring(date,1,4) == "2017" | substring(date,1,4) == "2018"){
      desc <- MAYP112785_descriptors$cal_2016xa
    } else if(substring(date,1,4) == "2019" | substring(date,1,4) == "2020"){
      desc <- MAYP112785_descriptors$cal_2019xa
    } else if(substring(date,1,4) == "2021" & as.numeric(substring(date,6,7)) < 3){
      desc <- MAYP112785_descriptors$cal_2019xa
    } else if(substring(date,1,4) == "2021" & as.numeric(substring(date,6,7)) >= 3){
      desc <- MAYP112785_descriptors$cal_2021xa
    } else {
      message("Calibration for spectrometer could not be found, using latest calibration")
      desc <- MAYP112785_descriptors$cal_2021xa
    }
    
    # Assign correction method
    corMeth <- MAYP112785_sun.mthd
  }
  
  if(SPCM == "MAYP112785"){
    desc[["bad.pixs"]] <- 88
  }
  
  ### Import files
  short_raw <- ooacquire::read_files2mspct(files = list(scan = short_path), descriptor = desc, verbose = FALSE)
  long_raw <- ooacquire::read_files2mspct(files = list(scan = long_path), descriptor = desc, verbose = FALSE)
  
  # convert to counts per seconds
  short_cps <- raw2corr_cps(short_raw, ref.pixs.range = corMeth[["inst.dark.pixs"]])
  long_cps <- raw2corr_cps(long_raw, ref.pixs.range = corMeth[["inst.dark.pixs"]])
  
  # Plot raw and cps
  # plot(counts ~ w.length, short_raw$scan, type = "l")
  # plot(counts ~ w.length, long_raw$scan, type = "l")
  # plot(cps ~ w.length, short_cps$scan, type = "l")
  # plot(cps ~ w.length, long_cps$scan, type = "l")

  # Find first NA in long scan
  wl250 <- min(which(long_cps$scan$w.length > 250))-1
  firstNA <- which(is.na(long_cps$scan[long_cps$scan$w.length > 250,"cps"]))[1]
  firstNA_wl <- long_cps$scan$w.length[firstNA+wl250]
  
  # Select CPS from 250nm to last recorded wavelength in long scan to apply correction on
  short_wb <- short_cps$scan[short_cps$scan$w.length > 250 & short_cps$scan$w.length < firstNA_wl, "cps"]
  long_wb <- long_cps$scan[long_cps$scan$w.length > 250 & long_cps$scan$w.length < firstNA_wl, "cps"]
  
  
  # Fit a second order polynomial equation (because difference between long and short is not linear for smaller wavelengths)
  #f <- nls(short_wb ~ a*long_wb^2 + b*long_wb + c, start = list(a = 1, b= 1, c = 1))
  func <- function(par){short_wb - (par[1]*long_wb^2 + par[2]*long_wb + par[3])}
  f <- nls.lm(par = c(1,1,1), fn = func)
  a = coef(f)[1]
  b = coef(f)[2]
  c = coef(f)[3]
  
  # If you want to see the fit
  if(plotit == TRUE){
    par(mfrow = c(1,2))
    plot(short_wb ~ long_wb, type = "p", pch = 20)
    abline(0,1, col = "black")
    curve(a*x^2 + b*x + c, from = 0, to = 60000, add = T, col = "red", lwd = 2)
  }
  
  ### Apply correction
  long_corrected <- long_cps
  long_corrected$scan$cps <- a*long_corrected$scan$cps^2 + b*long_corrected$scan$cps + c
  
  # To see correction
  if(plotit == TRUE){
    plot(-500, bty = "L", xaxt = "n", yaxt = "n", xlab = "Wavelength (nm)", ylab = "counts per second", xlim = c(250,firstNA_wl+50), ylim = c(0,max(c(short_cps$scan$cps, long_cps$scan$cps, long_corrected$cps), na.rm = T)))
    points(cps ~ w.length, data = short_cps$scan, type = "l", lwd = 4, col = "black")
    points(cps ~ w.length, data = long_cps$scan, type = "l", lwd = 1, col = "red")
    points(cps ~ w.length, data = long_corrected, type = "l", lwd = 2, col = "green4")
    axis(side = 1, font = 2)
    axis(side = 2, font = 2, las = 2)
    legend("topleft", bty = "n", legend = c("short scan", "long scan", "corrected long (scaled)"), col = c("black", "red", "green4"), lwd = 5)
  }
  
  ### Convert back into counts
  ref.wls.range <- unlist(short_raw$scan[range(2:4), "w.length"])
  baseline <- average_spct(trim_spct(spct = short_raw$scan, range = ref.wls.range, byref = FALSE))

  # Import short scan as template
  template <- read.table(short_path, colClasses = "character", fill = TRUE)
  
  counts <- as.numeric(template[18:2085,2])
  integration_time_s <- as.numeric(template[9,4]) / 1e6
  
  # Calculate new counts
  long_raw_corrected <- (long_corrected$cps * integration_time_s) + baseline
  new_counts <- c(counts[1:4], long_raw_corrected[5:(firstNA-1)], short_raw$scan$counts[firstNA:length(short_raw$scan$counts)])
  
  ### Write to file
  template[18:2085,2] <- round(new_counts,0)
  
  # Re-create and format dark and filter files
  SS <- apply(template, 1, paste, collapse = " ", recycle0 = TRUE)
  SS <- trimws(SS, which = "right")
  SS[18:2085] <- gsub(x = SS[18:2085], pattern = " ", replacement = "\t")

  # Write new dark and filter as temp files
  write.table(SS, out_path, quote = FALSE, row.names = FALSE, col.names = FALSE)
}

