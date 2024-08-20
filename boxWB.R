### Add color bars with wavebands (250-1000nm)
boxWB <- function(prop = 0.05, lines = F)
{
  xLim <- par("usr")[1:2]
  yLim <- par("usr")[3:4]
  space <- c(diff(xLim)*0.01, diff(yLim)*prop)
  
  WB <- vector()
  UVc <- new_waveband(100,280) ; if(!all(UVc$low > xLim[2] | UVc$high < xLim[1])){WB <- append(WB, "UVc")}
  UVb <- new_waveband(280,315) ; if(!all(UVb$low > xLim[2] | UVb$high < xLim[1])){WB <- append(WB, "UVb")}
  UVa <- new_waveband(315,400) ; if(!all(UVa$low > xLim[2] | UVa$high < xLim[1])){WB <- append(WB, "UVa")}
  Vo <- new_waveband(400,455) ; if(!all(Vo$low > xLim[2] | Vo$high < xLim[1])){WB <- append(WB, "Vo")}
  Bl <- new_waveband(455,492) ; if(!all(Bl$low > xLim[2] | Bl$high < xLim[1])){WB <- append(WB, "Bl")}
  Gr <- new_waveband(492,577) ; if(!all(Gr$low > xLim[2] | Gr$high < xLim[1])){WB <- append(WB, "Gr")}
  Yl <- new_waveband(577,597) ; if(!all(Yl$low > xLim[2] | Yl$high < xLim[1])){WB <- append(WB, "Yl")}
  Or <- new_waveband(592,622) ; if(!all(Or$low > xLim[2] | Or$high < xLim[1])){WB <- append(WB, "Or")}
  Rd <- new_waveband(622,700) ; if(!all(Rd$low > xLim[2] | Rd$high < xLim[1])){WB <- append(WB, "Rd")}
  FR <- new_waveband(700,770) ; if(!all(FR$low > xLim[2] | FR$high < xLim[1])){WB <- append(WB, "FR")}
  NIR <- new_waveband(770,3000) ; if(!all(NIR$low > xLim[2] | NIR$high < xLim[1])){WB <- append(WB, "NIR")}
  MIR <- new_waveband(3000,50000) ; if(!all(MIR$low > xLim[2] | MIR$high < xLim[1])){WB <- append(WB, "MIR")}
  FIR <- new_waveband(50000,1e6) ; if(!all(FIR$low > xLim[2] | FIR$high < xLim[1])){WB <- append(WB, "FIR")}
  
  # All
  polygon(x = c(max(c(100,xLim[1]))+space[1], min(c(1000,xLim[2]))-space[1], min(c(1000,xLim[2]))-space[1], max(c(100,xLim[1])+space[1])), 
          y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
          border = "gray30", col = "black")
  
  # UV-C
  if(any(WB == "UVc")){
    polygon(x = c(max(c(100,xLim[1]))+space[1], 280, 280, max(c(100,xLim[1])+space[1])), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "black")
    if(lines){abline(v=100, col = "gray80");abline(v=280, col = "gray80")}
  }
  
  # UV-B
  if(any(WB == "UVb")){
    polygon(x = c(max(c(280,xLim[1]))+ifelse(any(WB == "UVc"), 0, space[1]), 315, 315, max(c(280,xLim[1])+ifelse(any(WB == "UVc"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "gray20")
    if(lines){abline(v=280, col = "gray80");abline(v=315, col = "gray80")}
  }
  
  # UV-A
  if(any(WB == "UVa")){
    polygon(x = c(max(c(315,xLim[1]))+ifelse(any(WB == "UVb"), 0, space[1]), 400, 400, max(c(315,xLim[1])+ifelse(any(WB == "UVb"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "darkorchid4")
    if(lines){abline(v=315, col = "gray80");abline(v=400, col = "gray80")}
  }
  
  # Violet
  if(any(WB == "Vo")){
    polygon(x = c(max(c(400,xLim[1]))+ifelse(any(WB == "UVa"), 0, space[1]), 455, 455, max(c(400,xLim[1])+ifelse(any(WB == "UVa"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "darkorchid")
    if(lines){abline(v=400, col = "gray80");abline(v=455, col = "gray80")}
  }
  
  # Blue
  if(any(WB == "Bl")){
    polygon(x = c(max(c(455,xLim[1]))+ifelse(any(WB == "Vo"), 0, space[1]), 492, 492, max(c(455,xLim[1])+ifelse(any(WB == "Vo"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "royalblue4")
    if(lines){abline(v=455, col = "gray80");abline(v=492, col = "gray80")}
  }
  
  # Green
  if(any(WB == "Gr")){
    polygon(x = c(max(c(492,xLim[1]))+ifelse(any(WB == "Bl"), 0, space[1]), 577, 577, max(c(492,xLim[1])+ifelse(any(WB == "Bl"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "green4")
    if(lines){abline(v=492, col = "gray80");abline(v=577, col = "gray80")}
  }
  
  # Yellow
  if(any(WB == "Yl")){
    polygon(x = c(max(c(577,xLim[1]))+ifelse(any(WB == "Gr"), 0, space[1]), 597, 597, max(c(577,xLim[1])+ifelse(any(WB == "Gr"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "goldenrod1")
    if(lines){abline(v=577, col = "gray80");abline(v=597, col = "gray80")}
  }
  
  # Orange
  if(any(WB == "Or")){
    polygon(x = c(max(c(597,xLim[1]))+ifelse(any(WB == "Yl"), 0, space[1]), 622, 622, max(c(597,xLim[1])+ifelse(any(WB == "Yl"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "chocolate1")
    if(lines){abline(v=597, col = "gray80");abline(v=622, col = "gray80")}
  }
  
  # Red
  if(any(WB == "Rd")){
    polygon(x = c(max(c(622,xLim[1]))+ifelse(any(WB == "Or"), 0, space[1]), 700, 700, max(c(622,xLim[1])+ifelse(any(WB == "Or"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "firebrick1")
    if(lines){abline(v=622, col = "gray80");abline(v=700, col = "gray80")}
  }
  
  # Far Red
  if(any(WB == "FR")){
    polygon(x = c(max(c(700,xLim[1]))+ifelse(any(WB == "Rd"), 0, space[1]), 770, 770, max(c(700,xLim[1])+ifelse(any(WB == "Rd"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "darkred")
    if(lines){abline(v=700, col = "gray80");abline(v=770, col = "gray80")}
  }
  
  # NIR
  if(any(WB == "NIR")){
    polygon(x = c(max(c(770,xLim[1]))+ifelse(any(WB == "FR"), 0, space[1]), ifelse(any(WB == "MIR"),3000,3000-space[1]), ifelse(any(WB == "MIR"),3000,3000-space[1]), max(c(770,xLim[1])+ifelse(any(WB == "FR"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "tomato4")
    if(lines){abline(v=770, col = "gray80");abline(v=3000, col = "gray80")}
  }
  
  # MIR
  if(any(WB == "MIR")){
    polygon(x = c(max(c(3000,xLim[1]))+ifelse(any(WB == "NIR"), 0, space[1]), 50000, 50000, max(c(3000,xLim[1])+ifelse(any(WB == "NIR"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "black")
    if(lines){abline(v=3000, col = "gray80");abline(v=50000, col = "gray80")}
  }
  
  # FIR
  if(any(WB == "FIR")){
    polygon(x = c(max(c(50000,xLim[1]))+ifelse(any(WB == "MIR"), 0, space[1]), 1e6-space[1], 1e6-space[1], max(c(50000,xLim[1])+ifelse(any(WB == "MIR"), 0, space[1]))), 
            y = c(yLim[2], yLim[2], yLim[2]-space[2], yLim[2]-space[2]), 
            border = "gray40", col = "black")
    if(lines){abline(v=50000, col = "gray80");abline(v=1e6, col = "gray80")}
  }
}
