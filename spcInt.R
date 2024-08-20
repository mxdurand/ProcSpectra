# Calculate waveband integrals and ratios
spcInt <- function(spc, list)
{
  calIntegrales <- function(spc)
  {
    wb_IR <- new_waveband(750,900)
    spcValues <- c(e_irrad(spc, PAR()) * 1e6,
                   e_irrad(spc, UVB()),
                   e_irrad(spc, UVA()),
                   e_irrad(spc, Blue("Sellaro")),
                   e_irrad(spc, Green("Sellaro")),
                   e_irrad(spc, Red("Sellaro")),
                   e_irrad(spc, Far_red("Sellaro")),
                   e_irrad(spc, GEN_G(300)), 
                   e_irrad(spc, GEN_T(300)), 
                   e_irrad(spc, PG(300)), 
                   e_irrad(spc, DNA_N(300)), 
                   e_irrad(spc, CIE(298)),
                   e_irrad(spc, FLAV(300)),
                   e_irrad(spc, wb_IR),
                   q_irrad(spc, PAR()) * 1e6, # PPFD
                   q_irrad(spc, UVB()) * 1e6,
                   q_irrad(spc, UVA()) * 1e6,
                   q_irrad(spc, Blue("Sellaro")) * 1e6,
                   q_irrad(spc, Green("Sellaro")) * 1e6,
                   q_irrad(spc, Red("Sellaro")) * 1e6,
                   q_irrad(spc, Far_red("Sellaro")) * 1e6,
                   q_irrad(spc, GEN_G(300)) * 1e6,
                   q_irrad(spc, GEN_T(300)) * 1e6,
                   q_irrad(spc, PG(300)) * 1e6,
                   q_irrad(spc, DNA_N(300)) * 1e6,
                   q_irrad(spc, CIE(298)) * 1e6,
                   q_irrad(spc, FLAV(300)) * 1e6,
                   q_irrad(spc, wb_IR) * 1e6,
                   q_ratio(spc, UVB(), UVA()) * 1000, 
                   q_ratio(spc, UVB(), PAR()) * 1000,
                   q_ratio(spc, UVA(), PAR()),
                   R_FR(spc, "Sellaro"), 
                   R_FR(spc, "Smith10"), 
                   R_FR(spc, "Smith20"), 
                   q_ratio(spc, Blue("Sellaro"), Green("Sellaro")),
                   q_ratio(spc, Blue("Sellaro"), Red("Sellaro")),
                   Pfr_Ptot(spc))
    return(spcValues)
  }
  
  intNames <- c("ePAR", "eUVB", "eUVA", "eBlue", "eGreen", "eRed", "eFRed", "eGEN.G", "eGEN.T", "ePG", "eDNA.N", "eCIE", "eFLAV", "eIR",
                "qPAR", "qUVB", "qUVA", "qBlue", "qGreen", "qRed", "qFRed", "qGEN.G", "qGEN.T", "qPG", "qDNA.N", "qCIE", "qFLAV", "qIR",
                "UVB.UVA", "UVB.PAR", "UVA.PAR", "RFR.sell", "RFR.smi10", "RFR.smi20", "B.G", "B.R", "PhyEq")
  
  if(!missing("spc")){
    intVals <- calIntegrales(spc)
    dfRES <- data.frame("var" = intNames, "spectra" = intVals)
    return(dfRES)
  }
  if(!missing("list")){
    mat <- sapply(list, calIntegrales)
    dfRES <- data.frame(intNames, mat)
    colnames(dfRES) <- c("var", paste("s.e.irrad", seq(1,length(list),1), sep = ""))
    rownames(dfRES) <- NULL
    return(dfRES)
  }
  if(!missing("spc") & !missing("list")){
    stop("Can't provide both a spectrum and a list of spectra", "\n")
  }
}
