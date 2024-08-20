### Calculating all BSWF and return data frame. Not written by me.
BSWF.irrad.calc<-function(irrad.spct)
{
  #'BSWF.irrad.calc' is a function
  #spectral_irrad.data is a 'data frame' object with two columns: 'wavelength' and 'irradiance'
  #obtained with routine 'process_maya_files' from MayaCalc' package
  #'BSWF.irrad.calc' returns a 'data frame' object with two columns: 'spectra' and 'value'
  #
  # I would add this subset to avoid problems with noise in spectra for sunlight
  # For lamps with acetate 285 or 290 should be the right wavelength
  # 
  #  sub_spectral_irrad.data <- subset(spectral_irrad.data, wavelength >= 295)
  #  attach(sub_spectral_irrad.data)
  wb_UVb <- new_waveband(300,350) # b refers to below 350 nm ##why 315nm Titta?
  wb_UVa <- new_waveband(350,400) # a refers to above 350 nm
  wb_IR <- new_waveband(750,900)
  
  value <- c(
    e_irrad(irrad.spct, PAR()),
    e_irrad(irrad.spct, UVB()),
    e_irrad(irrad.spct, UVA()),
    e_irrad(irrad.spct, wb_UVb),
    e_irrad(irrad.spct, wb_UVa),
    e_irrad(irrad.spct, Blue("Sellaro")),
    e_irrad(irrad.spct, Green("Sellaro")),
    e_irrad(irrad.spct, Red("Sellaro")),
    e_irrad(irrad.spct, Far_red("Sellaro")),
    e_irrad(irrad.spct, GEN_G(300)), # default normaliztion is at 300 nm, so "300" could be omitted
    e_irrad(irrad.spct, GEN_T(300)), # default is 300 nm, but putting it as argument could help later
    e_irrad(irrad.spct, PG(300)), # default is 300 nm, so "300" could be omitted
    e_irrad(irrad.spct, DNA_N(300)), # default is now 300 nm, but may change to what Setlow used
    e_irrad(irrad.spct, CIE(298)), # default here is 298 nm which should be fine
    e_irrad(irrad.spct, FLAV(300)),  # default is 300 nm
    e_irrad(irrad.spct, wb_IR),
    q_irrad(irrad.spct, PAR()) * 1e6,#ppfd
    q_irrad(irrad.spct, UVB()) * 1e6,
    q_irrad(irrad.spct, UVA()) * 1e6,
    q_irrad(irrad.spct, wb_UVb)* 1e6,
    q_irrad(irrad.spct, wb_UVa)* 1e6,
    q_irrad(irrad.spct, Blue("Sellaro"))* 1e6,
    q_irrad(irrad.spct, Green("Sellaro"))* 1e6,
    q_irrad(irrad.spct, Red("Sellaro"))* 1e6,
    q_irrad(irrad.spct, Far_red("Sellaro"))* 1e6,
    q_irrad(irrad.spct, GEN_G(300))* 1e6, # default normaliztion is at 300 nm, so "300" could be omitted
    q_irrad(irrad.spct, GEN_T(300))* 1e6, # default is 300 nm, but putting it as argument could help later
    q_irrad(irrad.spct, PG(300))* 1e6, # default is 300 nm, so "300" could be omitted
    q_irrad(irrad.spct, DNA_N(300))* 1e6, # default is now 300 nm, but may change to what Setlow used
    q_irrad(irrad.spct, CIE(298))* 1e6, # default here is 298 nm which should be fine
    q_irrad(irrad.spct, FLAV(300))* 1e6,  # default is 300 nm    
    q_irrad(irrad.spct, wb_IR)* 1e6,
    q_ratio(irrad.spct, UVB(), UVA())*1000, 
    q_ratio(irrad.spct, UVB(), PAR()) * 1000,
    q_ratio(irrad.spct, UVA(), PAR()),
    R_FR(irrad.spct, "Sellaro"), 
    R_FR(irrad.spct, "Smith10"), 
    R_FR(irrad.spct, "Smith20"), 
    B_G(irrad.spct),
    q_ratio(irrad.spct, Blue("Sellaro"), Red("Sellaro")),
    Pfr_Ptot(irrad.spct))
  
  spectra <- c("PAR_e", "UVB_e", "UVA_e","UVb350_e","UVa350_e",
               "Blue_e","Green_e","Red_e","Far_red_e",
               "GEN_G_e", "GEN_T_e", "PG_e", "DNA_N_e", "CIE_e","FLAV_e","Infra_red_e",  
               "PAR_q","UVB_q", "UVA_q","UVb350_q","UVa350_q",
               "Blue_q","Green_q","Red_q","Far_red_q",
               "GEN_G_q", "GEN_T_q", "PG_q", "DNA_N_q", "CIE_q","FLAV_q", "Infra_red_q",
               "UVB_UVA", "UVB_PAR", "UVA_PAR", "R_FR_Sellaro","R_FR_Smith10","R_FR_Smith20",
               "B_G","B_R","PhyEqi")
  spectral_irrad_BSWF<-data.frame(spectra=spectra,value=round(value,10))
  #detach(irrad.dataspct)
  
  return(spectral_irrad_BSWF)
}
