common.excl <- "^Cum|UD|LYear|Pure|Tra|^cred|survey|^val|^Excess|dum.*Tim|^.Arm|Used|Time"
excl.base <- paste0("Size|Poo|With|InK|Cash|Trad|", common.excl)
excla.base <- paste0("dummy[TC]|rge$|rge[\\.G]|Witho|Poor|Small|Cows.C|",
  common.excl)
exclP.base <- paste0("dummy[CMST]|rge$|rge[\\.G]|Witho|Cows.C",
  common.excl)
exclT.base <- paste0("LY2|", excl.base)
exclTa.base <- paste0("LY2|", excla.base)
exclTP.base <- paste0("LY2|", exclP.base)
exclTPa.base <- paste0("LY2|", exclP.base)
# additions are common additional covariates after inclX1
additions <-     c(
    NA,
    "|^LY", # shows evolution of repayment without baseline outcome as a covariate
    "|^NetSaving0$",
    "|^LY",
    "|Head.*0|Flood|HHs.*0",
    NA,
    "|^LY",
    "|^Repaid0$",
    "|^LY",
    "|Head.*0|Flood|HHs.*0",
    "|^NetSaving0$", #added for regressions only of net saving, repayment
    NA,
    "|^LY",
    "|^EffectiveRepayment0$",
    "|^LY",
    "|Head.*0|Flood|HHs.*0",
    "|^NetSaving0$|^Repaid0$"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
  if (grepl("Ta?$", a))
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates
	#      "^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.") else
      # allow level covariates but not HadCows
       "^(?=dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$)(?!.*Had)") else
   # if TP, TPa: add any variable with "Poor"
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.|Poor") 
      # allow level covariates
      #"^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.|Poor")
      # allow level covariates but not HadConw
      "(?=^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|U.*Poor)(?!.*Had)")
  for (m in 2:length(additions)) 
    if (m == 3 | m == 8) # if m==3, 9, use inclX1
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, 1)), additions[m])) else
    #if (m == 6 | m == 11) # if m== 6,  11, use inclX1: marked off for regressions only of net saving, repayment
    if (m == 6) # if m== 6, use inclX1
      assign(paste0("incl", a, m), get(paste0("incl", a, 1))) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
