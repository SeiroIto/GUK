excl.base <- "Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Large$|Large[\\.G]|Small|Trad|yCow|Cows.C|Poo|Witho|Cash|Time"
exclP.base <- "Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|Time" 
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
exclTP.base <- "Pure|UD|Mod|Siz|Wi|InK|Ca|Tra|Time.?2|^Time$"
exclTPa.base <- paste0("Time.?2|", gsub("Time", "^Time$", exclP.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
    NA,
    "|^NetNL.*0$",
    "|Head|Flood|HHs",
    "|^dummyHadCows",
    "|NumCows0",
   # a dummy estimation to be used to construct mean/std column
    "|^dummyHadCows"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[wd]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
  if (grepl("Ta?$", a))
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[wd]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates
	#      "^dummy[CI].*[wd]|^dummy[LW].*[cgz]e|^Time\\.") else
      # allow level covariates but not HadCows
       "^(?=dummy[CI].*[wd].*|^dummy[LW].*[cgz]e.*|^Time\\.)(?!.*Had)") else
   # if TP, TPa: add any variable with "Poor"
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[wd]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.|Poor") 
      # allow level covariates
      #"^dummy[CI].*[wd]|^dummy[LW].*[cgz]e|^Time\\.|Poor")
      # allow level covariates but not HadConw
      "^(?=dummy[CI].*[wd]|^dummy[LW].*[cgz]e.*|^Time\\.|Poor)(?!.*Had)")
  for (m in 2:length(additions)) 
    if (m == 5) # if m==5, addo to inclX3
      assign(paste0("incl", a, 5), 
        paste0(get(paste0("incl", a, m-2)), additions[m])
      ) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
