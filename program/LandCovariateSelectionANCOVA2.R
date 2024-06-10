# exluding strings: variables with the following strings will be dropped
excl.base <- "Pure|^HA|^PA|UD|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|^HA|^PA|UD|Large$|Large[\\.G]|Small|Trad|yCow|Catt|Poo|Witho|Cash|Time"
exclP.base <- "Pure|^HA|^PA|UD|dummy[CMST]|yCow|Large$|Large\\.|[et]Grace|Time" 
exclT.base <- paste0("^dummyH.*[a-z]$|Time.?2|", 
  gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("^dummyH.*[a-z]$|Time.?2|", 
  gsub("Time", "^Time$", excla.base))
exclTP.base <- "Pure|UD|^dummyH.*[a-z]$|yCow|Mod|Siz|Wi|InK|Cash|Tra|Time.?2|^Time$"
exclTPa.base <- paste0("^dummyH.*[a-z]$|Time.?2|", 
  gsub("Time", "^Time$", exclP.base))
# additions: common additional covariates after inclX1 (starts from m = 2)
additions <- c(
  NA,  # this is a dummy slot for inclX1 (for HAssetValue)
  "|Am.*Fi.*0$", 
  "|Head|Flood|HH",
   "|^dummyHadCows",
   "|NumCows0",
  # a dummy estimation to be used to construct mean/std column
   "|^dummyHadCows"
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
      "^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.") else
   # if TP, TPa: add any variable with "Poor"
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.|Poor") 
      # allow level covariates
      "^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.|dummy.*Poor\\.?")
  for (m in 2:length(additions)) 
    if (m == 5) # if m==5, addo to inclX3
      assign(paste0("incl", a, 5), 
        paste0(get(paste0("incl", a, m-2)), additions[m])
      ) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
    #if (m == 4) # if m==4, copy inclX1
    #  assign(paste0("incl", a, 4), get(paste0("incl", a, 1))) else
    #  assign(paste0("incl", a, m), 
    #    paste0(get(paste0("incl", a, m-1)), additions[m])
    #  )
}
