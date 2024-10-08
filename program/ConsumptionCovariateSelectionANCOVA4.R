excl.base <- "Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Large$|Large[\\.G]|Small|Trad|yCow|Poo|Witho|Cash|Cattle|Time"
exclP.base <- "Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Time" 
# Drop Time2 (baseline) and Time3 (reference period).
exclT.base <- paste0("^dummyH.*[a-z]$|Time.?[23]|", 
  gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("^dummyH.*[a-z]$|Time.?[23]|", 
  gsub("Time", "^Time$", excla.base))
exclTP.base <- "Pure|UD|Mod|Siz|Wi|InK|Cash|Tra|Time.?[23]|^Time$"
exclTPa.base <- paste0("Time.?[23]|", 
  gsub("Time", "^Time$", exclP.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
    NA,
    "|PCExpenditure0$",
    "|Head.*0|Flood|HH.*0",
    NA,
    "|TotalExpenditure0$",
    "|Head.*0|Flood|HH.*0",
    "|TotalExpenditure0$"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    # {Cow/InKind/WithGrace/Large("",Size,Grace)} 
    # and {^dummyUltraPoor$}
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
  if (grepl("Ta?$", a))
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates but not HadConw
       "^(?=dummy[CI].*[ed].*|^dummy[LW].*[cgz]e.*|^Time\\.)(?!.*Had)") else
   # if TP, TPa: add any variable with "Poor"
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.|Poor") 
      # allow level covariates
      #"^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.|Poor")
      # allow level covariates but not HadConw
      "^(?=dummy[CI].*[ed]|^dummy[LW].*[cgz]e.*|^Time\\.|dummyUltraPoor)(?!.*Had)")
  for (m in 2:length(additions)) 
    if (m == 4) # if m==4, copy inclX1
      assign(paste0("incl", a, 4), get(paste0("incl", a, 1))) else
    if (m == 7) # if m==7, add to inclX3
      assign(paste0("incl", a, 7), 
        paste0(get(paste0("incl", a, 3)), additions[m])) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
