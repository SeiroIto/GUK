excl.base <- "Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Large$|Large[\\.G]|Small|Trad|yCow|Poo|Witho|Cattle|Cash|Cows.C|Time"
exclP.base <- "Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|Time" 
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
  NA,
  "|TotalIm.*0", 
  "|Head|Flood|HH",
  "|^dummyHadCows",
  # NumCows is collinear with TotalImputedValue0
  #"|NumCows0",
   # a dummy estimation to be used to construct mean/std column
  "|^dummyHadCows"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
    assign(paste0("incl", a, 1), 
        # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates
      "^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.")
  for (m in 2:length(additions)) 
    # Now I commented out NumCows0, I drop if m == 5
    #if (m == 5) # if m==5, add to inclX3
    #  assign(paste0("incl", a, m), 
     #    paste0(get(paste0("incl", a, m-2)), additions[m])
     # ) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
