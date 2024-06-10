excl.base <- "Pure|Cattle|HadCow|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Cattle|HadCow|Large$|Large[\\.G]|Small|Trad|yCow|Cows.C|Poo|Witho|Cash|Time"
exclP.base <- "Pure|Cattle|HadCow|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|Time" 
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
inclheader <- gsub("ex", "in", exclheader)
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
    NA,
    "|NumCows0$",
    "|Head|Flood|HH",
    # NumCows0 is zero for Adi and None, so use narrow net asset value for controlig baseline difference
    "|Narrow.*0$",
     # a dummy estimation to be used to construct mean/std column
   "|Narro.*0$"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    # {Cow/InKind/WithGrace/Large("",Size,Grace)} 
    # and {^dummyUltraPoor$}
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[wd]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[wd]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates
      "^dummy[CI].*[wd]|^dummy[LW].*[cgz]e|^Time\\.")
  for (m in 2:length(additions)) 
   # drop m == 5 because TotalImputedValue0 and NumCows0 are collinear
   # if (m >= 5) # if m==5 or 6, add to inclX3 or inclX4
   #   assign(paste0("incl", a, m), 
   #     paste0(get(paste0("incl", a, m-2)), additions[m])
   #   ) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
