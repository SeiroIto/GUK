excl.base <- "Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Cattle|Large$|Large[\\.G]|Small|Trad|yCow|Cows.C|Poo|Witho|Cash|Time"
exclP.base <- "Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|Time" 
#exclP.base <- "Pure|HadCow|Size|Poo|With|InK|Cash|Trad|Time" 
#exclPa.base <- "Pure|HadCow|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|Time" 
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
inclheader <- gsub("ex", "in", exclheader)
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
    NA, 
    "", # "|NumC.*0$" is NA if own=none, adi. Lead to error in linear hyp testing. Set to NA.
     # Deleted "|NumCows0$" as it is zero for Adi and None, which gives NA in estimate 
     # and causes an error in computing p values for linear hypothesis 
     # in glht function (multicomp package) when I construct confidence intervals for graphing.
    "|Head|Flood|HH|^NetVa.*0$",
    # NumCows0 is zero for Adi and None, so use narrow net asset value for controlig baseline difference
    #"|^dummyHadCows",
     # a dummy estimation to be used to construct mean/std column
   ""
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    # {Cattle/InKind/WithGrace/Large("",Size,Grace)} 
    # and {^dummyUltraPoor$}
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates
      "^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.")
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
