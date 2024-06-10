# Use elements of each dummy variables as names to exclude them 
# (makeDummyFromFactors gives such variable names)
excl.base <- "Pure|UD|o8|Size|Poo|With|InK|Cash|Trad|Time|^AgeCom"
excla.base <- "Pure|UD|o8|Large$|Large[\\.G]|Small|Trad|yCow|Cattle|Poo|Witho|Cash|Time"
exclP.base <- "Pure|UD|o8|dummy[CMST]|Large\\.|Large$|[et]Grace|Time" 
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <- c(
    NA,
    "|Enrolled0$",
    # add {dummyJunior/dummyHigh} and
    # {Arms}*{dummyJunior/dummyHigh} or
    # {Arms}*{dummyJunior/dummyHigh}*{Time.x}
    "|^dummy[JH].*[rh]$|^dummy[CI].*[ed]\\.dummy[JH].*[rh]$|^dummy[LW].*[cgz]e\\.dummy[JH].*[rh]$|^dummy[JH].*[rh]\\.T|^dummy[CI].*[ed]\\.dummy[JH].*[rh]\\.T|^dummy[LW].*[cgz]e\\.dummy[JH].*[rh]\\.T",
    "|ChildAge|Eldest|Head.*0|HHsize0|Flood",
    # add {Arms}*{dummyJunior/dummyHigh}*{Female}
    # {Arms}*{dummyJunior/dummyHigh}*{Female}*{Time.x}
    "|Female",
    "|ChildAge|Eldest|Head.*0|HHsize0|Flood"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    # {Cow/InKind/WithGrace/Large("",Size,Grace)} and {Junior/High}
    # and {^dummyUltraPoor$}
    assign(paste0("incl", a, 1), 
      "^(?=dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$)(?!.*Female)") else 
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates
      "^(?=dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^dummy|^Time\\.)(?!.*Female)")
  for (m in 2:length(additions)) 
     if (m == 5) assign(paste0("incl", a, m), 
       paste0(get(paste0("incl", a, 3)), additions[m])
     ) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}

