excl.base <- "Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Large$|Large[\\.G]|Small|Trad|yCat|Cows.C|Poo|Witho|Catt|Cash|Time"
exclP.base <- "Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|Time" 
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
exclTP.base <- "Pure|UD|Mod|Siz|Wi|InK|Cash|Tra|Time.?2|^Time$"
exclTPa.base <- paste0("Time.?2|", gsub("Time", "^Time$", exclP.base))
remove(list = ls(pattern = "^incl.?\\d"))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <- c(
  NA,
  "|TotalHHLabourIncome0$", 
  "|Head.*0|Flood|HHs.*0",
  "|^dummyHadCows",
  "|NumCows0",
  # a dummy estimation to be used to construct mean/std column
  "|^dummyHadCows"
  )
for (a in regsuffixes) {
  # covariate base: incl"X"1
  assign(paste0("incl", a, 1), 
    if (!grepl("T", a))
    # {Cow/InKind/WithGrace/Large("",Size,Grace)} 
    # and {^dummyUltraPoor$}
      "^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$" else 
    if (grepl("Ta?$", a))
      "^(?=dummy[CI].*[ed].*|^dummy[LW].*[cgz]e.*|^Time\\.)" else
      "^(?=dummy[CI].*[ed]|^dummy[LW].*[cgz]e.*|^Time\\.|dummyUltraPoor)"
    )
  # covariates: incl"X"2, ..., incl"X"6
  for (m in 2:length(additions)) 
    if (m == 5) # if m==5, addo to inclX3
      assign(paste0("incl", a, 5), 
        paste0(get(paste0("incl", a, m-2)), additions[m])
      ) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
