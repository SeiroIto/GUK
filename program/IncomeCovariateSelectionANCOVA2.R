excl.base <- "Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Large$|Large[\\.G]|Small|Trad|yCow|Poo|Witho|Cash|Time"
exclP.base <- "Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Time" 
exclT.base <- paste0("^dummyH.*[a-z]$|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("^dummyH.*[a-z]$|", gsub("Time", "^Time$", excla.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
  NA,
  "|TotalHHLabourIncome0$", 
  "|Head.*0|Flood|HH.*0",
  NA,
  "|TotalRevenue0$",
  "|Head.*0|Flood|HH.*0",
  "|TotalRevenue0$"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    # {Cow/InKind/WithGrace/Large("",Size,Grace)} 
    # and {^dummyUltraPoor$}
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[wd]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[wd]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
  for (m in 2:length(additions)) 
    if (m == 4) # if m==4, copy inclX1
      assign(paste0("incl", a, 4), get(paste0("incl", a, 1))) else
    if (m == 7) # if m==7, add to inclX3
      assign(paste0("incl", a, 7), 
        paste0(get(paste0("incl", a, 3)), additions[m])) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m]))
}
