excl.base <- "Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Large$|Large[\\.G]|Small|Trad|yCow|Poo|Witho|Cash|Time"
exclP.base <- "Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Time" 
exclT.base <- paste0("^dummyH.*[a-z]$|Time.?2|", 
  gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("^dummyH.*[a-z]$|Time.?2|", 
  gsub("Time", "^Time$", excla.base))
remove(list = ls(pattern = "^incl.?\\d"))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
  NA,
  "|TotalRevenue0$",
  "|Head.*0|Flood|HH.*0"
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
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m]))
}
