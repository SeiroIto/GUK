excl.base <- "Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Large$|Large[\\.G]|Small|Trad|yCow|Poo|Witho|Cash|Time"
exclP.base <- "dummy[CMST]|dummyLarge$|[et]Grace|Time" 
exclT.base <- paste0("^dummyH.*[a-z]$|Control$|Time.?2|", 
  gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("^dummyH.*[a-z]$|Control$|Time.?2|", 
  gsub("Time", "^Time$", excla.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
  NA,
  "|HAss.*0$", 
  "|Head|Flood|HH",
  NA,
  "|PAss.*0$",
  "|Head|Flood|HH",
   # a dummy estimation to be used to construct mean/std column
  "|HAss.*0$"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[wd]$|^dummy[LW].*[cgz]e$|dummy.*Poor$|Pure") else 
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[wd]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.|Pure") 
      # allow level covariates
      "^dummy[CI].*[wd]|^dummy[LW].*[cgz]e|^Time\\.|Pure")
  for (m in 2:length(additions)) 
    if (m == 4) # if m==4, copy inclX1
      assign(paste0("incl", a, 4), get(paste0("incl", a, 1))) else
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
