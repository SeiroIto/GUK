excl.base <- "Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "Pure|Large$|Large[\\.G]|Small|Trad|yCow|Cows.C|Poo|Witho|Cash|Time"
exclP.base <- "Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|Time" 
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
   NA,
    "|TotalValue0$",
    "|Head|Flood|HH",
    "|HadCows"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[wd]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[wd]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates
      "^dummy[CI].*[wd]|^dummy[LW].*[cgz]e|^Time\\.") 
  for (m in 2:length(additions)) 
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
