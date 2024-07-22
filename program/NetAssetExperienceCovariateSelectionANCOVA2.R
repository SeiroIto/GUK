# Specifying "Time": Time dummies and Time-interaction covariates are dropped
excl.base <- "HadCow|Nar|Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "HadCow|Nar|Pure|Large$|Large[\\.G]|Small|Trad|yCow|Cows.C|Catt|Poo|Witho|Cash|Time"
exclP.base <- "HadCow|Nar|Pure|dummy[CMST]|Cattle$|Cattle\\.|Large$|Large\\.|[et]Grace|Cows.C|Time" 
# Specifying "Time.?2": Time2-interaction are dropped (collinear with intercept and level covariates in ANCOVA)
# Changing "Time" => "^Time$": Only time dummies (level) are dropped
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
exclTP.base <- "Pure|UD|Mod|Siz|Wi|InK|Cash|Tra|Time.?2|^Time$"
exclTPa.base <- paste0("Time.?2|", gsub("Time", "^Time$", exclP.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
    NA,
    "|NetValue0$",  # Net2Value is net asset value based on TotalImputed2Value or Impute2Value,
    # ImputedValue: Use fixed prices for all rounds for cow value imputation
    # Imputed2Value: Cow prices vary a lot by years, so use annual prices for cows
    "|Head|Flood|HHs",
    "|^dummyOwnCattle|^dummyAdiCattle",
   # a dummy estimation to be used to construct mean/std column
    "|^dummyOwnCattle|^dummyAdiCattle"
  )
for (a in regsuffixes) {
  assign(paste0("incl", a, 1), 
    if (!grepl("T", a))
      "^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$|^dummyOwnCattle0$|^dummyAdiCattle0$" else 
    if (grepl("Ta?$", a))
      # allow level covariates but not level of OwnCattle0 nor LeaseInCattle0
      "^(?=dummy[CI].*[ed].*|^dummy[LW].*[cgz]e.*|^dummy.*tle0$|^Time\\.)" else
      # if TP, TPa: add any variable with "Poor"
      # allow level covariates including dummyOwnCattle0
      "^(?=dummy[CI].*[ed]|^dummy[LW].*[cgz]e.*|^Time\\.|^dummy.*tle0$|^d.*Poor)"
    )
  for (m in 2:length(additions)) 
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
