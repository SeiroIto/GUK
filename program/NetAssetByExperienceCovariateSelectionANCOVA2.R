# Specifying "Time": Time dummies and Time-interaction covariates are dropped
excl.base <- "HadCow|^Nar.*t2V|Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "HadCow|Cattle|^Nar.*t2V|Pure|Large$|Large[\\.G]|Small|Trad|yCow|Cows.C|Poo|Witho|Cash|Time"
exclP.base <- "HadCow|^Nar.*t2V|Pure|dummy[CMST]|Cash|Large$|Large\\.|[et]Grace|Cows.C|Time" 
# Specifying "Time.?2": Time2-interaction are dropped (collinear with intercept and level covariates in ANCOVA)
# Changing "Time" => "^Time$": Only time dummies (level) are dropped
exclT.base <- paste0("Time.?2|", gsub("Time", "^Time$", excl.base))
exclTa.base <- paste0("Time.?2|", gsub("Time", "^Time$", excla.base))
exclTP.base <- "Pure|UD|Mod|Siz|Wi|InK|Cash|Cattle|Tra|Time.?2|^Time$"
exclTPa.base <- paste0("Time.?2|", gsub("Time", "^Time$", exclP.base))
# additions are common additional covariates after inclX1 (starts from m = 2)
additions <-     c(
    NA,
    "|NetValue0$",  # Net2Value is net asset value based on TotalImputed2Value or Impute2Value,
    # ImputedValue: Use fixed prices for all rounds for cow value imputation
    # Imputed2Value: Cow prices vary a lot by years, so use annual prices for cows
    "|Head|Flood|HHs",
    # dummy specification for computing mean/std
    "|Head|Flood|HHs"
  )
for (a in regsuffixes) {
  assign(paste0("incl", a, 1), 
    if (!grepl("T", a))
      "^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$" else 
    if (grepl("Ta?$", a))
      # allow level covariates but not level of OwnCattle0 nor LeaseInCattle0
      "^(?=dummy[CI].*[ed].*|^dummy[LW].*[cgz]e.*|^Time\\.)" else
      # if TP, TPa: add any variable with "Poor"
      # allow level covariates but not HadConw
      "^(?=dummy[CI].*[ed]|^dummy[LW].*[cgz]e.*|^Time\\.|Poor)"
    )
  for (m in 2:length(additions)) 
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
