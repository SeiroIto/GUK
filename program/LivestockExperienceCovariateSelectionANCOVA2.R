# Specifying "Time": Time dummies and Time-interaction covariates are dropped
excl.base <- "HadCow|Nar|Pure|Size|Poo|With|InK|Cash|Trad|Time"
excla.base <- "HadCow|Nar|Pure|Large$|Large[\\.G]|Small|Trad|yCow|Cows.C|Cattle$|Cattle\\.|yCatt|Poo|Witho|Cash|Time"
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
    "|TotalImputedValue0$",  
    "|^dummyAdiCattle0$",
    "|Head|Flood|HHs",
    # dummyOwnCattle is not added because of collinearity with NumCows0
    "|^dummyAdiCattle", 
   # a dummy estimation to be used to construct mean/std column
    "|^dummyAdiCattle"
  )
for (a in regsuffixes) {
  if (!grepl("T", a))
    assign(paste0("incl", a, 1), 
      "^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$") else 
  if (grepl("Ta?$", a))
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.") 
      # allow level covariates
	#      "^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.") else
      # allow level covariates but not level of OwnCattle0 nor LeaseInCattle0
       "^(?=dummy[CI].*[ed].*|^dummy[LW].*[cgz]e.*|^Time\\.)") else
   # if TP, TPa: add any variable with "Poor"
    assign(paste0("incl", a, 1), 
      # disallow level covariates
      #"^dummy[CI].*[ed]\\.T|^dummy[LW].*[cgz]e\\.T|^Time\\.|Poor") 
      # allow level covariates
      #"^dummy[CI].*[ed]|^dummy[LW].*[cgz]e|^Time\\.|Poor")
      # allow level covariates but not HadConw
      "^(?=dummy[CI].*[ed]|^dummy[LW].*[cgz]e.*|^Time\\.|^d.*Poor)")
  for (m in 2:length(additions)) 
      assign(paste0("incl", a, m), 
        paste0(get(paste0("incl", a, m-1)), additions[m])
      )
}
