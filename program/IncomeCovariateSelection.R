excl.base <- "Time.?2|Poor|Size|With|Trad|InKind|^Arm$|BSta"
exclg.base <- "dummy[TLC]|Time.?2|Poor|Size|Witho|InKind|^Arm$|BSta"
exclp.base <- "dummy[TLC]|Time.?2|With|Size|Mode|InKind|^Arm$|BSta"
excls.base <- "dummy[TC]|Large$|Large\\.|LargeG|Time.?2|With|Poor|Small|InKind|^Arm$|BSta"
excla.base <- "dummy[TC]|Large$|Large\\.|LargeG|Time.?2|Witho|Poor|Small|^Arm$|BSta"
for (a in lbsuffixes) {
  if (a != "s") {
    assign(paste0("excl", a, 1), "hhid|Floo|RM|Eff|Head|Tim") 
    assign(paste0("excl", a, 2), "hhid|Floo|RM|Eff|Head")
    assign(paste0("excl", a, 3), "hhid|RM|Eff")
    assign(paste0("excl", a, 4), "hhid|Eff")
    assign(paste0("excl", a, 5), "HMid|Floo|RM|Eff|Head|Tim")
    assign(paste0("excl", a, 6), "HMid|Floo|RM|Eff|Head")
    assign(paste0("excl", a, 7), "HMid|DUMMY")
  } else if (a == "s") {
    assign(paste0("excl", a, 1), "hhid|Floo|RM|Time|Head")
    assign(paste0("excl", a, 2), "hhid|Floo|RM|Head")
    assign(paste0("excl", a, 3), "hhid|RM")
    assign(paste0("excl", a, 4), "hhid|DUMMY")
    assign(paste0("excl", a, 5), "HMid|Floo|RM|Head|Time")
    assign(paste0("excl", a, 6), "HMid|RM")
    assign(paste0("excl", a, 7), "HMid|DUMMY")
  }
}
dlabd[, Tee := NULL]
dfard[, Tee := NULL]
