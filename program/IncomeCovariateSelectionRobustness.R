excl.base <- "Time.?2|Poor|Size|With|Trad|InKind|BSta"
exclg.base <- "dummy[TLC]|Time.?2|Poor|Size|Witho|InKind|BSta"
exclp.base <- "dummy[TLC]|Time.?2|With|Size|Mode|InKind|BSta"
excls.base <- "dummy[TC]|Large$|Large\\.|LargeG|Time.?2|With|Poor|Small|InKind|BSta"
for (a in lbsuffixes) {
  if (a != "s") {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|Tim") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head")
    assign(paste0("excl", a, 3), "RM|Eff")
    assign(paste0("excl", a, 4), "Eff")
    assign(paste0("excl", a, 5), "Floo|RM|Eff|Head|Tim")
    assign(paste0("excl", a, 6), "Floo|RM|Eff|Head")
    assign(paste0("excl", a, 7), "DUMMY")
  } else if (a == "s") {
    assign(paste0("excl", a, 1), "Floo|RM|Time|Head")
    assign(paste0("excl", a, 2), "Floo|RM|Head")
    assign(paste0("excl", a, 3), "RM")
    assign(paste0("excl", a, 4), "DUMMY")
    assign(paste0("excl", a, 5), "Floo|RM|Head|Time")
    assign(paste0("excl", a, 6), "RM")
    assign(paste0("excl", a, 7), "DUMMY")
  }
}
