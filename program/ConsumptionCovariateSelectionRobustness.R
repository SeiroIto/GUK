excl.base <- "dummy[TLC]|With|Size|Poor|HH|Exp|Trad|Time.?3|^Arm$|BSta"
exclg.base <- "dummy[TLC]|Poor|Size|HH|Exp|Witho|Time.?3|^Arm$|BSta"
exclp.base <- "dummy[TLC]|With|Size|HH|Exp|Mode|Time.?3|^Arm$|BSta"
excls.base <- "dummy[TLC]|Large$|Large\\.T|LargeG|With|Poor|HH|Exp|Small|Time.?3|^Arm$|BSta"
excla.base <- "^credit|dummy[TC]|Large$|Large\\.|LargeG|Time.?3|Witho|Poor|Small|^Arm$|BSta"
for (a in lbsuffixes) {
  if (a != "s") {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|Hygi|Tim") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head|Hygi")
    assign(paste0("excl", a, 3), "RM|Eff|Hygi")
    assign(paste0("excl", a, 4), "Eff|Hygi")
    assign(paste0("excl", a, 5), "Floo|RM|Eff|Head|Tim|PCE")
    assign(paste0("excl", a, 6), "Floo|RM|Eff|Head|PCE")
    assign(paste0("excl", a, 7), "PCE")
  } else if (a == "s") {
    assign(paste0("excl", a, 1), "Floo|RM|Head|Hygi|Tim")
    assign(paste0("excl", a, 2), "Floo|RM|Head|Hygi")
    assign(paste0("excl", a, 3), "RM|Hygi")
    assign(paste0("excl", a, 4), "Hygi")
    assign(paste0("excl", a, 5), "Floo|RM|Head|Time|PCE")
    assign(paste0("excl", a, 6), "RM|PCE")
    assign(paste0("excl", a, 7), "PCE")
  }
}
