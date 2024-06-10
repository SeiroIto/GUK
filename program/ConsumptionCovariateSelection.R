excl.base <- "PureCon|With|Size|Poor|HH|Exp|Trad|Time.?3|InKind|^Arm$|BSta"
exclg.base <- "PureCon|dummy[TLC]|Poor|Size|HH|Exp|Witho|Time.?3|InKind|^Arm$|BSta"
exclp.base <- "PureCon|dummy[TLC]|With|Size|HH|Exp|Mode|Time.?3|InKind|^Arm$|BSta"
excls.base <- "PureCon|dummy[TC]|Large$|Large\\.T|LargeG|With|Poor|HH|Exp|Small|Time.?3|InKind|^Arm$|BSta"
excla.base <- "^PureCo|^credit|dummy[TC]|Large$|Large\\.|LargeG|Time.?3|Witho|Poor|Small|HHs|^Arm$|BSta"
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
