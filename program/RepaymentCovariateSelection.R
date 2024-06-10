excl.base <- "Poor|Size|Trad|With|Cash|InKind|survey|^val|^Excess|^Eff.*R.*nt$|dum.*Tim|^Arm$|BSta|Time"
excla.base <- "^credit|dummy[TC]|Large$|Large\\.|LargeG|Witho|Poor|Small|^Cum|survey|^val|^Excess|^Eff.*R.*nt$|dum.*Tim|^Arm$|BSta|Time"
exclg.base <- "dummy[TLC]|Poo|Size|Witho|InKind|survey|^val|^Excess|^Eff.*R.*nt$|dum.*Tim|^Arm$|BSta|Time"
exclp.base <- "With|Size|dummy[TLC]|Mode|InKind|survey|^val|^Excess|^Eff.*R.*nt$|dum.*Tim|^Arm$|BSta|Time"
excls.base <- "Poo|Large$|Large\\.|dummy[TC]|Gra|Smal|InKind|^val|^Excess|^Eff.*R.*nt$|dum.*Tim|^Arm$|BSta|Time"
for (jj in c("", "a", "g", "p", "s"))
{
  assign(paste0("excl", jj, 1), "^CumRepa|CumEx|Floo|RM|Eff|Head|Tim|LY")
  assign(paste0("excl", jj, 2), "^CumRepa|CumEx|RM|Eff|Head|Flood")
  assign(paste0("excl", jj, 3), "^CumNetSav|CumEx|Floo|RM|Eff|Head|Tim|LY")
  assign(paste0("excl", jj, 4), "^CumNetSav|CumEx|RM|Eff|Head|Flood")
  assign(paste0("excl", jj, 5), "^CumNetSav|CumEx|Eff")
  assign(paste0("excl", jj, 6), "^CumRepa|^CumNetSav|CumEx|^Eff.*R.*nt$|Floo|RM|Head|Tim|LY")
  assign(paste0("excl", jj, 7), "^CumRepa|^CumNetSav|CumEx|^Eff.*R.*nt$|RM|Head|Flood")
  assign(paste0("excl", jj, 8), "^CumRepa|^CumNetSav|CumEx|^Eff.*R.*nt$")
  assign(paste0("excl", jj, 9), "^CumRepa|^CumNetSav|Eff|Floo|RM|Head|Tim|LY")
  assign(paste0("excl", jj, 10), "^CumRepa|^CumNetSav|Eff|RM|Head|Flood")
  assign(paste0("excl", jj, 11), "^CumRepa|^CumNetSav|Eff")
}
