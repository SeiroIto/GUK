excl.base <- "^dummy[FTS]|With|Size|Poo|Trad|creditst|NumCows$|InKind|^Arm$|BSta"
exclG.base <- "^dummy[TLCS]|Poo|.Size|Witho|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
exclP.base <- "^dummy[FTLCS]|With|.Size|Mode|creditst|NumCows$|HadCows|InKind|^Arm$|BSta" 
exclS.base <- "^dummy[FTC]|Large\\.|Large$|Poo|With|Grace|Small|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
exclT.base <- "Poor|Size|With|Trad|^dummyT|\\..*Pri|Pri..*\\.|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
exclTG.base <- "dummy[TLC]|Size|Withou|Poo|HHM|RM|\\..*Pri|Pri..*\\.|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
exclTS.base <- "dummy[TC]|Large\\.|Large$|Gra|With|Poo|HHM|RM|\\..*Pri|Pri..*\\.|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
exclD.base <- "With|.Size|Poo|Trad|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
exclDG.base <- "^dummy[FTLCS]|.Size|Poo|Witho|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
exclDP.base <- "^dummy[FTLCS]|.Size|With|Mod|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
exclDS.base <- "dummy[TC]|Large\\.|Large$|Gra|With|Poo|HHM|RM|\\..*Pri|Pri..*\\.|Small|creditst|NumCows$|HadCows|InKind|^Arm$|BSta"
excla.base <- "^PureCo|^credit|dummy[TC]|Large$|Large\\.|LargeG|Witho|Poor|Small|^NumCows$|^Arm$|BSta"
exclDa.base <- "^PureCo|^credit|dummy[TC]|Large$|Large\\.|LargeG|Witho|Poor|Small|^NumCows$|^Arm$|BSta"
for (a in alsuffixes) {
  if (!grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|Had|NumC|0$") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head|Had|NumC")
    assign(paste0("excl", a, 3), "RM|Had|NumC")
    assign(paste0("excl", a, 4), "RM|NumCows|[TCL].*HadCows|HadCows\\.") # keep HadCows$
    assign(paste0("excl", a, 5), "RM|Trad|NumCows|[TCL].*HadCows|HadCows\\..*[CL]|HadCows\\..*Trad") # keep HadCows, Arm.HadCows except traditional and period interactions
    assign(paste0("excl", a, 6), "RM|HadCows") # keep NumCowsOwnedAtRd1
  } else if (grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Head|Cows|0$")
    assign(paste0("excl", a, 2), "Floo|RM|Cows")
    assign(paste0("excl", a, 3), "RM|Cows")
    assign(paste0("excl", a, 4), "RM|NumCows|HadCows\\.") 
    assign(paste0("excl", a, 5), "RM|Trad|NumCows|[TCL].*HadCows|HadCows\\..*[CL]|HadCows\\..*Trad")
    assign(paste0("excl", a, 6), "RM|HadCows")
  }
}
