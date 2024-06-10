excl.base <- "With|.Size|Poo|^Arm$|Trad|Live.*de$|credits|^NumCows$|InKind|Cash|BSta"
excla.base <- "Witho|Small|Poo|^Arm|Trad|Live.*de$|credits|^NumCows$|dummy[FTCS]|Large$|Large\\.|LargeG|Cash|BSta"
exclG.base <- "^dummy[FTLCS]|Poo|.Size|^Arm$|Witho|Live.*de$|credits|^NumCows$|InKind|Cash|BSta"
exclP.base <- "dummy[FTLCS]|Large|With|Size|^Arm$|Mode|Live.*de$|credits|^NumCows$|InKind|Cash|BSta" 
exclS.base <- "^dummy[FTC]|Large\\.|Large$|Poo|Grac|^Arm$|Small|Live.*de$|credits|^NumCows$|InKind|Cash|BSta"
exclT.base <- "Poor|Size|With|Trad|^dummyT|\\..*Pri|Pri..*\\.|^Arm$|Live.*de$|credits|^NumCows$|InKind|Cash|BSta"
exclTG.base <- "dummy[TLC]|Size|Withou|Poo|HHM|RM|\\..*Pri|Pri..*\\.|^Arm$|Live.*de$|credits|^NumCows$|InKind|Cash|BSta"
exclTS.base <- "dummy[TC]|Large\\.|Large$|Gra|Sma|With|Poo|HHM|RM|\\..*Pri|Pri..*\\.|^Arm$|Live.*de$|credits|^NumCows$|InKind|Cash|BSta"
exclD.base <- "With|.Size|Poo|^Arm$|Trad|Live.*de$|credits|^NumCows$|InKind|Cash|BSta"
exclDG.base <- "^dummy[FTLCS]|Witho|.Size|Poo|^Arm$|Live.*de$|credits|^NumCows$|InKind|Cash|BSta"
exclDa.base <- "^PureCo|^credit|dummy[TC]|Large$|Large\\.|LargeG|Witho|Poor|Small|^NumCows$|BSta"
for (a in Lvsuffixes) {
  if (!grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|Cows|0$") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head|Cows")
    assign(paste0("excl", a, 3), "Cows")
    assign(paste0("excl", a, 4), "RM|NumCows|\\.dummyH|HadCows\\.") # keep HadCows$
    assign(paste0("excl", a, 5), 
    "RM|Trad|NumCows|[CLG].*HadCows|HadCows.dummyLarge\\.T|HadCows.dummyLarge$|HadCows.dummyLargeG|HadCows.dummyCow|HadCows.dummyLargeS|HadCows.dummyW|HadCows.dummyCash|HadCows.dummyNonCash"
    ) # keep HadCows, Arm.HadCows, Arm.HadCows.Time except traditional
    assign(paste0("excl", a, 6), 
    "RM|Trad|NumCows|Large.HadCows|LargeGrace.HadCows|Cow.HadCows"
    ) # keep HadCows, Arm.HadCows, Arm.HadCows.Time except traditional
    assign(paste0("excl", a, 7), "RM|HadCows") # keep NumCowsOwnedAtRd1
  } else {
    assign(paste0("excl", a, 1), "Floo|RM|Head|Cows|0$")
    assign(paste0("excl", a, 2), "Floo|RM|Head|Cows")
    assign(paste0("excl", a, 3), "Cows")
    assign(paste0("excl", a, 4), "RM|NumCows|\\.dummyH|HadCows\\.") 
    assign(paste0("excl", a, 5), 
    "RM|Trad|NumCows|[TCL].*HadCows|HadCows\\..*[CL]|HadCows\\..*Trad"
    )
    assign(paste0("excl", a, 6), 
    "RM|Trad|NumCows|HadCows\\..*[CL]|HadCows\\..*Trad"
    )
    assign(paste0("excl", a, 7), "RM|HadCows")
  }
}
