excl.base <- "With|.Size|Poo|^Arm$|Trad|Live.*de$|credits|InKind|Cash|BSta"
excla.base <- "Witho|Small|Poo|^Arm|Trad|Live.*de$|credits|dummy[FTCS]|Large$|Large\\.|LargeG|Cash|BSta"
exclG.base <- "^dummy[FTLCS]|Poo|.Size|^Arm$|Witho|Live.*de$|credits|InKind|Cash|BSta"
exclP.base <- "dummy[FTLCS]|Large|With|Size|^Arm$|Mode|Live.*de$|credits|InKind|Cash|BSta" 
exclS.base <- "^dummy[FTC]|Large\\.|Large$|Poo|Grac|^Arm$|Small|Live.*de$|credits|InKind|Cash|BSta"
exclT.base <- "Poor|Size|With|Trad|^dummyT|\\..*Pri|Pri..*\\.|^Arm$|Live.*de$|credits|InKind|Cash|BSta"
exclTa.base <- "Witho|Small|Poo|^Arm|Trad|Live.*de$|credits|dummy[FTCS]|Large$|Large\\.|LargeG|Cash|BSta"
exclD.base <- "With|.Size|Poo|^Arm$|Trad|Live.*de$|credits|InKind|Cash|BSta"
exclDG.base <- "^dummy[FTLCS]|Witho|.Size|Poo|^Arm$|Live.*de$|credits|InKind|Cash|BSta"
exclDa.base <- "^PureCo|^credit|dummy[TC]|Large$|Large\\.|LargeG|Witho|Poor|Small|BSta"
for (a in regsuffixes) {
  if (!grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|Had|0$") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head|Had")
    assign(paste0("excl", a, 3), "RM")
    assign(paste0("excl", a, 4), "RM|\\.dummyH|HadCows\\.") # keep HadCows$
    assign(paste0("excl", a, 5), 
    "RM|Trad|[CLG].*HadCows|HadCows.dummyLarge\\.T|HadCows.dummyLarge$|HadCows.dummyLargeG|HadCows.dummyCow|HadCows.dummyLargeS|HadCows.dummyW|HadCows.dummyCash|HadCows.dummyNonCash"
    ) # keep HadCows, Arm.HadCows, Arm.HadCows.Time except traditional
    assign(paste0("excl", a, 6), 
    "RM|Trad|Large.HadCows|LargeGrace.HadCows|Cow.HadCows"
    ) # keep HadCows, Arm.HadCows, Arm.HadCows.Time except traditional
    assign(paste0("excl", a, 7), "RM|HadCows") # keep NumCowsOwnedAtRd1
  } else {
    assign(paste0("excl", a, 1), "Floo|RM|Head|Had|0$")
    assign(paste0("excl", a, 2), "Floo|RM|Head|Had")
    assign(paste0("excl", a, 3), "RM")
    assign(paste0("excl", a, 4), "RM|\\.dummyH|HadCows\\.") 
    assign(paste0("excl", a, 5), 
    "RM|Trad|[TCL].*HadCows|HadCows\\..*[CL]|HadCows\\..*Trad"
    )
    assign(paste0("excl", a, 6), 
    "RM|Trad|HadCows\\..*[CL]|HadCows\\..*Trad"
    )
    assign(paste0("excl", a, 7), "RM|HadCows")
  }
}
