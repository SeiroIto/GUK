excl.base <- "With|.Size|Poo|^Arm$|Trad|Time.?2|Live.*de$|credits|InKind|Cash|BSta"
excla.base <- "Witho|Small|Poo|^Arm|Trad|Time.?2|Live.*de$|credits|dummy[FTCS]|Large$|Large\\.|LargeG|Cash|BSta"
exclG.base <- "^dummy[FTLCS]|Poo|.Size|^Arm$|Witho|Time.?2|Live.*de$|credits|InKind|Cash|BSta"
exclP.base <- "dummy[FTLCS]|Large|With|Size|^Arm$|Mode|Time.?2|Live.*de$|credits|InKind|Cash|BSta" 
exclS.base <- "^dummy[FTC]|Large\\.|Large$|Poo|Grac|^Arm$|Small|Time.?2|Live.*de$|credits|InKind|Cash|BSta"
exclT.base <- "Time\\.2|Poor|Size|With|Trad|^dummyT|\\..*Pri|Pri..*\\.|^Arm$|Live.*de$|credits|InKind|Cash|BSta"
exclTG.base <- "dummy[TLC]|Time\\.2|Size|Withou|Poo|HHM|RM|\\..*Pri|Pri..*\\.|^Arm$|Live.*de$|credits|InKind|Cash|BSta"
exclTS.base <- "dummy[TC]|Large\\.|Large$|Gra|Time\\.2|Sma|With|Poo|HHM|RM|\\..*Pri|Pri..*\\.|^Arm$|Live.*de$|credits|InKind|Cash|BSta"
exclD.base <- "With|.Size|Poo|^Arm$|Trad|Time|Live.*de$|credits|InKind|Cash|BSta"
exclDG.base <- "^dummy[FTLCS]|Witho|.Size|Poo|Tim|^Arm$|Live.*de$|credits|InKind|Cash|BSta"
exclDa.base <- "^PureCo|^credit|dummy[TC]|Large$|Large\\.|LargeG|Time|Witho|Poor|Small|BSta"
for (a in Lvsuffixes) {
  if (!grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|Tim|Cows") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head|Cows")
    assign(paste0("excl", a, 3), "Cows")
    assign(paste0("excl", a, 4), "RM|NumCows|\\.dummyH|HadCows\\.") # keep HadCows$
    assign(paste0("excl", a, 5), 
    "RM|Trad|NumCows|[CLG].*HadCows|HadCows.dummyLarge\\.T|HadCows.dummyLarge$|HadCows.dummyLargeG|HadCows.dummyCow|HadCows.dummyLargeS|HadCows.dummyW|HadCows.dummyCash|HadCows.dummyNonCash"
    ) # keep HadCows, Arm.HadCows, Arm.HadCows.Time except traditional
    assign(paste0("excl", a, 6), 
    "RM|Trad|NumCows|Large.HadCows|LargeGrace.HadCows|Cow.HadCows"
    #"RM|Trad|NumCows|HadCows\\..*[CL]|HadCows\\..*Trad"
    ) # keep HadCows, Arm.HadCows, Arm.HadCows.Time except traditional
    assign(paste0("excl", a, 7), "RM|HadCows") # keep NumCowsOwnedAtRd1
  } else {
    assign(paste0("excl", a, 1), "Floo|RM|Head|Cows")
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
