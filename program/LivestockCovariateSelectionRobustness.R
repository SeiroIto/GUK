Lvsuffixes <- c("", "G", "P", "S", "T", "TG", "TS", "D", "DG")
excl.base <- "With|.Size|Poo|^Arm$|Trad|Time.?2|live.*de$|credits|^NumCows$"
exclG.base <- "^dummy[FTLCS]|Poo|.Size|^Arm$|Witho|Time.?2|live.*de$|credits|^NumCows$"
exclP.base <- "^dummy[FTLCS]|With|.Size|^Arm$|Mode|Time.?2|live.*de$|credits|^NumCows$" 
exclS.base <- "^dummy[FTC]|Large\\.|Large$|Poo|Grac|^Arm$|Small|Time.?2|live.*de$|credits|^NumCows$"
exclT.base <- "Time\\.2|Poor|Size|With|Trad|^dummyT|\\..*Pri|Pri..*\\.|^Arm$|live.*de$|credits|^NumCows$"
exclTG.base <- "dummy[TLC]|Time\\.2|Size|Withou|Poo|HHM|RM|\\..*Pri|Pri..*\\.|^Arm$|live.*de$|credits|^NumCows$"
exclTS.base <- "dummy[TC]|Large\\.|Large$|Gra|Time\\.2|Sma|With|Poo|HHM|RM|\\..*Pri|Pri..*\\.|^Arm$|live.*de$|credits|^NumCows$"
exclD.base <- "With|.Size|Poo|^Arm$|Trad|Time|live.*de$|credits|^NumCows$"
exclDG.base <- "^dummy[FTLCS]|Witho|.Size|Poo|Tim|^Arm$|live.*de$|credits|^NumCows$"
for (a in Lvsuffixes) {
  if (!grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|Tim|Cows") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head|Cows")
    assign(paste0("excl", a, 3), "Cows")
    assign(paste0("excl", a, 4), "RM|NumCows|\\.dummyH|HadCows\\.") # keep HadCows$
    assign(paste0("excl", a, 5), "RM|Trad|NumCows") # keep HadCows, Arm.HadCows, Arm.HadCows.Time except traditional
    assign(paste0("excl", a, 6), "RM|HadCows") # keep NumCowsOwnedAtRd1
  } else {
    assign(paste0("excl", a, 1), "Floo|RM|Head|Cows")
    assign(paste0("excl", a, 2), "Floo|RM|Head|Cows")
    assign(paste0("excl", a, 3), "Cows")
    assign(paste0("excl", a, 4), "RM|NumCows|\\.dummyH|HadCows\\.") 
    assign(paste0("excl", a, 5), "RM|Trad|NumCows")
    assign(paste0("excl", a, 6), "RM|HadCows")
  }
}
