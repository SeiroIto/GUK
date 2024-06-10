excl.base <- "^PureCo|^credit|With|.Size|Poo|Trad|InKind|Cash|^Arm$|BSta"
exclG.base <- "^PureCo|^credit|^dummy[FTLCS]|Poo|.Size|Witho|InKind|Cash|^Arm$|BSta"
exclP.base <- "^PureCo|^credit|^dummy[FTLCS]|With|.Size|Moder|InKind|Cash|^Arm$|BSta" 
exclS.base <- "^PureCo|^credit|^dummy[FTC]|Large\\.|Large$|Poo|Small|Grac|InKind|Cash|^Arm$|BSta"
exclD.base <- "^PureCo|^credit|With|.Size|Poo|Trad|Time|InKind|Cash|^Arm$|BSta"
exclDG.base <- "^PureCo|^credit|^dummy[FTLCS]|.Size|Poo|Witho|Time|InKind|Cash|^Arm$|BSta"
excla.base <- "^PureCo|^credit|dummy[TC]|Large$|Large\\.|LargeG|Time.?2|Witho|Poor|Small|^Arm$|BSta"
exclDa.base <- "^PureCo|^credit|dummy[TC]|Large$|Large\\.|LargeG|Time|Witho|Poor|Small|^NumCows$|^Arm$|BSta"
# For j == 4, 7, RM reduces sample to T = 3 (hence 2 period FD data), so drop period 2-3 dummy
for (a in Assuffixes) {
  if (a != "S" & !grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|Tim") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head")
    assign(paste0("excl", a, 3), "RM|Eff")
    assign(paste0("excl", a, 4), "Time.?3")
    assign(paste0("excl", a, 5), "Floo|RM|Eff|Head|Tim")
    assign(paste0("excl", a, 6), "Floo|RM|Eff|Head")
    assign(paste0("excl", a, 7), "RM")
    assign(paste0("excl", a, 8), "Time.?3")
  } else if (a == "S") {
    assign(paste0("excl", a, 1), "Floo|RM|Time|Head")
    assign(paste0("excl", a, 2), "Floo|RM|Head")
    assign(paste0("excl", a, 3), "RM")
    assign(paste0("excl", a, 4), "Time.?3")
    assign(paste0("excl", a, 5), "Floo|RM|Head|Time")
    assign(paste0("excl", a, 6), "RM")
    assign(paste0("excl", a, 7), "RM|Time.?3")
    assign(paste0("excl", a, 8), "Time.?3")
  } else if (grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Head")
    assign(paste0("excl", a, 2), "RM")
    assign(paste0("excl", a, 3), "DUMMYSTRING")
    assign(paste0("excl", a, 4), "DUMMYSTRING")
    assign(paste0("excl", a, 5), "Floo|RM|Head")
    assign(paste0("excl", a, 6), "RM")
    assign(paste0("excl", a, 7), "DUMMYSTRING")
    assign(paste0("excl", a, 8), "DUMMYSTRING")
  }
}
das1d[, Tee := NULL]
das2d[, Tee := NULL]
