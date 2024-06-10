excl.base <- "^dummy[FTS]|Arm$|Cash|InK|With|.Size|Poo|Trad|BSta|Time"
exclG.base <- "^dummy[FTLCSI]|Poo|.Size|Witho|BSta|Time"
exclP.base <- "^dummy[FTLCSI]|With|.Size|Moder|Time" 
exclS.base <- "^dummy[FTC]|InK|Large\\.|Large$|Poo|Small|Grac|BSta|Time"
excla.base <- "dummy[FTC]|Large$|Large\\.|LargeG|Time.?2|Witho|Poor|Small|BSta|Time"
exclT.base <- "^P.*ol$|^credit|Arm$|^dummy[LTC].*[a-z]$|With|.Size|Poo|Trad|InKind|Cash|^Arm$|BSta"
exclTa.base <- "^P.*ol$|^credit|ze$|ce$|nd$|Trad|Cow|Cash|Large$|Large\\.|LargeG|Witho|Poor|Small|^Arm$|BSta"
for (a in regsuffixes) {
  if (a != "S" & !grepl("D", a)) {
    assign(paste0("excl", a, 1), "Floo|RM|Eff|Head|0$") 
    assign(paste0("excl", a, 2), "Floo|RM|Eff|Head")
    assign(paste0("excl", a, 3), "RM|Eff")
    assign(paste0("excl", a, 4), "Floo|RM|Eff|Head|0$")
    assign(paste0("excl", a, 5), "Floo|RM|Eff|Head")
    assign(paste0("excl", a, 6), "RM")
  } else if (a == "S") {
    assign(paste0("excl", a, 1), "Floo|RM|Head|0$")
    assign(paste0("excl", a, 2), "Floo|RM|Head")
    assign(paste0("excl", a, 3), "RM")
    assign(paste0("excl", a, 4), "Floo|RM|Head|0$")
    assign(paste0("excl", a, 5), "RM")
    assign(paste0("excl", a, 6), "RM")
  }
}
