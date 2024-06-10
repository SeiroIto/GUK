# Use elements of each dummy variables as names to exclude them (makeDummyFromFactors gives such variable names)
excl.base <- "o8|Time|RArm|Poor|Size|With|InKind|[FTLC].*Pri|Trad|Sch.*P|^Arm$|^AgeComBSta"
exclg.base <- "o8|dummy[TLC]|Time|RArm|Poor|Size|InKind|Witho|WithG.*Pri|Pri.*F|Sch.*P|^Arm$|^AgeComBSta"
exclp.base <- "o8|dummy[TLC]|Time|RArm|Size|With|InKind|Mod|U.*Pri|Pri.*F|Sch.*P|^Arm$|^AgeComBSta"
excls.base <- "o8|dummy[TC]|Grace|RArm|Sma|Large\\.|Large$|Time|Poor|With|InKind|LargeS.*Pri|Pri.*F|Sch.*P|^Arm$|^AgeComBSta"
excla.base <- "o8|Time|dummy[TC]|RArm|Witho|Large\\.|Large$|LargeG|Small|Poor|Primary|Sch.*P|^Arm$|^AgeComBSta"
excla1 <- "RM|Floo|Head|Eldest|dummy[PJH]|Fem|Age"
excla2 <- "RM|Floo|Head|Eldest|Tr.*dummy[PJH]"
excla3 <- "RM|Tr.*dummy[PJH]"
excla4 <- "Tr.*dummy[PJH]"
excl1 <- "RM|Floo|Head|Eldest|[TLC].*dummy[PJH]|Fem|Age"
excl2 <- "RM|Floo|Head|Eldest|Tr.*dummy[PJH]"
excl3 <- "RM|Tr.*dummy[PJH]"
excl4 <- "Tr.*dummy[PJH]"
for (a in regsuffixes) {
  if (grepl("^g$|^p$|^s$", a)) {
    assign(paste0("excl", a, 1), "RM|Floo|Head|Eldest|Fem|Age")
    assign(paste0("excl", a, 2), "RM|Floo|Head|Eldest")
    assign(paste0("excl", a, 3), "RM")
    assign(paste0("excl", a, 4), "DUMMYSTRING")
  }
}
# period dummy interactions
exclT.base <- "o8|Time\\.2|RArm|Poor|Size|With|InKind|Trad|^dummyT|\\..*Pri|Pri..*\\.|Sch.*P|^Arm$|^AgeComBSta"
exclT1 <- "RM|Floo|Head|Eldest|Time|Fem|[TLC].*dummy[PJH]|Age"
exclT2 <- "RM|Tim|[TLC].*dummy[PJH]"
exclT3 <- "RM|Floo|Head|Eldest|Fem"
exclT4 <- "DUMMYSTRING"
exclTg.base <- "o8|dummy[TLC]|Time\\.2|RArm|Size|Withou|Poo|InKind|HHM|RM|\\..*Pri|Pri..*\\.|Sch.*P|^Arm$|^AgeComBSta"
exclTg1 <- "Floo|Head|Eldest|Time|Fem|Age"
exclTg2 <- "Head|Eldest|Tim"
exclTg3 <- "Floo|Head|Eldest|Fem"
exclTg4 <- "G.*Pri"
exclTs.base <- "o8|dummy[TC]|RArm|Large\\.|Large$|Gra|Time\\.2|Sma|With|Poo|InKind|HHM|RM|\\..*Pri|Pri..*\\.|Sch.*P|^Arm$|^AgeComBSta"
exclTs1 <- "Floo|Head|Eldest|Time|Fem|Age"
exclTs2 <- "Head|Eldest|Tim"
exclTs3 <- "Floo|Head|Eldest|Fem"
exclTs4 <- "S.*Pri"
# Rd 2, 4 differences
exclD.base <- "o8|Time|RArm|Size|With|Poo|InKind|HHM|RM|\\..*Pri|Pri..*\\.|Sch.*P|^Arm$|^AgeComBSta"
exclD1 <- "Floo|Trad|Head|Eldest|Fem|Age"
exclD2 <- "Floo|Trad|Head|Eldest"
exclD3 <- "Trad|Head"
exclD4 <- "Trad|Head"
exclDg.base <- "o8|dummy[TLC]|Time|RArm|Size|Withou|Poo|InKind|HHM|RM|\\..*Pri|Pri..*\\.|Sch.*P|^Arm$|^AgeComBSta"
exclDg1 <- "Floo|Head|Eldest|Fem|Age"
exclDg2 <- "Floo|Head|Eldest"
exclDg3 <- "Head"
exclDg4 <- "Head"
exclDs.base <- "o8|dummy[TLC]|Time|RArm|Size|Withou|Poo|InKind|HHM|RM|\\..*Pri|Pri..*\\.|Sch.*P|^Arm$|^AgeComBSta"
exclDs1 <- "Floo|Head|Eldest|Fem|Age"
exclDs2 <- "Floo|Head|Eldest"
exclDs3 <- "Head"
exclDs4 <- "Head"
exclDa.base <- "o8|dummy[TC]|Time|RArm|Witho|Small|Large\\.|Large$|LargeG|Poo|HHM|RM|\\..*Pri|Pri..*\\.|Sch.*P|^Arm$|^AgeCom|BSta"
exclDa1 <- "RM|Floo|Head|Eldest|[TLCWI].*dummy[PJH]|Fem|Age"
exclDa2 <- "RM|Floo|Head|Eldest|Tr.*dummy[PJH]"
exclDa3 <- "RM|Tr.*dummy[PJH]"
exclDa4 <- "Tr.*dummy[PJH]"
