arA <- readRDS(paste0(pathsaveHere, DataFileNames[2], "InitialSample.rds"))
arA[, grepout("^Time$|UD|[mM]issw|Small|^Size", 
  colnames(arA)) := NULL]
arA[, CumSave := CumNetSaving - CumRepaid]
arA[, CumEffectiveRepayment := CumNetSaving + CumRepaid]
arA[, Arm := droplevels(Arm)]
arA[, HeadLiteracy := HeadLiteracy + 0]
arA[, ExcessRepayment := 0]
arA[grepl("bo", BorrowerStatus), 
  ExcessRepayment := value.repay - PlannedInstallment]
for (rr in grepout("^RM", colnames(arA)))
  arA[, (rr) := eval(parse(text=paste0(rr, "*RMDenomination")))]
setkey(arA, hhid, tee)
arA[, c("Repaid", "NetSaving") := 
  .(c(CumRepaid[1], firstdiff(CumRepaid)), 
    c(CumNetSaving[1], firstdiff(CumNetSaving))), by = hhid]
arA[, EffectiveRepayment := Repaid + NetSaving]
# get HadCows
lvo0 <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo0 <- unique(lvo0[, .(hhid, dummyHadCows)])
arA[, dummyHadCows := 0L]
arA[hhid %in% lvo0[dummyHadCows == 1L, hhid], dummyHadCows := 1L]
# Inital values
IniVariables <- grepout("^Cum|^Repaid|^NetS|^EffectiveR|HHsize|HeadL", 
  colnames(arA))
setkey(arA, hhid, survey)
arA[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
arA[, FirstObs := 0L]
arA[, minrd := min(survey), by = hhid][minrd == survey, FirstObs := 1L]
arA[, FirstObs := NULL]
# create PureControl
arA[, PureControl := 0L]
#arA[!grepl("borro", BStatus), PureControl := 1L]
arA[!grepl("es$", creditstatus), PureControl := 1L]
arA[, paste0("PureControl.Time", 2:4) := PureControl]
arA[tee != 2, PureControl.Time2 := 0L]
arA[tee != 3, PureControl.Time3 := 0L]
arA[tee != 4, PureControl.Time4 := 0L]
FileNameForUD <- "arA"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
# drop first period for ANCOVA
arA <- arA[tee > 1, ]
# meanar1 <- ard1[, .(
#   MeanFDCumRepaid = mean(Repaid),
#   MeanFDCumNetSaving = mean(NetSaving),
#   MeanFDCumExcessRepayment = mean(ExcessRepayment)), 
#   by = survey][survey == 1, ]
if (Only800) arA <- arA[o800 == 1L & !is.na(LoanYear) &
  !grepl("tw|dou", TradGroup), ]
# arA2: use only borrowers
arA2 <- arA[grepl("bo", BorrowerStatus), 
  grepout("^groupid|^hhid|survey|tee|UD|LY|^dummy[A-Z]|^dummy.*[a-z]$|Time|CumRepaid$|CumE.*t$|CumNet|ExcessRepayment$|RMOther|RMvalue.[rN]|HeadA|HeadL|Floo|With|Size|^value\\.|^EffectiveR|^Arm$|BSta", 
  colnames(arA)), with = F]
datas <- c("arA", "arA2")
