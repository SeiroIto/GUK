ass <- readRDS(paste0(pathsaveHere, DataFileNames[4], "InitialSample.rds"))
arA <- readRDS(paste0(pathsaveHere, DataFileNames[2], "InitialSample.rds"))
ass[, grepout("Loan|UD|Forced", colnames(ass)) := NULL]
# merge debt outstanding to assets. 
# arA has on avg 12 meetings per survey round. Which meeting in a survey round
# should I use? Merge both immediate past and future dates.
# First, reshape AssD to wide: hhid survey IntDate1, ..., IntDate4
# Second, merge with arA and find meetings immediately before and after IntDatX
# Third, keep only meetings immediately before and after IntDatX
arD <- arA[, .(hhid, survey, tee, Date, CumLoanAmount, 
  CumEffectiveRepayment, CumRepaid, CumNetSaving, DebtOutstanding)]
assD <- ass[!is.na(IntDate), .(Arm, BStatus, hhid, survey, IntDate)]
assDW <- reshape(assD, direction = "wide", idvar = c("Arm", "BStatus", "hhid"), 
  timevar = "survey", v.names = "IntDate")
setkey(assDW, hhid); setkey(arD, hhid)
arDebt <- assDW[arD]
arDebt[, c("PeriodPos", "SVY") := .(as.character(NA), as.integer(NA))]
for (i in 1:4) {
  arDebt[, DiffDays := Date - eval(parse(text=paste0("IntDate.", i)))]
  arDebt[, ImmediatelyAfter := 
    min(DiffDays[DiffDays >= 0], na.rm = T), by = hhid]
  arDebt[, ImmediatelyBefore := 
    max(DiffDays[DiffDays < 0], na.rm = T), by = hhid]
  arDebt[, (paste0(c("MtgBefore.", "MtgAfter."), i)) := 0L]
  arDebt[DiffDays == ImmediatelyBefore, 
    (paste0("MtgBefore.", i)) := 1L]
  arDebt[DiffDays == ImmediatelyBefore, 
    c("PeriodPos", "SVY") := .("before", as.integer(i))]
  arDebt[DiffDays == ImmediatelyAfter, 
    (paste0("MtgAfter.", i)) := 1L]
  arDebt[DiffDays == ImmediatelyAfter, 
    c("PeriodPos", "SVY") := .("after", as.integer(i))]
}
arDebt <- arDebt[eval(parse(text=
  paste(
    paste0(grepout("Mtg", colnames(arDebt)), collapse = "+")
  , "!=0")
  )), ]
arDebt[, grepout("^IntD.*[1-4]$|^Diff|^M..Diff|^Immediately|^Mtg[AB]|survey",
  colnames(arDebt)) := NULL]
setnames(arDebt, "SVY", "survey")
arDebtW <- reshape(arDebt, direction = "wide", idvar = c("hhid", "survey"),
  timevar = "PeriodPos", v.names = 
  grepout("Cum|Date|Deb|tee", colnames(arDebt)))
setkey(arDebtW, Arm, BStatus, hhid, survey)
setkey(ass, Arm, BStatus, hhid, survey)
ass <- arDebtW[ass]
# use before. using after gives many cases of NetValue > TotalValue 
assstrings <- "^Arm$|^groupid$|hhid|tee|^.Asse|^dummy.*[a-z]$|Floo|HHsize|Time\\.?.|Head|With|.Size|^Cum[ERN].*before$|^Debt.*before$|BSta|00"
lvostrings <- "^groupid$|hhid|tee|^TotalIm|Cows|00"
ass1 <- ass[, grepout(assstrings, colnames(ass)), with = F]
ass1R <- ass[, grepout(paste0(assstrings, "|RM"), colnames(ass)), with = F]
lvo <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo[, grepout("Loan|UD|Forced", colnames(lvo)) := NULL]
lvo1 <- lvo[, grepout(lvostrings, colnames(lvo)), with = F]
# merge
commoncols <- intersect(colnames(ass1), colnames(lvo1))
NeA1R <- merge(ass1R, lvo1, by = commoncols, NeAl = T)
# Define NetValue
NeA1R[is.na(TotalImputedValue), TotalImputedValue := 0]
NeA1R[, TotalValue := TotalImputedValue + HAssetAmount + PAssetAmount]
NeA1R[, NetValue := TotalValue - a2b(DebtOutstanding.before, NA, 0)]
NeA1R[, c("TotalImputedValue", "HAssetAmount", 
  "PAssetAmount", "TotalValue") := NULL]
NeA1R[, grepout("before", colnames(NeA1R)) := NULL]
NeA1R <- unique(NeA1R)
# Inital values
IniVariables <- grepout("TotalV|NetV|HHsize|HeadL|NumCows$", colnames(NeA1R))
setkey(NeA1R, hhid, tee)
NeA1R[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
# create PureControl
NeA1R[, PureControl := 0L]
NeA1R[!grepl("borro", BStatus), PureControl := 1L]
#NeA1R[!grepl("es$", creditstatus), PureControl := 1L]
NeA1R[, paste0("PureControl.Time", 2:4) := PureControl]
NeA1R[tee != 2, PureControl.Time2 := 0L]
NeA1R[tee != 3, PureControl.Time3 := 0L]
NeA1R[tee != 4, PureControl.Time4 := 0L]
# create Arm*UltraPoor interactions (dummyArm, dummyUP are not demeaned)
# also create UDxxx for mean/std column in estimate table
FileNameForUD <- "NeA1R"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
# drop concurrent HHsize|HeadL
NeA1R[, grepout("HHsize$|HeadL$", colnames(NeA1R)) := NULL]
NeAfig = copy(NeA1R)
if (Only800) NeA1R <- NeA1R[o800 == 1L, ]
NeA1 <- NeA1R[tee > 1, ]
NeA1[, grepout("RM", colnames(NeA1)) := NULL]
# before-after style 2 time point data. 
saveRDS(NeA1R, paste0(pathsaveHere, "GUKNetAssetsANCOVA.rds"))
write.tablev(NeA1R, paste0(pathsaveHere, "GUKNetAssetsANCOVA.prn"), 
  colnamestrue = F)
NeAfig[, povertystatus := "ultra poor"]
NeAfig[UDdummyUltraPoor == 0L, povertystatus := "moderately poor"]
NeAfig[, povertystatus := factor(povertystatus, levels = c("ultra poor","moderately poor"))]
NeAfig[, UDdummyUltraPoor := NULL]
