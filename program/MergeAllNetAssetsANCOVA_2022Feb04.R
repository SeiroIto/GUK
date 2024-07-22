ass <- readRDS(paste0(pathsaveHere, DataFileNames[4], "Trimmed.rds"))
obr <- readRDS(paste0(pathsaveHere, DataFileNames[11], "Trimmed.rds"))
# need admin net saving data
arA <- readRDS(paste0(pathsaveHere, DataFileNames[2], "Trimmed.rds"))
ass[, grepout("Loan|UD|Forced", colnames(ass)) := NULL]
obr <- obr[, .(hhid, survey, IntDate, NonNGOBal, NetOutBal)]
setkey(ass, hhid, survey, IntDate)
setkey(obr, hhid, survey, IntDate)
asso <- obr[ass]
# merge debt outstanding to assets. 
# arA has on avg 12 meetings per survey round. Which meeting in a survey round
# should I use? Merge both immediate past and future dates.
# First, reshape AssD to wide: hhid survey IntDate1, ..., IntDate4
# Second, merge with arA and find meetings immediately before and after IntDatX
# Third, keep only meetings immediately before and after IntDatX
arD <- arA[, .(hhid, survey, tee, Date, AttritIn, CumLoanAmount, 
  #do not include TradGroup to avoid merge errors later, use TradGroup included in asso.
  CumEffectiveRepayment, CumRepaid, CumNetSaving, DebtOutstanding)]
assD <- asso[!is.na(IntDate), .(Arm, BStatus, hhid, survey, IntDate)]
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
setkey(arDebtW, Arm, BStatus, AttritIn, hhid, survey)
setkey(asso, Arm, BStatus, AttritIn, hhid, survey)
assoD <- arDebtW[asso]
# use before. using after gives many cases of NetValue > TotalValue 
assstrings <- "^Arm$|^groupid$|hhid|tee|^.Asse|^dummy.*[a-z]$|Floo|HHsize|Time\\.?.|Head|With|.Size|^Cum[ERN].*before$|^Debt.*before$|Bal|BSta|00|AttritIn|TradG"
lvostrings <- "^groupid$|hhid|tee|^TotalIm|Cows|00"
ass1 <- assoD[, grepout(assstrings, colnames(assoD)), with = F]
ass1R <- assoD[, grepout(paste0(assstrings, "|RM"), colnames(assoD)), with = F]
# add livestock values
lvo <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo[, grepout("Loan|UD|Forced", colnames(lvo)) := NULL]
lvo1 <- lvo[, grepout(lvostrings, colnames(lvo)), with = F]
# merge
commoncols <- intersect(colnames(ass1R), colnames(lvo1))
NeA1R <- merge(ass1R, lvo1, by = commoncols, all.x = T)
# Define NetValue etc.
NeA1R[is.na(TotalImputedValue), TotalImputedValue := 0]
NeA1R[is.na(TotalImputed2Value), TotalImputed2Value := 0]
NeA1R[, TotalValue := TotalImputedValue + HAssetAmount + PAssetAmount]
NeA1R[, Total2Value := TotalImputed2Value + HAssetAmount + PAssetAmount]
NeA1R[, NetValueGUK := TotalValue - a2b(DebtOutstanding.before, NA, 0)]
# net assets old (subtract GUK and nonGUK borrowing)
NeA1R[, NetValueOld := TotalValue - a2b(DebtOutstanding.before, NA, 0) - a2b(NetOutBal, NA, 0)]
NeA1R[, NetValue := TotalValue - a2b(DebtOutstanding.before, NA, 0) - a2b(NonNGOBal, NA, 0)]
NeA1R[, Net2Value := Total2Value - a2b(DebtOutstanding.before, NA, 0) - a2b(NonNGOBal, NA, 0)]
NeA1R[, c("TotalImputedValue", "HAssetAmount", 
  "PAssetAmount", "TotalValue", "Total2Value") := NULL]
NeA1R[, grepout("before", colnames(NeA1R)) := NULL]
print(addmargins(table0(NeA1R[o800 == 1L & tee == 1, .(BStatus, AttritIn)])))
print(addmargins(table0(NeA1R[o800 == 1L & tee == 1, .(TradGroup, AttritIn)])))
print(addmargins(table0(NeA1R[o800 == 1L & tee == 1, .(Arm, AttritIn)])))
NeA1R <- unique(NeA1R)
# Inital values
IniVariables <- grepout("Total2?V|Net2?V|HHsize|HeadL|NumCows$", colnames(NeA1R))
setkey(NeA1R, hhid, tee)
NeA1R[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
# create PureControl
NeA1R[, PureControl := 0L]
NeA1R[!grepl("borro", BStatus), PureControl := 1L]
NeA1R[, paste0("PureControl.Time", 2:4) := PureControl]
NeA1R[tee != 2, PureControl.Time2 := 0L]
NeA1R[tee != 3, PureControl.Time3 := 0L]
NeA1R[tee != 4, PureControl.Time4 := 0L]
# UDPureControl is used in mean/std column
NeA1R[, UDPureControl := PureControl]
pctstring <- paste0("PureControl.Time", 2:4)
NeA1R[, (paste0("UD", pctstring)) := eval(parse(text=
  paste0("list(", paste(pctstring, collapse = ","), ")")))]
# create Arm*UltraPoor interactions (dummyArm, dummyUP are not demeaned)
# also create UDxxx for mean/std column in estimate table
FileNameForUD <- "NeA1R"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
# drop concurrent HHsize|HeadL
NeA1R[, grepout("HHsize$|HeadL$", colnames(NeA1R)) := NULL]
NeAFig = copy(NeA1R)
if (Only800) NeA1R <- NeA1R[o800 == 1L, ]
saveRDS(NeA1R, paste0(pathsaveHere, "NetAssetsANCOVATrimmed.rds"))
NeA1R2 <- NeA1R[!grepl("tw|dou", TradGroup), ]
saveRDS(NeA1R2, paste0(pathsaveHere, "NetAssetsANCOVA.rds"))
write.tablev(NeA1R2, paste0(pathsaveHere, "NetAssetsANCOVA.prn"), 
  colnamestrue = F)
netAD <- NeA1R2[, .(
    MeanC = mean(NetValue, na.rm = T), 
    StdC = var(NetValue, na.rm = T)^(.5),
    N = .N
  ), by = .(tee, Arm)][, 
    .(Arm, tee, mean = MeanC, 
      upper = MeanC + 1.96*StdC/sqrt(N),
      lower = MeanC - 1.96*StdC/sqrt(N)
      )]
netAD[, Arm := factor(Arm, labels = c("traditional", "large", "large grace", "cattle"))]
saveRDS(netAD, paste0(pathsaveHere, "NetAssetsFigure.rds"))
if (UseTrimmedSample) NeA1R = copy(NeA1R2)
NeA1 <- NeA1R[tee > 1, ]
NeA1[, grepout("RM", colnames(NeA1)) := NULL]
NeAfig <- NeAFig[, .(Arm, groupid, hhid, UDdummyUltraPoor, tee, NetValue)]
setnames(NeAfig, "UDdummyUltraPoor", "UltraPoor")
NeAfig[, povertystatus := "ultra poor"]
NeAfig[UltraPoor == 0L, povertystatus := "moderately poor"]
NeAfig[, povertystatus := factor(povertystatus, 
  levels = c("ultra poor","moderately poor"))]
NeAfig[, UltraPoor := NULL]
