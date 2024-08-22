ass <- readRDS(paste0(pathsaveHere, DataFileNames[4], "Trimmed.rds")) # assets
obr <- readRDS(paste0(pathsaveHere, DataFileNames[11], "Trimmed.rds")) # debts
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
# Second, merge assDW with arD and find meetings immediately before and after IntDatX
# Third, keep only meetings immediately before and after IntDatX
arD <- arA[, .(hhid, survey, tee, Date, ObPattern, AttritIn, CumLoanAmount, 
  #do not include TradGroup to avoid merge errors later, use TradGroup included in asso.
  CumEffectiveRepayment, CumRepaid, CumNetSaving, DebtOutstanding)]
assD <- asso[!is.na(IntDate), .(Arm, BStatus, hhid, survey, IntDate)]
assDW <- reshape(assD, direction = "wide", idvar = c("Arm", "BStatus", "hhid"), 
  timevar = "survey", v.names = "IntDate")
setkey(assDW, hhid); setkey(arD, hhid)
arDebt <- assDW[arD]
arDebt[, c("PeriodPos", "SVY") := .(as.character(NA), as.integer(NA))]
for (i in 1:4) {
  # as.IDate gives differences in days. Simply Date - date gives difference  in secs.
  #arDebt[, DiffDays := Date - eval(parse(text=paste0("IntDate.", i)))]
  arDebt[, DiffDays := as.IDate(Date) - 
    as.IDate(eval(parse(text=paste0("IntDate.", i))))]
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
# keep only rows with MtgBefore.t + MtgAfter.t != 0
arDebt <- arDebt[eval(parse(text=
  paste(
    paste0(grepout("Mtg", colnames(arDebt)), collapse = "+")
  , "!=0")
  )), ]
# Some survey rounds t have two rows. Pick the closer one to IntDate.t.
arDebt[, Closer := abs(DiffDays) == min(abs(DiffDays)), by = .(hhid, survey)]
arDebt <- arDebt[(Closer), ]
arDebt[, grepout("^IntD.*[1-4]$|^Diff|^M..Diff|^Immediately|^Mtg[AB]|survey",
  colnames(arDebt)) := NULL]
setnames(arDebt, "SVY", "survey")
arDebtW <- reshape(arDebt, direction = "wide", idvar = c("hhid", "survey"),
  timevar = "PeriodPos", v.names = 
  grepout("Cum|Date|Deb|tee", colnames(arDebt)))
setkey(arDebtW, Arm, BStatus, AttritIn, hhid, survey)
setkey(asso, Arm, BStatus, AttritIn, hhid, survey)
assoD <- arDebtW[asso]
cat("\n\nNumber of obs by Arm and attrition\n")
print( addmargins(table0(assoD[!grepl("tw|dou", TradGroup)&o800==1L&tee==1, .(Arm, AttritIn)])))
# HHs not in arA data (nonborrowers)
# print( addmargins(table0(assoD[!grepl("tw|dou", TradGroup)&o800==1L&tee==1, .(Arm, ObPattern)])))
# use before. using after gives many cases of NetValue > TotalValue 
assstrings <- "^Arm$|^groupid$|hhid|tee|^..?Asse|NLH?A|Narrow|Broad|^dummy.*[a-z]$|Floo|HHsize|Time\\.?.|Head|With|.Size|^Cum[ERN].*before$|^Debt.*before$|Bal|BSta|00|AttritIn|TradG|ObP"
lvostrings <- "^groupid$|hhid|tee|^TotalIm|^NumCows|HadCows|00"
ass1 <- assoD[, grepout(assstrings, colnames(assoD)), with = F]
ass1R <- assoD[, grepout(paste0(assstrings, "|RM"), colnames(assoD)), with = F]
# add livestock values
lvo <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo[, grepout("Loan|UD|Forced", colnames(lvo)) := NULL]
lvo1 <- lvo[, grepout(lvostrings, colnames(lvo)), with = F]
tabulate.set.diff <- function(x, y, ReturnTable = T)
{
  xy <- intersect(x, y)
  xylist <- list(onlyx = x[!(x %in% y)], intersection = xy, onlyy = y[!(y %in% x)])
  tabxy <- unlist(lapply(xylist, length))
  if (ReturnTable) return(tabxy) else return(xylist)
}
x <- as.character(unlist(ass1R[o800==1L, .(idt = paste0(hhid, "-", tee))]))
y <- as.character(unlist(lvo1[o800==1L, .(idt = paste0(hhid, "-", tee))]))
tabulate.set.diff(x, y, ReturnTable = T)
tabulate.set.diff(x, y, ReturnTable = F)$onlyx
# merge: NeA1R is net assets with RM variables
commoncols <- intersect(colnames(ass1R), colnames(lvo1))
NeA1R <- merge(ass1R, lvo1, by = commoncols, all.x = T)
addmargins(table0(NeA1R[o800==1L, .(tee, NumCows)]))
# Livestock assets
#   TotalImputedValue: Median sales price are used to impute values 
#   TotalImputed2Value: Median annual prices for cows are used, because cow prices vary a lot by years.
NeA1R[is.na(TotalImputedValue), TotalImputedValue := 0]
NeA1R[is.na(TotalImputed2Value), TotalImputed2Value := 0]
addmargins(table0(NeA1R[o800 == 1L, 
  .(tee, 
    NonNAAllAssets = !is.na(TotalImputedValue))]))
# Given there are many NAs in livestock data, define non-livestock assets.
# xas has the following lines in read_cleaned_data.rnw in chunk define asset amounts
# xas[, NLAssetAmount := HAssetAmount+PAssetAmount]
# xas[, BroadNLAssetAmount := BroadHAssetAmount+PAssetAmount]
# xas[, NarrowNLAssetAmount := NarrowHAssetAmount+PAssetAmount]
# xas[, RNLAssetAmount := RHAssetAmount+PAssetAmount]
# xas[, RBroadNLAssetAmount := RBroadHAssetAmount+PAssetAmount]
# xas[, RNarrowNLAssetAmount := RNarrowHAssetAmount+PAssetAmount]
# NeA1R[, NLAssetValue := NLHAssetAmount + PAssetAmount] # not incl. livestock
# NeA1R[, BroadNLAssetValue := BroadNLHAssetAmount + PAssetAmount] # not incl. livestock
# NeA1R[, NarrowNLAssetValue := NarrowNLHAssetAmount + PAssetAmount] # not incl. livestock
# NeA1R[, RNLAssetValue := RNLHAssetAmount + PAssetAmount] # not incl. livestock
# NeA1R[, RBroadNLAssetValue := RBroadNLHAssetAmount + PAssetAmount] # not incl. livestock
# NeA1R[, RNarrowNLAssetValue := RNarrowNLHAssetAmount + PAssetAmount] # not incl. livestock
# Define NetValue.
#   NLAssetAmount is created in chunk: define asset amounts of read_cleaned_data.rnw
#    xas[, NLAssetAmount := NLHAssetAmount+PAssetAmount]
#    xas[, BroadNLAssetAmount := BroadNLHAssetAmount+PAssetAmount]
NeA1R[, TotalValue := TotalImputedValue + NLAssetAmount]
NeA1R[, Total2Value := TotalImputed2Value + NLAssetAmount]
NeA1R[, TotalBroadValue := TotalImputedValue + BroadNLAssetAmount]
NeA1R[, ProdValue := TotalImputedValue + PAssetAmount]
NeA1R[, TotalDebt := a2b(DebtOutstanding.before, NA, 0) + a2b(NonNGOBal, NA, 0)]
# NetValueOld: Subtract GUK and nonGUK [NonNGOBal] borrowing.
# NetValue: Subtract only nonGUK borrowing. Asset items: completeAsset
NeA1R[, NetValue := TotalValue - TotalDebt]
NeA1R[, NetValueOld := TotalValue - TotalDebt]
NeA1R[, Net2Value := Total2Value - TotalDebt]
NeA1R[, NetBroadValue := TotalBroadValue - TotalDebt]
# NetNLAssetValue: Net non livestock asset values.
NeA1R[, NetNLAssetValue := NLAssetAmount - TotalDebt]
NeA1R[, c(
  "NarrowNLHAssetAmount", "BroadNLHAssetAmount", 
  "TotalValue", "Total2Value") := NULL]
NeA1R[, grepout("before", colnames(NeA1R)) := NULL]
cat("\n\nNumber of obs by membership status and attrition\n")
print(addmargins(table0(NeA1R[o800 == 1L & tee == 1, .(BStatus, AttritIn)])))
NeA1R <- unique(NeA1R)
# Inital values
IniVariables <- grepout("Total2?V|NetBroadV|Net2?V|NL|PAsse|ProdV|HHsize|HeadL|NumCows$", colnames(NeA1R))
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
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions2.R"))
# drop concurrent HHsize|HeadL
NeA1R[, grepout("HHsize$|HeadL$", colnames(NeA1R)) := NULL]
# Only complete panel portion of HHs
NeA1R[, CompleteAssetPanel := F]
NeA1R[
  hhid %in% ass[tee == 1 & 
    !is.na(NLHAssetAmount) & !is.na(PAssetAmount) & !is.na(NumCows), hhid] &
  hhid %in% ass[tee == 2 & 
    !is.na(NLHAssetAmount) & !is.na(PAssetAmount) & !is.na(NumCows), hhid] &
  hhid %in% ass[tee == 3 & 
    !is.na(NLHAssetAmount) & !is.na(PAssetAmount) & !is.na(NumCows), hhid] &
  hhid %in% ass[tee == 4 & 
    !is.na(NLHAssetAmount) & !is.na(PAssetAmount) & !is.na(NumCows), hhid],
  CompleteAssetPanel := T]
# Not selected with o800 == 1 yet.
NeA1R2 <- NeA1R[!grepl("tw|dou", TradGroup), ]
# Select o800 but not dropping 24HHs in trad
NeA1R8 <- NeA1R[o800 == 1L, ]
# Define the estimation sample: NeA1
# To drop 24 HHs in trad, set UseTrimmedSample = T in EstimationMemo_OptionSetting.rnw
if (UseTrimmedSample) NeA1 <- NeA1R2[tee > 1, ] else NeA1 <- NeA1R[tee > 1, ]
NeA1[, grepout("RM", colnames(NeA1)) := NULL]
NeA1R8[, grepout("RM", colnames(NeA1R8)) := NULL]
# Save data for estimation and summary error bar figure
# We base our analysis on NeA1R2 (i.e., if UseTrimmedSample == T, 
# it drops 24 HHs and NeA1 bases on NeA1R2, if F, NeA1 bases on NeA1R).
saveRDS(NeA1, paste0(pathsaveHere, "NetAssetsRegData.rds"))
saveRDS(NeA1R, paste0(pathsaveHere, "NetAssetsANCOVATrimmed.rds"))
saveRDS(NeA1R2, paste0(pathsaveHere, "NetAssetsANCOVA.rds"))
saveRDS(NeA1R8, paste0(pathsaveHere, "NetAssetsOnly800ANCOVA.rds"))
write.tablev(NeA1R2, paste0(pathsaveHere, "NetAssetsANCOVA.prn"), 
  colnamestrue = F)

## NeAFig data for error bar plots
# NeA1R2 is o800 members with 26 dropped from traditional
NeA1R2 <- readRDS(paste0(pathsaveHere, "NetAssetsANCOVA.rds"))
NeAFig = copy(NeA1R2)
# Note: NarrowNetValue = RNarrowNetValue because cassette players and radio are excluded
NeAfig <- NeAFig[, .(Arm, groupid, hhid, o800, UDdummyUltraPoor, tee, BStatus,
  NLHAssetAmount, PAssetAmount, ProdValue, 
  NetValue, NetBroadValue
  )]
setnames(NeAfig, "UDdummyUltraPoor", "UltraPoor")
NeAfig[, povertystatus := "ultra poor"]
NeAfig[UltraPoor == 0L, povertystatus := "moderately poor"]
NeAfig[, povertystatus := factor(povertystatus, 
  levels = c("ultra poor","moderately poor"))]
NeAfig[, UltraPoor := NULL]
SumMeanStdCI <- function(x, NARM = T, OnlyMean = F, NoSum = F) {
  nx <- names(x)
  if (is.null(dim(x))) x <- matrix(x)
  ms <- c(
      apply(x, 2, sum, na.rm = NARM), 
      apply(x, 2, mean, na.rm = NARM), 
      apply(x, 2, function(z) var(z, na.rm = NARM)^.5),
      apply(x, 2, function(z) length(z[!is.na(z)])),
      apply(x, 2, function(z) mean(z, na.rm = NARM)-1.96*var(z, na.rm = NARM)^.5/sqrt(length(z[!is.na(z)]))),
      apply(x, 2, function(z) mean(z, na.rm = NARM)+1.96*var(z, na.rm = NARM)^.5/sqrt(length(z[!is.na(z)])))
    )
  names(ms) <- paste0(nx, rep(c(".sum", ".mean", ".std", ".N", ".lower", ".upper"), each = ncol(x)))
  if (NoSum) ms <- ms[-grep("sum", names(ms))]
  if (OnlyMean) {
    ms <- ms[grep("mean", names(ms))]
    names(ms) <- gsub("..mean$", "", names(ms))
  }
  return(ms) 
}
NeAfigAll <- 
  NeAfig[, 
    as.list(unlist(lapply(.SD, SumMeanStdCI, OnlyMean=F, NoSum = F))), 
    .SDcols = grepout("PAsset|Value", colnames(NeAfig)), 
    by = .(Arm, tee)]
setkey(NeAfigAll, Arm, tee)
NeAfig0 <- 
  NeAfig[, 
    as.list(unlist(lapply(.SD, SumMeanStdCI, OnlyMean=T, NoSum = F))), 
    .SDcols = grepout("Value", colnames(NeAfig)), 
    by = .(Arm, tee)]
setnames(NeAfig0, grepout("..mean$", names(NeAfig0)), 
  gsub("..mean", "", grepout("..mean$", names(NeAfig0))))
# manually computing group means
# nnetAD <- NeA1R2[, .(
#     MeanC = mean(NarrowNetValue, na.rm = T), 
#     StdC = var(NarrowNetValue, na.rm = T)^(.5),
#     N = .N
#   ), by = .(tee, Arm)][, 
#     .(Arm, tee, mean = MeanC, 
#       upper = MeanC + 1.96*StdC/sqrt(N),
#       lower = MeanC - 1.96*StdC/sqrt(N)
#       )]
# nnetAD[, Arm := factor(Arm, labels = c("traditional", "large", "large grace", "cattle"))]
# rnnetAD <- NeA1R2[, .(
#     MeanC = mean(RNarrowNetValue, na.rm = T), 
#     StdC = var(RNarrowNetValue, na.rm = T)^(.5),
#     N = .N
#   ), by = .(tee, Arm)][, 
#     .(Arm, tee, mean = MeanC, 
#       upper = MeanC + 1.96*StdC/sqrt(N),
#       lower = MeanC - 1.96*StdC/sqrt(N)
#       )]
# rnnetAD[, Arm := factor(Arm, labels = c("traditional", "large", "large grace", "cattle"))]
# Select only HHs with baseline
cpnea = copy(NeA1R2[(CompleteAssetPanel), ])
cpnea[, .(Arm, hhid, tee, NetValue)]
cpnea <- cpnea[hhid %in% hhid[tee == 4], ]
cpnea[, Arm := factor(Arm, labels = armsC)]
cpneaAll <- 
  cpnea[, 
    as.list(unlist(lapply(.SD, SumMeanStdCI, OnlyMean=F, NoSum = F))), 
    .SDcols = "NetValue", 
    by = .(Arm, tee)]
setkey(cpneaAll, Arm, tee)
# save all files
saveRDS(NeAfig, paste0(pathsaveHere, "NetAssetsFigureData.rds"))
saveRDS(NeAfig0, paste0(pathsaveHere, "NetAssetsFigureMeanData.rds"))
saveRDS(NeAfigAll, paste0(pathsaveHere, "AllNetAssetsFigureMeanData.rds"))
saveRDS(cpneaAll, paste0(pathsaveHere, "CPNetAssetsFigureMeanData.rds"))
NeA1R2[, Tee := .N, by = hhid]
NeA1R2[, LastRd := .N, by = hhid]
PrevWidth <- options()$width 
options(width = 100)
ar <- readRDS(paste0(pathsaveHere, DataFileNames[3], "Trimmed.rds"))
options(width = PrevWidth)
