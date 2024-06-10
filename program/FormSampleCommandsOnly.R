source(paste0(pathprogram, "MergeAllNarrowNetAssetsANCOVA.R"))
# this gives NarrowNetAssetsANCOVATrimmed.rds
#  trimmed sample are data before dropping 26 traditional HHs.
ar <- readRDS(paste0(pathsaveHere, DataFileNames[3], "Trimmed.rds"))
arA <- readRDS(paste0(pathsaveHere, DataFileNames[2], "Trimmed.rds"))
ass <- readRDS(paste0(pathsaveHere, DataFileNames[4], "Trimmed.rds"))
lvo <- readRDS(paste0(pathsaveHere, DataFileNames[5], "Trimmed.rds"))
#NeA <- readRDS(paste0(pathsaveHere, "NetAssetsANCOVATrimmed.rds"))
NeA1R <- readRDS(paste0(pathsaveHere, "NarrowNetAssetsANCOVATrimmed.rds"))
# NeA1R2 drops (from NeA1R) 24 members in trad who were disbursed loans only twice or once
NeA1R2 <- readRDS(paste0(pathsaveHere, "NarrowNetAssetsANCOVA.rds"))
rsk <- readRDS(paste0(pathsaveHere, "RiskPreferences.rds"))
rsk2 <- rsk[, .(hhid, RiskPrefVal, TimePref1Val, TimePref2Val, PresentBias)]
if (Only800) ar <- ar[o800 == 1L, ]
#To set to the trimmed sample, set the parameter \textsf{UseTrimmedSample} to T. Here, we set to F.
UseTrimmedSample <- F
TestMedian <- F
if (!UseTrimmedSample) ar <- ar[!grepl("tw|dou", TradGroup), ]
addmargins(table0(ar[o800 == 1L & tee == 1, .(Tee, AttritIn)]))
addmargins(table0(ar[tee==1 & o800==1 & AttritIn<9, .(BStatus,AttritIn)]))
#addmargins(table(ar[mid == 1 & Time == 1, .(BStatus, Arm)]), 1:2, sum, T)
#addmargins(table(ar[mid == 1 & Time == 4, .(BStatus, Arm)]), 1:2, sum, T)
# "ar" is roster
# AttritIn is created as below in read_cleaned_data.rnw
# (465): xid[, AttritIn := 9L]
# (466): xid[grepl("^En|^2nd and 4", missing_followup), AttritIn := 4L]
# (467): xid[grepl("^3rd and 4", missing_followup), AttritIn := 3L]
# (468): xid[grepl("^2.*3.*4", missing_followup), AttritIn := 2L]
ar[, Tee := max(survey), by = hhid]
arA[, Tee := max(survey), by = hhid]
ass[, Tee := max(survey), by = hhid]
lvo[, Tee := max(survey), by = hhid]
#Correct \textsf{AttritIn} for these \Sexpr{nrow(ar[grepl("tw|dou", TradGroup) & tee == 1, ])} members. Keep only the 1st obs for all members.
#addmargins(table(ar[tee == 1 & grepl("tw|dou", TradGroup), AttritIn]))
#ar[Tee == 1 & AttritIn == 9 & grepl("tw|dou", TradGroup), AttritIn := 2L]
psas <- ass[o800 == 1 & tee == 1, 
  .(hhid, tee, NLHAssetAmount, PAssetAmount)]
pslv <- lvo[o800 == 1 & tee == 1, 
  .(hhid, tee, TotalImputedValue, NumCows)]
nne <- NeA1R[o800 == 1 & tee == 1, 
  # BroadNetValue is similar to NetValue, so drop it.
  .(hhid, tee, NetValue, #NarrowNetValue, 
  BroadNetValue
  #, RNetValue, RNarrowNetValue, RBroadNetValue
  )]
source(paste0(pathprogram, "AttritionPermutationTableHeaders5.R"))
armerge <- ar[, c("groupid", "hhid", "mid", "o800", "TradGroup", 
  "BStatus", "AttritIn", "survey", "tee", "Time", vartobetested[1:5]), with = F]
armerge[, En := 1:.N, by = .(hhid, Time)]
armerge[, Tee := .N, by = .(hhid, mid, Time)]
armerge <- armerge[En == 1 & Time == 1 & o800 == 1, ]
as <- merge(armerge, psas, by = c("hhid", "tee"), all.x = T)
asl <- merge(as, pslv, by = c("hhid", "tee"), all.x = T)
asln <- merge(asl, nne, by = c("hhid", "tee"), all.x = T)
asv <- merge(asln, rsk2, by = "hhid", all.x = T)
addmargins(table0(asv[!grepl("tw|dou", TradGroup), .(Arm, AttritIn)]))
# keep only rational respondents of risk preferences
# use tee==4 to define attrition, where tee is survey round in asset and livestock 
# while tee in roster is meeting number (must rename survey to tee)
asv[, Attrited := 0L]
asv[hhid %in% hhid[AttritIn < 9], Attrited := 1L]
addmargins(table0(asv[!grepl("tw|dou", TradGroup), .(Arm, Attrited)]))
asv[, c("Rejected", "GRejected", "IRejected") := 0L]
asv[grepl("^i.*rej", BStatus), IRejected := 1L]
asv[grepl("^g.*rej", BStatus), GRejected := 1L]
asv[IRejected == 1L | GRejected == 1L, Rejected := 1L]
asv[, Active := 1L]
asv[Attrited == 1 | Rejected == 1, Active := 0L]
asv[, CompletePanel := F]
asv[hhid %in% intersect(hhid[tee == 1 & !is.na(NetValue)], hhid[tee == 4 & !is.na(NetValue)]),
  CompletePanel := T]
saveRDS(asv, paste0(pathsaveHere, "DestatData.rds"))
