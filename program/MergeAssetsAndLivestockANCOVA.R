ass0 <- readRDS(paste0(pathsaveHere, DataFileNames[4], "InitialSample.rds"))
ass0[, grepout("Loan|UD|Forced|00|^Time$", colnames(ass0)) := NULL]
ass0 <- ass0[!(hhid == 7043715 & NLHAssetAmount == 0), ]
ass0[, Tee := .N, by = hhid]
setkey(ass0, survey, Arm)
lvo <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo[, grepout("Loan|Forced|HadCows.dummyLarge$|HadCows.dummyLarge\\.T|HadCows.dummyLargeG|HadCows.dummyCow|Time", colnames(lvo)) := NULL]
assstrings <- "^groupid$|hhid|[tT]ee|Time|^dummy.*[a-z]$|[HP]Ass.*nt$|Floo|With|.size|Head|creditstatus$|^Arm$|BSta|Head|RM|800"
lvostrings <- "^groupid$|hhid|^Arm$|BSta|[tT]ee|^dummy[TLCMUWSNIH]|^TotalIm|^NumCows0?$|Floo|Head|RM|UD|800"
ass0 <- ass0[, grepout(assstrings, colnames(ass0)), with = F]
lvo0 <- lvo[, grepout(lvostrings, colnames(lvo)), with = F] 
# merge
#commonstrings <- "^groupid$|hhid|^Arm|tee|Floo|Time\\.?.|Head"
commoncols <- intersect(colnames(ass0), colnames(lvo0))
AL1R0 <- merge(ass0, lvo0, by = commoncols, ALl = T)
AL1R0[is.na(TotalImputedValue), TotalImputedValue := 0]
AL1R0[, TotalValue := TotalImputedValue + NLHAssetAmount + PAssetAmount]
AL1R0[, c("TotalImputedValue", "NLHAssetAmount", "PAssetAmount") := NULL]
# Inital values
IniVariables <- grepout("TotalV|HHsize|HeadL", colnames(AL1R0))
setkey(AL1R0, hhid, tee)
AL1R0[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
# create PureControl
AL1R0[, PureControl := 0L]
AL1R0[!grepl("es$", creditstatus), PureControl := 1L]
AL1R0[, paste0("PureControl.Time", 2:4) := PureControl]
AL1R0[tee != 2, PureControl.Time2 := 0L]
AL1R0[tee != 3, PureControl.Time3 := 0L]
AL1R0[tee != 4, PureControl.Time4 := 0L]
# create Arm*UltraPoor interactions (dummyArm, dummyUP are not demeaned)
# also create UDxxx for mean/std column in estimate table
FileNameForUD <- "AL1R0"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
# drop concurrent HHsize|HeadL
AL1R0[, grepout("HHsize$|HeadL$", colnames(AL1R0)) := NULL]
# drop first period for ANCOVA
AL1R <- AL1R0[tee > 1, ]
AL1R <- unique(AL1R)
if (Only800) AL1R <- AL1R[o800 == 1, ]
AL1 = copy(AL1R)
AL1[, grepout("RM", colnames(AL1)) := NULL]
AL2 <- AL1[tee == 4, ]
AL2R <- AL1R[tee == 4, ]
saveRDS(AL1, paste0(pathsave, "AL1.rds"))
saveRDS(AL1R, paste0(pathsave, "AL1R.rds"))
saveRDS(AL2, paste0(pathsave, "AL2.rds"))
saveRDS(AL2R, paste0(pathsave, "AL2R.rds"))
# data for figure
ALfig <- AL1R[, .(Arm, groupid, hhid, dummyUltraPoor, tee, TotalValue)]
setnames(ALfig, "dummyUltraPoor", "UltraPoor")
ALfig[, povertystatus := "ultra poor"]
ALfig[UltraPoor == 0L, povertystatus := "moderately poor"]
ALfig[, povertystatus := factor(povertystatus, 
  levels = c("ultra poor","moderately poor"))]
ALfig[, UltraPoor := NULL]
datas <- c(paste0("AL", 1:2), paste0("AL", 1:2, "R"))
