ass0 <- readRDS(paste0(pathsaveHere, DataFileNames[4], "InitialSample.rds"))
if (Only800) ass0 <- ass0[o800 == 1L, ]
print(addmargins(table(ass0[o800 == 1L & tee == 1, .(Arm, AttritIn)])))
ass0[, grepout("Loan|UD|Forced|00|^Time$|\\.before|\\.after", colnames(ass0)) := NULL]
ass0 <- ass0[!(hhid == 7043715 & HAssetAmount == 0), ]
ass0[, Tee := .N, by = hhid]
setkey(ass0, survey, Arm)
ass0[, .(
  N = .N, NonzeroAsset = sum(AssetAmount > 0), 
  MeanAsset = mean(AssetAmount),
  MeanHAsset = mean(HAssetAmount),
  MeanPAsset = mean(PAssetAmount)
  ), by = .(survey, Arm)]
# Inital values
IniVariables <- grepout("^.AssetA|HHsize|HeadL", colnames(ass0))
setkey(ass0, hhid, survey)
ass0[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
# create PureControl
ass0[, PureControl := 0L]
ass0[!grepl("es$", creditstatus), PureControl := 1L]
ass0[, paste0("PureControl.Time", 2:4) := PureControl]
ass0[tee != 2, PureControl.Time2 := 0L]
ass0[tee != 3, PureControl.Time3 := 0L]
ass0[tee != 4, PureControl.Time4 := 0L]
ass0[, UDPureControl := PureControl]
ass0[, (paste0("UDPureControl.Time", 2:4)) := eval(parse(text=paste0("PureControl.Time", 2:4)))]
# create Arm*UltraPoor interactions (dummyArm, dummyUP are not demeaned)
# also create UDxxx for mean/std column in estimate table
FileNameForUD <- "ass0"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
# drop first period obs
#ass0[, FirstObs := 0L]
#ass0[, minrd := min(survey), by = hhid][minrd == survey, FirstObs := 1L]
#ass0 <- ass0[FirstObs == 0L, ]
#ass0[, FirstObs := NULL]
ass0 <- ass0[tee > 1, ]
ass. <- ass0[, grepout("^groupid$|hhid|[tT]ee|^dummy.*[a-z]|^.Ass.*nt$|Floo|With|.Size|Head|creditstatus$|^Arm$|PureC|BSta|Time|.*0$|RM|UD", 
  colnames(ass0)), with = F]
ass1R = copy(ass.)
ass2R = copy(ass.)
ass1R[, grepout("^PAsse.*nt$", colnames(ass.)) := NULL]
ass2R[, grepout("^HAsse.*nt$", colnames(ass.)) := NULL]
ass1 = copy(ass1R)
ass2 = copy(ass2R)
ass1[, grepout("RM", colnames(ass1)) := NULL]
ass2[, grepout("RM", colnames(ass2)) := NULL]
# Before-after style 2 time point data: In ANCOVA, only period 4 is retained. 
table0(ass0[,.(tee, survey)])
ass3 <- ass1[tee == 4, ]
ass4 <- ass2[tee == 4, ]
ass3R <- ass1R[tee == 4, ]
ass4R <- ass2R[tee == 4, ]
# data sets
datas <- paste0("ass", rep(1:4, each = 2), c("", "R"))
