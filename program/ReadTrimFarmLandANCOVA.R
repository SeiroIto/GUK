ass0 <- readRDS(paste0(pathsaveHere, DataFileNames[4], "InitialSample.rds"))
if (Only800) ass. <- ass0[o800 == 1L, ]
ass.[, grepout("Loan|Forced|00|^Time$|\\.before|\\.after", colnames(ass.)) := NULL]
ass. <- ass.[!(hhid == 7043715 & HAssetAmount == 0), ]
ass.[, Tee := .N, by = hhid]
setkey(ass., survey, Arm)
# create PureControl
ass.[, PureControl := 0L]
ass.[!grepl("es$", creditstatus), PureControl := 1L]
ass.[, paste0("PureControl.Time", 2:4) := PureControl]
ass.[tee != 2, PureControl.Time2 := 0L]
ass.[tee != 3, PureControl.Time3 := 0L]
ass.[tee != 4, PureControl.Time4 := 0L]
# create Arm*UltraPoor interactions (dummyArm, dummyUP are not demeaned)
# also create UDxxx for mean/std column in estimate table
FileNameForUD <- "ass."
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
# Inital values
IniVariables <- grepout("^OwnedArea$|HHsize|HeadL", colnames(ass.))
setkey(ass., hhid, Year)
ass.[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
ass.[, c("HHsize", "HeadLiteracy", grepout("^[HP]A", colnames(ass.))) := NULL]
ass. <- ass.[, grepout("^groupid$|hhid|[tT]ee|^dummy.*[a-z]|^Ow.*ea0?$|Floo|With|.Size|Head|HH|creditstatus$|^Arm$|PureC|BSta|Time|.*0$|^Year$|UD", 
  colnames(ass.)), with = F]
# drop first period obs
ass.1 <- ass.[tee > 1, ]
addmargins(table0(ass.1[,.(tee, Year)]))
