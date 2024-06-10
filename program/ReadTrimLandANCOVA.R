ass0 <- readRDS(paste0(pathsaveHere, DataFileNames[4], "InitialSample.rds"))
lvo0 <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
if (Only800) ass0 <- ass0[o800 == 1L, ]
ass0[, grepout("Loan|Forced|00|^Time$|\\.before|\\.after", colnames(ass0)) := NULL]
ass0 <- ass0[!(hhid == 7043715 & NLHAssetAmount == 0), ]
ass0[, Tee := .N, by = hhid]
destat(ass0[, grepout("UD", colnames(ass0)), with = F])
destat(ass0[, grepout("^(?=^dummy)", colnames(ass0)), with = F])
setkey(ass0, survey, Arm)
# get HadCows
lvo0 <- unique(lvo0[, .(hhid, dummyHadCows)])
ass0[, dummyHadCows := 0L]
ass0[hhid %in% lvo0[dummyHadCows == 1L, hhid], dummyHadCows := 1L]
# merge imputed/filled-in land holding info
hasL <- readRDS(paste0(pathsaveHere, "LandNAFilled.rds"))
hasL <- hasL[, .(hhid, year, purchase_in_last_1_year)]
setnames(hasL, "year", "Year")
ass. <- merge(ass0, hasL, by = c("hhid", "Year"), all.x = T)
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
IniVariables <- grepout("^AmountF|HHsize|HeadL", colnames(ass.))
setkey(ass., hhid, Year)
ass.[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
ass.[, c("HHsize", "HeadLiteracy", grepout("^[HP]A", colnames(ass.))) := NULL]
ass. <- ass.[, grepout("^groupid$|hhid|[tT]ee|^dummy.*[a-z]|^Am.*ed0?$|Floo|With|.Size|Head|HH|creditstatus$|^Arm$|PureC|BSta|Time|.*0$|^Year$|UD", 
  colnames(ass.)), with = F]
# drop first period obs
ass.1 <- ass.[tee > 1, ]
addmargins(table0(ass.1[,.(tee, Year)]))
