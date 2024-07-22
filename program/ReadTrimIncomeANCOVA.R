lab <- readRDS(paste0(pathsaveHere, DataFileNames[8], "InitialSample.rds"))
far <- readRDS(paste0(pathsaveHere, DataFileNames[9], "InitialSample.rds"))
lab[, HMid := paste(hhid, mid)]
lab[, tee := survey]
# Create Demeaned and undemeaned (UD) incomes
lab[, UDpcHHLabourIncome := TotalHHLabourIncome/HHsize]
lab[, UDTotalHHLabourIncome := TotalHHLabourIncome]
far[, UDTotalRevenue := TotalRevenue]
lab[, pcHHLabourIncome := UDpcHHLabourIncome - 
  mean(UDpcHHLabourIncome, na.rm = T)]
lab[, TotalHHLabourIncome := UDTotalHHLabourIncome - 
  mean(UDTotalHHLabourIncome, na.rm = T)]
far[, TotalRevenue := UDTotalRevenue - mean(UDTotalRevenue, na.rm = T)]
far[, grepout("Forced|^Time$|LoanY", colnames(far)) := NULL]
lab[, grepout("Forced|^Time$|LoanY", colnames(lab)) := NULL]
# get NumCows0, HadCows
# add livestock values
lvo00 <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo00[, NumCows0 := NumCowsOwnedAtRd1]
lvo0 <- unique(lvo00[, .(hhid, NumCows0, dummyHadCows)])
lvo0[is.na(NumCows0), NumCows0 := 0L]
lvo0[is.na(dummyHadCows), dummyHadCows := 0L]
lab <- merge(lab, lvo0, by = "hhid", all.x = T)
far <- merge(far, lvo0, by = "hhid", all.x = T)
# lab[, dummyHadCows := 0L]
# lab[hhid %in% lvo0[dummyHadCows == 1L, hhid], dummyHadCows := 1L]
# far[, dummyHadCows := 0L]
# far[hhid %in% lvo0[dummyHadCows == 1L, hhid], dummyHadCows := 1L]
# Following is to be done in CreateDemeanedUndemeanedInteractions.R in line 73
# # HadCows*Time interactions
# lab[, dHadCows := dummyHadCows - mean(dummyHadCows, na.rm = T)]
# lab[, dTime3 := Time.3 - mean(Time.3, na.rm = T)]
# lab[, dTime4 := Time.4 - mean(Time.4, na.rm = T)]
# dtcols <- c("dTime3", "dTime4")
# lab[, (paste0("dummyHadCows.Time", 3:4)) := lapply(.SD, function(x) 
#   dHadCows*x), .SDcols = dtcols]
# # HadCows*Arm interactions
# lab[, dHadCows := dummyHadCows - mean(dummyHadCows, na.rm = T)]
# atts0 <- c("Large", "LargeGrace", "Cattle", "LargeSize", "WithGrace", "InKind")
# atts <- paste0("dummy", atts0)
# lab[, (paste0("dummyHadCows.", atts)) := lapply(.SD, function(x) 
#   dHadCows*(x-mean(x, na.rm = T))), .SDcols = atts]
# # HadCows*Arm*Time interactions
# lab[, (paste0("dummyHadCows.", atts, ".Time3")) := lapply(.SD, function(x) 
#   dHadCows*(x-mean(x, na.rm = T))*dTime3), .SDcols = atts]
# lab[, (paste0("dummyHadCows.", atts, ".Time4")) := lapply(.SD, function(x) 
#   dHadCows*(x-mean(x, na.rm = T))*dTime4), .SDcols = atts]
# lab[, c("dHadCows", "dTime3", "dTime4") := NULL]
lab <- lab[, 
  grepout("groupid|HMi|hhid|tee|^dummy[A-Z]|Floo|Tim|RM|Head|^HH|pcHH|T.*H.*L|^Arm$|BSta|00$|TradG|NumCows0", 
  colnames(lab)), with = F]
far <- far[, 
  grepout("groupid|hhid|HMi|tee|^dummy[A-Z]|Floo|Tim|RM|Head|^HH|T.*R|^Arm$|BSta|00$", 
  colnames(far)), with = F]
# Inital values
IniVariables <- grepout("TotalHHLabourIncome|HHsize|HeadL|pcH", colnames(lab))
setkey(lab, hhid, tee)
lab[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
IniVariables <- grepout("TotalRev|HHsize|HeadL", colnames(far))
setkey(far, hhid, tee)
far[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
# create PureControl
lab[, PureControl := 0L]
lab[!grepl("borro", BStatus), PureControl := 1L]
#lvo0[!grepl("borro", BStatus), PureControl := 1L]
lab[, paste0("PureControl.Time", 2:4) := PureControl]
lab[tee != 2, PureControl.Time2 := 0L]
lab[tee != 3, PureControl.Time3 := 0L]
lab[tee != 4, PureControl.Time4 := 0L]
far[, PureControl := 0L]
far[!grepl("borro", BStatus), PureControl := 1L]
far[, paste0("PureControl.Time", 2:4) := PureControl]
far[tee != 2, PureControl.Time2 := 0L]
far[tee != 3, PureControl.Time3 := 0L]
far[tee != 4, PureControl.Time4 := 0L]
# create Arm*UltraPoor interactions (dummyArm, dummyUP are not demeaned)
# also create UDxxx for mean/std column in estimate table
FileNameForUD <- "lab"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
FileNameForUD <- "far"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
# drop concurrent HHsize|HeadL
lab[, grepout("HHsize$|HeadLiteracy$", colnames(lab)) := NULL]
far[, grepout("HHsize$|HeadLiteracy$", colnames(far)) := NULL]
if (Only800) {
  lab <- lab[o800 == 1L, ]
  far <- far[o800 == 1L, ]
}
# data for raw plot figure
labDHH <- lab[!grepl("tw|dou", TradGroup) & o800 == 1L, .(
    MeanC = mean(UDTotalHHLabourIncome, na.rm = T), 
    StdC = var(UDTotalHHLabourIncome, na.rm = T)^(.5),
    N = .N
  ), by = .(tee, Arm)][, 
    .(Arm, tee, mean = MeanC, 
      upper = MeanC + 1.96*StdC/sqrt(N),
      lower = MeanC - 1.96*StdC/sqrt(N)
      )][order(Arm, tee), ]
labDpc <- lab[!grepl("tw|dou", TradGroup) & o800 == 1L, .(
    MeanC = mean(UDpcHHLabourIncome, na.rm = T), 
    StdC = var(UDpcHHLabourIncome, na.rm = T)^(.5),
    N = .N
  ), by = .(tee, Arm)][, 
    .(Arm, tee, mean = MeanC, 
      upper = MeanC + 1.96*StdC/sqrt(N),
      lower = MeanC - 1.96*StdC/sqrt(N)
      )][order(Arm, tee), ]
labDHH[, Arm := factor(Arm, labels = armsC)]
labDpc[, Arm := factor(Arm, labels = armsC)]
saveRDS(labDHH, paste0(pathsaveHere, "HHLabourIncomeFigure.rds"))
saveRDS(labDpc, paste0(pathsaveHere, "pcHHLabourIncomeFigure.rds"))
lab[, TradGroup := NULL]
lab <- lab[tee > 1, ]
far <- far[tee > 1, ]
farR = copy(far)
labR = copy(lab)
lab[, grepout("RM", colnames(lab)) := NULL]
far[, grepout("RM", colnames(far)) := NULL]
datas <- c("lab", "labR", "far", "farR")
