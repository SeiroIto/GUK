lab <- readRDS(paste0(pathsaveHere, DataFileNames[8], "InitialSample.rds"))
far <- readRDS(paste0(pathsaveHere, DataFileNames[9], "InitialSample.rds"))
lab[, HMid := paste(hhid, mid)]
lab[, tee := survey]
# Create Demeaned and undemeaned (UD) incomes
lab[, UDpcHHLabourIncome := TotalHHLabourIncome/HHsize]
lab[, UDTotalHHLabourIncome := TotalHHLabourIncome]
far[, UDTotalRevenue := TotalRevenue]
lab[, pcHHLabourIncome := UDpcHHLabourIncome - mean(UDpcHHLabourIncome)]
lab[, TotalHHLabourIncome := UDTotalHHLabourIncome - mean(UDTotalHHLabourIncome)]
far[, TotalRevenue := UDTotalRevenue - mean(UDTotalRevenue)]
far[, grepout("Forced|^Time$|LoanY", colnames(far)) := NULL]
lab[, grepout("Forced|^Time$|LoanY", colnames(lab)) := NULL]
# get HadCows
lvo0 <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo0 <- unique(lvo0[, .(hhid, dummyHadCows)])
lab[, dummyHadCows := 0L]
lab[hhid %in% lvo0[dummyHadCows == 1L, hhid], dummyHadCows := 1L]
far[, dummyHadCows := 0L]
far[hhid %in% lvo0[dummyHadCows == 1L, hhid], dummyHadCows := 1L]
lab <- lab[, 
  grepout("groupid|HMi|hhid|tee|^dummy[A-Z]|Floo|Tim|RM|Head|^HH|pcHH|T.*H.*L|^Arm$|BSta|00$|TradG", 
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
    MeanC = mean(TotalHHLabourIncome, na.rm = T), 
    StdC = var(TotalHHLabourIncome, na.rm = T)^(.5),
    N = .N
  ), by = .(tee, Arm)][, 
    .(Arm, tee, mean = MeanC, 
      upper = MeanC + 1.96*StdC/sqrt(N),
      lower = MeanC - 1.96*StdC/sqrt(N)
      )]
labDpc <- lab[!grepl("tw|dou", TradGroup) & o800 == 1L, .(
    MeanC = mean(pcHHLabourIncome, na.rm = T), 
    StdC = var(pcHHLabourIncome, na.rm = T)^(.5),
    N = .N
  ), by = .(tee, Arm)][, 
    .(Arm, tee, mean = MeanC, 
      upper = MeanC + 1.96*StdC/sqrt(N),
      lower = MeanC - 1.96*StdC/sqrt(N)
      )]
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
