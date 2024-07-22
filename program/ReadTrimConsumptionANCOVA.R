# consumption is recomputed as annual quantity in ImpactEstimationOriginal1600Memo2_body1.rnw  
# of ReadFilesMergeAdminRoster.rnw and 
# saved as paste0(pathsaveHere, DataFileNames[10], "InitialSample.rds")
# ImpactEstimationOriginal1600Memo2_body1.rnw reads 
# ZB <- readRDS(paste0(path1234, "data_read_in_a_list_with_baseline_patched.rds"))
# and con <- ZB[[grep("hh.con", names(ZB))]]
# where ZB is from read_cleaned_data.rnw(2226): 
# saveRDS(ZB, paste0(path1234, "data_read_in_a_list_with_baseline_patched.rds"))

con <- readRDS(paste0(pathsaveHere, DataFileNames[10], "InitialSample.rds"))
con[, ConsumptionBaseline := 0L]
con[as.Date(IntDate) < as.Date(DisDate1), ConsumptionBaseline := 1L]
con[, ConsumptionBaseline := as.integer(any(ConsumptionBaseline == 1L)), 
  by = hhid]
#Number of HHs with consumption before the loan is disbursed (\textsf{ConsumptionBaseline} == 1) is small.
table(con[, .(Arm, ConsumptionBaseline)])
# tee of consumption is survey round - 1
con[, tee := tee + 1]
con <- con[, grepout("groupid|hhid|tee|^dummy[A-Z]|Floo|Tim|Size|With|Poo|RM|Expen|Head|HH|^Arm$|BSta|00|TradG", 
  colnames(con)), with = F]
expcol <- grepout("Exp", colnames(con))
con[, paste0("PC", expcol) := .SD/HHsize, .SDcols = expcol]
pcexpcol <- grepout("PC", colnames(con))
con[, c("PCExpenditure", "TotalExpenditure") := 
  .(eval(parse(text=paste(pcexpcol, collapse = "+"))), 
    eval(parse(text=paste(expcol, collapse = "+"))))]
con[, grepout("Loan|UD|Ener|Soc|^Hygi|^Time$", colnames(con)) := NULL]
# get HadCows
lvo0 <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo0 <- unique(lvo0[, .(hhid, dummyHadCows)])
con[, dummyHadCows := 0L]
con[hhid %in% lvo0[dummyHadCows == 1L, hhid], dummyHadCows := 1L]
# Inital values
IniVariables <- grepout("^PCExpenditure|PCFoodExpenditure|TotalE|HHsize|HeadL", 
  colnames(con))
setkey(con, hhid, tee)
con[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
# create PureControl
con[, PureControl := 0L]
con[!grepl("borro", BStatus), PureControl := 1L]
con[, paste0("PureControl.Time", 2:4) := PureControl]
con[tee != 2, PureControl.Time2 := 0L]
con[tee != 3, PureControl.Time3 := 0L]
con[tee != 4, PureControl.Time4 := 0L]
# create Arm*UltraPoor interactions (dummyArm, dummyUP are not demeaned)
# also create UDxxx for mean/std column in estimate table
FileNameForUD <- "con"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
# drop concurrent HHsize, HeadLiteracy
con[, grepout("HHsize$|HeadLiteracy$", colnames(con)) := NULL]
conD <- con[!grepl("tw|dou", TradGroup), .(
    MeanC = mean(PCExpenditure, na.rm = T), 
    StdC = var(PCExpenditure, na.rm = T)^(.5),
    N = .N
  ), by = .(tee, Arm)][, 
    .(Arm, tee, mean = MeanC, 
      upper = MeanC + 1.96*StdC/sqrt(N),
      lower = MeanC - 1.96*StdC/sqrt(N)
      )]
conD[, Arm := factor(Arm, labels = c("traditional", "large", "large grace", "cattle"))]
saveRDS(conD, paste0(pathsaveHere, "ConsumptionFigure.rds"))
conOLS = copy(con)
con <- con[tee > 2 & !grepl("tw|dou", TradGroup), ]
con[, TradGroup := NULL]
if (Only800) con <- con[o800 == 1, ]
conR = copy(con)
con[, grepout("RM", colnames(con)) := NULL]
datas <- c("con", "conR")
