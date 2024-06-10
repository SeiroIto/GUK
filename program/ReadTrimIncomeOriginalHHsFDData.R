#lab <-  readRDS(paste0(pathsaveHere, "RosterLabourIncomeAdminOriginalHHsDataUsedForEstimation.rds"))
#far <-  readRDS(paste0(pathsaveHere, "RosterFarmIncomeAdminOriginalHHsDataUsedForEstimation.rds"))
lab <- readRDS(paste0(pathsaveHere, DataFileNames[7], "InitialSample.rds"))
if (Only800) lab <- lab[o800 == 1L, ]
far <- readRDS(paste0(pathsaveHere, DataFileNames[8], "InitialSample.rds"))
if (Only800) far <- far[o800 == 1L, ]
lab[, HMid := paste(hhid, mid)]
lab[, tee := survey]
far[, tee := survey]
lab[, num := 1:.N, by = .(hhid, IntDate)]
lab <- lab[num == 1, ]
lab[, TotalHHLabourIncome := TotalHHLabourIncome/1000]
far[, TotalRevenue := TotalRevenue/1000]
far[, grepout("Forced|^Time$|UD|LoanY", colnames(far)) := NULL]
lab[, grepout("Forced|^Time$|UD|LoanY", colnames(lab)) := NULL]
lab <- lab[, grepout("groupid|HMi|hhid|tee|^dummy[A-Z]|Floo|Tim|RM|Head|T.*H.*L|^Arm$|BSta", 
  colnames(lab)), with = F]
far <- far[, grepout("groupid|hhid|HMi|tee|^dummy[A-Z]|Floo|Tim|RM|Head|T.*R|^Arm$|BSta", 
  colnames(far)), with = F]
farR = copy(far)
labR = copy(lab)
lab[, grepout("RM", colnames(lab)) := NULL]
far[, grepout("RM", colnames(far)) := NULL]
datas <- c("lab", "labR", "far", "farR")
ddatas <- paste0("d", datas)
ddatasd <- paste0(ddatas, "d")
for (i in 1:length(datas)) {
#   dl <- prepFDData(get(datas[i]), Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
#     LevelCovariates = "^dummy[A-Z].*[a-z]$|Floo|Head|^Time\\..$", 
#     drop.if.NA.in.differencing = T, LevelPeriodToKeep = "last",
#     use.var.name.for.dummy.prefix = F, print.messages = F)
  if (i <= 2)
  dl <- FirstDiffPanelData(get(datas[i]), 
    Group = "^HMid$", TimeVar = "tee", Cluster = "groupid",
    LevelCovariates = "^dummy|Head|^Time\\..$|Female$|Floo|Eldest|^Arm|^cred.*s$|xid$|Sch.*Pa|BSta") else
  dl <- FirstDiffPanelData(get(datas[i]), 
    Group = "^hhid$", TimeVar = "tee", Cluster = "groupid",
    LevelCovariates = "^dummy|Head|^Time\\..$|Female$|Floo|Eldest|^Arm|^cred.*s$|xid$|Sch.*Pa|BSta")
  dat <- dl$diff
  dat[, grepout("^en$|RArm", colnames(dat)) := NULL]
  # Recreate Time.4 which is dropped when kept only 1:(T-1) obs.
  dat[, grepout("Time.?2", colnames(dat)) := NULL]
  assign(ddatas[i], dl)
  assign(ddatasd[i], dat)
}
dlabRd <- dlabRd[tee > 2, ]
dfard <- dfard[tee > 2, ]
dfarRd <- dfarRd[tee > 2, ]
dlabd[, Tee := .N, by = hhid]
dfard[, Tee := .N, by = hhid]
