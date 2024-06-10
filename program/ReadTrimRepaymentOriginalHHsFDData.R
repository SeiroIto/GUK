<<ReadTrimRepaymentFDData original HHs file contents>>=
ar <- readRDS(paste0(pathsaveHere, "RosterRepaymentAdminOriginalHHsDataUsedForEstimation.rds"))
ar[, Mid := 1:.N, by = .(hhid, survey)]
ar <- ar[Mid == 1, ]
ar[, Mid := NULL]
ar[, CumSave := CumNetSaving - CumRepaid]
ar[, CumEffectiveRepayment := CumNetSaving + CumRepaid]
ar[, Arm := droplevels(Arm)]
ar[, HeadLiteracy := HeadLiteracy + 0]
source("c:/migrate/R/startRbat/panel_estimator_functions.R")
setorder(ar, hhid, IntDate)
ar[, grepout("LoanY|^Time$", colnames(ar)) := NULL]
ar1 <- ar[, 
  #grepout("groupid|^hhid|tee|RArm|^dummy[A-Z]|^dummy.*[a-z]$|Time|CumRepaid$|CumE.*t$|CumNet|RMOther|RMvalue.[rN]|HeadA|HeadL|Floo|With|Size", 
  grepout("groupid|^hhid|tee|^dummy[A-Z]|^dummy.*[a-z]$|Time|CumRepaid$|CumE.*t$|CumNet|RMOther|RMvalue.[rN]|HeadA|HeadL|Floo|With|Size", 
  colnames(ar)), with = F]
ar1[, grepout("UD|[mM]issw|^Time$|Small|^Size", colnames(ar1)) := NULL]
#  hhid == 7096302, 3 have round 1 observation which corresponds to pre disbursement date. Drop their round 1 data.
# dar1 <- prepFDData(ar1[!((hhid == 7096302 & tee == 1) | (hhid == 7096303 & tee == 1)), ], 
#   Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
#   LevelCovariates = "^dumm.*[a-z]$|RAr|Floo|^Time\\..$|HeadL|HeadA|LoanY", 
#   drop.if.NA.in.differencing = T, LevelPeriodToKeep = "last",
#   use.var.name.for.dummy.prefix = F, print.messages = F)
# dar2 <- prepFDData(ar1, Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
#   LevelCovariates = "^dumm.*[a-z]$|RAr|Floo|^Time\\..$|HeadL|HeadA|LoanY", 
#   drop.if.NA.in.differencing = T, LevelPeriodToKeep = "last",
#   use.var.name.for.dummy.prefix = F, print.messages = F)
dl <- FirstDiffPanelData(X = ar1[!((hhid == 7096302 & tee == 1) | (hhid == 7096303 & tee == 1)), ], 
  Group = "^hhid$", TimeVar = "tee", Cluster = "groupid",
  LevelCovariates = "^dummy|Head|^Time\\..$|Female$|Floo|Eldest|^Arm|^cred.*s$|xid$|Sch.*Pa")
dar1d <- dl$diff
dar1d[, grepout("^en$", colnames(dar1d)) := NULL]
dl <- FirstDiffPanelData(X = ar1, 
  Group = "^hhid$", TimeVar = "tee", Cluster = "groupid",
  LevelCovariates = "^dummy|Head|^Time\\..$|Female$|Floo|Eldest|^Arm|^cred.*s$|xid$|Sch.*Pa")
dar2d <- dl$diff
dar2d[, grepout("^en$", colnames(dar2d)) := NULL]
datas <- c("dar1d", "dar2d")
for (i in 1:length(datas)) {
  dat <- get(datas[i])
  dat[, grepout("Time.?2", colnames(dat)) := NULL]
  assign(datas[i], dat)
}
dar2d[, Tee := .N, by = hhid]
table(dar2d[, .(Tee, tee)])
@
