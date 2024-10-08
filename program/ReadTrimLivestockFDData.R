#lvo <-  readRDS(paste0(pathsaveHere, "LivestockAdminDataUsedForEstimation.rds"))
lvo <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
if (Only800) lvo <- lvo[o800 == 1L, ]
table0(lvo[, .(tee, Arm)])
table0(lvo[grepl("ow", LivestockCode), .(tee, Arm)])
# xid <- readRDS(paste0(path1234, "ID.rds"))
# xidlv <- xid[,.(Mstatus, AssignOriginal, groupid, hhid, survey, year)]
# setnames(xidlv, "AssignOriginal", "Arm")
# setkey(lvo, Arm, groupid, hhid, survey, Mstatus)
# setkey(xidlv, Arm, groupid, hhid, survey, Mstatus)
# lvo <- merge(lvo, xidlv, by = key(xidlv), all = T)
lvo[, grepout("Loan|UD|Forced", colnames(lvo)) := NULL]
lvostrings <- "^groupid$|hhid|^Arm$|tee|^dummy[TLCMUWSNI]|creditst|^TotalIm|Floo|Time\\.|live.*de$|Head|Cows|BSta"
lvoR <- lvo[, grepout(paste0(lvostrings, "|RM"), colnames(lvo)), with = F]
lvo <- lvo[, grepout(lvostrings, colnames(lvo)), with = F]
lvo3 <- lvo[tee == 2 | tee == 4, ]
lvoR3 <- lvoR[tee == 2 | tee == 4, ]
datas <- c("lvo", "lvoR", "lvo3", "lvoR3")
ddatas <- paste0("d", datas)
ddatasd <- paste0(ddatas, "d")
for (i in 1:length(datas)) {
#   dl <- prepFDData(get(datas[i]), Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
#     LevelCovariates = "^dummy|^Arm$|Floo|^Time\\..$|Head|Cows|liv.*de$|credits", 
#     drop.if.NA.in.differencing = T, LevelPeriodToKeep = "last",
#     use.var.name.for.dummy.prefix = F, print.messages = F)
   dl <- FirstDiffPanelData(X = get(datas[i]), 
     Group = "^hhid$", TimeVar = "tee", Cluster = "groupid",
     LevelCovariates = "^dummy|^Arm$|Floo|^Time\\..$|Head|Cows|liv.*de$|credits|xid$|Sch.*Pa|HadC|BSta")
  if (i == 1) print(addmargins(table0(dl$diff[tee==2, .(BStatus, Arm)]), 1:2, sum, T))
  dat <- dl$diff
  dat[, grepout("^en$", colnames(dat)) := NULL]
  assign(ddatas[i], dl)
  assign(ddatasd[i], dat)
}
dlvoRd <- dlvoRd[tee > 1, ]
