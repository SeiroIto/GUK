#ass <-  readRDS(paste0(pathsaveHere, "RosterAssetAdminOriginalHHsDataUsedForEstimation.rds"))
ass <- readRDS(paste0(pathsaveHere, DataFileNames[4], "InitialSample.rds"))
if (Only800) ass <- ass[o800 == 1L, ]
ass[, grepout("Loan|UD|Forced", colnames(ass)) := NULL]
ass <- ass[!(hhid == 7043715 & HAssetAmount == 0), ]
ass1 <- ass[, grepout("^groupid$|hhid|tee|^dummy.*[a-z]$|^HAsse|Floo|Time\\.?.|With|.Size|Head|creditstatus$|^Arm$|BSta", 
  colnames(ass)), with = F]
ass1R <- ass[, grepout("^groupid$|hhid|tee|^dummy.*[a-z]$|^HAsse|Floo|Time\\.?.|RM|With|.Size|Head|creditstatus$|^Arm$|BSta", 
  colnames(ass)), with = F]
ass2 <- ass[, grepout("^groupid$|hhid|tee|^dummy.*[a-z]$|^PAsse|Floo|Time\\.?.|With|.Size|Head|creditstatus$|^Arm$|BSta", 
  colnames(ass)), with = F]
ass2R <- ass[, grepout("^groupid$|hhid|tee|^dummy.*[a-z]$|^PAsse|Floo|Time\\.?.|RM|With|.Size|Head|creditstatus$|^Arm$|BSta", 
  colnames(ass)), with = F]
# before-after style 2 time point data. Choose tee == 2 as baseline because there are many zeros in tee == 1.
ass <- readRDS(paste0(pathsaveHere, DataFileNames[4], "InitialSample.rds"))
if (Only800) ass <- ass[o800 == 1L, ]
ass <- ass[!(hhid == 7043715 & HAssetAmount == 0), ]
ass3 <- ass[(tee == 2 | tee == 4), 
  grepout("^groupid$|hhid|tee|^dummy.*[a-z]$|^HAsse|Floo|With|.Size|Head|^Arm$|BSta", colnames(ass)), with = F]
ass3R <- ass[(tee == 2 | tee == 4), 
  grepout("^groupid$|hhid|tee|^dum.*[a-z]$|^HAs|Floo|RM|With|.Size|Head|^Arm$|BSta", colnames(ass)), with = F]
ass4 <- ass[(tee == 2 | tee == 4), 
  grepout("^groupid$|hhid|tee|^dummy.*[a-z]$|^PAsse|Floo|With|.Size|Head|^Arm$|BSta", colnames(ass)), with = F]
ass4R <- ass[(tee == 2 | tee == 4), 
  grepout("^groupid$|hhid|tee|^dum.*[a-z]$|^PAs|Floo|RM|With|.Size|Head|^Arm$|BSta", colnames(ass)), with = F]
datas0 <- paste0("ass", rep(1:4, each = 2), c("", "R"))
datas <- paste0("as", rep(1:4, each = 2), c("", "R"))
ddatas <- paste0("d", datas)
ddatasd <- paste0(ddatas, "d")
for (i in 1:length(datas)) {
#   dl <- prepFDData(get(datas0[i]), Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
#     LevelCovariates = "^dummy[A-Z].*[a-z]$|Floo|^Time\\..$|Head", 
#     drop.if.NA.in.differencing = T, LevelPeriodToKeep = "last",
#     use.var.name.for.dummy.prefix = F, print.messages = F)
  dl <- FirstDiffPanelData(X = get(datas0[i]), 
    Group = "^hhid$", TimeVar = "tee", Cluster = "groupid",
    LevelCovariates = "^dummy|Head|^Time\\..$|Female$|Floo|Eldest|^Arm$|^cred.*s$|xid$|Sch.*Pa|BSta")
  dat <- dl$diff
  dat[, grepout("^en$", colnames(dat)) := NULL]
  # create PureControl*Time2, Time3 interactions and drop creditstatus
  if (grepl("ass[12]", datas0[i]) & any(grepl("creditstatus$", colnames(dat)))) {
    dat[, PureControl := 0L]
    dat[!grepl("es$", creditstatus), PureControl := 1L]
    dat[, creditstatus := NULL]
    # This is wrong: asset data has rd 2-4, so drop time 2 dummies and interactions
    dat[, c("PureControl.Time2", "PureControl.Time3", "PureControl.Time4") := 
      .(PureControl * Time.2, PureControl * Time.3, PureControl * Time.4)]
  }
  # Recreate Time.4 which is dropped when kept only 1:(T-1) obs.
  dat[, grepout("Time.?2", colnames(dat)) := NULL]
  assign(ddatas[i], dl)
  assign(ddatasd[i], dat)
}
# use only rd 2-4 for dasXRd: So after FD, tee == 3 or 4
das1Rd <- das1Rd[tee > 2, ]
das2Rd <- das2Rd[tee > 2, ]
das1d[, Tee := .N, by = hhid]
das2d[, Tee := .N, by = hhid]
