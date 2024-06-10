ass <-  readRDS(paste0(pathsaveHere, "AssetAdminDataUsedForEstimation.rds"))
# creaditstatus != yes are pure controls
table0(ass[survey == 1,.(BorrowerStatus, creditstatus)])
table0(ass[survey == 1,.(Mstatus, creditstatus)])
ass[, grepout("Loan|UD|Forced", colnames(ass)) := NULL]
CovStrings <- "^groupid$|hhid|tee|^dummy.*[a-z]$|Floo|Time\\.?.|With|.Size|Head|^creditstatus$|"
ass <- ass[!(hhid == 7043715 & HAssetAmount == 0), ]
ass1 <- ass[, grepout(paste0(CovStrings, "^HAsse"), colnames(ass)), with = F]
ass1R <- ass[, grepout(paste0(CovStrings, "^HAsse|RM"), colnames(ass)), with = F]
ass2 <- ass[, grepout(paste0(CovStrings, "^PAsse"), colnames(ass)), with = F]
ass2R <- ass[, grepout(paste0(CovStrings, "^PAsse|RM"), colnames(ass)), with = F]
# before-after style 2 time point data. Choose tee == 2 as baseline because there are many zeros in tee == 1.
ass <-  readRDS(paste0(pathsaveHere, "AssetAdminDataUsedForEstimation.rds"))
ass <- ass[!(hhid == 7043715 & HAssetAmount == 0), ]
ass[, grepout("Time|Loan", colnames(ass)) := NULL]
ass3 <- ass[tee == 2 | tee == 4, grepout(paste0(CovStrings, "^HAsse"), colnames(ass)), with = F]
ass3R <- ass[tee == 2 | tee == 4, grepout(paste0(CovStrings, "^HAsse|RM"), colnames(ass)), with = F]
ass4 <- ass[tee == 2 | tee == 4, grepout(paste0(CovStrings, "^PAsse"), colnames(ass)), with = F]
ass4R <- ass[tee == 2 | tee == 4, grepout(paste0(CovStrings, "^PAsse|RM"), colnames(ass)), with = F]
datas0 <- paste0("ass", rep(1:4, each = 2), c("", "R"))
datas <- paste0("as", rep(1:4, each = 2), c("", "R"))
ddatas <- paste0("d", datas)
ddatasd <- paste0(ddatas, "d")
for (i in 1:length(datas)) {
#   dl <- prepFDData(get(datas0[i]), Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
#     # before considering pure control contrast
#     #LevelCovariates = "^dummy|Floo|^Time\\..$|Head", 
#     # after considering pure control contrast
#     LevelCovariates = "^dummy|Floo|^Time\\..$|Head|^cred.*s$", 
#     drop.if.NA.in.differencing = T, LevelPeriodToKeep = "last",
#     use.var.name.for.dummy.prefix = F, print.messages = F)
   dl <- FirstDiffPanelData(X = get(datas0[i]), 
     Group = "^hhid$", TimeVar = "tee", Cluster = "groupid",
     LevelCovariates = "^dummy|Head|^Time\\..$|Female$|Floo|Eldest|^cred.*s$|xid$|SchPa")
  dat <- dl$diff
  dat[, grepout("^en$", colnames(dat)) := NULL]
  # create PureControl*Time2, Time3 interactions and drop creditstatus
  if (grepl("ass[12]", datas0[i]) & any(grepl("cred.*s$", colnames(dat)))) {
    dat[, PureControl := 0L]
    dat[!grepl("es$", creditstatus), PureControl := 1L]
    dat[, creditstatus := NULL]
    # asset data has rd 2-4, so drop time 2 dummies and interactions
    dat[, c("PureControl.Time3", "PureControl.Time4") := 
      .(PureControl * Time.3, PureControl * Time.4)]
  }
  assign(ddatas[i], dl)
  assign(ddatasd[i], dat)
}
das1Rd <- das1Rd[tee > 2, ]
das2Rd <- das2Rd[tee > 2, ]
das1d[, Tee := .N, by = hhid]
das2d[, Tee := .N, by = hhid]
