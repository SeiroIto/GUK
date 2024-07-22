#s.1x <- readRDS(paste0(pathsaveHere, "Roster", DataFileNames[1], "AdminDataUsedForEstimation.rds"))
s.1x <- readRDS(paste0(pathsaveHere, DataFileNames[1], "InitialSample.rds"))
if (Only800) s.1x <- s.1x[o800 == 1L, ]
s.1x[, Enrolled := as.numeric(Enrolled)]
for (t in 2:4){
  s.1x[, (paste0("Time.", t)) := 0L]
  s.1x[tee == t, (paste0("Time.", t)) := 1L]
}
#s1x <- s.1x[!grepl("nnn", Spattern), ]
#s1x <- s.1x[!grepl("1001", EnrollPattern), ]
s.1x[, SchObPattern := paste(as.character(tee), collapse = ""), 
by = .(hhid, mid)]
s.1x[, EnrollChar := as.character(Enrolled)]
s.1x[is.na(Enrolled), EnrollChar := "n"]
s.1x[, SchPattern := paste(EnrollChar, collapse = ""), 
by = .(hhid, mid)]
s.1x[SchObPattern == "123", SchPattern := 
  paste0(SchPattern, "n")]
s.1x[SchObPattern == "234", SchPattern := 
  paste0("n", SchPattern)]
s.1x[SchObPattern == "134", SchPattern := 
  paste0(substr(SchPattern, 1, 1), "n", substr(SchPattern, 2, 3))]
s.1x[SchObPattern == "124", SchPattern := 
  paste0(substr(SchPattern, 1, 2), "n", substr(SchPattern, 3, 3))]
s.1x[SchObPattern == "12", SchPattern := paste0(SchPattern, "nn")]
s.1x[SchObPattern == "13", SchPattern := 
  paste0(substr(SchPattern, 1, 1), "n", substr(SchPattern, 2, 2), "n")]
s.1x[SchObPattern == "14", SchPattern := 
  paste0(substr(SchPattern, 1, 1), "nn", substr(SchPattern, 2, 2))]
s.1x[SchObPattern == "24", SchPattern := 
  paste0("n", substr(SchPattern, 1, 1), "n", substr(SchPattern, 2, 2))]
s.1x[SchObPattern == "34", SchPattern := 
  paste0("nn", SchPattern)]
s.1x[SchObPattern == "1", SchPattern := 
  paste0(SchPattern, "nnn")]
s.1x[SchObPattern == "2", SchPattern := 
  paste0(0, SchPattern, "nn")]
s.1x[SchObPattern == "3", SchPattern := 
  paste0("nn", SchPattern, "n")]
s.1x[SchObPattern == "4", SchPattern := 
  paste0("nnn", SchPattern)]
table0(s.1x[grepl(".*?1.*?1.*?1", SchPattern), SchPattern])
table0(s.1x[grepl("nnn", SchPattern), SchPattern])
table0(s.1x[, tee])
# get HadCows
lvo0 <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
lvo0 <- unique(lvo0[, .(hhid, dummyHadCows)])
s.1x[, dummyHadCows := 0L]
s.1x[hhid %in% lvo0[dummyHadCows == 1L, hhid], dummyHadCows := 1L]
# Drop any string with \textsf{nnn} in \textsf{SchPattern} as it does not form a panel.
s1x <- s.1x[!grepl("nnn", SchPattern), ]
# schooling FD prepare data original HHs
schstrings <- "groupid|hhid|^mid$|sex|Eldest|Age|tee|^dummy[A-Z]|Tim|RM|HHsi|Head|Enrolled|Floo|Schoo|xid$|InKind|^Arm$|o8|BSta"
if (any(grepl("Fromxid", colnames(s1x)))) 
  s1x <- s1x[(Fromxid), grepout(schstrings, colnames(s1x)), with = F] else
  s1x <- s1x[, grepout(schstrings, colnames(s1x)), with = F]
s1x[, HHMid := paste(hhid, mid, sep = ".")]
# NAs in mid (from shk file) have no schooling info
if (any(s1x[, is.na(mid)])) {
  summary(s1x[HHMid %in% HHMid[is.na(mid)], .(hhid, mid, o800, Enrolled, BStatus)])
  s1x <- s1x[!(HHMid %in% HHMid[is.na(mid)]), ]
}
s1x[is.na(Schooling) & Age_1 >= 5 & Age_1 <= 13, Schooling := "primary0512"]
s1x[is.na(Schooling) & Age_1 >= 13 & Age_1 <= 15, Schooling := "junior1315"]
s1x[is.na(Schooling) & Age_1 >= 16 & Age_1 <= 18, Schooling := "high1618"]
s1x <- cbind(s1x, makeDummyFromFactor(s1x[, Schooling], NULL), 
  makeDummyFromFactor(s1x[, sex]))
setnames(s1x, grepout("Pri|Jun|High|Fem", colnames(s1x)), 
  c("UDdummyPrimary", "UDdummyJunior", "UDdummyHigh", "UDFemale"))
setnames(s1x, grepout("Time\\.", colnames(s1x)), 
  c("UDTime.2", "UDTime.3", "UDTime.4"))
s1x[, c("dummyPrimary", "dummyJunior", "dummyHigh", "Female", 
  "Time.2", "Time.3", "Time.4") := 
  .(UDdummyPrimary - mean(UDdummyPrimary), UDdummyJunior - mean(UDdummyJunior), 
    UDdummyHigh - mean(UDdummyHigh), UDFemale - mean(UDFemale), 
    UDTime.2 - mean(UDTime.2), UDTime.3 - mean(UDTime.3),
    UDTime.4 - mean(UDTime.4))]
# interaction with school type terms
Schdummies <- c("dummyPrimary", "dummyJunior", "dummyHigh")
# interaction with time dummies
Timedummies <- c("Time.2", "Time.3", "Time.4")
tobeinteracted1 <- c("dummyTraditional", "dummyLarge", "dummyLargeGrace", "dummyCattle")
tobeinteracted2 <- c("dummyModeratelyPoor", "dummyUltraPoor")
tobeinteracted3 <- c("dummyWithoutGrace", "dummyWithGrace")
tobeinteracted4 <- c("dummyLargeSize", "dummySmallSize")
tobeinteracted5 <- c("dummyInKind", "dummyCash")
  # tobeinteracted * Schdummies
  # tobeinteracted * Timedummies
  # Schdummies * Timedummies 
  # Schdummies * Female 
  # tobeinteracted * Female 
  # Female * Timedummies
  # tobeinteracted * Schdummies * Female
  # tobeinteracted * Female * Timedummies
  # tobeinteracted * Schdummes * Timedummies
  # tobeinteracted * Schdummes * Female * Timedummies
for (i in 1:5) {
  tobeint <- get(paste0("tobeinteracted", i))
  Schdum <- rep(Schdummies, each = length(tobeint))
  Timdum <- rep(Timedummies, each = length(tobeint))
  tobeintSchdumTimdum <- 
    combineNamesXYZ(tobeint, Schdummies, 
    #gsub("\\.", "", Timedummies), ".", pivot = "z")
    Timedummies, ".", pivot = "z")
  ForEval.tobeintSchdumTimdum <- 
    combineNamesXYZ(tobeint, Schdummies, Timedummies, "*", pivot = "z")
  tobeintSchdumFemaleTimdum <- 
    combineNamesXYZ(tobeint, paste0(Schdummies, ".Female"), 
      #gsub("\\.", "", Timedummies), ".", pivot = "z")
      Timedummies, ".", pivot = "z")
  ForEval.tobeintSchdumFemaleTimdum <- 
    combineNamesXYZ(tobeint, paste0(Schdummies, "*Female"), 
      Timedummies, "*", pivot = "z")
  for (j in 1) {
    sj <- get(paste0("s", j, "x"))
    for (k in c("Schooling", "hhid", "mid", "sex", "teeyr"))
    if (any(grepl(paste0("^", k, "$"), colnames(sj))))
      sj[, grepout(paste0("^", k, "$"), colnames(sj)) := NULL]
    # drop all time interactions terms created previously, if any
    if (any(grepl("Time[234]$", colnames(sj)))) 
      sj[, grepout("Time[234]$", colnames(sj)) := NULL]
    # tobeinteracted * Schdummies
    sj[, paste(gsub("\\.", "", tobeint), Schdum, sep = ".") := 
      eval(parse(text = 
        paste("list(", 
          paste(Schdum, tobeint, sep = "*", collapse = ",")
        , ")")))]
    # tobeinteracted * Timedummies
    sj[, paste(gsub("\\.", "", tobeint), Timdum, sep = ".") := 
      eval(parse(text = 
        paste("list(", 
          paste(Timdum, tobeint, sep = "*", collapse = ",")
        , ")")))]
    # tobeinteracted * Female 
    sj[, paste(gsub("\\.", "", tobeint), "Female", sep = ".") := 
      eval(parse(text = paste("list(", paste(tobeint, "Female", 
        sep = "*", collapse = ","), ")")))]
    # Schdummies * Timedummies 
    sj[, paste(rep(Schdummies, each = length(Timedummies)), 
      Timedummies, sep = ".") := 
      eval(parse(text = 
        paste("list(", 
          paste(rep(Schdummies, each = length(Timedummies)), 
            Timedummies, sep = "*", collapse = ",")
        , ")")))]
    # Schdummies * Female 
    sj[, paste(Schdummies, "Female", sep = ".") := 
      eval(parse(text = paste("list(", paste(Schdummies, "Female", 
        sep = "*", collapse = ","), ")")))]
    #  Female * Timedummies
    sj[, paste("Female", Timedummies, sep = ".") := 
      eval(parse(text = paste("list(", paste(Timedummies, "Female", 
        sep = "*", collapse = ","), ")")))]
    # tobeinteracted * Schdummies * Female
    sj[, paste(gsub("\\.", "", tobeint), Schdum, "Female", sep = ".") := 
      eval(parse(text = paste("list(", 
       paste(Schdum, tobeint, "Female", sep = "*", collapse = ","), 
       ")")))]
    # tobeinteracted * Female * Timedummies
    sj[, paste(gsub("\\.", "", tobeint), "Female", Timdum, sep = ".") := 
      eval(parse(text = paste("list(", 
       paste(Timdum, tobeint, "Female", sep = "*", collapse = ","), 
       ")")))]
    # tobeinteracted * Schdummes * Timedummies
    sj[, (tobeintSchdumTimdum) := 
      eval(parse(text = paste("list(", 
       paste(ForEval.tobeintSchdumTimdum, collapse = ","), 
       ")")))]
    # tobeinteracted * Schdummes * Female * Timedummies
    sj[, (tobeintSchdumFemaleTimdum) := 
      eval(parse(text = paste("list(", 
       paste(ForEval.tobeintSchdumFemaleTimdum, collapse = ","), 
       ")")))]
     assign(paste("s", j, "x"), sj)
   }
}
for (j in 1) {
  sj <- get(paste0("s", j, "x"))
  sj[, Female := UDFemale]
  sj[, dummyPrimary := UDdummyPrimary]
  sj[, dummyJunior := UDdummyJunior]
  sj[, dummyHigh := UDdummyHigh]
  sj[, Time.2 := UDTime.2]
  sj[, Time.3 := UDTime.3]
  sj[, Time.4 := UDTime.4]
  sj[, grepout("Forced|^Time$|LoanY|UD|Fromxid", colnames(sj)) := NULL]
  sjR = copy(sj) # keep RM variables in s1xR, s2xR
  sj34 = copy(sj[tee == 1 | tee == 4, ])
  sj[, grepout("RM", colnames(sj)) := NULL]
  sj34[, grepout("RM", colnames(sj34)) := NULL]
  assign(paste0("s", j, "x"), sj)
  assign(paste0("s", j, "xR"), sj)
  assign(paste0("s", j, "x34"), sj34)
}
s1x[, c("Age_1", grepout("Primary", colnames(s1x))) := NULL]
s1xR[, c("Age_1", grepout("Primary", colnames(s1xR))) := NULL]
s1x34[, c("Age_1", grepout("Primary", colnames(s1x34))) := NULL]
datas <- c(paste0("s", rep(1, each = 2), c("x", "xR")), "s1x34")
ddatas <- paste0("d", datas)
ddatasd <- paste0(ddatas, "d")
for (i in 1:length(datas)) {
# keep last period level variable...
#   dl <- prepFDData(X = get(datas[i]), 
#     Group = "^HHMid$", TimeVar = "tee", Cluster = "groupid", 
#     LevelCovariates = paste0("^dummy[A-Z].*[a-z]$|Head|",
#       "^Time\\..$|Female$|Floo|Eldest|xid$|SchPa"), 
#     drop.if.NA.in.differencing = T, LevelPeriodToKeep = "last",
#     use.var.name.for.dummy.prefix = F, print.messages = F)
   dl <- FirstDiffPanelData(X = get(datas[i]), 
     Group = "^HHMid$", TimeVar = "tee", Cluster = "groupid",
     LevelCovariates = paste0("^dummy[A-Z].*[a-z]$|Head|",
      "^Time\\..$|Female$|Floo|Eldest|xid$|Sch.*Pa|^Arm$|BSta"))
  dat <- dl$diff
  dat[, grepout("^en$|Sch.*P", colnames(dat)) := NULL]
  assign(ddatas[i], dl)
  assign(ddatasd[i], dat)
}
