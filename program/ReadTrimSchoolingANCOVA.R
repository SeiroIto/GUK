s.1x <- readRDS(paste0(pathsaveHere, DataFileNames[1], "InitialSample.rds"))
s.1x[, Enrolled := as.numeric(Enrolled)]
for (t in 2:4){
  s.1x[, (paste0("Time.", t)) := 0L]
  s.1x[tee == t, (paste0("Time.", t)) := 1L]
}
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
# Drop any string with \textsf{nnn} in \textsf{SchPattern} as it does not form a panel.
s1x <- s.1x[!grepl("nnn", SchPattern), ]
# schooling FD prepare data original HHs
schstrings <- "groupid|hhid|^mid$|sex|Eldest|Age|tee|^dummy[A-Z]|Tim|RM|HHsi|Head|Enrolled|Floo|Schoo|xid$|RArm|InKind|^Arm$|o8|BSta|TradG"
if (any(grepl("Fromxid", colnames(s1x)))) 
  s1x <- s1x[(Fromxid), grepout(schstrings, colnames(s1x)), with = F] else
  s1x <- s1x[, grepout(schstrings, colnames(s1x)), with = F]
s1x[, HHMid := paste(hhid, mid, sep = ".")]
s1x[is.na(Schooling) & Age_1 >= 5 & Age_1 <= 13, Schooling := "primary0512"]
s1x[is.na(Schooling) & Age_1 >= 13 & Age_1 <= 15, Schooling := "junior1315"]
s1x[is.na(Schooling) & Age_1 >= 16 & Age_1 <= 18, Schooling := "high1618"]
# data for raw plot figure
schD <- s1x[o800 == 1L & !grepl("tw|dou", TradGroup), .(
    MeanC = mean(Enrolled, na.rm = T), 
    StdC = var(Enrolled, na.rm = T)^(.5),
    N = .N
  ), by = .(tee, Arm, Schooling, sex)][, 
    .(Schooling, Arm, sex, tee, N, mean = MeanC, 
      upper = MeanC + 1.96*StdC/sqrt(N),
      lower = MeanC - 1.96*StdC/sqrt(N)
      )][order(Schooling, Arm, sex, tee), ]
schD[, Arm := factor(Arm, labels = c("traditional", "large", "large grace", "cattle"))]
saveRDS(schD, paste0(pathsaveHere, "SchoolingFigure.rds"))
# create demeaned interactions of school type and female
# Note that demeaned interactions are created in DataTrimmingOriginal1600Memo3.rnw in all modules. Here, we will further created demeaned interactions for school types and female. Then we will also create demeaned interactions of Arm*Ultrapoor. 
s1x <- cbind(s1x, makeDummyFromFactor(s1x[, Schooling], NULL), 
  makeDummyFromFactor(s1x[, sex]))
setnames(s1x, grepout("Pri|Jun|High|Fem", colnames(s1x)), 
   c("UDdummyPrimary", "UDdummyJunior", "UDdummyHigh", "UDFemale"))
setnames(s1x, grepout("Time\\.", colnames(s1x)), 
   c("UDTime.2", "UDTime.3", "UDTime.4"))
# Arms are not demeaned: Create (demeaned) Arm and UDArm
setnames(s1x, paste0("dummy", c(ArmsC2, "UltraPoor", Attributes[-1])), 
  paste0("UDdummy", c(ArmsC2, "UltraPoor", Attributes[-1])))
for (g in c(paste0("dummy", c(ArmsC2, "UltraPoor", Attributes[-1])))) 
  s1x[, (g) := eval(parse(text=paste0("UD", g))) - 
    mean(eval(parse(text=paste0("UD", g))))]
s1x[, c("dummyPrimary", "dummyJunior", "dummyHigh", 
  "Female", "Time.2", "Time.3", "Time.4") := 
  .(UDdummyPrimary - mean(UDdummyPrimary), 
    UDdummyJunior - mean(UDdummyJunior), 
    UDdummyHigh - mean(UDdummyHigh), 
    UDFemale - mean(UDFemale), 
    UDTime.2 - mean(UDTime.2), 
    UDTime.3 - mean(UDTime.3),
    UDTime.4 - mean(UDTime.4))]
# interaction with school type terms
Schdummies <- c("dummyPrimary", "dummyJunior", "dummyHigh")
UDSchdummies <- paste0("UD", Schdummies)
# interaction with time dummies
Timedummies <- c("Time.2", "Time.3", "Time.4")
UDTimedummies <- paste0("UD", Timedummies)
tobeinteracted1 <- c("dummyTraditional", "dummyLarge", 
  "dummyLargeGrace", "dummyCattle")
UDtobeinteracted1 <- paste0("UD", tobeinteracted1)
# tobeinteracted2 <- c("dummyModeratelyPoor", "dummyUltraPoor")
# tobeinteracted3 <- c("dummyWithoutGrace", "dummyWithGrace")
# tobeinteracted4 <- c("dummyLargeSize", "dummySmallSize")
# tobeinteracted5 <- c("dummyInKind", "dummyCash")
tobeinteracted2 <- c("dummyWithGrace", "dummyLargeSize", "dummyInKind")
UDtobeinteracted2 <- paste0("UD", tobeinteracted2)
  # <below is all demeaned>
  # tobeinteracted * Schdummies
  # tobeinteracted * Timedummies
  # Schdummies * Timedummies 
  # Schdummies * Female 
  # tobeinteracted * Female 
  # Female * Timedummies
  # Schdummies * Female * Timedummies
  # tobeinteracted * Schdummies * Female
  # tobeinteracted * Female * Timedummies
  # tobeinteracted * Schdummes * Timedummies
  # tobeinteracted * Schdummes * Female * Timedummies
for (i in 1:2) {
  tobeint <- get(paste0("tobeinteracted", i))
  Schdum <- rep(Schdummies, each = length(tobeint))
  Timdum <- rep(Timedummies, each = length(tobeint))
  UDtobeint <- paste0("UD", tobeint)
  UDSchdum <- paste0("UD", Schdum)
  UDTimdum <- paste0("UD", Timdum)
  tobeintSchdumTimdum <- 
    combineNamesXYZ(tobeint, Schdummies, 
    #gsub("\\.", "", Timedummies), ".", pivot = "z")
    Timedummies, ".", pivot = "z")
  tobeintSchdumFemaleTimdum <- 
    combineNamesXYZ(tobeint, paste0(Schdummies, ".Female"), 
      #gsub("\\.", "", Timedummies), ".", pivot = "z")
      Timedummies, ".", pivot = "z")
  ForEval.tobeintSchdumTimdum <- 
    combineNamesXYZ(tobeint, Schdummies, Timedummies, "*", pivot = "z")
  ForEval.tobeintSchdumFemaleTimdum <- 
    combineNamesXYZ(tobeint, paste0(Schdummies, "*Female"), 
      Timedummies, "*", pivot = "z")

 # Names: Only 1 UD is needed at the start
  UDtobeintSchdumTimdum <- 
    combineNamesXYZ(UDtobeint, Schdummies, 
    Timedummies, ".", pivot = "z")
  UDtobeintSchdumFemaleTimdum <- 
    combineNamesXYZ(UDtobeint, paste0(Schdummies, ".Female"), 
      Timedummies, ".", pivot = "z")
 # Eval: UD is needed at all parts
  ForEval.UDtobeintSchdumTimdum <- 
    combineNamesXYZ(UDtobeint, UDSchdummies, 
      UDTimedummies, "*", pivot = "z")
  ForEval.UDtobeintSchdumFemaleTimdum <- 
    combineNamesXYZ(UDtobeint, paste0(UDSchdummies, "*UDFemale"), 
      UDTimedummies, "*", pivot = "z")
  for (m in c("Schooling", "mid", "sex", "teeyr"))
    if (any(grepl(paste0("^", m, "$"), colnames(s1x))))
      s1x[, grepout(paste0("^", m, "$"), colnames(s1x)) := NULL]
  # drop all time interactions terms created previously, if any
  if (any(grepl("Time[234]$", colnames(s1x)))) 
    s1x[, grepout("Time[234]$", colnames(s1x)) := NULL]
  # tobeinteracted * Schdummies
  s1x[, paste(gsub("\\.", "", tobeint), Schdum, sep = ".") := 
    eval(parse(text = 
      paste("list(", 
        paste(Schdum, tobeint, sep = "*", collapse = ",")
      , ")")))]
  s1x[, paste0("UD", paste(gsub("\\.", "", tobeint), Schdum, sep = ".")) := 
    eval(parse(text = 
      paste("list(", 
        paste(UDSchdum, UDtobeint, sep = "*", collapse = ",")
      , ")")))]
  # tobeinteracted * Timedummies
  s1x[, paste(gsub("\\.", "", tobeint), Timdum, sep = ".") := 
    eval(parse(text = 
      paste("list(", 
        paste(Timdum, tobeint, sep = "*", collapse = ",")
      , ")")))]
  s1x[, paste0("UD", paste(gsub("\\.", "", tobeint), Timdum, sep = ".")) := 
    eval(parse(text = 
      paste("list(", 
        paste(UDTimdum, UDtobeint, sep = "*", collapse = ",")
      , ")")))]
  # tobeinteracted * Female 
  s1x[, paste(gsub("\\.", "", tobeint), "Female", sep = ".") := 
    eval(parse(text = paste("list(", paste(tobeint, "Female", 
      sep = "*", collapse = ","), ")")))]
  s1x[, paste0("UD", paste(gsub("\\.", "", tobeint), "Female", sep = ".")) := 
    eval(parse(text = paste("list(", paste(UDtobeint, "UDFemale", 
      sep = "*", collapse = ","), ")")))]
  # Schdummies * Timedummies 
  s1x[, paste(rep(Schdummies, each = length(Timedummies)), 
    Timedummies, sep = ".") := 
    eval(parse(text = 
      paste("list(", 
        paste(rep(Schdummies, each = length(Timedummies)), 
          Timedummies, sep = "*", collapse = ",")
      , ")")))]
  s1x[, paste0("UD", paste(rep(Schdummies, each = length(Timedummies)), 
    Timedummies, sep = ".")) := 
    eval(parse(text = 
      paste("list(", 
        paste(rep(UDSchdummies, each = length(Timedummies)), 
          UDTimedummies, sep = "*", collapse = ",")
      , ")")))]
  # Schdummies * Female 
  s1x[, paste(Schdummies, "Female", sep = ".") := 
    eval(parse(text = paste("list(", paste(Schdummies, "Female", 
      sep = "*", collapse = ","), ")")))]
  s1x[, paste0("UD", paste(Schdummies, "Female", sep = ".")) := 
    eval(parse(text = paste("list(", paste(UDSchdummies, "UDFemale", 
      sep = "*", collapse = ","), ")")))]
  #  Female * Timedummies
  s1x[, paste("Female", Timedummies, sep = ".") := 
    eval(parse(text = paste("list(", paste(Timedummies, "Female", 
      sep = "*", collapse = ","), ")")))]
  s1x[, paste("UDFemale", Timedummies, sep = ".") := 
    eval(parse(text = paste("list(", paste(UDTimedummies, "UDFemale", 
      sep = "*", collapse = ","), ")")))]
  #  Schdummies * Female * Timedummies
  s1x[, paste(rep(Schdummies, each = length(Timedummies)), 
    "Female", Timedummies, sep = ".") := 
    eval(parse(text = 
      paste("list(", 
        paste(rep(Schdummies, each = length(Timedummies)), 
          "Female", Timedummies, sep = "*", collapse = ",")
      , ")")
      ))]
  s1x[, paste0("UD", paste(rep(Schdummies, each = length(Timedummies)), 
    "Female", Timedummies, sep = ".")) := 
    eval(parse(text = 
      paste("list(", 
        paste(rep(UDSchdummies, each = length(Timedummies)), 
          "UDFemale", UDTimedummies, sep = "*", collapse = ",")
      , ")")
      ))]
  # tobeinteracted * Schdummies * Female
  s1x[, paste(gsub("\\.", "", tobeint), Schdum, "Female", sep = ".") := 
    eval(parse(text = paste("list(", 
     paste(Schdum, tobeint, "Female", sep = "*", collapse = ","), 
     ")")))]
  s1x[, paste0("UD", paste(gsub("\\.", "", tobeint), Schdum, "Female", sep = ".")) := 
    eval(parse(text = paste("list(", 
     paste(UDSchdum, UDtobeint, "UDFemale", sep = "*", collapse = ","), 
     ")")))]
  # tobeinteracted * Female * Timedummies
  s1x[, paste(gsub("\\.", "", tobeint), "Female", Timdum, sep = ".") := 
    eval(parse(text = paste("list(", 
     paste(Timdum, tobeint, "Female", sep = "*", collapse = ","), 
     ")")))]
  s1x[, paste0("UD", paste(gsub("\\.", "", tobeint), "Female", Timdum, sep = ".")) := 
    eval(parse(text = paste("list(", 
     paste(UDTimdum, UDtobeint, "UDFemale", sep = "*", collapse = ","), 
     ")")))]
  # tobeinteracted * Schdummes * Timedummies
  s1x[, (tobeintSchdumTimdum) := 
    eval(parse(text = paste("list(", 
     paste(ForEval.tobeintSchdumTimdum, collapse = ","), 
     ")")))]
  s1x[, (UDtobeintSchdumTimdum) := 
    eval(parse(text = paste("list(", 
     paste(ForEval.UDtobeintSchdumTimdum, collapse = ","), 
     ")")))]
  # tobeinteracted * Schdummes * Female * Timedummies
  s1x[, (tobeintSchdumFemaleTimdum) := 
    eval(parse(text = paste("list(", 
     paste(ForEval.tobeintSchdumFemaleTimdum, collapse = ","), 
     ")")))]
  s1x[, (UDtobeintSchdumFemaleTimdum) := 
    eval(parse(text = paste("list(", 
     paste(ForEval.UDtobeintSchdumFemaleTimdum, collapse = ","), 
     ")")))]
}
# restore dummies (with no demeaning)
s1x[, Female := UDFemale]
s1x[, dummyFemale := UDFemale]
for (g in c(paste0("dummy", c(ArmsC2, "LargeSize", "WithGrace", "InKind", 
  "Junior", "High")), paste0("Time.", 2:4)))
  s1x[, (g) := eval(parse(text=paste0("UD", g)))]
#s1x[, grepout("Forced|^Time$|LoanY|UD|Fromxid", colnames(s1x)) := NULL]
s1x[, grepout("Forced|^Time$|LoanY|Fromxid", colnames(s1x)) := NULL]
s1x[, c("Age_1", grepout("Primary", colnames(s1x))) := NULL]
# Inital values
IniVariables <- grepout("HHsize|HeadL|HeadA", colnames(s1x))
setkey(s1x, hhid, tee)
s1x[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
s1x[, Enrolled0 := .SD[1, ], by = HHMid, .SDcols = "Enrolled"]
# create PureControl
s1x[, PureControl := 0L]
s1x[!grepl("borro", BStatus), PureControl := 1L]
s1x[, paste0("PureControl.Time", 2:4) := PureControl]
s1x[tee != 2, PureControl.Time2 := 0L]
s1x[tee != 3, PureControl.Time3 := 0L]
s1x[tee != 4, PureControl.Time4 := 0L]
# create Arm*UltraPoor interactions (dummyArm, dummyUP are not demeaned)
s1x[, UDdummyFemale := Female]
s1x[, dummyFemale := Female]
for (aa in c(ArmsC2[-1], "LargeSize", "WithGrace", "InKind", 
  "Junior", "High", "Female"))
  {
    s1x[, paste0("dummy", aa, ".UltraPoor") := 
      (eval(parse(text=paste0("dummy", aa)))-
        mean(eval(parse(text=paste0("dummy", aa))), na.rm = T))
      * (dummyUltraPoor - mean(dummyUltraPoor))]
    s1x[, paste0("UDdummy", aa, ".UltraPoor") := 
      eval(parse(text=paste0("UDdummy", aa))) * UDdummyUltraPoor]
  }
s1x[, dummyFemale := NULL]
# rename interaction terms: Arm.Time.X, Female.Time.X => Arm.TimeX, Female.TimeX
colnames(s1x) <- gsub("(\\.Time).([1-4])", ".Time\\2", colnames(s1x), perl = T)
# colnames(s1x) <- gsub("UDdummyFemale.UltraPoor", "dummyFemale.UltraPoor", colnames(s1x))
if (Only800) s1x <- s1x[o800 == 1L, ]
s1 <- s1x[tee > 1, ]

