# this is the sample to use (July 30, 2019)
lvo0 <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
print(addmargins(table(lvo0[o800 == 1L & tee == 1, .(Arm, AttritIn)])))
# Inital values
IniVariables <- grepout("Tota|NumCows$|HHsize|HeadL", colnames(lvo0))
setkey(lvo0, hhid, survey)
lvo0[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
lvo0[, FirstObs := 0L]
lvo0[, minrd := min(survey), by = hhid][minrd == survey, FirstObs := 1L]
lvo0[, FirstObs := NULL]
# create PureControl
lvo0[, PureControl := 0L]
#lvo0[!grepl("borro", BStatus), PureControl := 1L]
lvo0[!grepl("es$", creditstatus), PureControl := 1L]
lvo0[, paste0("PureControl.Time", 2:4) := PureControl]
lvo0[tee != 2, PureControl.Time2 := 0L]
lvo0[tee != 3, PureControl.Time3 := 0L]
lvo0[tee != 4, PureControl.Time4 := 0L]
FileNameForUD <- "lvo0"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
saveRDS(lvo0, paste0(pathsaveHere, "NumCowRegData.rds"))
# drop first period for ANCOVA
lvo. <- lvo0[tee > 1, ]
lvo. <- lvo.[, grepout("^groupid$|hhid|[tT]ee|^dummy[TLCMUWSNIH]|Floo|.Size|Head|creditstatus$|^Arm$|PureC|BSta|Time|.*0$|RM|TotalIm|^NumCows0?$|UD|TradG", 
  colnames(lvo.)), with = F]
if (any(grepl("Loan|Forced", colnames(lvo.)))) 
  lvo.[, grepout("Loan|Forced", colnames(lvo.)) := NULL]
lvo.[, grepout("cowox|goat|chicken", colnames(lvo.)) := NULL]
if (Only800) lvo. <- lvo.[o800 == 1, ]
# data for raw plot figure
lvoD <- lvo0[o800 == 1 & !grepl("tw|dou", TradGroup), .(
    MeanC = mean(NumCows, na.rm = T), 
    StdC = var(NumCows, na.rm = T)^(.5),
    N = .N
  ), by = .(tee, Arm)][, 
    .(Arm, tee, N,
      mean = MeanC, 
      upper = MeanC + 1.96*StdC/sqrt(N),
      lower = MeanC - 1.96*StdC/sqrt(N)
      )]
lvoD[, Arm := factor(Arm, labels = c("traditional", "large", "large grace", "cattle"))]
saveRDS(lvoD, paste0(pathsaveHere, "NumCowsFigure.rds"))
lvoR = copy(lvo.)
lvo = copy(lvo.)
lvo[, grepout("RM", colnames(lvo)) := NULL]
print(addmargins(table0(lvo[o800==1, .(tee, NumCows)])))
lvo3 <- lvo[tee == 4, ]
lvoR3 <- lvoR[tee == 4, ]
datas <- c("lvo", "lvoR", "lvo3", "lvoR3")
