# this is the sample to use (July 30, 2019)
lvo0 <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
#### print(addmargins(table(lvo0[o800 == 1L & tee == 1, .(Arm, AttritIn)])))
# get net asset values
NeAE1 <- readRDS(paste0(pathsaveHere, "NetAssetsExperienceRegData.rds"))
nev <- unique(NeAE1[, .(hhid, NetValue0)])
setkey(nev, hhid); setkey(lvo0, hhid)
lvo0 <- nev[lvo0]
# livestock rearing experience from rd 1 survey
lvLv <- readRDS(paste0(pathsaveHere, "LivestockExperienceAtBaselineCorrected.rds"))
lvLv[, c("NumCows0", "o800") := NULL]
setnames(lvLv, "LeaseInCattle", "AdiCattle")
setkey(lvLv, hhid); setkey(NeA1R, hhid)
lvo1 <- lvLv[lvo0]
# Inital values
IniVariables <- grepout("Tota|HHsize|HeadL|NumCows$|OwnCattle|AdiCattle|TotalImpu",
  colnames(lvo1))
setkey(lvo1, hhid, survey)
lvo1[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
lvo1[, FirstObs := 0L]
lvo1[, minrd := min(survey), by = hhid][minrd == survey, FirstObs := 1L]
lvo1[, FirstObs := NULL]
# create PureControl
lvo1[, PureControl := 0L]
#lvo1[!grepl("borro", BStatus), PureControl := 1L]
lvo1[!grepl("es$", creditstatus), PureControl := 1L]
lvo1[, paste0("PureControl.Time", 2:4) := PureControl]
lvo1[tee != 2, PureControl.Time2 := 0L]
lvo1[tee != 3, PureControl.Time3 := 0L]
lvo1[tee != 4, PureControl.Time4 := 0L]
FileNameForUD <- "lvo1"
source(paste0(pathprogram, "CreateDemeanedUndemeanedInteractions.R"))
saveRDS(lvo1, paste0(pathsaveHere, "NumCowRegData.rds"))
# drop first period for ANCOVA
lvo. <- lvo1[tee > 1, ]
lvstE <- lvo.[, grepout("^groupid$|hhid|[tT]ee|^dummy[TLCMUWSNIH]|Floo|.Size|Head|creditstatus$|^Arm$|PureC|BSta|Time|.*0$|[in]Catt|RM|^NetV|TotalIm|^NumCows0?$|UD", 
  colnames(lvo.)), with = F]
if (any(grepl("Loan|Forced", colnames(lvstE)))) 
  lvstE[, grepout("Loan|Forced", colnames(lvstE)) := NULL]
lvstE[, grepout("cowox|goat|chicken", colnames(lvstE)) := NULL]
saveRDS(lvstE, paste0(pathsaveHere, "NumCowAllMemberRegData.rds"))
if (Only800) lvoE <- lvstE[o800 == 1, ]
lvoER = copy(lvoE)
lvoE[, grepout("RM", colnames(lvoE)) := NULL]
if (CreateHTMLTable)
  knitr::kable(addmargins(table0(lvoE[o800==1, .(tee, NumCows)])),
    caption = "Number of cattle holding by survey round")
lvo3E <- lvoE[tee == 4, ]
lvoER3 <- lvoER[tee == 4, ]
