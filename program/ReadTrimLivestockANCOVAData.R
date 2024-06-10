lvo <- readRDS(paste0(pathsaveHere, DataFileNames[5], "InitialSample.rds"))
if (Only800) lvo <- lvo[o800 == 1L, ]
table0(lvo[, .(tee, Arm)])
table0(lvo[grepl("ow", LivestockCode), .(tee, Arm)])
lvo[, grepout("Loan|UD|Forced", colnames(lvo)) := NULL]
lvo[, Tee := .N, by = hhid]
lvo[, PureControl := 0L]
lvo[!grepl("borr", BStatus), PureControl := 1L]
# Inital values
IniVariables <- grepout("AssetA|HHsize|HeadL", colnames(ass0))
setkey(lvo, hhid, survey)
lvo[, paste0(IniVariables, 0) := .SD[1, ], by = hhid, .SDcols = IniVariables]
lvo[, FirstObs := 0L]
lvo[, minrd := min(survey), by = hhid][minrd == survey, FirstObs := 1L]
lvo <- lvo[FirstObs == 0L, ]
lvo[, FirstObs := NULL]
lvostrings <- "^groupid$|hhid|^Arm$|tee|^dummy[TLCMUWSNI]|creditst|^TotalIm|Floo|live.*de$|Head|Cows|PureC|BSta|00$"
lvoR <- lvo[, grepout(paste0(lvostrings, "|RM"), colnames(lvo)), with = F]
lvo <- lvo[, grepout(lvostrings, colnames(lvo)), with = F]
lvo3 <- lvo[tee == 1 | tee == 4, ]
lvoR3 <- lvoR[tee == 1 | tee == 4, ]
datas <- c("lvo", "lvoR", "lvo3", "lvoR3")
ddatas <- paste0("d", datas)
ddatasd <- paste0(ddatas, "d")
for (i in 1:length(datas)) assign(ddatasd[i], get(datas0[i]))
