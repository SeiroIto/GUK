TabNetAssets <- 
paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
 paste(TabFNAncovaTop, TabFNArm), 
 "Household assets do not include livestock. Regressions (1)-(3), (5)-(6) use only arm and calendar information. (4) and (7) use previous six month repayment and saving information which is lacking in rd 1, hence starts from rd 2.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabNarrowNetAssets <- 
paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
 paste(TabFNAncovaTop, TabFNArm), 
 "Narrow net assets = Narrow assets + net saving - debt to GUK - debts to relatives and money lenders. Narrow assets use only items observed for all 4 rounds for household assets. Household assets do not include livestock. Regressions (1)-(3), (5)-(6) use only arm and calendar information. (4) and (7) use previous six month repayment and saving information which is lacking in rd 1, hence starts from rd 2.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabNarrowNetAssetsByExperience <- 
paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
 paste(TabFNAncovaTop, TabFNArm), 
 "Narrow net assets = Narrow assets + net saving - debt to GUK - debts to relatives and money lenders. Narrow assets use only items observed for all 4 rounds for household assets. Household assets do not include livestock. Regressions (1)-(3), (5)-(6) use only arm and calendar information. (4) and (7) use previous six month repayment and saving information which is lacking in rd 1, hence starts from rd 2.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")

TabNarrowNetNLAssets <- 
paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
 paste(TabFNAncovaTop, TabFNArm), 
 "Net non livestock assets = (assets - livestock value) + net saving - debt to GUK - debts to relatives and money lenders. Assets use only items observed for all 4 rounds for household assets. \\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabRepayment <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &", 
paste(TabFNAncovaTop, TabFNRepay, TabFNArmRepay),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabShortfallRegression <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative data.}\\\\
Notes: & 1. &", 
ShortfallTabFN,
"\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabSchooling <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNArm, TabFNSch, TabFNRound234, TabFN1stCol),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabAssets <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNUP, TabFNArm, TabFNRound234, TabFN1stCol),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabAssetsRobustness <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & ANCOVA estimates. Pure control is members not receiving loans while they were put on a wait list. Sample is continuing members and replacing members of early rejecters. Household assets do not include livestock. Regressions (1)-(2), (4)-(5) use only arm and calendar information. (3) and (6) information if the household was exposed to the flood in round 1. Pure controls are households who rejected to receive a loan.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabLand <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNUP, TabFNArm, TabFNRound234, TabFN1stCol),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabLivestock <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNUP, TabFNArm, TabFNRound234),
"Regressand is \\textsf{TotalImputedValue}, a sum of all livestock holding values evaluated at respective median market prices in the same year. \\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabNumCows <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNArm, TabFNRound234),
"Sample is continuing members and replacing members of early rejecters and received loans prior to 2015 Janunary. Regressand is \\textsf{NumCows}, number of cattle holding. \\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabConsumption <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &", 
paste(TabFNAncovaTop, TabFNUP, TabFNArm),
"Consumption is annualised values. \\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabIncome <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNArm, TabFNRound234, TabFNLabour),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")

TabNetAssetsA <- 
paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
 paste(TabFNAncovaTop, TabFNAttributes), 
 "Household assets do not include livestock. Regressions (1)-(3), (5)-(6) use only arm and calendar information. (4) and (7) use previous six month repayment and saving information which is lacking in rd 1, hence starts from rd 2.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabNarrowNetAssetsA <- 
paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
 paste(TabFNAncovaTop, TabFNAttributes), 
 "Narrow net assets = Narrow assets + net saving - debt to GUK - debts to relatives and money lenders. Narrow assets use only items observed for all 4 rounds for household assets. Household assets do not include livestock. Regressions (1)-(3), (5)-(6) use only arm and calendar information. (4) and (7) use previous six month repayment and saving information which is lacking in rd 1, hence starts from rd 2.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabNarrowNetAssetsByExperienceA <- 
paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
 paste(TabFNAncovaTop, TabFNAttributes), 
 "Narrow net assets = Narrow assets + net saving - debt to GUK - debts to relatives and money lenders. Narrow assets use only items observed for all 4 rounds for household assets. Household assets do not include livestock. Regressions (1)-(3), (5)-(6) use only arm and calendar information. (4) and (7) use previous six month repayment and saving information which is lacking in rd 1, hence starts from rd 2.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabNarrowNetNLAssetsA <- 
paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
 paste(TabFNAncovaTop, TabFNAttributes), 
 "Narrow net assets = Narrow assets + net saving - debt to GUK - debts to relatives and money lenders. Narrow assets use only items observed for all 4 rounds for household assets. Household assets do not include livestock. Regressions (1)-(3), (5)-(6) use only arm and calendar information. (4) and (7) use previous six month repayment and saving information which is lacking in rd 1, hence starts from rd 2.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabRepaymentA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &", 
paste(TabFNAncovaTop, TabFNRepay, TabFNAttributesRepay),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabShortfallRegressionA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative data.}\\\\
Notes: & 1. &", 
ShortfallTabFN,
"\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabSchoolingA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNAttributes, TabFNSch, TabFNRound234, TabFN1stCol),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabAssetsA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNUP, TabFNAttributes, TabFNRound234, TabFN1stCol),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabAssetsRobustnessA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & ANCOVA estimates. Pure control is members not receiving loans while they were put on a wait list. Sample is continuing members and replacing members of early rejecters. Household assets do not include livestock. Regressions (1)-(2), (4)-(5) use only arm and calendar information. (3) and (6) information if the household was exposed to the flood in round 1. Pure controls are households who rejected to receive a loan.\\\\
& 2. & ",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabLandA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNUP, TabFNAttributes, TabFNRound234, TabFN1stCol),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabLivestockA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNUP, TabFNAttributes, TabFNRound234),
"Regressand is \\textsf{TotalImputedValue}, a sum of all livestock holding values evaluated at respective median market prices in the same year. \\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabNumCowsA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNAttributes, TabFNRound234),
"Sample is continuing members and replacing members of early rejecters and received loans prior to 2015 Janunary. Regressand is \\textsf{NumCows}, number of cattle holding. \\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabConsumptionA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &", 
paste(TabFNAncovaTop, TabFNUP, TabFNAttributes),
"Consumption is annualised values. \\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")
TabIncomeA <- paste("
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. &",
paste(TabFNAncovaTop, TabFNAttributes, TabFNRound234, TabFNLabour),
"\\\\
& 2. &",
TabFNPval,
"%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
")

# if regression uses attributes, replace table footnotes
# in TabNumCows, TabFNArm => TabFNAttributes and call it TabNumCowsA
# invisible(
#   lapply(
#     paste0("Tab", gsub("ANCOVA", "", outfilenames)), 
#     function(x) 
#       assign(paste0(x, "A"), 
#         gsub("TabFNArm", "TabFNAttributes", eval(parse(text=x)))
#         , envir = .GlobalEnv)
#     )
# )
# table contents for regressions with Arm as regressors
TabTableContents <- lapply(
  paste0("Tab", gsub("ANCOVA", "", outfilenames)), 
  function(x) eval(parse(text=x)))
# table contents for regressions with attributes as regressors
TabTableContentsA <- lapply(
  paste0("Tab", gsub("ANCOVA", "", outfilenames), "A"), 
  function(x) eval(parse(text=x)))
names(TabTableContents) <- gsub("ANCOVA", "", outfilenames) 
names(TabTableContentsA) <- gsub("ANCOVA", "", outfilenames) 
