TabNetAssets <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNArm)} Household assets do not include livestock. Regressions (1)-(3), (5)-(6) use only arm and calendar information. (4) and (7) use previous six month repayment and saving information which is lacking in rd 1, hence starts from rd 2.\\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabRepayment <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNRepay, TabFNArmRepay)}\\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabShortfallRegression <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative data.}\\\\
Notes: & 1. & \\Sexpr{ShortfallTabFN}\\\\
& 2. & ${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabSchooling <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNAttributes, TabFNSch, TabFNRound234, TabFN1stCol)}\\\\
& 2. & \\Sexpr{TabFNPval}%
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabAssets <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNUP, TabFNAttributes, TabFNRound234)} \\Sexpr{TabFN1stCol}\\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabAssetsRobustness <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & ANCOVA estimates. Pure control is members not receiving loans while they were put on a wait list. Sample is continuing members and replacing members of early rejecters. Household assets do not include livestock. Regressions (1)-(2), (4)-(5) use only arm and calendar information. (3) and (6) information if the household was exposed to the flood in round 1. Pure controls are households who rejected to receive a loan.\\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabLand <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNUP, TabFNAttributes, TabFNRound234)} \\Sexpr{TabFN1stCol}\\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabLivestock <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNUP, TabFNAttributes, TabFNRound234)} Regressand is \\textsf{TotalImputedValue}, a sum of all livestock holding values evaluated at respective median market prices in the same year. \\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabNumCows <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNArm, TabFNRound234)} Sample is continuing members and replacing members of early rejecters and received loans prior to 2015 Janunary. Regressand is \\textsf{NumCows}, number of cattle holding. \\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabConsumption <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNUP, TabFNAttributes)} Consumption is annualised values. \\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabIncome <- "
Source:& \\multicolumn{2}{l}{\\scriptsize Estimated with GUK administrative and survey data.}\\\\
Notes: & 1. & \\Sexpr{paste(TabFNAncovaTop, TabFNAttributes, TabFNRound234, TabFNLabour)} \\\\
& 2. & \\Sexpr{TabFNPval}
%${}^{***}$, ${}^{**}$, ${}^{*}$ indicate statistical significance at 1\\%, 5\\%, 10\\%, respetively. Standard errors are clustered at group (village) level.
"
TabTableContents <- lapply(
  paste0("Tab", gsub("ANCOVA", "", outfilenames)), 
  function(x) eval(parse(text=x)))
