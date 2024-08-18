gc()
GroupVar <- rep("^hhid$", jay)
if (grepl("LabourInc", FileName)) GroupVar <- rep("^HMid$", jay)
if (grepl("Sch", FileName)) GroupVar <- rep("^HHMid$", jay)
if (grepl("Con.*n$", FileName)) iniperiod <- 2 else iniperiod <- 1
# k: index of regression tables
estlist <- vector(mode = "list", length = length(listheader))
FormulaList <- vector(mode = "list", length = length(listheader))
# listheader (regression specifications) for net assets
# "nea"    "neaP"   "neaa"   "neaT"   "neaTa"  "neaTP"  "neaTPa"
#  nea: net assets, P: PovertyStatus, a:Attributes,
#    T: TimeVarying, Ta: TimeVaryingAttributes,
#    TP: TimeVaryingPovertyStatus, TPa: TimeVaryingPovertyStatusAttributes
# For specification with Ta and beyond, use DataToUse2
for (k in 1:length(listheader)) {
  if (k <= grep("^Ta$", regsuffixes)) 
    DataToUse <- DataToUse1 else
    DataToUse <- DataToUse2
###_ Estimate	###_
  Formulae <- DepMean <- IniMean <- e.T <- NULL
  CovariateMean <- CovNames <- formlist <- vector(mode = "list", length = jay)
  for (j in 1:jay) {
    x = copy(get(DataToUse[j]))
    # first, drop unnecessary variables for an ease of writing reg expr #
    # exclheader: "excl"    "exclP"   "excla"   "exclT"   "exclTa"  "exclTP"  "exclTPa"
    # excl.base: Pure|Size|Poo|With|InK|Cash|Trad|Time
    # exclP.base: Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|Time
    # excla.base: Pure|Large$|Large[\\.G]|Small|Trad|yCow|Cows.C|Catt|Poo|Witho|Cash|Time
    # exclT.base: Time.?2|Pure|Size|Poo|With|InK|Cash|Trad|^Time$
    # exclTP.base: Pure|UD|Mod|Siz|Wi|InK|Cash|Tra|Time.?2|^Time$
    # exclTPa.base: Time.?2|Pure|dummy[CMST]|Large$|Large\\.|[et]Grace|Cows.C|^Time$
    exclstring <- paste(get(paste0(exclheader[k], ".base")), 
          "UD|^Tee$|teeyr|^Time$|00$|_|^Ass|status$|cy$", sep = "|")
    x <- x[, grepout(exclstring, colnames(x)) := NULL]
    # second, pick covariates #
    # For nea:
    # incl1: ^dummy[CI].*[ed]$|^dummy[LW].*[cgz]e$|dummy.*Poor$
    # incl2: incl1 + Net2?Value0$
    # incl3: incl2 + Head|Flood|HHs
    # incl4: incl3 + ^dummyHadCows
    # incl5: incl3 + NumCows0
    # incl6: incl3 + ^dummyHadCows|NumCows0
    Covariates <- colnames(x)[
      grepl(get(paste0(inclheader[k], j)), colnames(x), perl = T)
      ]
    CovNames[[j]] <- Covariates
    # third, write formula. If regression has time-varying slopes, drop intercept #
    # except for Assets, LabourIncome, FarmIncome
    # Note: grepl("Sch|Income|Con|Ass|Liv|Num|Lan|Rep", FileNames) = T, so 
    # 2nd conditioning !grepl("Sch|Income|Con|Ass|Liv|Num|Lan|Rep", FileName) is
    # always F, so intercept is used for all estimation
    if (grepl("T", regsuffixes[k]) & !grepl("Sch|Income|Con|Ass|Liv|Num|Lan|Rep", FileName)) 
      fsim <- "~ -1 +" else fsim <- "~"
    # E.g., for j == 1, NetValue ~ dummyLarge + dummyLargeGrace + dummyCattle
    Formula <- as.formula(paste(Regressands[j], fsim, 
      paste(Covariates, collapse = "+")))
    Formulae <- c(Formulae, Formula)
    # four, estimate and do inference #
    lmx <- lm(Formula, data = x)
    assign(paste0(listheader[k], j),
      if (is.null(lmx$na.action))
        list(lm = lmx, robust = clx(lmx, cluster = x[, groupid], returnV = T), data = x) else
        list(lm = lmx, robust = clx(lmx, cluster = x[-lmx$na.action, groupid], returnV = T),
          data = x[-lmx$na.action, ])
      )
    # five, calculate other statistics #
    DepMean <- c(DepMean, mean(unlist(x[, Regressands[j], with = F]), na.rm = T))
    IniMean <- c(IniMean, 
      # in Asset regression, initial dep var is not present in some specification
      if (any(grepl(paste0(Regressands[j], 0), colnames(x))) )
       mean(unlist(x[tee==iniperiod+1, paste0(Regressands[j], 0), with = F]), 
         na.rm = T) else NA)
    # six, add a column of mean(Dummy==1) for each dummy variables #
    # using the data with all covariates (typically the last regression data)
    CovariateM <- unlist(lapply(get(paste0(listheader[k], j))$
      data[, Covariates, with = F], mean))
    CovM <- matrix(c(
        rbind(
          formatC(CovariateM, digits = 3, format = "f"),
          paste0("(", 
            formatC(
              unlist(lapply(get(paste0(listheader[k], j))$data[, Covariates, with = F], 
                function(x) var(x)^(1/2))), digits = 2, format = "f")
            , ")")
        )
      ))
    rownames(CovM) <- c(
      rbind(
        names(CovariateM), 
        paste0("p$_{", names(CovariateM), "}$")
      )
    )
    CovM <- rbind("(Intercept)" = "", "p$_{(Intercept)}$" = "", CovM)
    CovariateMean[[j]] <- CovM
  } # j: end of reg spec loop
  FormulaList[[k]] <- Formulae
  if ((PrintFormulae)) {
    print0(exclheader[k])
    print0(Formulae)
  }
###_ Collect estimates ###_
  # elist: list of ANCOVA estimation results objects #
  elist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:jay, sep = "", collapse = ", "), ")")
    ))
  dataused <- lapply(elist, "[[", "data")
  e.roblist <- lapply(elist, "[[", "robust")
  e.estlist <- lapply(e.roblist, "[[", "est")
  e.estlist <- lapply(e.estlist, function(x) x[, -3, drop = F])
  # covariance matrices (not used): e.Vlist <- lapply(e.roblist, "[[", "V")
  # collect other regression info #
  e.N <- unlist(lapply(dataused, nrow))
  e.R <- unlist(lapply(lapply(lapply(elist, "[[", "lm"), summary), "[[", "adj.r.squared"))
  IniMean <- round(IniMean, dig.depmean)
  DepMean <- round(DepMean, dig.depmean)
  e.R <- formatC(e.R, digits = 3, format = "f")
  # sample T table #
    # labour income
  if (grepl("^lb", listheader[k])) {
    ttab1 <- lapply(dataused[1:3], function(z) 
      table(z[, .(tee = tee, Tee = .N), by = HMid][tee == min(tee), Tee])) 
    ttab2 <- lapply(dataused[-(1:3)], function(z) 
      table(z[, .(tee = tee, Tee = .N), by = hhid][tee == min(tee), Tee]))
    ttab <- c(ttab1, ttab2)
   } else 
    # schooling
   if (grepl("^sc", listheader[k])) {
    ttab <- lapply(dataused, function(z) 
      table(z[, .(tee = tee, Tee = .N), by = HHMid][tee == min(tee), Tee]))
   } else
    # rest of data
    ttab <- lapply(dataused, function(z) 
      table(z[, .(tee, mintee = min(tee), Tee = .N), by = hhid][tee == mintee, Tee])
      )
  ttab <- rbindlist(lapply(ttab, function(x) 
    data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
#  e.T <- Reduce(merge, ttab)
  if (!grepl("Repay", FileName) #& k <= grep("^Ta$", regsuffixes)
  ) {
    if (ncol(ttab) == 3) {
      # Sometimes ttab is cbind-ed in an unodered way.
      # names(ttab) (3, 1, 2) should be mapped to 2, 3, 1
      if (any(names(ttab) != c("1", "2", "3"))) 
        setcolorder(ttab, order(as.numeric(names(ttab)))) else 
        setcolorder(ttab, 1:3)
      ttab <- a2b.data.table(ttab, NA, 0)
      e.T <- cbind(paste("T =", 2:4), t(ttab))
    } else if (ncol(ttab) == 2) {
      setcolorder(ttab, 1:2)
      ttab <- a2b.data.table(ttab, NA, 0)
      if (!grepl("Con.*n$", FileName))
        e.T <- cbind(paste("T =", 3:4), t(ttab)) else
        e.T <- cbind(paste("T =", 2:3), t(ttab)) 
    }
  } else e.T <- NULL
  # tabulate in latex table form #
  if (grepl("School|Inco|NumCows", FileName)) 
    e.tab0 <- tabs2latex3(e.estlist, digits = 2, use.Pvalue = T, xx.yyy = T) else
    e.tab0 <- tabs2latex3(e.estlist, digits = 1, use.Pvalue = T, xx.yyy = T)
  # Cova: add mean, standard deviation column #
  if (AddMeanStdColumn) {
    e.tab0 <- cbind(e.tab0, rownum = 1:nrow(e.tab0), rowname = rownames(e.tab0))
    if (!UseRawDataForDestat)
    # use the last regression data (jay) means and stds
      e.tab <- merge(CovariateMean[[jay]], e.tab0, by = "row.names") else
    {
      # use raw data: find covariates of last regression
      x = copy(get(DataToUse[jay]))
      Covnames <- CovNames[[jay]]
      # pick binary covariates to use UDxxx (undemeaned binary variables)
      ## First, pick cardinal covariates, Second, pick other (= binary) covariates 
      Cardi <- grep(
        #paste0("ChildAge|Eldest|Head.*0|HHsize0|Flood|NumC|Total.*0$|Net.*0$|Narrow.*0$|Bro.*0$|", 
        paste0("ChildAge|Eldest|Head.*0|HHsize0|Flood|NumC|Total.*0$|Net.*0$|", 
        paste("^", paste0(unique(Regressands), "0$"), collapse = "|", sep = "")
        )
        , Covnames, value = T)
      Cova <- 
        x[, c(paste0("UD", Covnames[!(Covnames %in% Cardi)]), Cardi), with = F]
      # keep only nonNA rows
      Cova <- Cova[apply(!is.na(Cova), 1, all), ]
      # all dummies == 1 ==> max of demeaned interaction values (No: neg*neg=pos)
      # all other smaller values have zero in at least one of dummy variables
  #       for (ii in grepout("\\.", colnames(Cova)))
  #         Cova[, (ii) := as.numeric(eval(parse(text = ii)) == 
  #           max(eval(parse(text = ii))))]
      CovariateP <- rbindlist(lapply(Cova, 
        function(x) data.table(t(c(mean(x), var(x)^(1/2))))))
      CovRow <- matrix(c(rbind(
          formatC(CovariateP[, V1], digits = 3, format = "f")
          ,
          paste0("(", formatC(CovariateP[, V2], digits = 2, format = "f"), ")")
        )))
      cncr <- gsub("UD", "", colnames(Cova))
      rownames(CovRow) <- c(rbind(cncr, paste0("p$_{", cncr, "}$")))
      CovRow <- rbind("(Intercept)" = "", "p$_{(Intercept)}$" = "", CovRow)
      e.tab <- merge(CovRow, e.tab0, by = "row.names")
    }
    e.tab <- e.tab[order(e.tab[, "rownum"]), ]
    rownames(e.tab) <- e.tab[, "rowname"]
    e.tab <- e.tab[, -(ncol(e.tab)-0:1)]
    colnames(e.tab)[1:2] <- c("covariates", "mean/std")
    e.tab <- as.matrix(e.tab)
    e.tab <- e.tab[, -1]
    # add an empty column in all row-objects
    e.N <- c(nrow(Cova), e.N)
    e.R <- c("", e.R)
    if (!is.null(e.T)) e.T <- cbind(e.T[, 1, drop = F], "", e.T[, -1])
    IniMean <- c("", IniMean)
    DepMean <- c("", DepMean)
  } else e.tab <- e.tab0
  # drop the dummy estimation in the last column 
  # (Asset, Livestock, NumCows, NetAsset)
  if (AddMeanStdColumn & 
    grepl("Asset$|^Livestock|NumCow|^Con|Repay", FileName)) {
    e.tab <- e.tab[, -ncol(e.tab)]
    e.N <- e.N[-length(e.N)]
    e.R <- e.R[-length(e.R)]
    if (!is.null(e.T)) e.T <- e.T[, -ncol(e.T)]
    IniMean <- IniMean[-length(IniMean)]
    DepMean <- DepMean[-length(DepMean)]
    elist <- elist[-length(elist)]
  }
  rn <- rownames(e.tab)
  thisEsttab <- e.tab
###_ Format tables ###_
  # reorder rows: rn.new #
  source(paste0(pathprogram, 
    "ReorderingOfRowsInEstimatedResultsANCOVATable.R"))
  rn <- rn[rn.new]
  rn0 <- rn
  e.tab <- e.tab[rn.new, ]
  # substitute covariate names # paste0(pathprogram, "SubstTableANCOVA.R")
  for (i in 1:nrow(subst.tableA)) 
    rn <- gsub(subst.tableA[i, 1], subst.tableA[i, 2], rn)
  rn <- gsub("^p\\$.*", "", rn)
  rn <- paste0("\\makebox[3cm]{\\scriptsize\\hfill ", rn, "}")
  # attach bottom of table rows #
  e.tb <- rbind(as.matrix(cbind(covariates = rn, e.tab)), 
#    c("\\mbox{mean of initial value}", IniMean), 
    c("\\mbox{mean of dependent variable}", DepMean), 
    e.T,
    c("\\bar{R}^{2}", e.R), 
    c("N", e.N))
  centerBox <- 1.3
  if (grepl("Repay", FileName)) centerBox <- .88
  #### ConsumptionOLS: Drop reg spec 3. 
  ####  (first 2 cols are variable name and mean)
  if (grepl("Con.*O", FileName)) e.tb <- e.tb[, -5]
  ## Number of output tables: How many to be split into
  #    Sch with T (3 tables), NumCows, NetAssets, income (2 tables), 
  #    ByExperience (combine subsample tables after this file), others (1 table)
  if (
    grepl("Sch", FileName) & grepl("T", regsuffixes[k]) & 
     grepl("^s1$", DataToUse[k])
  )
  { # 1st if starts
    # save: divide a long table into 3 in time-varying regressions #
      e.ltxtb1 <- latextab(e.tb[1:(grep("hfill rd 3\\}$", rn)-1), ], 
        hleft = "\\scriptsize\\hfil$", 
        hcenter = c(3.25, rep(centerBox, ncol(e.tb)-1)), hright = "$", 
        headercolor = "gray80", adjustlineskip = "-.65ex", 
        delimiterline= NULL, alternatecolor2 = "gray90", 
        addseparatingcols = Addseparatingcols, 
        separatingcolwidth = Separatingcolwidth, 
        separatingcoltitle = Separatingcoltitle, 
        addsubcoltitlehere = length(Addseparatingcols) > 0)
      e.ltxtb2 <- latextab(e.tb[(grep("hfill rd 3\\}$", rn):(grep("hfill rd 4\\}$", rn)-1)), ], 
        hleft = "\\scriptsize\\hfil$", 
        hcenter = c(3.25, rep(centerBox, ncol(e.tb)-1)), hright = "$", 
        headercolor = "gray80", adjustlineskip = "-.65ex", 
        delimiterline= NULL, alternatecolor2 = "gray90", 
        addseparatingcols = Addseparatingcols, 
        separatingcolwidth = Separatingcolwidth, 
        separatingcoltitle = Separatingcoltitle, 
        addsubcoltitlehere = length(Addseparatingcols) > 0)
      e.ltxtb3 <- latextab(e.tb[grep("hfill rd 4\\}$", rn):nrow(e.tb), ], 
        hleft = "\\scriptsize\\hfil$", 
        hcenter = c(3.25, rep(centerBox, ncol(e.tb)-1)), hright = "$", 
        headercolor = "gray80", adjustlineskip = "-.65ex", 
        delimiterline= NULL, alternatecolor2 = "gray90", 
        addseparatingcols = Addseparatingcols, 
        separatingcolwidth = Separatingcolwidth, 
        separatingcoltitle = Separatingcoltitle, 
        addsubcoltitlehere = length(Addseparatingcols) > 0)
      write.tablev(e.ltxtb1, 
        paste0(pathsaveHere, FileName, 
          FileNameHeader[k], "ANCOVAEstimationResults_1.tex")
        , colnamestrue = F)
      write.tablev(e.ltxtb2, 
        paste0(pathsaveHere, FileName, 
          FileNameHeader[k], "ANCOVAEstimationResults_2.tex")
        , colnamestrue = F)
      write.tablev(e.ltxtb3, 
        paste0(pathsaveHere, FileName, 
          FileNameHeader[k], "ANCOVAEstimationResults_3.tex")
        , colnamestrue = F)
  } else if (
    # 1st if ends
     # NumCows, NetAsset, Labour incomes, Schooling with TP or TPa
    grepl("NumCows$|NetAssets$|inc|Sch", FileName) & grepl("TP", regsuffixes[k])
  )
  { # 2nd if starts
    # save: divide a long table into 2 in time-varying regressions #
      e.ltxtb1 <- latextab(e.tb[1:(grep("hfill rd 4\\}$", rn)-1), ], 
        hleft = "\\scriptsize\\hfil$", 
        hcenter = c(3.5, rep(centerBox, ncol(e.tb)-1)), hright = "$", 
        headercolor = "gray80", adjustlineskip = "-.65ex", 
        delimiterline= NULL, alternatecolor2 = "gray90", 
        addseparatingcols = Addseparatingcols, 
        separatingcolwidth = Separatingcolwidth, 
        separatingcoltitle = Separatingcoltitle, 
        addsubcoltitlehere = length(Addseparatingcols) > 0)
      e.ltxtb2 <- latextab(e.tb[grep("hfill rd 4\\}$", rn):nrow(e.tb), ], 
        hleft = "\\scriptsize\\hfil$", 
        hcenter = c(3.5, rep(centerBox, ncol(e.tb)-1)), hright = "$", 
        headercolor = "gray80", adjustlineskip = "-.65ex", 
        delimiterline= NULL, alternatecolor2 = "gray90", 
        addseparatingcols = Addseparatingcols, 
        separatingcolwidth = Separatingcolwidth, 
        separatingcoltitle = Separatingcoltitle, 
        addsubcoltitlehere = length(Addseparatingcols) > 0)
      # FileName: E.g., NetAssets
      # FileNameHeader:  "", "PovertyStatus"
      # "Attributes", "TimeVarying", "TimeVaryingAttributes" 
      # "TimeVaryingPovertyStatus", "TimeVaryingPovertyStatusAttributes"
      write.tablev(e.ltxtb1, 
        paste0(pathsaveHere, FileName, 
          FileNameHeader[k], "ANCOVAEstimationResults_1.tex")
        , colnamestrue = F)
      write.tablev(e.ltxtb2, 
        paste0(pathsaveHere, FileName, 
          FileNameHeader[k], "ANCOVAEstimationResults_2.tex")
        , colnamestrue = F)
  } else if (
    # 2nd if ends
     # livestock, net assets, num cows by cattle rearing experience: 
     # To do so,  after running ANCOVAEstimationFile3.R:
     #   unify (1), (2), ... of 3 tables into 1 table
     #   collect each (1), (2), ... and keep them to be cbind
     # Each subsample is mm=a, o, n with k = 1, ..., length(listheader)
    grepl("ByExperience", FileName) 
  ) 
  { # 3rd if starts
    assign(paste0("etb", mm, k), e.tb)
  } else 
   # 3rd if ends
  { # all other starts
    # save: other tables #
      e.ltxtb <- latextab(e.tb, 
        hleft = "\\scriptsize\\hfil$", 
        hcenter = c(3.25, rep(centerBox, ncol(e.tb)-1)), hright = "$", 
        headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
        alternatecolor2 = "gray90", 
        addseparatingcols = Addseparatingcols, 
        separatingcolwidth = Separatingcolwidth, 
        separatingcoltitle = Separatingcoltitle, 
        addsubcoltitlehere = length(Addseparatingcols) > 0)
      write.tablev(e.ltxtb, 
        paste0(pathsaveHere, FileName, 
          FileNameHeader[k], "ANCOVAEstimationResults.tex")
        , colnamestrue = F)
  } # all other ends
  # Shrink adjustlineskip for a long table if not splitting into multiple pages.
  if (grepl("^TPa$", regsuffixes[k])){
      e.ltxtb <- latextab(e.tb, 
        hleft = "\\scriptsize\\hfil$", 
        hcenter = c(3.25, rep(centerBox, ncol(e.tb)-1)), hright = "$", 
        headercolor = "gray80", adjustlineskip = "-.65ex", delimiterline= NULL,
        alternatecolor2 = "gray90", 
        addseparatingcols = Addseparatingcols, 
        separatingcolwidth = Separatingcolwidth, 
        separatingcoltitle = Separatingcoltitle, 
        addsubcoltitlehere = length(Addseparatingcols) > 0)
      write.tablev(e.ltxtb, 
        paste0(pathsaveHere, FileName, 
          FileNameHeader[k], "ANCOVAEstimationResults.tex")
        , colnamestrue = F)
  }
###_ confidence interval data ###_
  estlist[[k]] <- elist
###_ HTML table ###_
  if (CreateHTMLTable) {
    for (i in 1:nrow(subst.tableA)) 
      rn0 <- gsub(subst.tableA[i, 1], subst.tableA[i, 2], rn0)
    rn0 <- gsub("^p\\$.*", "", rn0)
    rn0 <- gsub("$\\times$", " * ", rn0)
    slt <- cbind(covariates = gsub("\\scriptsize", "", rn0), e.tab)
    #### slt <- slt[-(rep(grep("fill rd [2-4]|Flood|Head|6M", rn0), each = 2)+
    ####  rep(0:1, length(grep("fill rd [2-4]|Flood|Head|6M", rn0)))), ]
    slt <- rbind(as.matrix(slt), 
      e.T,
      c("R2", e.R),
      c("Mean of dependent variable", DepMean), 
      c("N", e.N))
    #### ConsumptionOLS: Drop reg spec 3. 
    ####  (first cols are variable names, mean/std)
    if (grepl("Con.*O", FileName)) slt <- slt[, -5]
    colnames(slt) <- c("covariates", "mean/std", 1:(ncol(slt)-2))
    if (SimpleHTMLTable) {
    ## kableExtra does not place tables correctly in Tufte
####       kt <- kbl(slt, row.names = F, align = c("l", rep("c", ncol(slt)-1)),
####         caption = paste0(FileName, ", ", FileNameHeader[k]),
####         format = "html", label = paste0(FileName, FileNameHeader[k]))
####       kt <- kable_styling(kt, full_width = FALSE, position="left")
      kt <- kable(slt, row.names = F, align = c("l", rep("c", ncol(slt)-1)),
        caption = paste0(FileName, ", ", FileNameHeader[k]),
        format = "html", label = paste0(FileName, FileNameHeader[k]))
      kt <- column_spec(kt, 1, width = "6cm; min-width:5cm;")
      kt <- column_spec(kt, 2:ncol(slt), width = "2.5cm; min-width:2.5cm;")
      assign(paste0("HTML_", FileName, FileNameHeader[k]), kt)
    } else {
      kt <- kbl(slt, 
        caption = paste0(FileName, FileNameHeader[k]),
        format = "html", table.attr = "style='width:70%;'")
      TabFN1 <- TabFNArm
      if (grepl("Attri", FileNameHeader[k])) TabFN1 <- TabFNAttributes
      if (grepl("TimeV", FileNameHeader[k])) TabFN1 <- paste(TabFN1, TabFNRound234)
      kt <- kableExtra::footnote(kt, general = paste(TabFNAncovaTop, TabFN1))
      kt <- kable_paper(kt, full_width = F)
      kt <- kable_styling(kt, fixed_thead = T)
      assign(paste0("HTML_", FileName, FileNameHeader[k]), 
        kable_classic(kt, full_width = F, html_font = "Cambria"))
    }
  }
}
names(estlist) <- listheader
# estlist[[k]][[j]][[c("lm", "robust", "data")]]
saveRDS(estlist, paste0(pathsaveHere, "ANCOVA_", FileName, ".rds"))
qsave(estlist, paste0(pathsaveHere, "ANCOVA_", FileName, ".qs"))
qsave(FormulaList, paste0(pathsaveHere, "FormulaList", FileName, ".qs"))
###_  reset switches to default, drop inclX ###_
remove(list = ls(pattern = "^incl.?\\d"))
CreateHTMLTable <- AddMeanStdColumn <- UseRawDataForDestat <- F
