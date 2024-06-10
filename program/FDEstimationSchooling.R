Scsuffixes <- c("", "g", "p", "s", "a", "T", "Tg", "Ts", "D", "Dg")
exclheader <- paste0("excl", Scsuffixes)
listheader <- paste0("sc", Scsuffixes)
or (k in 1:length(listheader)) {
  if (k >= 5 & k <= 7) 
    CovariateBoxSize <- 5 else 
    CovariateBoxSize <- 3.5
  if (k < 8) 
    DataToUse <- DataToUse1 else
    DataToUse <- DataToUse2
  # Estimate
  # 1
  assign(paste0(listheader[k], 1),
    FDestimation(dX = get(DataToUse[1]), Regressand = Regressands[1], 
      Group = "^HHMid$", TimeVar = "tee", Cluster = "groupid", 
      Exclude = paste(get(paste0(exclheader[k], ".base")), 
        get(paste0(exclheader[k], ".1")), sep = "|"),
      intercept = F, return.V = T, print.messages = T)
  )
  # 2, 3, 4
  for (j in 2:4)
    assign(paste0(listheader[k], j),
      FDestimation(get(DataToUse[j]), Regressand = Regressands[j], 
        Group = "^HHMid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], ".", j)), sep = "|"),
        intercept = F, return.V = T, print.messages = T)
    )
  # 5
  assign(paste0(listheader[k], 5),
    FDestimation(get(DataToUse[5]), Regressand = Regressands[5], 
      Group = "^HHMid$", TimeVar = "tee", Cluster = "groupid", 
      Exclude = paste(get(paste0(exclheader[k], ".base")), 
        get(paste0(exclheader[k], ".5")), sep = "|"),
      intercept = F, return.V = T, print.messages = T)
  )
  # 6, 7
  for (j in 6:7)
    assign(paste0(listheader[k], j),
      FDestimation(get(DataToUse[j]), Regressand = Regressands[j], 
        Group = "^HHMid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], ".", j)), sep = "|"),
        intercept = F, return.V = T, print.messages = T)
    )
  # Format and save
  sclist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:7, sep = "", collapse = ", "), ")")))
  sc.estlist <- lapply(sclist, "[[", "est")
  sc.estlist <- lapply(sc.estlist, function(x) x[, -3, drop = F])
  sc.N <- unlist(lapply(sclist, "[[", "N"))
  if (k < 7) {
    ttab <- lapply(sclist, "[[", "ActualTTable")
    ttab <- rbindlist(lapply(ttab, function(x) data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
    setcolorder(ttab, c("1", "2", "3"))
    ttab <- a2b.data.table(ttab, NA, 0)
    sc.T <- cbind(c("T = 2", "T = 3", "T = 4"), t(ttab))
  } else sc.T <- NULL
  sc.R <- unlist(lapply(lapply(lapply(sclist, "[[", "nonrobust"), summary), "[[", "adj.r.squared"))
  sc.tab <- tabs2latex(sc.estlist, digits = 2)
  rn <- rownames(sc.tab)
  # reorder rows
  thisEsttab <- sc.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn[rn.new]
  sc.tab <- sc.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  rn <- paste0("\\makebox[", CovariateBoxSize, "cm]{\\scriptsize\\hfill ", rn, "}")
  sc.tb <- rbind(as.matrix(cbind(covariates = rn, sc.tab)), 
    sc.T,
    c("\\bar{R}^{2}", round(sc.R, 3)),
    c("N", sc.N))
  if (k == which(grepl("^T$", Scsuffixes))) {
  # table with time interaction is too long so split into 2 tables
    sc.ltxtb1 <- latextab(sc.tb[1:(grep("hfill rd 2 - 3}$", rn)-1), ], 
      hleft = "\\scriptsize\\hfil$", hcenter = c(CovariateBoxSize, rep(1.1, ncol(sc.tb)-1)), 
      hright = "$", headercolor = "gray80", adjustlineskip = "-.6ex", 
      delimiterline= NULL, alternatecolor2 = "gray90", 
      addseparatingcols = 4, separatingcolwidth = .2,
      separatingcoltitle = c("complete panel", "all panel"))
    sc.ltxtb2 <- latextab(sc.tb[-(1:(grep("hfill rd 2 - 3}$", rn)-1)), ], 
      hleft = "\\scriptsize\\hfil$", hcenter = c(CovariateBoxSize, rep(1.1, ncol(sc.tb)-1)), 
      hright = "$", headercolor = "gray80", adjustlineskip = "-.6ex", 
      delimiterline= NULL, alternatecolor2 = "gray90", 
      addseparatingcols = 4, separatingcolwidth = .2,
      separatingcoltitle = c("complete panel", "all panel"))
    write.tablev(sc.ltxtb1, 
      paste0(pathsaveHere, "Schooling", FileNameHeader[k],
        "FDEstimationResults1.tex"), 
      colnamestrue = F)
    write.tablev(sc.ltxtb2, 
      paste0(pathsaveHere, "Schooling", FileNameHeader[k],
        "FDEstimationResults2.tex"), 
      colnamestrue = F)
  } else {
    sc.ltxtb <- latextab(sc.tb, 
      hleft = "\\scriptsize\\hfil$", 
      hcenter = c(CovariateBoxSize, rep(1.1, ncol(sc.tb)-1)), 
      hright = "$", 
      headercolor = "gray80", adjustlineskip = "-.6ex", 
      delimiterline= NULL, alternatecolor2 = "gray90", 
      addseparatingcols = 4, separatingcolwidth = .2,
      separatingcoltitle = c("complete panel", "all panel"))
    write.tablev(sc.ltxtb, 
      paste0(pathsaveHere, "Schooling", FileNameHeader[k],
        "FDEstimationResults.tex"), 
      colnamestrue = F)
  }
}
