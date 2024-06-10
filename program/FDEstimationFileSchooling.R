jay <- length(Regressands)
for (k in 1:length(listheader)) {
    CovariateBoxSize <- 5
    DataToUse <- DataToUse1
# Estimate
  for (j in 1:jay)
    assign(paste0(listheader[k], j),
      FDestimation(get(DataToUse[j]), Regressand = Regressands[j], 
        Group = "^HHMid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), sep = "|"),
        intercept = F, return.V = T, print.messages = T)
    )
# Tests: Serial correlation in FD errors (Wooldridge textbook 10.71)
  fdplist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:jay, sep = "", collapse = ", "), ")")))
  fdAR <- 
    t(ARTestInFD(fdplist, sigp = .025, idvar = "HHMid", timevar= "tee") )
  if (all(is.na(fdAR))) 
    fdAR <- NULL else
    fdAR <- cbind(c("\\hat{\\rho}", "\\mbox{Pr}[\\hat{\\rho}=0]"), formatC(fdAR, 3, format = "f"))
# Format and save
  fdplist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:jay, sep = "", collapse = ", "), ")")))
  fdp.estlist <- lapply(fdplist, "[[", "est")
  fdp.estlist <- lapply(fdp.estlist, function(x) x[, -3, drop = F])
  fdp.N <- unlist(lapply(fdplist, "[[", "N"))
  if (k <= 5) {
    ttab <- lapply(fdplist, "[[", "ActualTTable")
    ttab <- rbindlist(lapply(ttab, function(x) data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
    if (ncol(ttab) == 3) {
      setcolorder(ttab, c("1", "2", "3"))
      ttab <- a2b.data.table(ttab, NA, 0)
      fdp.T <- cbind(c("T = 2", "T = 3", "T = 4"), t(ttab))
    } else if (ncol(ttab) == 2) {
      setcolorder(ttab, c("1", "2"))
      ttab <- a2b.data.table(ttab, NA, 0)
      fdp.T <- cbind(c("T = 2", "T = 3"), t(ttab))
    }
  } else fdp.T <- NULL
  fdp.R <- unlist(lapply(lapply(lapply(fdplist, "[[", "nonrobust"), summary), "[[", "adj.r.squared"))
  fdp.tab <- tabs2latex3(fdp.estlist, digits = 2)
  rn <- rownames(fdp.tab)
  thisEsttab <- fdp.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn[rn.new]
  rn0 <- rn
  fdp.tab <- fdp.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  rn <- paste0("\\makebox[3cm]{\\scriptsize\\hfill ", rn, "}")
  fdp.tb <- rbind(as.matrix(cbind(covariates = rn, fdp.tab)), 
    fdp.T,
    c("\\bar{R}^{2}", round(fdp.R, 3)),
    fdAR,
    c("N", fdp.N))
  # omit year effects
  # table with time interaction is too long so split into 2 tables
  if (k == 1) {
    fdp.ltxtb1 <- latextab(fdp.tb[1:(grep("hfill rd 3 - 4}$", rn)-1), ], 
      hleft = "\\scriptsize\\hfil$", hcenter = c(CovariateBoxSize, rep(1.1, ncol(fdp.tb)-1)), 
      hright = "$", headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
      alternatecolor2 = "gray90", 
      addseparatingcols = Addseparatingcols, separatingcolwidth = Separatingcolwidth, 
      separatingcoltitle = Separatingcoltitle, addsubcoltitlehere = length(Addseparatingcols) > 0)
    fdp.ltxtb2 <- latextab(fdp.tb[-(1:(grep("hfill rd 3 - 4}$", rn)-1)), ], 
      hleft = "\\scriptsize\\hfil$", hcenter = c(CovariateBoxSize, rep(1.1, ncol(fdp.tb)-1)), 
      hright = "$", headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
      alternatecolor2 = "gray90", 
      addseparatingcols = Addseparatingcols, separatingcolwidth = Separatingcolwidth, 
      separatingcoltitle = Separatingcoltitle, addsubcoltitlehere = length(Addseparatingcols) > 0)
    write.tablev(fdp.ltxtb1, 
      paste0(pathsaveHere, FileName, FileNameHeader[k], "FDEstimationResults1.tex")
      , colnamestrue = F)
    write.tablev(fdp.ltxtb2, 
      paste0(pathsaveHere, FileName, FileNameHeader[k], "FDEstimationResults2.tex")
      , colnamestrue = F)
  } else {
    fdp.ltxtb <- latextab(fdp.tb, 
      hleft = "\\scriptsize\\hfil$", hcenter = c(CovariateBoxSize, rep(1.1, ncol(fdp.tb)-1)), 
      hright = "$", headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
      alternatecolor2 = "gray90", 
      addseparatingcols = Addseparatingcols, separatingcolwidth = Separatingcolwidth, 
      separatingcoltitle = Separatingcoltitle, addsubcoltitlehere = length(Addseparatingcols) > 0)
    write.tablev(fdp.ltxtb, 
      paste0(pathsaveHere, FileName, FileNameHeader[k], "FDEstimationResults.tex")
      , colnamestrue = F)
  }
}
remove(list = ls(pattern = "^incl.?\\d"))
