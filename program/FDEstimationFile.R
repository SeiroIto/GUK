gc()
jay <- length(Regressands)
GroupVar <- rep("^hhid$", jay)
if (grepl("Inc", FileName)) GroupVar <- c(rep("^HMid$", 4), rep("^hhid$", 3))
if (grepl("Sch", FileName)) GroupVar <- rep("^HHMid$", jay)
# k: index of regression tables
estlist <- vector(mode = "list", length = length(listheader))
for (k in 1:length(listheader)) {
  if (k <= 5) 
    DataToUse <- DataToUse1 else
    DataToUse <- DataToUse2
# Estimate
  for (j in 1:jay)
    assign(paste0(listheader[k], j),
      FDestimation(get(DataToUse[j]), Regressand = Regressands[j], 
        Group = GroupVar[j], TimeVar = "tee", Cluster = "^groupid$", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), sep = "|"),
        intercept = T, return.V = T, print.messages = T)
    )
# Tests: Serial correlation in FD errors (Wooldridge textbook 10.71)
  # fdplist is list of FD estimation results objects
  fdplist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:jay, sep = "", collapse = ", "), ")")))
  if (grepl("Inc", FileName)) {
    fdAR <- 
      t(ARTestInFD(fdplist[grepl("Labour", Regressands)], sigp = .025, 
      idvar = grepout(GroupVar[1], colnames(get(DataToUse[1]))), timevar= "tee") )
    fdAR <- cbind(fdAR,
      t(ARTestInFD(fdplist[grepl("Revenu", Regressands)], sigp = .025, 
      idvar = grepout(GroupVar[grep("Revenu", Regressands)[1]], 
        colnames(get(DataToUse[grep("Revenu", Regressands)[1]]))), timevar= "tee") ))
  } else
    fdAR <- 
      t(ARTestInFD(fdplist, sigp = .025, 
        idvar = grepout(GroupVar[1], colnames(get(DataToUse[1]))), timevar= "tee") )
  if (all(is.na(fdAR))) 
    fdAR <- NULL else
    fdAR <- cbind(c("\\hat{\\rho}", "\\mbox{Pr}[\\hat{\\rho}=0]"), formatC(fdAR, 3, format = "f"))
# Format and save
  fdp.estlist <- lapply(fdplist, "[[", "est")
  fdp.estlist <- lapply(fdp.estlist, function(x) x[, -3, drop = F])
  fdp.N <- unlist(lapply(fdplist, "[[", "N"))
  if (!grepl("Sav", FileName) & k <= 5) {
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
  if (grepl("School|Inco", FileName)) 
    fdp.tab <- tabs2latex3(fdp.estlist, digits = 2) else
    fdp.tab <- tabs2latex3(fdp.estlist, digits = 1)
  rn <- rownames(fdp.tab)
  thisEsttab <- fdp.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn[rn.new]
  rn0 <- rn
  fdp.tab <- fdp.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  if (grepl("Sav", FileName)) # rd (x)-(x+1) => rd x+1
    rn <- gsub("rd . - (.)", "rd \\1", rn)
  rn <- paste0("\\makebox[3cm]{\\scriptsize\\hfill ", rn, "}")
  fdp.tb <- rbind(as.matrix(cbind(covariates = rn, fdp.tab)), 
    fdp.T,
    c("\\bar{R}^{2}", round(fdp.R, 3)),
    fdAR,
    c("N", fdp.N))
  # omit year effects
  centerBox <- 1.3
  if (grepl("Sav", FileName)) centerBox <- .98
  fdp.ltxtb <- latextab(fdp.tb, 
    hleft = "\\scriptsize\\hfil$", hcenter = c(3.25, rep(centerBox, ncol(fdp.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = Addseparatingcols, separatingcolwidth = Separatingcolwidth, 
    separatingcoltitle = Separatingcoltitle, addsubcoltitlehere = length(Addseparatingcols) > 0)
  write.tablev(fdp.ltxtb, 
    paste0(pathsaveHere, FileName, FileNameHeader[k], "FDEstimationResults.tex")
    , colnamestrue = F)
  # for slides
  if (grepl("Sav", FileName)) {
    slt <- cbind(covariates = gsub("scriptsize", "tiny", rn), fdp.tab)
    slt[, 1] <- gsub("3cm", "2.5cm", slt[, 1])
    slt <- slt[-(rep(grep("fill rd [2-4]|Flood|Head|6M", rn), each = 2)+
      rep(0:1, length(grep("fill rd [2-4]|Flood|Head|6M", rn)))), ]
    addtoslt <- rbind(
      c("HH controls", rep("", 2), rep(c("", "", "\\mbox{yes}"), 3))
      ,
      c("survey round controls", "", "\\mbox{yes}", rep(c("", "\\mbox{yes}", "\\mbox{yes}"), 3))
      )
    addtoslt[, 1] <- paste0("\\makebox[2.5cm]{\\tiny\\hfill ", addtoslt[, 1], "}")
    slt <- rbind(as.matrix(slt), 
      addtoslt, 
      fdp.T,
      c("\\bar{R}^{2}", round(fdp.R, 3)),
      fdAR,
      c("N", fdp.N))
    slt <- slt[, 1:9]
    centerBox <- 1.2
    slt.ltxtb <- latextab(slt, 
      hleft = "\\tiny\\hfil$", hcenter = c(2.5, rep(centerBox, ncol(slt)-1)), hright = "$", 
      headercolor = "gray80", adjustlineskip = "-.5ex", delimiterline= NULL,
      alternatecolor2 = "gray90", 
      addseparatingcols = Addseparatingcols[-3], separatingcolwidth = Separatingcolwidth[-1], 
      separatingcoltitle = Separatingcoltitle[-4], addsubcoltitlehere = length(Addseparatingcols) > 0)
    slt.ltxtb[2, ] <- gsub("scriptsize", "tiny", slt.ltxtb[2, ])
    slt.ltxtb[3, ] <- gsub("\\\\\\\\", "", slt.ltxtb[3, ])
    write.tablev(slt.ltxtb, 
      paste0(pathsaveHere, FileName, FileNameHeader[k], "FDEstimationResults_ForSlides.tex")
      , colnamestrue = F)
  }
  # confidence interval data
  estlist[[k]] <- fdplist
}
