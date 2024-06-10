lbsuffixes <- c("", "g", "p", "s")
listheader <- paste0("lb", lbsuffixes)
exclheader <- paste0("excl", lbsuffixes)
Regressands <- c(rep("TotalHHLabourIncome", 4), rep("TotalRevenue", 3))
DataToUse <- c(rep("dlabdh", 3), "dlabRdh", rep("dfard", 2), "dfarRd")
hhs <- fread(paste0(pathsave, "rd1_roster_summary.prn"))
hhs[, grepout("num[B-Z]|^head", colnames(hhs)) := NULL]
hhs <- hhs[!duplicated(hhs), ]
dlabdh <- merge(dlabd, hhs, by = "hhid", all.x = T)
dlabRdh <- merge(dlabRd, hhs, by = "hhid", all.x = T)
for (k in 1:length(listheader)) {
  # Estimate
  for (j in 1:4) {
    # Select households with more than 2 adults
    thisdata <- get(DataToUse[j])
    ThisDataA <- thisdata[numAdults <= 2, ]
    ThisDataB <- thisdata[numAdults > 2, ]
    ThisDataA[, numAdults := NULL]
    ThisDataB[, numAdults := NULL]
    assign(paste0(listheader[k], "A", j),
      FDestimation(ThisDataA, Regressand = Regressands[j], 
        Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), sep = "|"),
        intercept = T, return.V = T, print.messages = T)
    )
    assign(paste0(listheader[k], "B", j),
      FDestimation(ThisDataB, Regressand = Regressands[j], 
        Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), sep = "|"),
        intercept = T, return.V = T, print.messages = T)
    )
  }
  # Format and save
  lblist <- eval(parse(text = 
    paste("list(", 
      paste(
        c(rbind(paste(listheader[k], "A", 1:3, sep = ""), 
        paste(listheader[k], "B", 1:3, sep = "")))
      , collapse = ", "),
    ")")
    ))
  lb.estlist <- lapply(lblist, "[[", "est")
  lb.estlist <- lapply(lb.estlist, function(x) x[, -3, drop = F])
  lb.N <- unlist(lapply(lblist, "[[", "N"))
#   lb.T <- cbind(c("T = 2", "T = 3", "T = 4"), 
#      matrix(unlist(lapply(lblist[1:3], "[[", "ActualTTable")  ), byrow = F, nrow = 3),
#      rbind(0, matrix(unlist(lapply(lblist[4], "[[", "ActualTTable")  ), byrow = F, nrow = 2)),
#      #matrix(unlist(lapply(lblist[5:6], "[[", "ActualTTable")  ), byrow = F, nrow = 3),
#      rbind(0, matrix(unlist(lapply(lblist[5:7], "[[", "ActualTTable")  ), byrow = F, nrow = 2))
#      )
  ttab <- lapply(lblist, "[[", "ActualTTable")
  ttab <- rbindlist(lapply(ttab, function(x) data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
  setcolorder(ttab, c("1", "2", "3"))
  ttab <- a2b.data.table(ttab, NA, 0)
  lb.T <- cbind(c("T = 2", "T = 3", "T = 4"), t(ttab))
  lb.R <- unlist(lapply(lapply(lapply(lblist, "[[", "nonrobust"), summary), "[[", "adj.r.squared"))
  lb.tab <- tabs2latex3(lb.estlist, digits = 2)
  rn <- rn0 <- rownames(lb.tab)
  thisEsttab <- lb.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn[rn.new]
  lb.tab <- lb.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  rn <- paste0("\\makebox[3cm]{\\scriptsize\\hfill ", rn, "}")
  lb.tb <- rbind(as.matrix(cbind(covariates = rn, lb.tab)), 
    lb.T,
    c("\\bar{R}^{2}", round(lb.R, 3)),
    c("N", lb.N))
  # omit year effects
  lb.ltxtb <- latextab(lb.tb, 
    hleft = "\\scriptsize\\hfil$", hcenter = c(3.25, rep(1.1, ncol(lb.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = c(2, 4), separatingcolwidth = c(.2, .2))
  lb.ltxtb <- rbind(lb.ltxtb[1:2, , drop = F], 
    "\\multicolumn{9}{c}{}\\\\[-.5ex]",
    "\\makebox[3cm]{\\scriptsize\\hfil } & \\multicolumn{1}{c}{\\scriptsize\\hfil small} & \\multicolumn{1}{c}{\\scriptsize\\hfil large} & & \\multicolumn{1}{c}{\\scriptsize\\hfil small} & \\multicolumn{1}{c}{\\scriptsize\\hfil large} & &\\multicolumn{1}{c}{\\scriptsize\\hfil small} & \\multicolumn{1}{c}{\\scriptsize\\hfil large} \\\\",
    "\\cline{2-3} \\cline{5-6} \\cline{8-9}\\\\",
    lb.ltxtb[-(1:2), , drop = F])
  write.tablev(lb.ltxtb, 
    paste0(pathsaveHere, "Incomes", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
   # for slides
  Rn <- unlist(lapply(list("^\\(Inter", "^dummyLarge$", "^dummyLargeGrace$", "^dummyCow$", 
    "^Time.?3", "Large.Time.?3$", "LargeGrace.Time.?3$", "Cow.Time.?3$", 
    "^Time.?4", "Large.Time.?4$", "LargeGrace.Time.?4$", "Cow.Time.?4$"), grep, rn0))
  Rn <- c(rbind(Rn, Rn+1))
  sl.tb <- lb.tb[, c(1:4, 6:7)]
  colnames(sl.tb) <- gsub("5", "4", colnames(sl.tb))
  colnames(sl.tb) <- gsub("6", "5", colnames(sl.tb))
  sl.ltxtb <- latextab(sl.tb, 
    hleft = "\\tiny\\hfil$", hcenter = c(3, rep(1.3, ncol(sl.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.5ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = 3, separatingcolwidth = .05)
  sl.tab <- rbind(sl.ltxtb[1:2, , drop = F],
    "\\multicolumn{7}{c}{}\\\\[-.5ex]",
    "\\makebox[3cm]{\\scriptsize\\hfil } & \\multicolumn{3}{c}{\\scriptsize\\hfil Labour income (Tk)} & & \\multicolumn{2}{c}{\\scriptsize\\hfil Farm revenue (Tk)} \\\\",
    "\\cline{2-4} \\cline{6-7}\\\\",
    sl.ltxtb[c(3:4, Rn+4), , drop = F],
    "\\makebox[3cm]{\\tiny\\hfill HH level controls} &  & \\mbox{\\tiny yes} & \\mbox{\\tiny yes} & && \\mbox{\\tiny yes} \\\\[-.5ex]\\rowcolor{gray90}",
    sl.ltxtb[nrow(sl.ltxtb)-(1:0), , drop = F])
  sl.tab <- gsub("scriptsize", "tiny", sl.tab)
  #sl.tab <- gsub("gray[89]0", "darkblue", sl.tab)
  write.tablev(sl.tab, 
    paste0(pathsaveHere, "ForSlide_Income", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
}
