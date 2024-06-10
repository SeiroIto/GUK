arsuffixes <- c("", "g", "p", "s")
listheader <- paste0("sv", arsuffixes)
exclheader <- paste0("excl", arsuffixes)
Regressands <-  c(rep("CumNetSaving", 2), rep("CumRepaid", 3), 
  rep("CumEffectiveRepayment", 3))
for (k in 1:length(listheader)) {
# Estimate
  # CumNetSaving
  for (j in 1:8)
    assign(paste0(listheader[k], j),
      FDestimation(dar2, Regressand = Regressands[j], 
        Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], ".", j)), sep = "|"),
        intercept = T, return.V = T, print.messages = T)
    )
#    print(sv4$est)
#    print(summary(sv4$non))
#  for (j in c(2, 4:5, 7:8))
#    assign(paste0(listheader[k], j),
#      FDestimation(dard, Regressand = Regressands[j], 
#        Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
#        Exclude = paste(get(paste0(exclheader[k], ".base")), 
#          get(paste0(exclheader[k], ".", j)), sep = "|"),
#        intercept = F, return.V = T, print.messages = T)
#    )
# Format and save
  svlist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:8, sep = "", collapse = ", "), ")")))
  sv.estlist <- lapply(svlist, "[[", "est")
  sv.estlist <- lapply(sv.estlist, function(x) x[, -3, drop = F])
  sv.N <- unlist(lapply(svlist, "[[", "N"))
#  sv.T <- cbind(c("T = 2", "T = 3"), matrix(unlist(lapply(svlist, "[[", "ActualTTable")  ), byrow = F, nrow = 2))
  ttab <- lapply(svlist, "[[", "ActualTTable")
  ttab <- rbindlist(lapply(ttab, function(x) data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
  if (ncol(ttab) == 3) {
    setcolorder(ttab, c("1", "2", "3"))
    ttab <- a2b.data.table(ttab, NA, 0)
    sv.T <- cbind(c("T = 2", "T = 3", "T = 4"), t(ttab))
  } else if (ncol(ttab) == 2) {
    setcolorder(ttab, c("1", "2"))
    ttab <- a2b.data.table(ttab, NA, 0)
    sv.T <- cbind(c("T = 2", "T = 3"), t(ttab))
  }
  sv.R <- unlist(lapply(lapply(lapply(svlist, "[[", "nonrobust"), summary), "[[", "adj.r.squared"))
  sv.tab <- tabs2latex3(sv.estlist, digits = 1)
  rn <- rn0 <- rownames(sv.tab)
  thisEsttab <- sv.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn[rn.new]
  sv.tab <- sv.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  rn <- paste0("\\makebox[3cm]{\\scriptsize\\hfill ", rn, "}")
  sv.tb <- rbind(as.matrix(cbind(covariates = rn, sv.tab)), 
    sv.T,
    c("\\bar{R}^{2}", round(sv.R, 3)),
    c("N", sv.N))
  # omit year effects
  sv.ltxtb <- latextab(sv.tb, 
    hleft = "\\scriptsize\\hfil$", hcenter = c(3.25, rep(1.2, ncol(sv.tb)-1)), hright = "$", 
    headercolor = "gray80",, adjustlineskip = "-.6ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = c(2, 5), separatingcolwidth = rep(.2, 2), 
    separatingcoltitle = c("Cumulative net saving", "Cumulative repayment", 
      "Cumulative net saving + cumulative repayment"))
#   sv.ltxtb <- rbind(sv.ltxtb[1:2, , drop = F], 
#     "\\multicolumn{11}{c}{}\\\\[-.5ex]",
#     "\\makebox[3cm]{\\scriptsize\\hfil } & \\multicolumn{2}{c}{\\scriptsize\\hfil Cumulative net saving} & &\\multicolumn{3}{c}{\\scriptsize\\hfil Cumulative repayment} & & \\multicolumn{3}{c}{\\scriptsize\\hfil Cumulative net saving + cumulative repayment} \\\\",
#     "\\cline{2-3} \\cline{5-7} \\cline{9-11}\\\\",
#     sv.ltxtb[-(1:2), , drop = F])
  write.tablev(sv.ltxtb, 
    paste0(pathsaveHere, "Saving", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
   # for slides
  Rn <- unlist(lapply(list("^\\(Inter", "^dummyLarge$", "^dummyLargeGrace$", "^dummyCow$", 
    "^Time.?3", "Large.Time.?3$", "LargeGrace.Time.?3$", "Cow.Time.?3$", 
    "^Time.?4", "Large.Time.?4$", "LargeGrace.Time.?4$", "Cow.Time.?4$"), grep, rn0))
  if (k == 3) 
    Rn <- unlist(lapply(list("^\\(Inter", "^dummyUltraPoor$", 
      "^Time.?3", "UltraPoor.Time.?3$", 
      "^Time.?4", "UltraPoor.Time.?4$"), grep, rn0))
  Rn <- c(rbind(Rn, Rn+1))
  sl.tb <- sv.tb[, c(1:3, 7:9)]
  colnames(sl.tb) <- gsub("6", "3", colnames(sl.tb))
  colnames(sl.tb) <- gsub("7", "4", colnames(sl.tb))
  colnames(sl.tb) <- gsub("8", "5", colnames(sl.tb))
  sl.ltxtb <- latextab(sl.tb, 
    hleft = "\\tiny\\hfil$", hcenter = c(3, rep(1.3, ncol(sl.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.5ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = 2, separatingcolwidth = .05,
    separatingcoltitle = c("Net net saving (Tk)", "Net saving + repayment (Tk)"))
  sl.tab <- rbind(sl.ltxtb[1:(Rn+4), , drop = F],
#  sl.ltxtb[1:3, , drop = F],
#     "\\multicolumn{7}{c}{}\\\\[-.5ex]",
#     "\\makebox[3cm]{\\scriptsize\\hfil } & \\multicolumn{2}{c}{\\scriptsize\\hfil Net saving (Tk)} & & \\multicolumn{3}{c}{\\scriptsize\\hfil Net saving + repayment (Tk)} \\\\",
#     "\\cline{2-3} \\cline{5-7}\\\\",
#    sl.ltxtb[c(3:4, Rn+4), , drop = F],
    "\\makebox[3cm]{\\tiny\\hfill HH level controls} &&&&&& \\mbox{\\tiny yes} \\\\[-.5ex]\\rowcolor{gray90}",
    sl.ltxtb[nrow(sl.ltxtb)-(4:0), , drop = F])
  sl.tab <- gsub("scriptsize", "tiny", sl.tab)
  #sl.tab <- gsub("gray[89]0", "darkblue", sl.tab)
  write.tablev(sl.tab, 
    paste0(pathsaveHere, "ForSlide_Repayment", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
}
