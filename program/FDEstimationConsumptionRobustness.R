cnsuffixes <- c("", "g", "p", "s")
listheader <- paste0("cn", cnsuffixes)
exclheader <- paste0("excl", cnsuffixes)
Regressands <- c(rep("PCExpenditure", 4), rep("PCFoodExpenditure", 3))
DataToUse <- c(rep("dcond", 3), "dconRd", rep("dcond", 2), "dconRd")
for (k in 1:length(listheader)) {
  for (j in 1:7)
    assign(paste0(listheader[k], j),
      FDestimation(get(DataToUse[j]), Regressand = Regressands[j], 
        Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), sep = "|"),
        intercept = T, return.V = T, print.messages = T)
    )
  # Format and save
  cnlist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:7, sep = "", collapse = ", "), ")")))
  cn.estlist <- lapply(cnlist, "[[", "est")
  cn.estlist <- lapply(cn.estlist, function(x) x[, -3, drop = F])
  cn.N <- unlist(lapply(cnlist, "[[", "N"))
#   cn.T <- cbind(c("T = 2", "T = 3"), 
#      matrix(unlist(lapply(cnlist[1:7], "[[", "ActualTTable")  ), byrow = F, nrow = 2)
#      )
  ttab <- lapply(cnlist, "[[", "ActualTTable")
  ttab <- rbindlist(lapply(ttab, function(x) data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
  setcolorder(ttab, c("1", "2"))
  ttab <- a2b.data.table(ttab, NA, 0)
  cn.T <- cbind(c("T = 2", "T = 3"), t(ttab))
  cn.R <- unlist(lapply(lapply(lapply(cnlist, "[[", "nonrobust"), summary), "[[", "adj.r.squared"))
  cn.tab <- tabs2latex3(cn.estlist, digits = 2)
  rn <- rownames(cn.tab)
  thisEsttab <- cn.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn0 <- rn[rn.new]
  cn.tab <- cn.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  rn <- paste0("\\makebox[3cm]{\\scriptsize\\hfill ", rn, "}")
  cn.tb <- rbind(as.matrix(cbind(covariates = rn, cn.tab)), 
    cn.T,
    c("\\bar{R}^{2}", round(cn.R, 3)),
    c("N", cn.N))
  # omit year effects
  cn.ltxtb <- latextab(cn.tb, 
    hleft = "\\scriptsize\\hfil$", hcenter = c(3.25, rep(1.3, ncol(cn.tb)-1)), hright = "$", 
    headercolor = "gray80",, adjustlineskip = "-.6ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = 4, separatingcolwidth = .2)
  cn.ltxtb <- rbind(cn.ltxtb[1:2, , drop = F], 
    "\\multicolumn{9}{c}{}\\\\[-.5ex]",
    "\\makebox[3cm]{\\scriptsize\\hfil } & \\multicolumn{4}{c}{\\scriptsize\\hfil Per capita consumption (Tk)} & & \\multicolumn{3}{c}{\\scriptsize\\hfil Per capita food consumption (Tk)}\\\\",
    "\\cline{2-5} \\cline{7-9}\\\\",
    cn.ltxtb[-(1:2), , drop = F])
  write.tablev(cn.ltxtb, 
    paste0(pathsaveHere, "Consumption", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
   # for slides
  Rn <- unlist(lapply(list("^\\(Inter", "^dummyLarge$", "^dummyLargeGrace$", "^dummyCow$", 
#    "^Time.?3", "Large.Time.?3$", "LargeGrace.Time.?3$", "Cow.Time.?3$", 
    "^Time.?4", "Large.Time.?4$", "LargeGrace.Time.?4$", "Cow.Time.?4$"), grep, rn0))
  Rn <- c(rbind(Rn, Rn+1))
  sl.tb <- cn.tb[, c(1:4, 6:7)]
  colnames(sl.tb) <- gsub("5", "4", colnames(sl.tb))
  colnames(sl.tb) <- gsub("6", "5", colnames(sl.tb))
  sl.ltxtb <- latextab(sl.tb, 
    hleft = "\\tiny\\hfil$", hcenter = c(3, rep(1.3, ncol(sl.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.5ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = 3, separatingcolwidth = .05)
  sl.tab <- rbind(sl.ltxtb[1:2, , drop = F],
    "\\multicolumn{7}{c}{}\\\\[-.5ex]",
    "\\makebox[3cm]{\\scriptsize\\hfil } & \\multicolumn{3}{c}{\\scriptsize\\hfil Per capita consumption (Tk)} & & \\multicolumn{2}{c}{\\scriptsize\\hfil Per capita food consumption (Tk)} \\\\",
    "\\cline{2-4} \\cline{6-7}\\\\",
    sl.ltxtb[c(3:4, Rn+4), , drop = F],
    "\\makebox[3cm]{\\tiny\\hfill HH level controls} &  & \\mbox{\\tiny yes} & \\mbox{\\tiny yes} & && \\mbox{\\tiny yes} \\\\[-.5ex]\\rowcolor{gray90}",
    sl.ltxtb[nrow(sl.ltxtb)-(1:0), , drop = F])
  sl.tab <- gsub("scriptsize", "tiny", sl.tab)
  #sl.tab <- gsub("gray[89]0", "darkblue", sl.tab)
  write.tablev(sl.tab, 
    paste0(pathsaveHere, "ForSlide_Consumption", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
}
