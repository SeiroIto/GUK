Assuffixes <- c("", "G", "P", "S", "D", "DG")
listheader <- paste0("as", Assuffixes)
exclheader <- paste0("excl", Assuffixes)
DataToUse1 <- c(rep("das1d", 3), "das1Rd", rep("das2d", 3), "das2Rd")
DataToUse2 <- c(rep("das3d", 3), "das3Rd", rep("das4d", 3), "das4Rd")
Regressands <- c(rep("HAssetAmount", 4), rep("PAssetAmount", 4))
for (k in 1:length(listheader)) {
  if (k <= 4) 
    DataToUse <- DataToUse1 else
    DataToUse <- DataToUse2
# Estimate
  for (j in 1:8)
    assign(paste0(listheader[k], j),
      FDestimation(get(DataToUse[j]), Regressand = Regressands[j], 
        Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), sep = "|"),
        intercept = T, return.V = T, print.messages = T)
    )
# Format and save
  asplist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:8, sep = "", collapse = ", "), ")")))
  asp.estlist <- lapply(asplist, "[[", "est")
  asp.estlist <- lapply(asp.estlist, function(x) x[, -3, drop = F])
  asp.N <- unlist(lapply(asplist, "[[", "N"))
#   asp.T <- cbind(c("T = 2", "T = 3", "T = 4"), 
#      matrix(unlist(lapply(asplist[1:3], "[[", "ActualTTable")  ), byrow = F, nrow = 3),
#      matrix(c(unlist(lapply(asplist[4], "[[", "ActualTTable")  ), 0)),
#      matrix(unlist(lapply(asplist[5:6], "[[", "ActualTTable")  ), byrow = F, nrow = 3),
#      matrix(c(unlist(lapply(asplist[7], "[[", "ActualTTable")  ), 0))
#      )
  if (k <= 4) {
    ttab <- lapply(asplist, "[[", "ActualTTable")
    ttab <- rbindlist(lapply(ttab, function(x) data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
    if (ncol(ttab) == 3) {
      setcolorder(ttab, c("1", "2", "3"))
      ttab <- a2b.data.table(ttab, NA, 0)
      asp.T <- cbind(c("T = 2", "T = 3", "T = 4"), t(ttab))
    } else if (ncol(ttab) == 2) {
      setcolorder(ttab, c("1", "2"))
      ttab <- a2b.data.table(ttab, NA, 0)
      asp.T <- cbind(c("T = 2", "T = 3"), t(ttab))
    }
  } else asp.T <- NULL
  asp.R <- unlist(lapply(lapply(lapply(asplist, "[[", "nonrobust"), summary), "[[", "adj.r.squared"))
  asp.tab <- tabs2latex3(asp.estlist, digits = 1)
  rn <- rownames(asp.tab)
  thisEsttab <- asp.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn[rn.new]
  rn0 <- rn
  asp.tab <- asp.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  rn <- paste0("\\makebox[3cm]{\\scriptsize\\hfill ", rn, "}")
  asp.tb <- rbind(as.matrix(cbind(covariates = rn, asp.tab)), 
    asp.T,
    c("\\bar{R}^{2}", round(asp.R, 3)),
    c("N", asp.N))
  # omit year effects
  asp.ltxtb <- latextab(asp.tb, 
    hleft = "\\scriptsize\\hfil$", hcenter = c(3.25, rep(1.3, ncol(asp.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = 4, separatingcolwidth = .2)
  asp.ltxtb <- rbind(asp.ltxtb[1:2, , drop = F], 
    "\\multicolumn{10}{c}{}\\\\[-.5ex]",
    "\\makebox[3cm]{\\scriptsize\\hfil } & \\multicolumn{4}{c}{\\scriptsize\\hfil Household asset amount (Tk)} & & \\multicolumn{4}{c}{\\scriptsize\\hfil Pro	ductive asset amount (Tk)} \\\\",
    "\\cline{2-5} \\cline{7-10}\\\\",
    asp.ltxtb[-(1:2), , drop = F])
  write.tablev(asp.ltxtb, 
    paste0(pathsaveHere, "Asset", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
   # for slides
  Rn <- unlist(lapply(list("^\\(Inter", "^dummyLarge$", "^dummyLargeGrace$", "^dummyCow$", 
    "^Time.?3", "Large.Time.?3$", "LargeGrace.Time.?3$", "Cow.Time.?3$", 
    "^Time.?4", "Large.Time.?4$", "LargeGrace.Time.?4$", "Cow.Time.?4$"), grep, rn0))
  Rn <- c(rbind(Rn, Rn+1))
  sl.tb <- asp.tb[, c(1:4, 6, 8)]
  colnames(sl.tb) <- gsub("5", "4", colnames(sl.tb))
  colnames(sl.tb) <- gsub("7", "5", colnames(sl.tb))
  sl.ltxtb <- latextab(sl.tb, 
    hleft = "\\tiny\\hfil$", hcenter = c(3, rep(1.3, ncol(sl.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.5ex", delimiterline= NULL,
    alternatecolor2 = "gray90", 
    addseparatingcols = 3, separatingcolwidth = .05)
  sl.tab <- rbind(sl.ltxtb[1:2, , drop = F],
    "\\multicolumn{7}{c}{}\\\\[-.5ex]",
    "\\makebox[3cm]{\\scriptsize\\hfil } & \\multicolumn{3}{c}{\\scriptsize\\hfil Household asset amount (Tk)} & & \\multicolumn{2}{c}{\\scriptsize\\hfil Productive asset amount (Tk)} \\\\",
    "\\cline{2-4} \\cline{6-7}\\\\",
    sl.ltxtb[c(3:4, Rn+4), , drop = F],
    "\\makebox[3cm]{\\tiny\\hfill HH level controls} &  & \\mbox{\\tiny yes} & \\mbox{\\tiny yes} & && \\mbox{\\tiny yes} \\\\[-.5ex]\\rowcolor{gray90}",
    sl.ltxtb[nrow(sl.ltxtb)-(1:0), , drop = F])
  sl.tab <- gsub("scriptsize", "tiny", sl.tab)
  #sl.tab <- gsub("gray[89]0", "darkblue", sl.tab)
  write.tablev(sl.tab, 
    paste0(pathsaveHere, "ForSlide_Asset", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
}
