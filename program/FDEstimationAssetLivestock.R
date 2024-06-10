alsuffixes <- c("", "G", "P", "S", "T", "TG", "TS", "D", "DG", "DP", "DS")
listheader <- paste0("al", alsuffixes)
exclheader <- paste0("excl", alsuffixes)
DataToUse1 <- rep("dAL1d", 6)
DataToUse2 <- rep("dAL2d", 6)
tableboxwidth <- 4.5
for (k in 1:length(listheader)) {
  if (k < 8)
    DataToUse <- DataToUse1 else
    DataToUse <- DataToUse2
  # Estimate
  for (j in 1:6)
    assign(paste0(listheader[k], j),
      FDestimation(get(DataToUse[j]), Regressand = "TotalValue", 
        Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), sep = "|"),
        intercept = T, return.V = T, print.messages = T)
    )
  # Format and save
  allist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:6, sep = "", collapse = ", "), ")")))
  al.estlist <- lapply(allist, "[[", "est")
  al.estlist <- lapply(al.estlist, function(x) x[, -3, drop = F])
  al.N <- unlist(lapply(allist, "[[", "N"))
  if (k < 8) {
    ttab <- lapply(allist, "[[", "ActualTTable")
    ttab <- rbindlist(lapply(ttab, function(x) data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
    if (ncol(ttab) == 3) {
      setcolorder(ttab, c("1", "2", "3"))
      ttab <- a2b.data.table(ttab, NA, 0)
      al.T <- cbind(c("T = 2", "T = 3", "T = 4"), t(ttab))
    } else if (ncol(ttab) == 2) {
      setcolorder(ttab, c("1", "2"))
      ttab <- a2b.data.table(ttab, NA, 0)
      al.T <- cbind(c("T = 2", "T = 3"), t(ttab))
    }
  } else al.T <- NULL
  al.R <- unlist(lapply(lapply(lapply(allist, "[[", "nonrobust"), summary), "[[", "adj.r.squared"))
  al.tab <- tabs2latex3(al.estlist, digits = 1)
  rn <- rownames(al.tab)
  thisEsttab <- al.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn0 <- rn[rn.new]
  al.tab <- al.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  rn <- paste0("\\makebox[", tableboxwidth, "cm]{\\scriptsize\\hfill ", rn, "}")
  al.tb <- rbind(as.matrix(cbind(covariates = rn, al.tab)), 
    al.T,
    c("\\bar{R}^{2}", round(al.R, 3)),
    c("N", al.N))
  al.ltxtb <- latextab(al.tb, 
    hleft = "\\scriptsize\\hfil$", hcenter = c(tableboxwidth, rep(1.6, ncol(al.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
    alternatecolor2 = "gray90")
  write.tablev(al.ltxtb, 
    paste0(pathsaveHere, "AssetLivestock", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
   # for slides
  Rn <- unlist(lapply(list("^\\(Inter", "^dummyLarge$", "^dummyLargeGrace$", "^dummyCow$", 
    "^Time.?3", "Large.Time.?3$", "LargeGrace.Time.?3$", "Cow.Time.?3$", 
    "^Time.?4", "Large.Time.?4$", "LargeGrace.Time.?4$", "Cow.Time.?4$"), grep, rn0))
  Rn <- c(rbind(Rn, Rn+1))
  sl.tb <- al.tb[, c(1:5, 7)]
  sl.ltxtb <- latextab(sl.tb, 
    hleft = "\\tiny\\hfil$", hcenter = c(tableboxwidth, rep(1.5, ncol(sl.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.5ex", delimiterline= NULL,
    alternatecolor2 = "gray90")
  sl.tab <- rbind(
    sl.ltxtb[c(1:2, Rn+2), , drop = F],
    paste0("\\makebox[", tableboxwidth, "cm]{\\tiny\\hfill HH level controls} &  &  & \\mbox{\\tiny yes}& \\mbox{\\tiny yes}& \\mbox{\\tiny yes}& \\mbox{\\tiny yes} \\\\[-.5ex]\\rowcolor{gray90}"),
    sl.ltxtb[nrow(sl.ltxtb)-(1:0), , drop = F])
  sl.tab <- gsub("scriptsize", "tiny", sl.tab)
  #sl.tab <- gsub("gray[89]0", "darkblue", sl.tab)
  write.tablev(sl.tab, 
    paste0(pathsaveHere, "ForSlide_AssetLivestock", FileNameHeader[k], "FDEstimationResults.tex"), 
    colnamestrue = F)
}
