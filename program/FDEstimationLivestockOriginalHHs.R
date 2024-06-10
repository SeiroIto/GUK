Lvsuffixes <- c("", "G", "P", "S", "T", "TG", "TS", "D", "DG")
listheader <- paste0("lv", Lvsuffixes)
exclheader <- paste0("excl", Lvsuffixes)
DataToUse1 <- rep("dlvod", 6)
DataToUse2 <- rep("dlvo3d", 6)
tableboxwidth <- 4.5
for (k in 1:length(listheader)) {
  if (k < 8) 
    DataToUse <- DataToUse1 else
    DataToUse <- DataToUse2
  # Estimate
  for (j in 1:6)
    assign(paste0(listheader[k], j),
      FDestimation(get(DataToUse[j]), Regressand = "TotalImputedValue", 
        Group = "^hhid$", TimeVar = "tee", Cluster = "groupid", 
        Exclude = paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), sep = "|"),
        intercept = T, return.V = T, print.messages = T)
    )
  # Format and save
  lvlist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:6, sep = "", collapse = ", "), ")")))
  lv.estlist <- lapply(lvlist, "[[", "est")
  lv.estlist <- lapply(lv.estlist, function(x) x[, -3, drop = F])
  lv.N <- unlist(lapply(lvlist, "[[", "N"))
  if (k < 8) {
    ttab <- lapply(lvlist, "[[", "ActualTTable")
    ttab <- rbindlist(lapply(ttab, function(x) data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
    setcolorder(ttab, c("1", "2", "3"))
    ttab <- a2b.data.table(ttab, NA, 0)
    lv.T <- cbind(c("T = 2", "T = 3", "T = 4"), t(ttab))
  } else lv.T <- NULL
  lv.R <- unlist(lapply(lapply(lapply(lvlist, "[[", "nonrobust"), summary), "[[", "adj.r.squared"))
  lv.tab <- tabs2latex3(lv.estlist, digits = 1)
  rn <- rn0 <- rownames(lv.tab)
  thisEsttab <- lv.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsTable.R"))
  rn <- rn[rn.new]
  lv.tab <- lv.tab[rn.new, ]
  for (i in 1:nrow(subst.table)) 
    rn <- gsub(subst.table[i, 1], subst.table[i, 2], rn)
  rn <- paste0("\\makebox[", tableboxwidth, "cm]{\\scriptsize\\hfill ", rn, "}")
  lv.tb <- rbind(as.matrix(cbind(covariates = rn, lv.tab)), 
    lv.T,
    c("\\bar{R}^{2}", round(lv.R, 3)),
    c("N", lv.N))
  lv.ltxtb <- latextab(lv.tb, 
    hleft = "\\scriptsize\\hfil$", hcenter = c(tableboxwidth, rep(1.5, ncol(lv.tb)-1)), hright = "$", 
    headercolor = "gray80", adjustlineskip = "-.6ex", delimiterline= NULL,
    alternatecolor2 = "gray90")
  write.tablev(lv.ltxtb, 
    paste0(pathsaveHere, "Livestock", FileNameHeader[k], "OriginalHHsFDEstimationResults.tex"), 
    colnamestrue = F)
   # for slides
  Rn <- unlist(lapply(list("^\\(Inter", "^dummyLarge$", "^dummyLargeGrace$", "^dummyCow$", 
    "^Time.?3", "Large.Time.?3$", "LargeGrace.Time.?3$", "Cow.Time.?3$", 
    "^Time.?4", "Large.Time.?4$", "LargeGrace.Time.?4$", "Cow.Time.?4$"), grep, rn0))
  Rn <- c(rbind(Rn, Rn+1))
  sl.tb <- lv.tb[, c(1:5, 7)]
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
    paste0(pathsaveHere, "ForSlide_Livestock", FileNameHeader[k], "OriginalHHsFDEstimationResults.tex"), 
    colnamestrue = F)
}
