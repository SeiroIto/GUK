jay <- length(Regressands)
GroupVar <- rep("^hhid$", jay)
if (grepl("Inc", FileName)) GroupVar <- c(rep("^HMid$", 4), rep("^hhid$", 3))
if (grepl("Sch", FileName)) GroupVar <- rep("^HHMid$", jay)
# k: index of regression tables
estlist <- vector(mode = "list", length = length(listheader))
for (k in 1:length(listheader)) {
  if (k <= grep("^Ta$", regsuffixes)) 
    DataToUse <- DataToUse1 else
    DataToUse <- DataToUse2
# Estimate
  Formulae <- DepMean <- NULL
  for (j in 1:jay) {
    x = copy(get(DataToUse[j]))
    exclstring <- paste(get(paste0(exclheader[k], ".base")), 
          get(paste0(exclheader[k], j)), 
          "^Tee$|teeyr|^Time$|00$|^Ass|status$|cy$", sep = "|")
    x <- x[, grepout(exclstring, colnames(x)) := NULL]
    Covariates <- colnames(x)[!grepl(
        paste0("^groupid$|hhid|^tee$|", 
        paste0("^", Regressands[j], "$|"), GroupVar[j])
        , colnames(x) )
      ]
    # if regression has time-varying slopes, drop intercept
    if (grepl("T", listheader[k])) fsim <- "~ -1 +" else fsim <- "~"
    Formula <- as.formula(paste(Regressands[j], fsim, paste(Covariates, collapse = "+")))
    Formulae <- c(Formulae, Formula)
    lmx <- lm(Formula, data = x)
    assign(paste0(listheader[k], j),
      if (is.null(lmx$na.action))
        list(lm = lmx, robust = clx(lmx, cluster = x[, groupid]), data = x) else
        list(lm = lmx, robust = clx(lmx, cluster = x[-lmx$na.action, groupid]),
          data = x[-lmx$na.action, ])
      )
#     if (is.null(lmx$na.action)) {
#       assign(paste0(listheader[k], j),
#         list(lm = lmx, robust = clx(lmx, cluster = x[, groupid]), data = x)
#         )
#     } else {
#       assign(paste0(listheader[k], j),
#         list(lm = lmx, robust = clx(lmx, cluster = x[-lmx$na.action, groupid]),
#           data = x[-lmx$na.action, ])
#         )
#     }
    DepMean <- c(DepMean, mean(unlist(x[, Regressands[j], with = F]), na.rm = T))
  }
  print0(exclheader[k])
  print0(Formulae)
  # elist is list of ANCOVA estimation results objects
  elist <- eval(parse(text = 
    paste("list(", paste(listheader[k], 1:jay, sep = "", collapse = ", "), ")")
    ))
# Format and save
  dataused <- lapply(elist, "[[", "data")
  e.estlist <- lapply(elist, "[[", "robust")
  e.estlist <- lapply(e.estlist, function(x) x[, -3, drop = F])
  e.N <- unlist(lapply(dataused, nrow))
  ttab <- lapply(dataused, function(z) 
    table(z[, .(tee = tee, Tee = .N), by = hhid][tee == min(tee), Tee]))
  ttab <- rbindlist(lapply(ttab, function(x) 
    data.table(as.data.frame.matrix(t(x)))), use.names = T, fill = T)
#  e.T <- Reduce(merge, ttab)
  if (!grepl("Sav", FileName) & k <= grep("^Ta$", regsuffixes)) {
    if (ncol(ttab) == 3) {
      setcolorder(ttab, 1:3)
      ttab <- a2b.data.table(ttab, NA, 0)
      e.T <- cbind(paste("T =", 2:4), t(ttab))
    } else if (ncol(ttab) == 2) {
      setcolorder(ttab, 1:2)
      ttab <- a2b.data.table(ttab, NA, 0)
      e.T <- cbind(paste("T =", 3:4), t(ttab))
    }
  } else e.T <- NULL
  e.R <- unlist(lapply(lapply(lapply(elist, "[[", "lm"), summary), "[[", "adj.r.squared"))
  if (grepl("School|Inco", FileName)) 
    e.tab <- tabs2latex3(e.estlist, digits = 2) else
    e.tab <- tabs2latex3(e.estlist, digits = 1)
  rn <- rownames(e.tab)
  thisEsttab <- e.tab
  # reorder rows: rn.new
  source(paste0(pathprogram, "ReorderingOfRowsInEstimatedResultsANCOVATable.R"))
  rn <- rn[rn.new]
  rn0 <- rn
  e.tab <- e.tab[rn.new, ]
  for (i in 1:nrow(subst.tableA)) 
    rn <- gsub(subst.tableA[i, 1], subst.tableA[i, 2], rn)
  rn <- paste0("\\makebox[3cm]{\\scriptsize\\hfill ", rn, "}")
  e.tb <- rbind(as.matrix(cbind(covariates = rn, e.tab)), 
    c("\\mbox{mean of dependent variable}", round(DepMean, 0)),
    e.T,
    c("\\bar{R}^{2}", round(e.R, 3)),
    c("N", e.N))
  # omit year effects
  centerBox <- 1.3
  if (grepl("Sav", FileName)) centerBox <- .98
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
    paste0(pathsaveHere, FileName, FileNameHeader[k], "ANCOVAEstimationResults.tex")
    , colnamestrue = F)
  # for slides
  if (grepl("Sav", FileName)) {
    slt <- cbind(covariates = gsub("scriptsize", "tiny", rn), e.tab)
    slt[, 1] <- gsub("3cm", "2.5cm", slt[, 1])
    slt <- slt[-(rep(grep("fill rd [2-4]|Flood|Head|6M", rn), each = 2)+
      rep(0:1, length(grep("fill rd [2-4]|Flood|Head|6M", rn)))), ]
    addtoslt <- rbind(
      c("HH controls", rep("", 2), rep(c("", "", "\\mbox{yes}"), 3))
      ,
      c("survey round controls", "", "\\mbox{yes}", 
        rep(c("", "\\mbox{yes}", "\\mbox{yes}"), 3))
      )
    addtoslt[, 1] <- paste0("\\makebox[2.5cm]{\\tiny\\hfill ", addtoslt[, 1], "}")
    slt <- rbind(as.matrix(slt), 
      addtoslt, 
      e.T,
      c("\\bar{R}^{2}", round(e.R, 3)),
      ancAR,
      c("N", e.N))
    slt <- slt[, 1:9]
    centerBox <- 1.2
    slt.ltxtb <- latextab(slt, 
      hleft = "\\tiny\\hfil$", 
      hcenter = c(2.5, rep(centerBox, ncol(slt)-1)), hright = "$", 
      headercolor = "gray80", adjustlineskip = "-.5ex", delimiterline= NULL,
      alternatecolor2 = "gray90", 
      addseparatingcols = Addseparatingcols[-3], 
      separatingcolwidth = Separatingcolwidth[-1], 
      separatingcoltitle = Separatingcoltitle[-4], 
      addsubcoltitlehere = length(Addseparatingcols) > 0)
    slt.ltxtb[2, ] <- gsub("scriptsize", "tiny", slt.ltxtb[2, ])
    slt.ltxtb[3, ] <- gsub("\\\\\\\\", "", slt.ltxtb[3, ])
    write.tablev(slt.ltxtb, 
      paste0(pathsaveHere, FileName, FileNameHeader[k], 
        "ANCOVAEstimationResults_ForSlides.tex")
      , colnamestrue = F)
  }
  # confidence interval data
  estlist[[k]] <- elist
}
