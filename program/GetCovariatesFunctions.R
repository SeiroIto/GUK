# These functions get covariates specified in exclXX,
# and paste them by specification
GetCovariates <- function(Listheader, Listcovariates, Jay = jay) {
  for (k in 1:length(Listheader)) {
    if (k <= grep("Ta", inclheader)) 
      DataToUse <- DataToUse1 else
      DataToUse <- DataToUse2
    for (j in 1:Jay) {
      x = copy(get(DataToUse[j]))
      # first, drop unnecessary variables for an ease of writing reg expr
      exclstring <- paste(get(paste0(exclheader[k], ".base")), 
            "^Tee$|teeyr|^Time$|00$|_|^Ass|status$|cy$", sep = "|")
      x <- x[, grepout(exclstring, colnames(x)) := NULL]
      # second, pick covariates
      Covariates <- colnames(x)[
          if (grepl("\\^", get(paste0(inclheader[k], j)))) # if using perl regex
            grepl(get(paste0(inclheader[k], j)), colnames(x), perl = T) else
            grepl(get(paste0(inclheader[k], j)), colnames(x))
        ]
      Listcovariates[[k]][[j]] <- Covariates
    }
  }
  return(Listcovariates)
}
AttachSymbolsToLC <- function(listcovariates, bracket = "(", CutBy = 4) {
  if (bracket == "(") {
    brLeft <- "("; brRight <- ")"
  } else if (bracket == "[") {
    brLeft <- "["; brRight <- "]"
  } else if (bracket == "{") {
    brLeft <- "{"; brRight <- "}"
  }
  ListCov <- 
    lapply(1:length(listcovariates), function(i) 
      lapply(1:length(listcovariates[[i]]), function(j)
        if (j == 1) 
          c(paste0("\n\n", paste0(brLeft, i, brRight, "\n"), " ~ "), listcovariates[[i]][[j]]) else 
          c("", listcovariates[[i]][[j]][!listcovariates[[i]][[j]] %in% listcovariates[[i]][[j-1]]])
      )
    )
  # wrap long lines (more than 4 terms) by CutBy terms
  ListCov <- lapply(ListCov, function(x) 
    lapply(x, function(z) {
      if (length(z) > CutBy) {
        z2 <- unlist(strsplit(z, "\\+"))
        NumLineBreaks <- floor(length(z2)/CutBy)
        for (k in 1:NumLineBreaks) {
          if (k == 1) z3 <- paste0(paste(z2[1:4], collapse = " + "), "\n")
          if (NumLineBreaks > 1) {
            if (k < NumLineBreaks)
              z3 <- c(z3, paste0(" + ", paste(z2[CutBy*k+1:CutBy], collapse = " + "), "\n")) else
              z3 <- c(z3, paste0(" + ", paste(z2[-(1:(CutBy*k))], collapse = " + "), "\n"))
          }
        }
        return(paste(z3, collapse = ""))
      } else return(z)
    })
  )
  ListCov <- lapply(ListCov, function(x) 
    lapply(x, function(z) paste(z, collapse = " + ")))
  return(ListCov)
}
