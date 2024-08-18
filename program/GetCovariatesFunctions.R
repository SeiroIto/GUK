grepout <- function(str, x)
  # returns element of match (not numbers)
  x[grep(str, x, perl = T)]
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
# Interaction between 2 matrices.
interactXY <- function(x, y, sepcharacter = "_", ReturnMatrix = F) {
#  Create an interaction matrix from matrix x and matrix y.
#  Column names are created by pasting "x sepcharacter y".
# Returns a data.table or a matrix.
  if (!is.data.table(x)) x <- data.table(x)
  if (!is.data.table(y)) y <- data.table(y)
  if (all(grepl("integer", c(sapply(x, class), sapply(y, class))))) {
    z <- data.table(array(dim = c(nrow(x), ncol(x)*ncol(y)), as.integer(NA)))
    for (j1 in 1:ncol(x)) 
      for (j2 in 1:ncol(y)) 
        set(z, j = (j1 - 1) * ncol(y) + j2, value = as.integer(x[[j1]] * y[[j2]]))
  } else {
    z <- data.table(array(dim = c(nrow(x), ncol(x)*ncol(y)), as.numeric(NA)))
    for (j1 in 1:ncol(x)) 
      for (j2 in 1:ncol(y)) 
        set(z, j = (j1 - 1) * ncol(y) + j2, value = as.numeric(x[[j1]] * y[[j2]]))
  }
  setnames(z, unlist(lapply(1:ncol(x), function(i) 
     paste0(colnames(x)[i], sepcharacter, colnames(y)))))
  if (ReturnMatrix) z <- as.matrix(z)
  return(z)
}
#z <- interactXY(x, y)
interactXYZ2 <- function(x, y, z, sepcharacter = "_", ReturnMatrix = F) {
#  Create an interaction matrix from matrices x, y, z.
#  Column names are created by pasting "x sepcharacter y sepcharacter z".
# Returns a data.table or a matrix.
  if (!is.data.table(x)) x <- data.table(x)
  if (!is.data.table(y)) y <- data.table(y)
  if (!is.data.table(z)) z <- data.table(z)
  xy <- interactXY(x, y)
  xz <- interactXY(x, z)
  yz <- interactXY(y, z)
  if (all(grepl("integer", c(sapply(x, class), sapply(y, class), sapply(z, class))))) {
    xyz <- data.table(array(dim = c(nrow(x), ncol(x)*ncol(y)*ncol(z)), as.integer(NA)))
    for (j1 in 1:ncol(x)) 
      for (j2 in 1:ncol(y)) 
        for (j3 in 1:ncol(z)) 
          set(xyz, j = (j1 - 1) * (ncol(y) * ncol(z)) +
                          (j2 - 1) * (ncol(z)) + 
                           j3, 
            value = as.integer(x[[j1]] * y[[j2]] * z[[j3]]))
  } else {
    xyz <- data.table(array(dim = c(nrow(x), ncol(x)*ncol(y)*ncol(z)), as.numeric(NA)))
    for (j1 in 1:ncol(x)) 
      for (j2 in 1:ncol(y)) 
        for (j3 in 1:ncol(z)) 
          set(xyz, j = (j1 - 1) * (ncol(y) * ncol(z)) +
                          (j2 - 1) * ncol(z) + 
                          j3, 
            value = as.numeric(x[[j1]] * y[[j2]] * z[[j3]]))
  }
  cn.xyz <- unlist(lapply(1:ncol(x), function(i) 
     paste0(colnames(x)[i], sepcharacter, colnames(y)))) 
  cn.xyz <- unlist(lapply(1:length(cn.xyz), function(i) 
     paste0(cn.xyz[i], sepcharacter, colnames(z)))) 
  setnames(xyz, cn.xyz)
  w <- data.table(xy, xz, yz, xyz)
  if (ReturnMatrix) w <- as.matrix(w)
  return(w)
}
interactXYZ <- function(x, y, z, sepcharacter = "_", ReturnMatrix = F) {
#  Create an interaction matrix from matrices x, y, z.
#  Column names are created by pasting "x sepcharacter y sepcharacter z".
# Returns a data.table or a matrix.
  if (!is.data.table(x)) x <- data.table(x)
  if (!is.data.table(y)) y <- data.table(y)
  if (!is.data.table(z)) z <- data.table(z)
  xy <- interactXY(x, y)
  xz <- interactXY(x, z)
  yz <- interactXY(y, z)
  xyz <- data.table(array(dim = c(nrow(x), ncol(x)*ncol(y)*ncol(z)), unclass(NA)))
  for (j1 in 1:ncol(x)) 
    for (j2 in 1:ncol(y)) 
      for (j3 in 1:ncol(z)) 
        set(xyz, j = (j1 - 1) * (ncol(y) * ncol(z)) +
                        (j2 - 1) * (ncol(z)) + 
                        j3, 
          value = x[[j1]] * y[[j2]] * z[[j3]])
  cn.xyz <- unlist(lapply(1:ncol(x), function(i) 
     paste0(colnames(x)[i], sepcharacter, colnames(y)))) 
  cn.xyz <- unlist(lapply(1:length(cn.xyz), function(i) 
     paste0(cn.xyz[i], sepcharacter, colnames(z)))) 
  setnames(xyz, cn.xyz)
  w <- data.table(xy, xz, yz, xyz)
  if (ReturnMatrix) w <- as.matrix(w)
  return(w)
}
combineNamesXYZ <- function(x, y, z, sepcharacter = "*", pivot = "x") {
#  x, y, z: character vectors
  if (pivot == "x") {
    cn.xyz <- unlist(lapply(1:length(x), function(i) 
       paste0(x[i], sepcharacter, y))) 
    cn.xyz <- unlist(lapply(1:length(cn.xyz), function(i) 
       paste0(cn.xyz[i], sepcharacter, z))) 
  } else 
  if (pivot == "z") {
    cn.xyz <- unlist(lapply(1:length(y), function(i) 
       paste0(x, sepcharacter, y[i]))) # 1a, 2a, 1b, 2b
    cn.xyz <- unlist(lapply(1:length(z), function(i) 
       paste0(cn.xyz, sepcharacter, z[i]))) # 1aA, 2aA, 1bA, 2bA, 1aB, 2aB, 1bB, 2bB, 
  }
  cn.xyz
}
