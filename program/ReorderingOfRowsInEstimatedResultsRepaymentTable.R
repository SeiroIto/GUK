# reorder rows
AppendThisMatch <- function(str, rownames, Perl = F) {
  if (Perl) {
    if (length(iim <- grep(str, rownames, perl = Perl)) > 0) 
      iim else 
      iim <- NULL
  } else {
    if (length(iim <- grep(str, rownames)) > 0) 
      iim <- iim else 
      iim <- NULL
  }
  return(iim)
}
rn.new1 <- list(
  "^\\(Intercept\\)$",
  "^traditional$", 
  "^Large$", 
  "^LargeGrace$", 
  "^Cattle$",
  "^Upfront$",
  "^WithGrace$",
  "^InKind$",
  "^UltraPoor$")
Rn <- unlist(lapply(rn.new1, AppendThisMatch, rn))
RN <- c(rbind(Rn, Rn+1))
  # dummy*dummy[MU].*Poor
Rn <- AppendThisMatch("(?=^[CILUW].*\\\\times\\$ *U.*Poor$)(?!.*LY)", rn, Perl = T)
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # time interactions: TimeY, dummyX*TimeY, 
Rn <- AppendThisMatch("^Time.2$|^LY2$", rn, Perl = T)
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list32 <- list(
  "(?=^[CILUW].*\\$ *LY2$)(?!.*HadCows)(?!.*Ultra)"
  ) 
Rn <- unlist(lapply(rn.list32, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # time interactions: 
  # UltraPoor*TimeY, dummyX*UltraPoor*TimeY
rn.list32u <- list(
  "(?=^Ul.*\\$ *LY2$)(?!.*HadCows)", 
  "(?=^[CILUW].*\\$ *Ul.*\\$ *LY2$)(?!.*HadCows)"
  ) 
Rn <- unlist(lapply(rn.list32u, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))

Rn <- AppendThisMatch("^Time.?3$|^LY3$", rn)
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list33 <- list(
  "(?=^[CILUW].*\\$ *LY3$)(?!.*HadCows)(?!.*Ultra)"
  )
Rn <- unlist(lapply(rn.list33, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list33u <- list(
  "(?=^Ul.*\\$ *LY3$)(?!.*HadCows)", 
  "(?=^[CILUW].*\\$ *Ul.*\\$ *LY3$)(?!.*HadCows)"
  )
Rn <- unlist(lapply(rn.list33u, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))

Rn <- AppendThisMatch("^Time.?4$|^LY4$", rn)
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list34 <- list(
  "(?=^[CILUW].*\\$ *LY4$)(?!.*HadCows)(?!.*Ultra)"
  )
Rn <- unlist(lapply(rn.list34, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list34u <- list(
  "(?=^Ul.*\\$ *LY4$)(?!.*HadCows)", 
  "(?=^[CILUW].*\\$ *Ul.*\\$ *LY4$)(?!.*HadCows)"
  )
Rn <- unlist(lapply(rn.list34u, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # Arm*HadCows
rn.list7 <- list("(?=^dummyHadCows$)", 
  "(?=^dummyHadCows:Arm[lc])",
  "(?=^dummyHadCows:Attributes[tILW])",
  "(?=^dummyHadCows.LY2)", 
  "(?=^dummyHadCows:Arm[lc].*:LY2)", 
  "(?=^dummyHadCows:Attributes[tILW].*:LY2)", 
  "(?=^dummyHadCows.LY3)", 
  "(?=^dummyHadCows:Arm[lc].*:LY3)", 
  "(?=^dummyHadCows:Attributes[tILW].*:LY3)", 
  "(?=^dummyHadCows:Arm[lc].*:LY4)", 
  "(?=^dummyHadCows:Attributes[tILW].*:LY4)", 
  "(?=^NumCowsO)")
Rn <- unlist(lapply(rn.list7, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # other level covariates
rn.listother <- list(
  "(?=^GRSRhigh$)", 
  "(?=^Group shortfall)", 
  "(?=^Shortfall)", 
  "(?=^GRSRhigh )"
  )
Rn <- unlist(lapply(rn.listother, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
Rn <- AppendThisMatch("(?=^RM|^Flood|^Head|^Eldest)", rn, T)
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.new <- c(RN, (1:nrow(r.tab))[-RN])
