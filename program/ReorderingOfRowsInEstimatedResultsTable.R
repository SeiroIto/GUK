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
# rn.new <- c(
#   #grep("\\(Intercept\\)$", rn)+0:1, 
#   grep("^dummyPrimary$", rn)+0:1, 
#   grep("^dummyJunior$", rn)+0:1, 
#   grep("^dummyHigh$", rn)+0:1,
#   grep("^dummyTraditional$", rn)+0:1, 
#   grep("^dummyLarge$", rn)+0:1, 
#   grep("^dummyLargeGrace$", rn)+0:1, 
#   grep("^dummyCow$", rn)+0:1,
#   grep("^dummyModeratelyPoor$", rn)+0:1,
#   grep("^dummyUltraPoor$", rn)+0:1,
#   grep("^dummyWithoutGrace$", rn)+0:1,
#   grep("^dummyWithGrace$", rn)+0:1,
#   grep("^dummySmallSize$", rn)+0:1,
#   grep("^dummyLargeSize$", rn)+0:1,
#   # dummy*School
#   c(rbind(grep("(?=^dummy[LCWMU].*dummy[JH])(?!.*Female)(?!.*Time)", rn, perl = T), 
#         grep("(?=^dummy[LCWMU].*dummy[JH])(?!.*Female)(?!.*Time)", rn, perl = T)+1)),
#   # Female, School*Female*
#   grep("^Female$", rn)+0:1,
#   grep("^dummyPrimary.Female$", rn)+0:1, 
#   grep("^dummyJunior.Female$", rn)+0:1, 
#   grep("^dummyHigh.Female$", rn)+0:1,
#   # dummy*Female
#   c(rbind(grep("(?=^dummy[TLCWMUS].*\\.Female)(?!.*Pri|.*J|.*H)(?!.*Time)", rn, perl = T), 
#         grep("(?=^dummy[TLCWMUS].*\\.Female)(?!.*Pri|.*J|.*H)(?!.*Time)", rn, perl = T)+1)),
#   # dummy*School*Female*
#   c(rbind(grep("(?=^dummy[TLCWMUS].*Female)(?=.*Pri|.*J|.*H)(?!.*Time)", rn, perl = T), 
#         grep("(?=^dummy[TLCWMUS].*Female)(?=.*Pri|.*J|.*H)(?!.*Time)", rn, perl = T)+1)),
#    # other level covariates
#   c(rbind(grep("(?=^Flood|^Head|^Eldest)", rn, perl = T), 
#         grep("(?=^Flood|^Head|^Eldest)", rn, perl = T)+1)),
#   # time interactions: Time3, School*Time3, dummyX*Time3, School*dummyX*Time3, 
#   # Female*Time3, dummyX*Female*Time3, School*dummyX*Female*Time3
#   grep("^Time.3$", rn) +0:1,
#   c(rbind(grep("(?=^dummy[PJH].*[yrh]\\.Time.?3$)(?!.*Female)", rn, perl = T), 
#     grep("(?=^dummy[PJH].*[yrh]\\.Time.?3$)(?!.*Female)", rn, perl = T)+1)),
#   c(rbind(grep("(?=^dummy[TLCSWUM].*\\.Time.?3$)(?!.*Female)", rn, perl = T), 
#     grep("(?=^dummy[TLCSWUM].*\\.Time.?3$)(?!.*Female)", rn, perl = T)+1)),
#   c(rbind(grep("(?=^dummy[PJH].*\\.Time.?3$)(?!.*Female)", rn, perl = T), 
#     grep("(?=^dummy[PJH].*\\.Time.?3$)(?!.*Female)", rn, perl = T)+1)),
#   grep("^Fe.*\\.Time.?3$", rn) +0:1,
#   c(rbind(grep("^(?=dummy[TLCSWUM].*\\.Time.?3$)(?=.*Female)", rn, perl = T), 
#     grep("^(?=dummy[TLCSWUM].*\\.Time.?3$)(?=.*Female)", rn, perl = T)+1)),
#   c(rbind(grep("(?=^dummy[PJH].*\\.Time.?3$)(?=.*Female)", rn, perl = T), 
#     grep("(?=^dummy[PJH].*\\.Time.?3$)(?=.*Female)", rn, perl = T)+1)),
#   grep("^Time.4$", rn) +0:1,
#   c(rbind(grep("(?=^dummy[PJH].*[yrh]\\.Time.?4$)(?!.*Female)", rn, perl = T), 
#     grep("(?=^dummy[PJH].*[yrh]\\.Time.?4$)(?!.*Female)", rn, perl = T)+1)),
#   c(rbind(grep("^(?=dummy[TLCSWUM].*\\.Time.?4$)(?!.*Female)", rn, perl = T), 
#     grep("^(?=dummy[TLCSWUM].*\\.Time.?4$)(?!.*Female)", rn, perl = T)+1)),
#   grep("^Fe.*\\.Time.?4$", rn) +0:1,
#   c(rbind(grep("^(?=dummy[TLCSWUM].*\\.Time.?4$)(?=.*Female)", rn, perl = T), 
#     grep("^(?=dummy[TLCSWUM].*\\.Time.?4$)(?=.*Female)", rn, perl = T)+1)),
#   c(rbind(grep("^(?=dummy[PJH].*\\.Time.?4$)(?=.*Female)", rn, perl = T), 
#     grep("^(?=dummy[PJH].*\\.Time.?4$)(?=.*Female)", rn, perl = T)+1))
#   )

rn.new1 <- list(
  "^\\(Intercept\\)$",
  "^dummyPrimary$",
  "^dummyJunior$", 
  "^dummyHigh$",
  "^dummyTraditional$", 
  "^dummyLarge$", 
  "^dummyLargeGrace$", 
  "^dummyCow$",
  "^dummyModeratelyPoor$",
  "^dummyUltraPoor$",
  "^dummySmallSize$",
  "^dummyLargeSize$",
  "^dummyWithoutGrace$",
  "^dummyWithGrace$",
  "^dummyInKind$",
  "^dummyCash$",
  "^PureControl$")
Rn <- unlist(lapply(rn.new1, AppendThisMatch, rn))
RN <- c(rbind(Rn, Rn+1))
  # dummy*School
Rn <- AppendThisMatch("(?=^dummy[LCWMUNI].*dummy[JH])(?!.*Female)(?!.*Time)(?!.*HadCows)", rn, Perl = T)
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # Female, School*Female*
rn.new2 <-  list("^Female$",
  "^dummyPrimary.Female$", 
  "^dummyJunior.Female$", 
  "^dummyHigh.Female$")
Rn <- unlist(lapply(rn.new2, AppendThisMatch, rn))
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # dummy*Female
  # dummy*School*Female*
rn.list2 <- list("(?=^dummy[TLCWMUSNI].*\\.Female)(?!.*Pri|.*J|.*H)(?!.*Time)", 
    "(?=^dummy[TLCWMUSNI].*Female)(?=.*Pri|.*J|.*H)(?!.*Time)")
Rn <- unlist(lapply(rn.list2, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
Rn <- AppendThisMatch("^Time.?2$|^LY2$", rn)
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # time interactions: Time2, School*Time2 (Primary, Junior, High)
  # dummyX*Time2, School*dummyX*Time2, 
  # Female*Time2, dummyX*Female*Time2, School*dummyX*Female*Time2
rn.list32 <- list("(?=^dummy[PJH].*[yrh]\\.Time.?2$)(?!.*Female)(?!.*HadCows)",
  "(?=^dummy[TLCSWIUMN].*\\.Time.?2$)(?!.*Female)(?!.*HadCows)", 
  "(?=^dummy[TLCSWIUMN].*\\.LY2$)(?!.*Female)(?!.*HadCows)", 
  "(?=^Pure.*\\.Time.?2$)(?!.*Female)(?!.*HadCows)",
    "(?=^dummy[PJH].*\\.Time.?2$)(?!.*Female)(?!^dummy[PJH].*[yrh]\\.Time.?2)(?!.*HadCows)") 
Rn <- unlist(lapply(rn.list32, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
Rn <- AppendThisMatch("^Fe.*\\.Time.?2$", rn)
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list33 <- list("^(?=dummy[TLCSWIUMN].*\\.Time.?2$)(?=.*Female)(?!.*HadCows)",  
    "(?=^dummy[PJH].*\\.Time.?2$)(?=.*Female)(?!.*HadCows)") 
Rn <- unlist(lapply(rn.list33, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
Rn <- AppendThisMatch("^Time.?3$|^LY3$", rn)
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # time interactions: Time3, School*Time3 (Primary, Junior, High), 
  # dummyX*Time3, School*dummyX*Time3, 
  # Female*Time3, dummyX*Female*Time3, School*dummyX*Female*Time3
rn.list34 <- list("(?=^dummy[PJH].*[yrh]\\.Time.?3$)(?!.*Female)(?!.*HadCows)",
  "(?=^dummy[TLCSWIUMN].*\\.Time.?3$)(?!.*Female)(?!.*HadCows)", 
  "(?=^dummy[TLCSWIUMN].*\\.LY3$)(?!.*Female)(?!.*HadCows)", 
    "(?=^Pure.*\\.Time.?3$)(?!.*Female)(?!.*HadCows)",
    "(?=^dummy[PJH].*\\.Time.?3$)(?!.*Female)(?!^dummy[PJH].*[yrh]\\.Time.?3)(?!.*HadCows)") 
Rn <- unlist(lapply(rn.list34, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
Rn <- AppendThisMatch("^Fe.*\\.Time.?3$", rn)
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list35 <- list("^(?=dummy[TLCSWIUMN].*\\.Time.?3$)(?=.*Female)(?!.*HadCows)",  
    "(?=^dummy[PJH].*\\.Time.?3$)(?=.*Female)(?!.*HadCows)") 
Rn <- unlist(lapply(rn.list35, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
Rn <- AppendThisMatch("^Time.?4$|^LY4$", rn)
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list5 <- list("(?=^dummy[PJH].*[yrh]\\.Time.?4$)(?!.*Female)(?!.*HadCows)", 
    "^(?=dummy[TLCSWIUMN].*\\.Time.?4$)(?!.*Female)(?!.*HadCows)",
    "^(?=dummy[TLCSWIUMN].*\\.LY4$)(?!.*Female)(?!.*HadCows)",
  "(?=^Pure.*\\.Time.?4$)(?!.*Female)(?!.*HadCows)"
  )
Rn <- unlist(lapply(rn.list5, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
Rn <- AppendThisMatch("^Fe.*\\.Time.?4$", rn)
RN <- c(RN, c(rbind(Rn, Rn+1)))
rn.list6 <- list("^(?=dummy[TLCSWIUMN].*\\.Time.?4$)(?=.*Female)(?!.*HadCows)", 
  "^(?=dummy[PJH].*\\.Time.?4$)(?=.*Female)(?!.*HadCows)(?!.*HadCows)")
Rn <- unlist(lapply(rn.list6, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # Arm*HadCows
rn.list7 <- list("(?=^dummyHadCows$)", 
  "(?=^dummyHadCows\\.dummy[TLCWI].*$)",
  "(?=^dummyHadCows.T.*2)", 
  "(?=^dummyHadCows\\.dummy[TLCWI].*\\.T.*2)", 
  "(?=^dummyHadCows.T.*3)", 
  "(?=^dummyHadCows\\.dummy[TLCWI].*\\.T.*3)", 
  "(?=^dummyHadCows.T.*4)", 
  "(?=^dummyHadCows\\.dummy[TLCWI].*\\.T.*4)", 
  "(?=^NumCowsO)")
Rn <- unlist(lapply(rn.list7, AppendThisMatch, rn, T))
RN <- c(RN, c(rbind(Rn, Rn+1)))
  # other level covariates
Rn <- AppendThisMatch("(?=^RM|^Flood|^Head|^Eldest)", rn, T)
RN <- c(RN, c(rbind(Rn, Rn+1)))

rn.new <- c(RN, (1:nrow(thisEsttab))[-RN])
