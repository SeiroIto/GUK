map2bigmat <- function(tabl,bigmatrownames,perl=TRUE,startwithslash=FALSE)
{
  ii <- NULL;
  if (perl==FALSE)
  {
  for (i in 1:nrow(tabl))
    ii <- c(ii,
        grep(rownames(tabl)[i],bigmatrownames, ignore.case=F, fixed=T));
  } else 
  {
    str1 <- "^";
    if (startwithslash==TRUE) str1 <- paste(str1,rep("\\",1),sep=",");
    for (i in 1:nrow(tabl))
      ii <- c(ii,
      #  since each entries are considered as a line
      #  we need ^XXX$ for shortest matching
              grep(paste(paste(str1,rownames(tabl)[i],sep=""),"$",sep=""),
              perl=T,bigmatrownames,ignore.case=F));
}
return(ii);
}

tabs2latex <- function(est, digits = 3, useperl = T, ...)
#  produces estimated results table with asterisks, se,
#  from est,  a list of estimated tables.
#  each est[[i]] is a table of estimate, std error, p value
#  e.g., est <- list(e1=est1,e2=est2,e3=est3,e4=est4)
#  requires "map2bigmat"
#  useperl: if F, not use perl in map2bigmat 
#                (perl creates problems with rownames with escape characters)
{
  n <- length(est); rn <- rownames(est[[1]])
  for (i in 2:n) rn <- union(rn, rownames(est[[i]]))
  rn <- c(t(cbind(rn, paste("se$_{", rn, "}$", sep=""))))
  bigmat <- array("",  dim=c(length(rn), n))
  for (i in 1:n)
  {
    tab <- matrix(as.numeric(as.matrix(est[[i]])), byrow=F, ncol=3)
    dimnames(tab) <- dimnames(est[[i]])
    tab <- tabstar1(tab, digits=digits)
    if (length(grep("\\(?Intercept\\)?$|^Const\\.$", rn, perl=T))>0)
    {
      bigmat[c(1, map2bigmat(tab, rn)), i] <- 
        as.character(tab[seq(1, nrow(tab)-1, 2), 1])
      bigmat[c(1, map2bigmat(tab, rn))+1, i] <- 
        as.character(tab[seq(2, nrow(tab), 2), 1])
    } else
    {
      bigmat[map2bigmat(tab, rn), i] <- 
        as.character(tab[seq(1, nrow(tab)-1, 2), 1])
      bigmat[map2bigmat(tab, rn)+1, i] <- 
        as.character(tab[seq(2, nrow(tab), 2), 1])
    }
  }
  bigmat <- data.frame(bigmat)
  rownames(bigmat) <- rn
  colnames(bigmat) <- paste("(", 1:n, ")", sep="")
  return(bigmat)
}

tabulate.est <- function(est, reorder = NULL, output.in.list = F,
  drop.dots = F, 
  lastLevelVariable = NULL, inter.with = NULL, 
  addbottom = NULL, subst.table = NULL)
# Tabulate est (list of estimated results) in a single table,
# and output in cbind(rn, table) or list(rn, table).
# Uses tabs2latex, reordertab, addaseparatingline functions.
#   reorder: ordering of variable names in regexp (to be used in reordertab)
#   output.in.list: if T, output is list(rn, table), if F, cbind(rn, table)
#   drop.dots: if T, drop "." from variable names
#   lastLevelVariable: regexp for the last level variable 
#     so I can insert a separating line using addaseparatingline
#   inter.with: name of interaction variables (main of main*cross)
#   addbottom: Lines (such as "n", "R2", etc.) to be added at the bottom
#   subst.table: variable name substitution table 
#    (see: c:/dropbox/data/ramadan/program/substitution_table.R)
# 
{
  tb <- tabs2latex(est)
  rn <- rownames(tb)
  if (drop.dots) rownames(tb) <- rn <- gsub("\\.", "", rn)
  #  order according to regexp order
  if (!is.null(reorder)) tb <- reordertab(tb, reorder)
  # set a separating line for interaction terms using addaseparatingline
  sepline.text <- paste0("\\hspace{-.1em}\\textit{\\footnotesize interaction with ", inter.with, "}")
  if (!is.null(lastLevelVariable))
    tb <- addaseparatingline(tb, lastLevelVariable, add = 1, message = sepline.text) else
    tb <- addaseparatingline(tb, "^any", add = 1, message = sepline.text)
  tb <- addaseparatingline(tb, "interaction", message = "")
  if (!is.null(addbottom)) tb <- rbind(as.matrix(tb), addbottom)
  # Get rownames again after reordering/addition of rows
  rn <- rownames(tb)
  #  replace variable names, deleting std error names
  if (!is.null(subst.table)) {
    for (k in 1:nrow(subst.table)){
      if (any(grepl(subst.table[k, "org"], rn))) 
        rn[grep(subst.table[k, "org"], rn)] <- subst.table[k, "changedto"]
    }
  }
  if (output.in.list) list(rn, tb) else return(cbind(rn, tb))
}

AddSubTitleInTable <- function(tb, addseparatingcols, separatingcoltitle, hcenter) 
# create subtitles within a table like below: 
# -----------------------------------------
#    This group  ...   That group
#  -----------------  ...   ----------------
{
  require(data.table)
  SepColNum <- length(addseparatingcols)
  if (SepColNum+1 != length(separatingcoltitle)) 
    warning("length(separatingcoltitle) needs to be length(addseparatingcols)+1. Possible NA in subtitle.")
  sepcolrange <- data.table(
    addthis = 0:(SepColNum-1), 
    first = c(0, addseparatingcols[-SepColNum])+1, 
    second = addseparatingcols)
  sepcolrange[, start := first + addthis + 1]
  sepcolrange[, end := second + addthis + 1]
  sepcolrange[, width := end-start + 1]
  if (length(hcenter) == 1) hcenter <- rep(hcenter, ncol(tb))
  sepcoltitle <- NULL
  for (m in 1:(length(addseparatingcols)))
    sepcoltitle <- c(sepcoltitle, 
       paste0(
         "\\multicolumn{", sepcolrange[m, width], "}{c}{",
           "\\makebox[", 
           sum(hcenter[sepcolrange[m, start]:sepcolrange[m, end]]), 
#             sum(hcenter[sepcolrange[m, first]:sepcolrange[m, second]]), 
           "cm]{\\scriptsize ",  
           separatingcoltitle[m],
           "}",
         "}")
       )
  # last group column number in table: subtract 1 for covariate name column 
  LastGroupColNum <- ncol(tb)-sum(sepcolrange[, width]) -1
  sepcoltitle <- c(sepcoltitle, 
     paste0(
       "\\multicolumn{",  LastGroupColNum, "}{c}{",
         "\\makebox[", 
         sum(hcenter[m+(1:LastGroupColNum)]), 
         "cm]{\\scriptsize ",  
         separatingcoltitle[m+1],
         "}",
       "}")
     )
  sepcoltitle <- paste(sepcoltitle, collapse = "&&")
  sepcoltitle <- paste(
    paste0(
       "\\makebox[", 
       hcenter[1], 
       "cm]{\\scriptsize\\hfil }"
     ),
     sepcoltitle, sep = "&")
  sepcoltitle <- paste(sepcoltitle, "\\\\[-.5ex]")
  sepcoltitle <- rbind(sepcoltitle, 
       paste(
         paste(paste0("\\cline{", sepcolrange[width >= 1, start], "-", 
           sepcolrange[width >= 1, end], "}"), collapse = " "), 
         paste(paste0("\\cline{", sepcolrange[nrow(sepcolrange), end]+2, "-", 
           sepcolrange[nrow(sepcolrange), end]+1+LastGroupColNum, "}"), collapse = " "), 
          "\\\\")
    )
  rownames(sepcoltitle) <- NULL
  return(sepcoltitle)
}

latextab <- function(tab, hleft = NULL, hcenter = NULL, unit = "cm", 
                               hright = NULL, hline = NULL, cline = F, 
                               addseparatingcols = NULL,
                               separatingcolwidth = NULL,
                               separatingcoltitle = NULL,
                               addsubcoltitlehere = T,
                               NoVariablenameColumn = F,
                               # When using addheaderAbove or addheaderBelow, they do not take addseparatingcols into consideration, need to correct
                               LastDiffVariable = NULL, 
                               SepLineText = "interaction with",
                               inter.with = NULL,
                               AdjustInterWith = NULL,
                               InterWithLength = ".5em",
                               adjustlineskip = NULL, adjlskiprows = 2,
                               delimiterline = NULL, nohorizontalline = T,
                               addheaderAbove = NULL, addheaderBelow = NULL,
                               addtopstripspace = F, 
                               headercolor = NULL, alternatecolor = NULL,
                               alternatecolor2 = NULL, alternatestart2 = 3, 
                               alternatecolor3 = NULL, alternatestart3 = 4, 
                               alternatecolor4 = NULL, alternatestart4 = 5, 
                               alternatecolorManual = NULL,
                               alternatecolorManualColor = NULL,
                               estimationspace = NULL, estimationspacelast = 0, 
                               altestspace = NULL,
                               yesnospace = "-.5ex")
#  produces a LaTeX table.
#    hleft = header left: >{ "here" }
#    hcenter = header center length: p{ "here" lengthunit}, need for 
#    unit = header center length unit: p{ length "here" }
#    hright = header right: <{ "here" }
#    hline = T: inserts "\hline" after each line
#             = numeric vector: inserts "\hline" at specified rows
#    cline = matrix(row, from, to) that gives "\cline{from-to}" at specified rows
#    addseparatingcols: insert thin column(s) specified by column number. 
#      E.g., c(2, 4) give:
#                       (1) (2) [HERE] (3) (4) [HERE] (5)
#    separatingcolwidth: Thin column widths in cm.
#    separatingcoltitle: Titles of separat*ed* columns.
#    addsubcoltitlehere: If T, add titles of separat*ed* columns. 
#    NoVariablenameColumn: if T, shift one column to right
#    LastDiffVariable: regexp for the last differenced variable (last row of panel A)
#      so I can insert a separating line "paste(SepLineText, inter.with)"
#    SepLineText: Default is "'interation with'".
#    inter.with: name of interaction variables (main of main*cross)
#    AdjustInterWith: number of rows to be added to LastDiffVariable. (1st row of panel B)
#      If I want to place separting row 1 below from deafault, set to 1.
#    InterWithLength: length to be shifted right, \hspace{HERE}, default is .5em
#    adjustlineskip: insert specified string to every second ""\\[here]""
#    adjlskiprows: If a numeric vector, ""\\[adjustlineskip]"" is added to adjlskiprows rows, 
#      not every second row. If a scalar of 2, adds after every second rows. If a scalar of 3, 
#      adds a vertical space every after second and third rows, so 
#      estimate, p value, confidence interval can be grouped together.
#      Default is 2. If adjustlineskip is NULL, nothing is added.
#    delimiterline: if NULL, "|" will be eliminated
#    nohorizontalline: if T, drop all horizontal lines
#    addheaderAbove: if not NULL, additional header row attached above the header 
#      If "^num" ("numeric"), add (1), (2), ... or c("str1", "str2", "str3", ...) is added to header
#    addheaderBelow: if not NULL, additional header row attached below the header 
#    addtopstripspace: Add space by ["addtopstripspace"] between 
#       TopStripRow [(1), (2), ... line] and "(Intercept)".
#    headercolor: if not NULL, specify color of the header row
#    alternatecolor: if not NULL, specify color of the alternating rows (1, 3, ...)
#    alternatecolor2: if not NULL, specify color of the alternating 2 rows (3:4, 7:8, ...)
#    alternatecolor3: if not NULL, specify color of the alternating 3 rows (4:6, 10:12, ...)
#    alternatecolor4: if not NULL, specify color of the alternating 4 rows (5:8, 13:16, ...)
#    alternatecolorManualColor: if not NULL, specify color of the alternating rows specified
#    alternatecolorManual: if not NULL, specify rows of the alternating colour
#      These are rows before SepLineText is inserted. Need to specify rows anticipating SepLineText.
#      If SepLineText is inserted after line 3 (=2nd+1), alternatecolorManual may be 
#        2, [3], [+3], 4, ...
#      If SepLineText is inserted after line 4 (=3rd+1), alternatecolorManual may be
#        2, [3], 4, [+4], [5], 6,..
#      If SepLineText is inserted after line k (=kth+1), alternatecolorManual may be 
#        3:4, [5:6], 7:8, ..., (k-1):k, [+(k+1)], [(k+1):(k+2)], (k+3):(k+4), ...
#        3:4, [5:6], 7:8, ..., (k-3):(k-2), [(k-1):k], [+(k+1)], (k+1):(k+2), ...
#    estimationspace: if not NULL, insert empty half line space in 3, 5, 7, ... rows
#    estimationspacelast: if not NULL, do not put empty space of last estimationspacelast rows
#    altestspace: if not NULL, specify color of the alternating 2+1 rows (4:5, 10:11, 16:17...)
#    yesnospace: if not NULL, insert ["here"] line space in rows only with "yes"/"no"
{
  tab0 = copy(tab)
  ResultNCol <- ncol(tab0)+length(addseparatingcols)
  if (is.null(hcenter)) hcenter <- rep(2, ResultNCol)
  if (length(hleft) == 1) 
    hleftForTab <- rep(hleft, ResultNCol) else
    hleftForTab <- c(hleft[1], rep(hleft[2], ResultNCol-1))
  if (length(hcenter) == 1) 
    hcenterForTab <- rep(hcenter, ResultNCol) else
    hcenterForTab <- c(hcenter[1], rep(hcenter[2], ResultNCol-1))
  if (length(hright) == 1) 
    hrightForTab <- rep(hright, ResultNCol) else
    hrightForTab <- c(hright[1], rep(hright[2], ResultNCol-1))
  if (!is.null(delimiterline)) dlm <- "|" else dlm <- ""
  if (nohorizontalline) holine <- "" else holine <- "\\hline"
  # column additions as separator columns
  if (!is.null(addseparatingcols)) {
    if (max(addseparatingcols) > ncol(tab)) 
      message("max(addseparatingcols) must be smaller than ncol(tab).")
    if (length(addseparatingcols) > length(separatingcolwidth))
      message("length(addseparatingcols) must be smaller than length(separatingcolwidth).")
    ## Shift table contents
    # add 1 to take the variable name column into account
    if (!NoVariablenameColumn) addseparatingcols <- addseparatingcols + 1
    # add spaces in a table
    tab <- tab0[, 1:addseparatingcols[1]]
    if (length(addseparatingcols) > 1) 
      for (i in 2:length(addseparatingcols))
         tab <- cbind(tab, "", tab0[, (addseparatingcols[i-1]+1):addseparatingcols[i]])
    tab <- cbind(tab, "", tab0[, (addseparatingcols[length(separatingcolwidth)]+1):ncol(tab0)])
    # add separating columns to hleft, hcenter, hright
    # do I need this if (!is.null(hleft)) loop?
    if (!is.null(hleft)) {
      if (any(grepl("\\$", hright))) space.hleft.char <- "$" else space.hleft.char <- ""
      # first block
      hleft0 <- hleftForTab[1:addseparatingcols[1]]
      hcenter0 <- hcenterForTab[1:addseparatingcols[1]]
      hright0 <- hrightForTab[1:addseparatingcols[1]]
      # second - (n-1) blocks
      if (length(addseparatingcols) > 1) {
        for (i in 2:length(addseparatingcols)) {
           hleft0 <- c(hleft0, space.hleft.char, 
              hleftForTab[(addseparatingcols[i-1]+1):addseparatingcols[i]])
           hcenter0 <- c(hcenter0, separatingcolwidth[i], 
              hcenterForTab[(addseparatingcols[i-1]+1):addseparatingcols[i]])
           hright0 <- c(hright0, space.hleft.char,
              hrightForTab[(addseparatingcols[i-1]+1):addseparatingcols[i]])
        }
      }
      # final block after the last separating column
      hleft0 <- c(hleft0, space.hleft.char, 
         hleftForTab[(addseparatingcols[length(separatingcolwidth)]+1):ncol(tab0)])
      hcenter0 <- c(hcenter0, separatingcolwidth[length(separatingcolwidth)], 
         hcenterForTab[(addseparatingcols[length(separatingcolwidth)]+1):ncol(tab0)])
      hright0 <- c(hright0, space.hleft.char, 
         hrightForTab[(addseparatingcols[length(separatingcolwidth)]+1):ncol(tab0)])
      hleft <- hleft0; hcenter <- hcenter0; hright <- hright0
      ## Title rows
      SubColTitle <- 
        AddSubTitleInTable(tab0, addseparatingcols-1, separatingcoltitle, hcenter)
    } 
  } else SubColTitle <- NULL
  if (!addsubcoltitlehere) SubColTitle <- NULL
  # insert "&" in table. ltab is table body, coln is header
  ltab <- coln <- NULL
  for (i in 1:ncol(tab)) 
  # tab has embeded separating columns
  {
    ltab <- cbind(ltab, tab[, i], " & ")
    if (!is.null(headercolor)) 
      coln <- c(coln, 
          paste("\\makebox[", hcenter[i], unit, "]{", 
          "\\cellcolor{", headercolor, "}", 
          colnames(tab)[i], "}", 
            sep = "", collapse = ""),
          " & ") else 
      coln <- c(coln, 
          paste("\\makebox[", hcenter[i], unit, "]{", 
          colnames(tab)[i], "}", 
            sep = "", collapse = ""),
          " & ")
  } # end for i loop
  dim(coln) <- c(1, length(coln))
  # add additional header rows
  if (!is.null(addheaderAbove)) {
    if (all(grepl("^num", addheaderAbove))) {
      if (!NoVariablenameColumn)
        coln <- 
        rbind(
          c(c("", "&"), rbind(
            paste0(
              "\\makebox[", hcenter[-1], unit, "]{",
              paste0("\\cellcolor{", headercolor, "}"),
              "(", 1:(ncol(tab)-1), ")}" 
            )
          , rep("&", ncol(tab)-1)))
          , coln) else
        coln <- rbind(
          c("", "&", rbind(
            paste0(
              "\\makebox[", hcenter[-1], unit, "]{",
              paste0("\\cellcolor{", headercolor, "}"),
              "(", 1:(ncol(tab)-1), ")}"
            )
          , rep("&", ncol(tab)-1)))
          , coln)
    } else {
      if (length(addheaderAbove) != ncol(tab)) 
        message("length(addheaderAbove) is not equal to ncol(tab).")
      if (!NoVariablenameColumn)
        coln <- rbind(
          c("", "&", rbind(
            paste0(
              "\\makebox[", hcenter, unit, "]{", 
              paste0("\\cellcolor{", headercolor, "}"),
              addheaderAbove, "}"
            )
          , rep("&", ncol(tab))))
          , coln) else 
        coln <- rbind(
          c(rbind(
            paste0(
              "\\makebox[", hcenter, unit, "]{", 
              paste0("\\cellcolor{", headercolor, "}"),
              addheaderAbove, "}"
            )
          , rep("&", ncol(tab))))
          , coln) 
     }
  } # end if: addheaderAbove
  if (!is.null(addheaderBelow)) {
    if (all(grepl("^num", addheaderBelow))) {
      coln <- rbind(coln,
        c("", "&", rbind(
          paste(
           "\\makebox[", hcenter[-1], unit, "]{", 
            paste0("\\cellcolor{", headercolor, "}"),
            "(", 1:(ncol(tab)-1), ")}", 
          sep = "")
        , rep("&", ncol(tab)-1)))
      ) 
    } else {
      if (length(addheaderBelow) != ncol(tab)) 
        message("length(addheaderBelow) is not equal to ncol(tab).")
      coln <- rbind(coln,
        c(rbind(
          paste(
            "\\makebox[", hcenter, unit, "]{", 
            paste0("\\cellcolor{", headercolor, "}"),
            addheaderBelow, "}", sep = "")
        , rep("&", ncol(tab))))
      ) 
    }
  } # end if: addheaderBelow
  if (!is.null(estimationspace))
  {
    ltab0 <- ltab[1:2, ]
    for (j in seq(3, nrow(ltab) - estimationspacelast, 2))
      ltab0 <- rbind(ltab0, rep(c("", "&"), ncol(ltab)/2), ltab[j + 0:1, ])
    # add skipped last X rows
    if (estimationspacelast > 0) ltab0 <- rbind(ltab0, ltab[nrow(ltab) - (estimationspacelast-1):0, ])
    ltab <- ltab0
    ltab <- rbind(coln, ltab)
    espace <- rep("", nrow(ltab))
    espace[seq(3, nrow(ltab) - estimationspacelast - 1, 3)] <- paste("[", estimationspace, "]", sep = "")
    linebreak <- paste(rep("\\\\", nrow(ltab)), espace, sep = "")
  } else
  {
    ltab <- rbind(coln, ltab)
    linebreak <- rep("\\\\", nrow(ltab))
  }
  # AddTopStripSpace
  if (!is.logical(addtopstripspace) & is.character(addtopstripspace)) 
    linebreak[1] <- paste0(linebreak[1], "[", addtopstripspace, "]")
  # Adjust line skip space. 
  if (is.null(adjustlineskip) & (!is.null(adjlskiprows) & length(adjlskiprows) > 1)) 
    message("Please set adjustlineskip if you set adjlskiprows.")
  if (!is.null(adjustlineskip)) {
    if (max(adjlskiprows)+1 == nrow(ltab)) 
      stop("Row space change cannot applied to the last row of table. Drop the last row from adjlskiprows.")
    if (length(adjlskiprows) == 1) {
      if (grepl(2, adjlskiprows)) {
        linebreak[seq(2, nrow(ltab), 2)] <- 
          paste0(linebreak[seq(2, nrow(ltab), 2)], "[", adjustlineskip, "]")
      } else if (grepl(3, adjlskiprows)) {
        linebreak[c(seq(2, nrow(ltab), 2), seq(3, nrow(ltab), 3))] <- 
          c(paste0(linebreak[seq(2, nrow(ltab), 2)], "[", adjustlineskip, "]"), 
             paste0(linebreak[seq(3, nrow(ltab), 3)], "[", adjustlineskip, "]")
            )
      }
    } else if (length(adjlskiprows)>1) {
      linebreak[adjlskiprows+1] <- 
        paste(linebreak[adjlskiprows+1], "[", adjustlineskip, "]", sep = "")
    }
  }
  if (!is.null(yesnospace))
  {
    iiyesnospace <- apply(apply(ltab[, seq(3, ncol(ltab), 2), drop = F], 1, 
                                                  function(x) whichgrep("yes|^no$", x)), 2, all)
    linebreak[iiyesnospace] <- paste("\\\\[", yesnospace, "]", sep = "")
  }
  if (!is.null(hline)) linebreak <- paste(linebreak, holine, sep ="") 
  if (!is.null(alternatecolorManual) & is.null(alternatecolorManualColor)) 
    message("You need to specify alternatecolorManualColor.")
  if (is.null(alternatecolorManual) & !is.null(alternatecolorManualColor))
    message("You need to specify alternatecolorManual.")
  if (!is.null(alternatecolorManual) & !is.null(alternatecolorManualColor)) {
    linebreak[alternatecolorManual] <- 
      paste(linebreak[alternatecolorManual], "\\rowcolor{", alternatecolorManualColor, "}", sep = "")
  } else {
    if (!is.null(alternatecolor))
        linebreak[seq(2, nrow(ltab), 2)] <- 
          paste(linebreak[seq(2, nrow(ltab), 2)], "\\rowcolor{", alternatecolor, "}", sep = "")
    if (!is.null(alternatecolor2))
    {
      #  block sequence:  3:4, 7:8, ..., nrow(ltab)
      seqby2 <- rep(seq(alternatestart2, nrow(ltab), 4), each=2)
      seqby2[seq(2, length(seqby2), 2)] <- seqby2[seq(1, length(seqby2), 2)] + 1
      if (seqby2[length(seqby2)] > nrow(ltab)) seqby2 <- seqby2[-length(seqby2)] 
        linebreak[seqby2] <- 
          paste(linebreak[seqby2], "\\rowcolor{", alternatecolor2, "}", sep = "")
     }
    if (!is.null(alternatecolor3))
    {
      #  block sequence:  4:6, 10:12, ..., nrow(ltab)
      seqby3 <- rep(seq(alternatestart3, nrow(ltab), 6), each=3)
      for (k in 2:3)
        seqby3[seq(k, length(seqby3), 3)] <- seqby3[seq(1, length(seqby3), 3)] + k-1
      while (seqby3[length(seqby3)] > nrow(ltab)) seqby3 <- seqby3[-length(seqby3)] 
        linebreak[seqby3] <- 
          paste(linebreak[seqby3], "\\rowcolor{", alternatecolor3, "}", sep = "")
     }
    if (!is.null(alternatecolor4))
    {
      #  block sequence:  5:8, 13:16, ..., nrow(ltab)
      seqby4 <- rep(seq(alternatestart4, nrow(ltab), 8), each = 4)
      for (k in 2:4)
        seqby4[seq(k, length(seqby4), 4)] <- seqby4[seq(1, length(seqby4), 4)] + k-1
      while (seqby4[length(seqby4)] > nrow(ltab)) seqby4 <- seqby4[-length(seqby4)] 
        linebreak[seqby4] <- 
          paste(linebreak[seqby4], "\\rowcolor{", alternatecolor4, "}", sep = "")
    }
    if (!is.null(altestspace))
    {
      #  block sequence:  4, 5, 10, 11, 16, 17,..., nrow(ltab) - estimationspacelast
      seqby2 <- rep(seq(4, nrow(ltab) - estimationspacelast, 6), each=2)
      seqby2[seq(2, length(seqby2), 2)] <- seqby2[seq(1, length(seqby2), 2)] + 1
      if (seqby2[length(seqby2)] > nrow(ltab) - estimationspacelast) seqby2 <- seqby2[-length(seqby2)] 
        linebreak[seqby2] <- 
          paste(linebreak[seqby2], "\\rowcolor{", altestspace, "}", sep = "")
    }
  } # end if: !is.null(alternatecolorManual) & !is.null(alternatecolorManualColor)
  ltab[, ncol(ltab)] <- linebreak
      # cline[, 1] + 1 because of the header line
  if (!is.null(dim(cline)))
  {
    if (nrow(ltab) == cline[nrow(cline), 1])
    {
      cat("\\cline cannot be added at the bottom row...ignored.")
      cline <- cline[-nrow(cline), , drop = FALSE]
    }
    clineadd <- rep("", nrow(ltab))
    clineadd[cline[, 1] + 1] <- paste("\\cline{", cline[, 2], "-", cline[, 3], "}", sep = "")
      # cline[, 1] + 1 because of the header line
    ltab[, ncol(ltab)] <- paste(ltab[, ncol(ltab)], clineadd, sep = "")
  }
  # Make sure 1st and last rows are only "\\\holine"
  ltab[c(1, nrow(ltab)), ncol(ltab)] <- paste("\\\\", holine, sep = "")
  # Collapse table into a single column table.
  ltab2 <- NULL
  for (i in 1:nrow(ltab)) ltab2 <- rbind(ltab2, paste(ltab[i, ], collapse = ""))
  if (!is.null(hleft))
    head <- paste(
      c("\\begin{tabular}{", 
         t(
            cbind(
              paste(dlm, ">{", hleft, "}", sep = ""), 
              paste("p{", hcenter, unit, "}", sep = ""),
              paste("<{", hright, "}", sep = "")
            )
          ), dlm, "}")
        , collapse = "") else
    head <- paste(
      c("\\begin{tabular}{", 
         t(
            cbind(
              paste(dlm, ">{\\scriptsize\\hfil", rep("", ncol(tab)), "}", sep = ""), 
              paste("p{", hcenter, unit, "}", sep = ""),
              paste("<{", rep("", ncol(tab)), "}", sep = "")
            )
          ), dlm, "}")
      , collapse = "")
  foot <- "\\end{tabular}"
  if (!is.null(headercolor)) head <- paste0(head, "\\rowcolor{", headercolor, "}")
  ltab3 <- rbind(head, holine, SubColTitle, ltab2, foot)
  # Drop any empty rows.
  if (any(iidrop <- which(ltab3 == ""))) ltab3 <- ltab3[-iidrop, , drop = F]
  rownames(ltab3) <- NULL
  # set a separating line for interaction terms using addaseparatingline
  if (!is.null(LastDiffVariable) & !is.null(inter.with)) {
    sepline.text <- paste0("\\multicolumn{", ncol(ltab)/2, "}{l}{\\textit{\\footnotesize ", 
      SepLineText, " ", inter.with, "}}\\\\")
    inserthere <- grep(LastDiffVariable, ltab[, 1])
    # shift separating line by AdjustInterWith rows
    if (!is.null(AdjustInterWith)) inserthere <- inserthere + AdjustInterWith
    if (length(inserthere) == 0) 
      stop("LastDiffVariable (line starting inter.with) is not found in ltab[,1].")
    # shift first column InterWithLength (default: .5em) to right
    ltab2[(inserthere+2):nrow(ltab2), 1] <- 
      paste0("\\hspace{", InterWithLength, "}", 
        gsub(paste0(hcenter[1], unit), paste0(hcenter[1]-.5, unit), 
          ltab2[(inserthere+2):nrow(ltab2), 1]))
    ltab2 <- matrix(c(ltab2[1:(inserthere+1), ], sepline.text, 
      ltab2[(inserthere+2):nrow(ltab2), ]))
    ltab3 <- rbind(head, holine, SubColTitle, ltab2, foot)
#    ltab3asis[(inserthere+3):nrow(ltab3asis), 1] <- 
#      paste0("\\hspace{.5em}", 
#        gsub(paste0(hcenter[1], unit), paste0(hcenter[1]-.5, unit), 
#          ltab3asis[(inserthere+3):nrow(ltab3asis), 1]))
#    ltab3 <- matrix(c(ltab3asis[1:(inserthere+2), ], sepline.text, 
#      ltab3asis[(inserthere+3):nrow(ltab3asis), ], foot))
    rownames(ltab3) <- NULL
  }
  return(ltab3)
}

