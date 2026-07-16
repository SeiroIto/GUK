#### ==========================================================
#### Linearised from: C:/data/GUK/analysis/program/EstimationGUK_Tufte.Rmd
#### docdir: C:/data/GUK/analysis/program
#### Generated 2026-07-16 20:26 JST by extract_chunks.R -- edit the source document, not this file.
#### eval = F bodies are #-commented (safe whole-file paste); [inert]
#### children never run under this parent (see tool header for why).
#### ==========================================================
#### ---- project preamble: C:/data/GUK/analysis/.claude/.scratch/extract_chunks/guk_preamble.R ----
#### GUK prerequisites (mirrors knit_guk.R render harness)
source("c:/seiro/settings/Rsetting/functions.R")   # grepout() etc.
path0 <- "c:/data/GUK/"
path  <- paste0(path0, "analysis/")
setwd(path)
library(rmarkdown)
library(knitr)
#### ---- end project preamble ----
setwd("C:/data/GUK/analysis/program")  # knitr evaluates chunks in the document directory
#### runchunks(to, from): run entries of this file in order,
####   from = header substring of the first chunk to run (default: top
####          of file, including the preamble above)
####   to   = header substring; stop on the line BEFORE it (default: end)
#### eval = F entries are always skipped. Examples:
####   runchunks("construct confi")               # top -> before chunk
####   runchunks("reshape confi", "set parameters") # slice
runchunks <- function(to = NULL, from = NULL,
  file = "C:/data/GUK/analysis/.claude/.scratch/extract_chunks/EstimationGUK_Tufte_chunks.R") {
  old <- getOption("knitr.duplicate.label")          # keep old value
  options(knitr.duplicate.label = "allow")            # allow re-runs
  on.exit(options(knitr.duplicate.label = old))       # restore on exit
  L <- readLines(file, warn = FALSE, encoding = "UTF-8")
  ## cwd change is NECESSARY to mock knitr's internal behaviour for these
  ## globalenv() replays: knit()/render() evaluate every chunk with
  ## cwd = the document dir (in_dir(input_dir())), so doc-relative paths
  ## ("../program/x.prn") only resolve from there. Save/set/restore.
  dd <- sub("^#### docdir: ", "", grep("^#### docdir: ", L, value = TRUE)[1])
  if (!is.na(dd)) {
    oldwd <- getwd()                                   # keep old cwd
    setwd(dd)                                          # set document dir
    on.exit(setwd(oldwd), add = TRUE)                  # restore on exit
  }
  hi <- grep("^#### (knit_child:|inline,|.*, lines [0-9]+-[0-9]+)", L)
  hit <- length(L) + 1L
  if (!is.null(to)) {
    hit <- hi[grepl(to, L[hi], fixed = TRUE)][1]
    if (is.na(hit)) stop("'to' not found: ", to)
  }
  lo <- 1L
  if (!is.null(from)) {
    lo <- hi[grepl(from, L[hi], fixed = TRUE)][1]
    if (is.na(lo)) stop("'from' not found: ", from)
    if (lo >= hit) stop("'from' is at or after 'to'")
  }
  keep <- rep(TRUE, hit - lo)
  for (s in hi[hi >= lo & hi < hit]) {
    if (grepl("eval = F", L[s], fixed = TRUE)) {
      e <- hi[hi > s][1]; if (is.na(e) || e > hit) e <- hit
      keep[(s:(e - 1)) - lo + 1L] <- FALSE
    }
  }
  eval(parse(text = L[lo:(hit - 1L)][keep]), envir = globalenv())
}
#### ==========================================================

#### inline, line 5    eval = T
format(Sys.time(), '%Y年%m月%d日 %R')

#### setup, lines 122-139    {r setup, echo = F, results = "hide"}    eval = T
#### include = F <==> echo=F & results = F
#### renv::init()
#### library(renv)
library(knitr)
library(tufte)
#### invalidate cache when the tufte version changes
knitr::opts_chunk$set(
  tidy = FALSE, cache.extra = packageVersion('tufte'), 
  margin_references = TRUE,
  #### remove leading hashes in html output
  comment = "", 
  echo = T, cache = F, 
  class.source = "SeiroBenign", class.output = "SeiroLightGreen"
  )
options(htmltools.dir.version = FALSE, width = 100)
gc()

#### here setup, lines 141-160    {r here setup, eval = F, echo = F}    eval = F
# #### here package tests and workshop chunk
# library(here)
# library(rprojroot)
# #### Need to set project root
# setwd("c:/data/GUK/analysis/")
# getwd()
# here::i_am("GUK/analysis/program/EstimationGUK_Tufte.Rmd")
# getwd()
# find_root_file("Workshop2024Mar26/code/EstimationCode/R", 
#   "data_simulated_20230426.csv", criterion = has_file(".git/index"))
# (RootFile <- find_root(has_file(".git/index")))
# #### List all files and directories below the root
# dir(find_root(has_file(".git/index")))
# FPath <- function(SubFolder, FileName) 
#   find_root_file(SubFolder, FileName, criterion = has_file(".git/index"))
# FPath("docs/Workshop2024Mar26/code/EstimationCode/R", "data_simulatedCD_20230426.csv")
# data_sim <- fread(FPath("docs/Workshop2024Mar26/code/EstimationCode/R", 
#   "data_simulatedCD_20230426.csv"))

#### path and def files, lines 167-222    {r path and def files, echo = T, eval = T, cache = F, warning = F, results = "hide"}    eval = T
library(data.table)
library(qs)
library(kableExtra)
pathprogram <- paste0(path, "program/");  
pathsource <- paste0(path0, "source/")
pathsave <- paste0(path, "save/")
pathsaveHere <- paste0(pathsave, "EstimationMemo/")
#### CLAUDE dea: 2026-04-29 pathfigure, pathsavefigure, pathtable assigned but
####   never read in any project file (verified via xref.sqlite, 832k refs)
# pathfigure <- pathsavefigure <- paste0(pathprogram, "figure/")
# pathtable <- paste0(pathprogram, "table/")
pathreceived <- paste0(path0, "received/")
pathcleaned <- paste0(pathreceived, "cleaned_by_RA/")
path1234 <- paste0(pathcleaned, "clean_panel_data_by_section/")
dir.create(pathsave)
dir.create(pathsaveHere)
file.remove(list.files("program/cache", full.names = T))
#### Uncommenting below deletes all input/output of R from html. Why???
#### render_listings()	####	it changes "<-" to real arrows, etc., prettifying
#### Tabulation functions
source(paste0(pathprogram, "TabulationFunctions.R"))
#### substitution table "sbt"
source(paste0(pathprogram, "SubstTable.R"))
source(paste0(pathprogram, "SubstTableANCOVA.R"))
source(paste0(pathprogram, "SubstTablePerm.R"))
#### Estimation functions
source(paste0(pathprogram, "EstimationFunctions.R"))
#### CombinenamesXYZ functions
source(paste0(pathprogram, "GetCovariatesFunctions.R"))
RMDenomination <- 1000
datafiles <- c("s1", "arA", "ar", "ass", "lvo", "lvoL", "lvp", "lab", "far", "con", "obr")
Datafiles <- c("S1", "ArA", "Ar", "Ass", "Lvo", "LvoL", "Lvp", "Lab", "Far", "Con", "Obr")
DataFileNames <- c(
  "Schooling", "AllMeetingsRepayment", "Repayment", "Asset", 
  "Livestock", "LivestockLong", "LivestockProducts",
  "LabourIncome", "FarmIncome", "Consumption", "OtherBorrowing")
ShortfallFileNames<- c("Group", "Individual", "o800")
FileNameHeader <- paste0(c("", "Grace", "PovertyStatus", "Size", "Attributes"),
  "OriginalHHs")
#### CLAUDE dea: 2026-04-29 filenamelist assigned but never read anywhere
# filenamelist <- c("Group", "Individual")
arms <- c("traditional", "large", "large grace", "cow")
Arms <- c("Traditional", "Large", "LargeGrace", "Cow")
armsC <- c("traditional", "large", "large grace", "cattle")
ArmsC <- c("Traditional", "Large", "Large grace", "Cattle")
ArmsC2 <- c("Traditional", "Large", "LargeGrace", "Cattle")
Attributes <- c("Traditional", "LargeSize", "WithGrace", "InKind")
InitialSampleMonthUpperBound <- 6
Only800 <- T
UseTrimmedSample <- T
NotPrintFileNames <- ""
#NotPrintFileNames <- "%"
PrintFormulae <- F
PermRepTimes <- 100000

#### TableFootnotes, lines 224-225    {r TableFootnotes, file = "../program/TableFootnotesHTML.R", echo = F}    eval = T
source("C:/data/GUK/analysis/program/TableFootnotesHTML.R")  # from chunk option file=

#### schooling read reg types and covariates, lines 232-244    {r schooling read reg types and covariates, echo = F, warning = F, message = F}    eval = T
FileName <- "Schooling"
FileNameHeader <- c("", "Attributes", "PovertyStatus",
  "TimeVarying", "TimeVaryingAttributes",
  "TimeVaryingPovertyStatus", "TimeVaryingPovertyStatusAttributes")
rm(additions)
#### regsuffixes <- c("", "a", "P", "T", "Ta", "TP", "TPa")
regsuffixes <- c("", "a", "P", "T", "Ta")
listheader <- paste0("sc", regsuffixes)
exclheader <- paste0("excl", regsuffixes)
inclheader <- gsub("ex", "in", exclheader)
source(paste0(pathprogram, "SchoolingCovariateSelectionANCOVA3.R"))

#### inline, line 280    eval = T
length(FileNameHeader)

#### inline, line 290    eval = T
length(additions)-1

#### inline, line 358    eval = T
CreateHTMLTable <- SimpleHTMLTable <- T

#### knit_child: subsection_Schooling.rmd, line 359    eval = T
knit_child(paste0(pathprogram, "subsection_Schooling.rmd"))

#### inline, line 363    eval = T
CreateHTMLTable <- T

#### knit_child: subsection_RepaymentAndNetSaving.rmd, line 364    eval = T
knit_child(paste0(pathprogram, "subsection_RepaymentAndNetSaving.rmd"))

#### inline, line 368    eval = T
CreateHTMLTable <- T

#### knit_child: subsection_Incomes.rmd, line 369    eval = T
knit_child(paste0(pathprogram, "subsection_Incomes.rmd"))

#### inline, line 373    eval = T
CreateHTMLTable <- T

#### knit_child: subsection_ConsumptionOLS.rmd, line 374    eval = T
knit_child(paste0(pathprogram, "subsection_ConsumptionOLS.rmd"))

#### knit_child: subsubsection_HomesteadLand.rnw, line 378    eval = F [in <!-- -->] [inert under .rmd parent]
# knit_child(paste0(pathprogram, "subsubsection_HomesteadLand.rnw"))

#### inline, line 379    eval = T [in <!-- -->]
sec <- "homestead"

#### knit_child: subsubsection_Livestock.rnw, line 381    eval = F [in <!-- -->] [inert under .rmd parent]
# knit_child(paste0(pathprogram, "subsubsection_Livestock.rnw"))

#### inline, line 382    eval = T [in <!-- -->]
sec <- "livestock"

#### knit_child: subsubsection_ProductiveAssets.rnw, line 385    eval = F [in <!-- -->] [inert under .rmd parent]
# knit_child(paste0(pathprogram, "subsubsection_ProductiveAssets.rnw"))

#### inline, line 386    eval = T [in <!-- -->]
sec <- "passets"

#### knit_child: subsubsection_NarrowProductiveAssets.rnw, line 389    eval = F [in <!-- -->] [inert under .rmd parent]
# knit_child(paste0(pathprogram, "subsubsection_NarrowProductiveAssets.rnw"))

#### inline, line 390    eval = T [in <!-- -->]
sec <- "npassets"

#### knit_child: subsubsection_ProductiveAssetsLivestock.rnw, line 393    eval = F [in <!-- -->] [inert under .rmd parent]
# knit_child(paste0(pathprogram, "subsubsection_ProductiveAssetsLivestock.rnw"))

#### inline, line 394    eval = T [in <!-- -->]
sec <- "passetslivestock"

#### inline, line 399    eval = T
sec <- "nassets"

#### inline, line 400    eval = T
CreateHTMLTable <- T

#### knit_child: subsubsection_NetAssets.rmd, line 401    eval = T
knit_child(paste0(pathprogram, "subsubsection_NetAssets.rmd"))

#### inline, line 405    eval = T
CreateHTMLTable <- T

#### knit_child: subsubsection_NetNLAssets.rmd, line 406    eval = T
knit_child(paste0(pathprogram, "subsubsection_NetNLAssets.rmd"))

#### inline, line 407    eval = T
sec <- "nnlassets"

#### inline, line 411    eval = T
CreateHTMLTable <- T

#### knit_child: subsubsection_CattleHolding.rmd, line 412    eval = T
knit_child(paste0(pathprogram, "subsubsection_CattleHolding.rmd"))

#### knit_child: subsubsection_NetBroadAssets.rmd, line 416    eval = T
knit_child(paste0(pathprogram, "subsubsection_NetBroadAssets.rmd"))

#### inline, line 417    eval = T
sec <- "bnassets"

#### inline, line 423    eval = T
CreateHTMLTable <- T

#### knit_child: subsubsection_NetAssetsExperiencedVSInexperienced.rmd, line 424    eval = T
knit_child(paste0(pathprogram, "subsubsection_NetAssetsExperiencedVSInexperienced.rmd"))

#### inline, line 425    eval = T
sec <- "nassetsbyexperience"

#### inline, line 429    eval = T
CreateHTMLTable <- T

#### knit_child: subsubsection_CattleHoldingExperiencedVSInexperienced.rmd, line 430    eval = T
knit_child(paste0(pathprogram, "subsubsection_CattleHoldingExperiencedVSInexperienced.rmd"))

#### inline, line 431    eval = T
sec <- "cattlebyexperience"

#### ConfiTableContents, lines 455-471    {r ConfiTableContents}    eval = T
contab <- fread("../program/ConfiTableContents.prn")
contab <- do.call(cbind, lapply(contab, function(x) gsub("\\*", "\\\\*", x)))
library(kableExtra)
kt <- kable(contab, format = "html")
kt <- column_spec(kt, 1, extra_css = "vertical-align:top;")
kt <- column_spec(kt, 2, width = "5cm; min-width:5cm;", 
  extra_css = "vertical-align:top;")
kt <- column_spec(kt, 3, width = "8cm; min-width:8cm;", 
  extra_css = "vertical-align:top;")
kt <- column_spec(kt, 4, extra_css = "vertical-align:top;")
kt <- kable_classic(kt, html_font = "Cambria")
#### Below will put the table in the centre of a page. Not visually good.
#### full_width and position arguments are ignored in tufte for some reasons.
#### kt <- kable_paper(kt)
#### kt <- kable_styling(kt, fixed_thead = T, full_width = F, position = "left")

#### inline, line 475    eval = T
kt

#### SchoolingConfiTableContents, lines 480-492    {r SchoolingConfiTableContents}    eval = T
contab <- fread("../program/ConfiTableContents2.prn")
contab <- do.call(cbind, lapply(contab, function(x) gsub("\\*", "\\\\*", x)))
library(kableExtra)
kt <- kable(contab, format = "html")
kt <- column_spec(kt, 1, extra_css = "vertical-align:top;")
kt <- column_spec(kt, 2, width = "5cm; min-width:5cm;", 
  extra_css = "vertical-align:top;")
kt <- column_spec(kt, 3, width = "8cm; min-width:8cm;", 
  extra_css = "vertical-align:top;")
kt <- column_spec(kt, 4, extra_css = "vertical-align:top;")
kt <- kable_classic(kt, html_font = "Cambria")

#### inline, line 496    eval = T
kt

#### set parameters, lines 504-586    {r set parameters, eval = T}    eval = T
lattributeList <- list(
  c("Large", "LargeGrace", "Cattle"),
  c("LargeSize", "WithGrace", "InKind")
  )
#### these are used for letting XX=Large, Cow, etc.
covadd0 <- list(c("\\(Intercept\\)", "dummyXX"), 
  c("Time.3", "dummyXX.Time3"),
  c("Time.4", "dummyXX.Time4"))
covaddsav <- list(c("\\(Intercept\\)", "dummyXX"), 
  c("LY2", "dummyXX.LY2"), # LY is loan year
  c("LY3", "dummyXX.LY3"),
  c("LY4", "dummyXX.LY4"))
covaddsch <- list(
  # male, traditional (MofT)
  MofT=c("\\(Intercept\\)", "^dummyJunior$", "^dummyHigh$"), 
  # female, traditional (FofT)
  FofT=c("^Female$", "^dummyJunior.Female$", "^dummyHigh.Female$"),
  # male, other arms (MofN)
  MofN=c("^dummyXX$", "^dummyXX.dummyJunior$", "^dummyXX.dummyHigh$"), 
  # female, other arms (FofN)
  FofN=c("^dummyXX.Female$", "^dummyXX.dummyJunior.Female$", 
    "^dummyXX.dummyHigh.Female$"),
  # male, trad, time (MofTinT)
  MofTinT=c("^Time.YY$", "^dummyJunior.TimeYY$", "^dummyHigh.TimeYY$"), 
  # female, trad, time (FofTinT)
  FofTinT=c("^Female.TimeYY$", "^dummyJunior.Female.TimeYY$", 
   "^dummyHigh.Female.TimeYY$"), 
  # male, other arms, time (MofNinT)
  MofNinT=c("^dummyXX.TimeYY$", "^dummyXX.dummyJunior.TimeYY$", 
  "^dummyXX.dummyHigh.TimeYY$"), 
  # female, other arms, time (FofNinT)
  FofNinT=c("^dummyXX.Female.TimeYY$", 
    "^dummyXX.dummyJunior.Female.TimeYY$", 
    "^dummyXX.dummyHigh.Female.TimeYY$")
  )
FileNames <- c(
  "Schooling", 
  "Land", "Livestock", "NumCows",
  "AssetLivestock", "NetAssets", "NetBroadAssets",
  "NetAssetsAnnualPrices", "NetNLAssets", 
  "NetAssetsExperience", 
  "NumCowsExperience", 
  paste0("NetAssetsByExperience", c("a", "o", "n")),
  paste0("NumCowsByExperience", c("a", "o", "n")),
  "LabourIncome", 
  "Consumption", "ConsumptionOLS")
ListHeaderList <- c(
  "sc", 
  "ld", "lv", "cw", 
  "al", "na", "nb", 
  "np", "nl", # non livestock assets
  "nE", "lE", # net assets by experience, livestock by experience, OwnCow0, AdiCow0
  "nA", "nO", "nN", 
  "lA", "lO", "lN",
  "lb", 
  "cn", "co")
add5 <- c("b", "P", "a", "T", "Ta")
add7 <- c("b", "P", "a", "T", "Ta", "TP", "TPa")
reglists <- list(
  paste0("sc", add5),
  paste0("ld", add7),
  paste0("lv", add5),
  paste0("cw", add7),
  paste0("al", add5),
  paste0("na", add7),
  paste0("nb", add7),
  paste0("np", add7),
  paste0("nl", add7),
  paste0("nE", add7),
  paste0("lE", add7),
  paste0("nA", add7),
  paste0("nO", add7),
  paste0("nN", add7),
  paste0("lA", add5),
  paste0("lO", add5),
  paste0("lN", add5),
  paste0("lb", add7),
  paste0("cn", add7),
  paste0("co", add7)
  )
names(reglists) <- ListHeaderList

#### run this after reestimation Read This Mannually qs, lines 589-597    {r run this after reestimation Read This Mannually qs, eval = T}    eval = T
gc()
FNstrings <- "AssetLivestock"
FNames <- FileNames[!grepl(FNstrings, FileNames)]
ListHeaderListForQS <- ListHeaderList[!grepl(FNstrings, FileNames)]
reglists <- reglists[!grepl(FNstrings, FileNames)]
robj <- lapply(1:length(FNames), function(i) 
  qread(paste0(pathsaveHere, "ANCOVA_", FNames[i],".qs")))

#### linhyp rexlog init, lines 607-616    {r linhyp rexlog init, eval = T, echo = F}    eval = T
#### Added by Claude
#### rexlog: every regex actually used by the linear-hypothesis vectors
#### below, captured in the same statement that builds it. The map chunk
#### after this section renders from rexlog, so the map can never drift
#### from the patterns the loop really used.
#### CLAUDE spl: 2026-07-14
rexlog <- list()
rexctx <- list()

#### construct confi Do This Mannually, lines 619-1165    {r construct confi Do This Mannually, eval = T, warning = F}    eval = T
confi <- linhyp <- NULL
library(car)
library(multcomp)
#### r:
#### 4 (in ListHeaderList, 5) is number of cattle
#### 5 (in ListHeaderList, 6) is net assets
#### 9 (in ListHeaderList, 11) is net non livestock assets
r <- grep("NetAssets$", FNames)
for (r in 2:length(ListHeaderListForQS)) {
  # r: outcomes
  for (rr in 1:length(robj[[r]])) {
    # rr: regression type: "b", "P", "a", "T", "Ta", "PT", "PTa"
    #### CLAUDE com: 2026-07-16 old comment said "s=2,...,7"; s runs
    ####   1..length(regobj) covariate specifications (6/4/2 by outcome)
    # regobj: one regression type's results, e.g. "asT"; elements = s specs
    regtype <- gsub("^..", "", reglists[[r]][rr])
    if (grepl("a", regtype)) 
      lattributes <- lattributeList[[2]] else 
      lattributes <- lattributeList[[1]]
    regobj <- robj[[r]][[rr]]
    if (grepl("cn", ListHeaderListForQS[r])) regobj <- regobj[1:3] # consumption: 4-6 are HH aggregates
    if (grepl("co", ListHeaderListForQS[r])) regobj <- regobj[1:2] # consumptionOLS: 4-5 are HH aggregates
    lmlist <- lapply(regobj, "[[", "lm")
    coefflist <- lapply(lmlist, "[[", "coefficients")
    c(reglists[[r]][rr]) 
    # drop NAs in coeff
    coefflist <- lapply(coefflist, function(x) x[!is.na(x)])
    Vlist <- lapply(lapply(regobj, "[[",  "robust"), "[[", "V")
    # Define covadd (a string vector used to pick coefficients for testing)
    # if regressand is saving:
    #  Change to loan year: TimeX => LYX
    #  Multiply with 12 (turn to monthly to yearly)
    if (any(grepl("sv", reglists[[r]]))) {
      covadd <- covaddsav 
      Mult <- 12
    } else {
      covadd <- covadd0
      Mult <- 1
    }
    covadd.trad <- lapply(covadd, function(x) x[1])
    for (s in 1:length(regobj)) {
      #### CLAUDE com: 2026-07-16 old comment said "7 regression
      ####   specifications" -- 7 is the count of regression TYPES rr
      ####   (regsuffixes "", "P", "a", "T", "Ta", "TP", "TPa"), not of s.
      # s: covariate specifications, length(regobj) varies by outcome:
      #    6 (assets, schooling), 4 (livestock), 2 (consumption)
      # Spec 1:  no covariates (OLS)
      # Spec 2:  └── + lagged outcomes (ANCOVA)
      # Spec 3:      └── + household level variables
      # Spec 4:          ├── + cattle ownership and interaction
      # Spec 5:          └── + cattle herd size
      # Spec 6:              └── + cattle ownership and interaction
      thisreg <- lmlist[[s]]
      coeffvec <- coefflist[[s]]
      thisV <- Vlist[[s]]
      # Consumption: No rd1 so period= 1, 2. Drop period 3 variables.
      if (grepl("cn", ListHeaderListForQS[r])) startnum <- 2 else startnum <- 1
      # construct conf int for linear combination of coeffs
      #if (grepl("cn", ListHeaderList[r])) addcova <- covadd[-2]
       ## period 2 (=overall, when no Time is present) effects ##
      # trad
      # hvT0: picks covariates to test overall change
      #  [[1]]"\\(Intercept\\)"
      #### CLAUDE spl: 2026-07-14 pattern named + logged in one statement.
      rexlog$hvT0 <- paste0("^", covadd.trad[[1]], "$")
      hvT0 <- rep(0, length(coeffvec))
      hvT0[grepl(rexlog$hvT0, names(coeffvec))] <- 1*Mult
      lhcow <- glht(model=thisreg, linfct = matrix(hvT0, byrow = T, nrow=1), 
        alternative="two.sided", vcov.=thisV)
      confi <- rbind(confi, 
         c(FNames[r], regtype, s, "trad", "None", "level of reference trad", "T0", 
         startnum, confint(lhcow)$confint[1, ], 
         if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
         )
      linhyp <- c(linhyp, list(hvT0, thisreg$coeff))
      names(linhyp)[length(linhyp)] <- 
        paste0(c(FNames[r], regtype, s, "trad", "None", "level of reference trad", startnum), collapse = "")
      # nontrad
      for (g in lattributes) {
        # g: attributes or arm
        # construct coefficient names for attribute g (replace XX with g)
        covadd.nontrad <- lapply(covadd, function(x) gsub("XX", g, x))
        # E.g., if g = Large, 
        #  [[1]]
        #  [1] "\\(Intercept\\)" "dummyLarge"     
        #  [[2]]
        #  [1] "Time.3"           "dummyLarge.Time3"
        #  [[3]]
        #  [1] "Time.4"           "dummyLarge.Time4"
        # hvN0: period 2 level for Arm
        #  [[1]]"\\(Intercept\\)", "dummyInKind"
        #### CLAUDE spl: 2026-07-14
        rexlog$hvN0 <- paste(paste0("^", covadd.nontrad[[1]], "$"),
          collapse = "|")
        hvN0 <- rep(0, length(coeffvec))
        hvN0[grepl(rexlog$hvN0, names(coeffvec))] <- 1*Mult
        lhcow <- glht(model=thisreg, linfct = matrix(hvN0, byrow = T, nrow=1), 
          alternative="two.sided", vcov.=thisV)
        confi <- rbind(confi, 
          c(FNames[r], regtype, s, g, "None", "level of reference nontrad", "N0", 
           startnum, confint(lhcow)$confint[1, ], 
           if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
          )
        linhyp <- c(linhyp, list(hvN0, thisreg$coeff))
        names(linhyp)[length(linhyp)] <- 
          paste0(c(FNames[r], regtype, s, g, "None", "level of reference nontrad", startnum), collapse = "")
        # hvN1: difference of period 2 Arm relative to period 2 trad
        #  [[1]][2] "dummyInKind"
        #### CLAUDE dea: 2026-04-29 cumstrings0 assigned but never read; peristrings0 kept
        # cumstrings0 <- peristrings0 <- paste0("^", covadd.nontrad[[1]][2], "$")
        #### CLAUDE spl: 2026-07-14
        peristrings0 <- rexlog$hvN1 <- 
          paste0("^", covadd.nontrad[[1]][2], "$")
        hvN1 <- rep(0, length(coeffvec))
        hvN1[grepl(rexlog$hvN1, names(coeffvec))] <- 1*Mult
        lhcow <- glht(model=thisreg, linfct = matrix(hvN1, byrow = T, nrow=1), 
          alternative="two.sided", vcov.=thisV)
        confi <- rbind(confi, 
          c(FNames[r], regtype, s, g, "None", "reference nontrad - reference trad", "N1", 
           startnum, confint(lhcow)$confint[1, ], 
           if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
         )
        linhyp <- c(linhyp, list(hvN1, thisreg$coeff))
        names(linhyp)[length(linhyp)] <- 
          paste0(c(FNames[r], regtype, s, g, "None", "reference nontrad - reference trad", 
          startnum), collapse = "")
         ## 2 way (Arm*Time) interactions ##
        if (grepl("T", regtype)) {
          # i: period loop. Start from startnum+1 (=2. Only in consumption, =3.)
          # for (i in (startnum+1):length(covadd.trad)) {
          #  i: period loop. Start from startnum (=1. Only in consumption, =2.)
          #  1: Time2 or reference period. 2: Time3, 3: Time4
          for (i in startnum:length(covadd.trad)) {
            # i: period index
            # trad
            # hvTinT: difference = 0 (of trad in time X relative to trad in time 2)
            # [[2]] "Time.4"
            #### CLAUDE spl: 2026-07-14
            rexlog$hvTinT <- paste0("^", covadd.trad[[i]], "$")
            hvTinT <- rep(0, length(coeffvec))
            hvTinT[grepl(rexlog$hvTinT, names(coeffvec))] <- 1*Mult
            lhcow <- glht(model=thisreg, linfct = matrix(hvTinT, byrow = T, nrow=1), 
              alternative="two.sided", vcov.=thisV)
            confi <- rbind(confi, 
              c(FNames[r], regtype, s, "trad", "None", "trad in each period - trad in period 2", 
                "TinT", 
                i, confint(lhcow)$confint[1, ], 
                if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
              )
            linhyp <- c(linhyp, list(hvTinT, thisreg$coeff))
            names(linhyp)[length(linhyp)] <- 
              paste0(c(FNames[r], regtype, s, "trad", "None", "trad in each period - trad in period 2", i), collapse = "")
            # hvTL: level = 0 (of trad in TimeX)
            # intercept + Time.X
            hvTL <- hvT0 + hvTinT
            lhcow <- glht(model=thisreg, linfct = matrix(hvTL, byrow = T, nrow=1), 
              alternative="two.sided", vcov.=thisV)
            confi <- rbind(confi, 
              c(FNames[r], regtype, s, "trad", "None", "level of trad in period X", 
                "TL", 
                i, confint(lhcow)$confint[1, ], 
                if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
            linhyp <- c(linhyp, list(hvTL, thisreg$coeff))
            names(linhyp)[length(linhyp)] <- 
              paste0(c(FNames[r], regtype, s, "trad", "None", "level of trad in period X", i), collapse = "")
            # nontrad
            # hvNinT: difference = 0 (of arm g in time X relative to arm g at period 2)
            #   intercept + Arm +TimeX + Arm.TimeX - (intercept + Arm) 
            #   = TimeX + Arm.TimeX
            # [[2]] "Time.4", "dummyInKind.Time4"
            # For period 2, it gives period 2 level of Arm. 
            #### CLAUDE spl: 2026-07-14
            rexlog$hvNinT <- paste(paste0("^", covadd.nontrad[[i]], "$"),
              collapse = "|")
            hvNinT <- rep(0, length(coeffvec))
            hvNinT[grepl(rexlog$hvNinT, names(coeffvec))] <- 1*Mult
            lhcow <- glht(model=thisreg, linfct = matrix(hvNinT, byrow = T, nrow=1), 
              alternative="two.sided", vcov.=thisV)
            confi <- rbind(confi, 
              c(FNames[r], regtype, s, g, "None", "nontrad in each period - nontrad in period 2", 
               "NinT", 
                i, confint(lhcow)$confint[1, ], 
                if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
              )
            linhyp <- c(linhyp, list(hvNinT, thisreg$coeff))
            names(linhyp)[length(linhyp)] <- 
              paste0(c(FNames[r], regtype, s, g, "None", "nontrad in each period - nontrad in period 2", i), collapse = "")
            # dhvNinT: Difference = 0 (of Arm g relative to trad, in time X)
            # Marginal difference between g and trad in time X.
            # [[2]][1] "dummyInKind.Time4"
            #### CLAUDE spl: 2026-07-14
            rexlog$dhvNinT <- paste0("^", covadd.nontrad[[i]][2], "$")
            dhvNinT <- rep(0, length(coeffvec))
            dhvNinT[grepl(rexlog$dhvNinT, names(coeffvec))] <- 1*Mult
            lhcow <- glht(model=thisreg, linfct = matrix(dhvNinT, byrow = T, nrow=1), 
              alternative="two.sided", vcov.=thisV)
            confi <- rbind(confi, 
              c(FNames[r], regtype, s, g, "None", "nontrad - trad, in each period", 
                "dNinT", 
                i, confint(lhcow)$confint[1, ], 
                if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
              )
            linhyp <- c(linhyp, list(dhvNinT, thisreg$coeff))
            names(linhyp)[length(linhyp)] <- 
              paste0(c(FNames[r], regtype, s, g, "None", "nontrad - trad in each period", i), collapse = "")
            # hvNinT2: difference = 0 (of arm g in time X relative to trad in time 2)
            #   intercept + Arm +TimeX + Arm.TimeX - (intercept) 
            #   = Arm + TimeX + Arm.TimeX 
            #   = hvN1 + hvNinT
            # [[2]] "dummyInKind", "Time.4", "dummyInKind.Time4"
            hvNinT2 <- hvN1 + hvNinT
            lhcow <- glht(model=thisreg, linfct = matrix(hvNinT2, byrow = T, nrow=1), 
              alternative="two.sided", vcov.=thisV)
            confi <- rbind(confi, 
              c(FNames[r], regtype, s, g, "None", "nontrad in each period - trad in period 2", 
               "NinT2", 
                i, confint(lhcow)$confint[1, ], 
                if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
              )
            linhyp <- c(linhyp, list(hvNinT2, thisreg$coeff))
            names(linhyp)[length(linhyp)] <- 
              paste0(c(FNames[r], regtype, s, g, "None", "nontrad in each period - trad in period 2", i), collapse = "")
            # periNrelativeT: Cumulative difference = 0 (of nontrad Arm g relative to trad, in time i)
            # Total difference between g and trad in time X.
            #  "dummyInKind"+"dummyInKind.TimeX" for cumulative effects relative to trad in time X
            # "dummyInKind" is stored in peristrings0 at hvN1
            #### CLAUDE spl: 2026-07-14 peristrings2 -> rexlog$periNrelativeT
            rexlog$periNrelativeT <- paste(peristrings0,
              paste0("^", covadd.nontrad[[i]][2], "$"), sep = "|")
            periNrelativeT <- rep(0, length(coeffvec))
            periNrelativeT[grepl(rexlog$periNrelativeT,
              names(coeffvec))] <- 1*Mult
            lhcow <- glht(model=thisreg, linfct = matrix(periNrelativeT, byrow = T, nrow=1), 
              alternative="two.sided", vcov.=thisV)
            confi <- rbind(confi, 
              c(FNames[r], regtype, s, g, "None", "sum of (nontrad - trad, in each period)", 
                "periNrelativeT", 
                i, confint(lhcow)$confint[1, ], 
                if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
              )
            linhyp <- c(linhyp, list(periNrelativeT, thisreg$coeff))
            names(linhyp)[length(linhyp)] <- 
              paste0(c(FNames[r], regtype, s, g, "None", 
              "sum of nontrad - trad in each period", i), collapse = "")
            # hvN2: level = 0 (of nontrad in period t)
            # = cumulative trad + relative to concurrent trad
            # Intercept+TimeX+Arm+TimeX.Arm=hvT0 + hvNinT
            #### CLAUDE bug: 2026-07-16 code omitted Arm, contradicting the
            #### intent comment above (user-confirmed intended effect):
            #### hvN2 = (Intercept + TimeX) + (Arm + Arm.TimeX)
            ####      = <hvTL>              + <periNrelativeT>
            #### old hvT0 + hvNinT = Intercept + TimeX + Arm.TimeX (no Arm);
            #### at i = 1 old and new coincide (both 2*Intercept + Arm).
            # hvN2 <- hvT0 + hvNinT
            hvN2 <- hvTL + periNrelativeT
            lhcow <- glht(model=thisreg, linfct = matrix(hvN2, byrow = T, nrow=1), 
              alternative="two.sided", vcov.=thisV)
            confi <- rbind(confi, 
              c(FNames[r], regtype, s, g, "None", "level of nontrad in each period", 
              "N2", 
                i, confint(lhcow)$confint[1, ], 
                if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
              )
            linhyp <- c(linhyp, list(hvN2, thisreg$coeff))
            names(linhyp)[length(linhyp)] <- 
              paste0(c(FNames[r], regtype, s, g, "None", "level of nontrad in each period", i), collapse = "")
            ## 3 way (OwnCattle0*Arm*Time) interactions ##
            # Loop over OwnCattle0, AdiCattle0, 
            if (grepl("sExpe", FNames[r])) {
              for (j in paste0("dummy", c("Own", "Adi"), "Cattle0")) {
              # j: experience type
                # dhvJ0: Difference between j and trad at period 2
                #  dummyAdiCattle0
                # dhvJG0: Difference between j and arm/attribute g at period 2
                #  dummyAdiCattle0.Large
                # dhvJinT: Difference between j and concurrent trad in period X.
                #  dummyAdiCattle0.TimeX
                # dhvJGinT: Difference between j*g and g in X
                #  dummyAdiCattle0.Large.TimeX
                # hvJinT: Cumulative difference between j and concurrent trad in period X.
                #  dummyAdiCattle0 + dummyAdiCattle0.TimeX
                # hvJGinT: Cumulative difference between j*g and g in T
                #  dummyAdiCattle0.Large + dummyAdiCattle0.Large.TimeX
                # periJGinT: Cumulative difference between j*g and concurrent trad in time X.
                #  dummyLarge + dummyLarge.TimeX : periNrelativeT
                # +dummyAdiCattle0 + dummyAdiCattle0.TimeX : hvJinT
                # +dummyAdiCows0.Large+dummyAdiCows0.Large.TimeX : hvJGinT

                # dhvJ0: dummyAdiCattle0
                # Difference between j and trad at period 2
                 #### CLAUDE spl: 2026-07-14 record which iteration the map shows.
                 #### CLAUDE spl: 2026-07-15 also record names(coeffvec) so the
                 #### map chunk can print the coefficient vector it matches on.
                 rexctx <- list(FileName = FNames[r], regtype = regtype,
                   g = g, i = i, j = j, coefnames = names(coeffvec))
                 rexlog$dhvJ0 <- paste0("^", j, "$")
                 dhvJ0 <- rep(0, length(coeffvec))
                 dhvJ0[grepl(rexlog$dhvJ0, names(coeffvec))] <- 1*Mult
                 lhcow <- glht(model=thisreg, linfct = matrix(dhvJ0, byrow = T, nrow=1), 
                   alternative="two.sided", vcov.=thisV)
                 confi <- rbind(confi, 
                   c(FNames[r], regtype, s, g, j, "j*g - trad, in period 2", 
                     "dJ0", 
                     i, confint(lhcow)$confint[1, ], 
                     if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
                   )
                 linhyp <- c(linhyp, list(dhvJ0, thisreg$coeff))
                 names(linhyp)[length(linhyp)] <- 
                   paste0(c(FNames[r], regtype, s, g, j, "jg - trad in period 2", i), collapse = "")
                # dhvJinT: dummyAdiCattle0.TimeX
                # dhvJinT: Difference between j and concurrent trad in period X.
                 #### CLAUDE spl: 2026-07-14
                 rexlog$dhvJinT <- paste0("^", j, ".Time.$")
                 dhvJinT <- rep(0, length(coeffvec))
                 dhvJinT[grepl(rexlog$dhvJinT, names(coeffvec))] <- 1*Mult
                 lhcow <- glht(model=thisreg, linfct = matrix(dhvJinT, byrow = T, nrow=1), 
                   alternative="two.sided", vcov.=thisV)
                 confi <- rbind(confi, 
                   c(FNames[r], regtype, s, g, j, "j - trad, in each period", 
                     "dJinT", 
                     i, confint(lhcow)$confint[1, ], 
                     if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
                   )
                 linhyp <- c(linhyp, list(dhvJinT, thisreg$coeff))
                 names(linhyp)[length(linhyp)] <- 
                   paste0(c(FNames[r], regtype, s, g, j, "j - trad in each period", i), collapse = "")
                # hvJinT: Total difference between j and concurrent trad in period T.
                #  dummyAdiCattle0 + dummyAdiCattle0.TimeX
                 hvJinT <- dhvJ0+dhvJinT
                # hvJG0: dummyAdiCattle0.Large
                 #### CLAUDE bug: 2026-07-14 StandingIssues #1 FIXED.
                 #### Was: paste0("^", j, "$") -- matched dummyAdiCattle0, the
                 #### 2-way base, i.e. exactly what dhvJ0 already selects, so
                 #### JGinT re-tested the j main effect instead of the 3-way
                 #### j*g term. Verified on ANCOVA_NetAssetsExperience.qs
                 #### (rr=6, s=4): the real coefficient is dummyAdiCattle0.Cattle.
                 # rexlog$hvJG0 <- paste0("^", j, "$")
                 rexlog$hvJG0 <- paste0("^", j, ".", g, "$")
                 hvJG0 <- rep(0, length(coeffvec))
                 hvJG0[grepl(rexlog$hvJG0, names(coeffvec))] <- 1*Mult
                # dhvJGinT: dummyAdiCattle0.Large.TimeX
                 #### CLAUDE bug: 2026-07-14 StandingIssues #1 FIXED.
                 #### Was: paste0("^", j, ".", covadd.nontrad[[i]][2]) -- but
                 #### covadd.nontrad[[i]][2] is already "dummyCattle.Time4", so
                 #### the pattern became "^dummyAdiCattle0.dummyCattle.Time4"
                 #### and matched NOTHING (all-zero vector), and had no closing
                 #### "$". Strip the "dummy" prefix; real coefficient is
                 #### dummyAdiCattle0.Cattle.Time4.
                 # rexlog$dhvJGinT <- paste0("^", j, ".", covadd.nontrad[[i]][2])
                 rexlog$dhvJGinT <- paste0("^", j, ".",
                   gsub("^dummy", "", covadd.nontrad[[i]][2]), "$")
                 dhvJGinT <- rep(0, length(coeffvec))
                 dhvJGinT[grepl(rexlog$dhvJGinT, names(coeffvec))] <- 1*Mult
                # hvJGinT: dummyAdiCattle0.Large + dummyAdiCattle0.Large.TimeX
                 hvJGinT <- hvJG0 + dhvJGinT
                 lhcow <- glht(model=thisreg, linfct = matrix(hvJGinT, byrow = T, nrow=1), 
                   alternative="two.sided", vcov.=thisV)
                 confi <- rbind(confi, 
                   c(FNames[r], regtype, s, g, j, "j*g - g, in each period", 
                     "JGinT", 
                     i, confint(lhcow)$confint[1, ], 
                     if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
                   )
                 linhyp <- c(linhyp, list(hvJGinT, thisreg$coeff))
                 names(linhyp)[length(linhyp)] <- 
                   paste0(c(FNames[r], regtype, s, g, j,
                     "RelativeToConcurrentArmAttribute", i), collapse = "")
                # periJGinT: Cumulative difference between j*g and concurrent trad in time X.
                #  dummyLarge + dummyLarge.TimeX : periNrelativeT
                # +dummyAdiCattle0 + dummyAdiCattle0.TimeX : hvJinT
                # +dummyAdiCows0.Large+dummyAdiCows0.Large.TimeX : hvJGinT
                periJGinT <- periNrelativeT+hvJinT+hvJGinT
                lhcow <- glht(model=thisreg, linfct = matrix(periJGinT, byrow = T, nrow=1), 
                  alternative="two.sided", vcov.=thisV)
                confi <- rbind(confi, 
                  c(FNames[r], regtype, s, g, j, "j*g - trad, in each period", 
                    "periJGinT", 
                     i, confint(lhcow)$confint[1, ], 
                     if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
                  )
                linhyp <- c(linhyp, list(periNrelativeT, thisreg$coeff))
                names(linhyp)[length(linhyp)] <- 
                  paste0(c(FNames[r], regtype, s, g, j, "jg - trad in each period", i), collapse = "")
              } # end: cattle experience (Own/Adi) j loop
            } # end: if "sExperience in FileName[r]" loop
            #### Some outcomes do not have dummyUltraPoor as a level covariate. Need to fix it.
            #### Before doing so, abort these outcomes.
            if (grepl("Exper", FNames[r])) next
            if (grepl("Pa?$", reglists[[r]][rr])) {
              # if time-invariant poverty wise impacts are estimated
              # matP
              # Poor = 0 for trad
              # Poor + Arm.Poor = 0 for nontrad
              # covadd.trad
              # [[1]][1] "\\(Intercept\\)" ==> dummyUltraPoor
              # [[2]][1] "Time.3" ==> dummyUltraPoor.Time3
              # [[3]][1] "Time.4" ==> dummyUltraPoor.Time4
              # trad
              covadd.tradP <- lapply(covadd.trad, 
                function(x) gsub(".*ntercept.*", "dummyUltraPoor", x))
              covadd.tradP <- lapply(covadd.tradP, 
                function(x) gsub("T", "dummyUltraPoor.T", x))
              covadd.tradP <- lapply(covadd.tradP, 
                function(x) gsub("Time\\.", "Time", x))
              #### CLAUDE bug: 2026-07-16 user-directed redesign to
              #### per-period poverty tests (was [[1]] only, "time
              #### invariant"):
              #### matPT = Poor + Poor.TimeX   (poor - nonpoor | trad,
              ####         period i+1; at i = 1 just Poor)
              #### CLAUDE spl: 2026-07-14
              # rexlog$matPT <- paste0("^", covadd.tradP[[1]], "$")
              rexlog$matPT <- paste(paste0("^",
                unique(c(covadd.tradP[[1]], covadd.tradP[[i]])), "$"),
                collapse = "|")
              matPT <- rep(0, length(coeffvec))
              matPT[grepl(rexlog$matPT, names(coeffvec))] <- 1*Mult
              # dummyInKind.Time4 ==> dummyInKind.UltraPoor.Time4
              covadd.nontradP <- lapply(covadd.nontrad,
                function(x) gsub("T", "UltraPoor.T", x[2]))
              # nontrad
              #### CLAUDE bug: 2026-07-16 matPN = Arm.Poor + Arm.Poor.TimeX;
              #### row2 = matPT + matPN = Poor + Arm.Poor + Poor.TimeX
              ####        + Arm.Poor.TimeX (user formula).
              #### Old gsub could not build the time-invariant Arm.Poor
              #### (arm names carry no "T"): i = 1 picked the arm dummy
              #### itself, i >= 2 only the .TimeX interaction. Verified on
              #### ANCOVA_Land coefficient names (dummyInKind.UltraPoor).
              #### CLAUDE spl: 2026-07-14
              # rexlog$matPN <- paste0("^", covadd.nontradP[[i]], "$")
              rexlog$matPN <- paste(unique(c(
                paste0("^", covadd.nontrad[[1]][2], ".UltraPoor$"),
                if (i > 1) paste0("^", covadd.nontradP[[i]], "$"))),
                collapse = "|")
              matPN <- rep(0, length(coeffvec))
              matPN[grepl(rexlog$matPN, names(coeffvec))] <- 1*Mult
              matP <- rbind(matPT, matPN=matPT+matPN)
              lhcow <- glht(model=thisreg, linfct = matP, 
                alternative="two.sided", vcov.=thisV)
              #### CLAUDE bug: 2026-07-16 store i (was literal "2-4" ->
              #### as.numeric NA at reshape); labels = the formulas (old
              #### ", time invariant" no longer applies).
              confi <- rbind(confi,
                c(FNames[r], regtype, s, g, "poor", "Poor + Poor.TimeX = 0, trad",
                  "matP",
                   i, confint(lhcow)$confint[1, ],
                   if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA),
                c(FNames[r], regtype, s, g, "poor + arm*poor", "Poor + Arm.Poor + Poor.TimeX + Arm.Poor.TimeX = 0",
                  "matP",
                   i, confint(lhcow)$confint[2, ],
                   if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[2] else NA)
               )
            } # end: if time-invariant poverty wise impacts are estimated
            if (grepl("TPa?$", reglists[[r]][rr])) {
              # if time-variant poverty wise impacts are estimated
              # dmatPTinT
              # Time = 2: Poor = 0; Time = 3: Poor.Time3 = 0  for trad
              # dmatPNinT
              # Time = 2: Arm.Poor = 0; Time = 3: Arm.Poor.Time3 = 0 for nontrad
              # TimeYY ==> dummyUltraPoor.TimeYY
              covadd.tradP <- lapply(covadd.trad, 
                function(x) gsub(".*ntercept.*", "dummyUltraPoor", x))
              covadd.tradP <- lapply(covadd.tradP, 
                function(x) gsub("T", "dummyUltraPoor.T", x))
              covadd.tradP <- lapply(covadd.tradP, 
                function(x) gsub("Time\\.", "Time", x))
              #### CLAUDE spl: 2026-07-14
              rexlog$dmatPTinT <- paste0("^", covadd.tradP[[i]], "$")
              dmatPTinT <- rep(0, length(coeffvec))
              dmatPTinT[grepl(rexlog$dmatPTinT, names(coeffvec))] <- 1*Mult
              # dummyInKind.Time4 ==> dummyInKind.UltraPoor.Time4
              covadd.nontradP <- lapply(covadd.nontrad,
                function(x) gsub("T", "UltraPoor.T", x[2]))
              # nontrad
              #### CLAUDE bug: 2026-07-16 at i=1 the gsub returned the arm
              #### dummy unchanged (no "T" to anchor on), so the row tested
              #### dummyInKind = 0 instead of dummyInKind.UltraPoor = 0.
              #### Build the interaction name directly at i=1.
              #### CLAUDE spl: 2026-07-14
              # rexlog$dmatPNinT <- paste0("^", covadd.nontradP[[i]], "$")
              rexlog$dmatPNinT <- if (i == 1)
                paste0("^", covadd.nontrad[[1]][2], ".UltraPoor$") else
                paste0("^", covadd.nontradP[[i]], "$")
              dmatPNinT <- rep(0, length(coeffvec))
              dmatPNinT[grepl(rexlog$dmatPNinT, names(coeffvec))] <- 1*Mult
              # matPTinT: sum of (poor trad - nonpoor trad, in each period)
              # Time = 2: Poor = 0; Time = 3: Poor + Poor.Time3 = 0  for trad
              #   = matP + dmatPTinT
              # matPNinT: sum of (poor nontrad - nonpoor nontrad, in each period)
              # Time = 2: Poor + Arm.Poor = 0; Time = 3: Poor + Arm.Poor + Arm.Poor.Time3 = 0 for nontrad
              #   = matPTinT + dmatPNinT
              # Poor = 0 for trad
              matP <- rep(0, length(coeffvec))
              matP[grep("^dummyUltraPoor$", names(coeffvec))] <- 1*Mult
              #### CLAUDE bug: 2026-07-16 dmatPTinT at i=1 already holds
              #### dummyUltraPoor; adding matP doubled it:
              #### skip "+ matP" at i=1 => matPTinT = dummyUltraPoor
              ####  add "+ matP" at i>1 => matPTinT = dummyUltraPoor
              ####                                  + dummyUltraPoor.TimeX
              # matPTinT <- matP + dmatPTinT
              matPTinT <- if (i == 1) dmatPTinT else matP + dmatPTinT
              #### CLAUDE bug: 2026-07-16 dummyInKind.UltraPoor never entered
              #### matPNinT (unreachable by the gsub); add it via dmatPN0:
              #### at i=1 => matPNinT = dummyUltraPoor + dummyInKind.UltraPoor
              #### at i>1 => matPNinT = dummyUltraPoor + dummyInKind.UltraPoor
              ####            + dummyUltraPoor.TimeX + dummyInKind.UltraPoor.TimeX
              dmatPN0 <- rep(0, length(coeffvec))
              dmatPN0[grepl(paste0("^", covadd.nontrad[[1]][2],
                ".UltraPoor$"), names(coeffvec))] <- 1*Mult
              # matPNinT <- matPTinT + dmatPNinT
              matPNinT <- matPTinT + dmatPN0 +
                (if (i == 1) 0 else dmatPNinT)
              lhcow1 <- glht(model=thisreg, linfct = t(matrix(dmatPTinT)),
                alternative="two.sided", vcov.=thisV)
              lhcow2 <- glht(model=thisreg, linfct = t(matrix(dmatPNinT)),
                alternative="two.sided", vcov.=thisV)
              lhcow3 <- glht(model=thisreg, linfct = t(matrix(matPTinT)),
                alternative="two.sided", vcov.=thisV)
              lhcow4 <- glht(model=thisreg, linfct = t(matrix(matPNinT)),
                alternative="two.sided", vcov.=thisV)
              confi <- rbind(confi, 
                c(FNames[r], regtype, s, g, "dpoor.TimeX", 
                  "poor trad - nonpoor trad, in each period", "dmatPTinT", 
                   i, confint(lhcow1)$confint[1, ], 
                   if (sum(lhcow1$linfct) != 0) summary(lhcow1)$test$pvalues[1] else NA),
                c(FNames[r], regtype, s, g, "dpoor.Arm.TimeX", 
                  "poor nontrad - nonpoor nontrad, in each period", "dmatPNinT", 
                   i, confint(lhcow2)$confint[1, ], 
                   if (sum(lhcow2$linfct) != 0) summary(lhcow2)$test$pvalues[1] else NA),
                c(FNames[r], regtype, s, g, "poor.TimeX", 
                  "sum of (poor trad - nonpoor trad, in each period)", "matPTinT", 
                   i, confint(lhcow3)$confint[1, ], 
                   if (sum(lhcow3$linfct) != 0) summary(lhcow3)$test$pvalues[1] else NA),
                c(FNames[r], regtype, s, g, "poor.Arm.TimeX", 
                  "sum of (poor nontrad - nonpoor nontrad, in each period)", "matPNinT", 
                   i, confint(lhcow4)$confint[1, ], 
                   if (sum(lhcow4$linfct) != 0) summary(lhcow4)$test$pvalues[1] else NA)
               )
            } # end: if time-variant poverty wise impacts are estimated
          } # end: period i loop
        } # end: if "T in regtype" loop
      } # end: attribute g loop
    } # end: reg specification s loop 2:7
  } # end: reg type rr loop ("", "P", "a", "T", "Ta", ...)
} # end: outcome r loop

#### confi example table, lines 1175-1198    {r confi example table, eval = T, warning = F, message = F}    eval = T
library(tinytable)
cfe <- data.table::as.data.table(confi)
setnames(cfe, c("FileName", "regtype", "num", "attributes", "experience",
  "ImpactType", "hv", "period", "Estimate", "lwr", "upr", "pvalue"))
cfe <- cfe[FileName == "NetAssets" & regtype == "Ta" & num == "1" &
  period == "2" & attributes %in% c("trad", "LargeSize"), ]
combi <- c(
  TinT           = "Time3",
  TL             = "Intercept + Time3",
  NinT           = "Time3 + Arm*Time3",
  dNinT          = "Arm*Time3",
  NinT2          = "Time3 + Arm + Arm*Time3",
  periNrelativeT = "Arm + Arm*Time3",
  N2             = "Intercept + Time3 + Arm + Arm*Time3")
cfe[, CombiOfCoeff := combi[hv]]
cfe[, Estimate := round(as.numeric(Estimate))]
cfe[, lwr := round(as.numeric(lwr))]
cfe[, upr := round(as.numeric(upr))]
cfe[, pvalue := round(100*as.numeric(pvalue), 2)]
setnames(cfe, "pvalue", "p (%)")
tt(cfe[, .(hv, CombiOfCoeff, ImpactType, Estimate, lwr, upr, `p (%)`)],
  caption = "The seven per-period tests: NetAssets, Ta, spec 1, period 3")

#### linhyp map, lines 1224-1335    {r linhyp map, echo = F, results = 'asis'}    eval = T
#### Renders the linear-hypothesis vectors as a nested map. The regex column
#### is taken from rexlog, i.e. the strings the loop above actually used, so
#### it cannot drift from the code. Only the nesting shape is static here.
#### rexctx records which iteration the shown values come from.
#### CLAUDE spl: 2026-07-14
lhstruct <- rbind(
  c(0, "hvT0",                                          "regex"),
  c(0, "for (g in lattributes) {",                      "open"),
  c(1, "hvN0",                                          "regex"),
  c(1, "hvN1",                                          "regex"),
  c(1, "if (grepl(\"T\", regtype)) {",                  "open"),
  c(2, "for (i in startnum:length(covadd.trad)) {",     "open"),
  c(3, "hvTinT",                                        "regex"),
  c(3, "hvTL = hvT0 + hvTinT",                          "derived"),
  c(3, "hvNinT",                                        "regex"),
  c(3, "dhvNinT",                                       "regex"),
  c(3, "hvNinT2 = hvN1 + hvNinT",                       "derived"),
  c(3, "periNrelativeT",                                "regex"),
  c(3, "hvN2 = hvTL + periNrelativeT",                  "derived"),
  c(3, "if (grepl(\"sExpe\", FNames[r])) {",            "open"),
  c(4, "for (j in dummy(Own|Adi)Cattle0) {",            "open"),
  c(5, "dhvJ0",                                         "regex"),
  c(5, "dhvJinT",                                       "regex"),
  c(5, "hvJinT = dhvJ0 + dhvJinT",                      "derived"),
  c(5, "hvJG0",                                         "regex"),
  c(5, "dhvJGinT",                                      "regex"),
  c(5, "hvJGinT = hvJG0 + dhvJGinT",                    "derived"),
  c(5, "periJGinT = periNrelativeT + hvJinT + hvJGinT", "derived"),
  c(4, "}",                                             "close"),
  c(3, "}",                                             "close"),
  c(3, "if (grepl(\"Pa?$\", reglists[[r]][rr])) {",      "open"),
  c(4, "matPT",                                         "regex"),
  c(4, "matPN",                                         "regex"),
  c(4, "matP = rbind(matPT, matPT + matPN)",            "derived"),
  c(3, "}",                                             "close"),
  c(3, "if (grepl(\"TPa?$\", reglists[[r]][rr])) {",     "open"),
  c(4, "dmatPTinT",                                     "regex"),
  c(4, "dmatPNinT",                                     "regex"),
  c(3, "}",                                             "close"),
  c(2, "}",                                             "close"),
  c(1, "}",                                             "close"),
  c(0, "}",                                             "close")
  )
colnames(lhstruct) <- c("lev", "vec", "kind")

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
namecol <- 34
body <- character(nrow(lhstruct))
for (a in 1:nrow(lhstruct)) {
  lev <- as.integer(lhstruct[a, "lev"])
  ind <- strrep("  ", lev)
  vec <- lhstruct[a, "vec"]
  knd <- lhstruct[a, "kind"]
  if (knd %in% c("open", "close")) {
    body[a] <- paste0(ind, '<span class="lp">', esc(vec), "</span>")
  } else if (knd == "derived") {
    body[a] <- paste0(ind, '<span class="dv">', esc(vec), "</span>")
  } else {
    pat <- rexlog[[vec]]
    if (is.null(pat)) pat <- "(not reached in this run)"
    pad <- max(1, namecol - nchar(ind) - nchar(vec))
    body[a] <- paste0(ind, '<span class="vc">', esc(vec), "</span>",
      strrep(" ", pad), '<span class="rx">', esc(pat), "</span>")
  }
}
hdr <- if (length(rexctx))
  paste0("shown for: ", rexctx$FileName, " | regtype ", rexctx$regtype,
    " | g = ", rexctx$g, " | i = ", rexctx$i, " | j = ", rexctx$j) else
  "no iteration recorded"
#### CLAUDE spl: 2026-07-15 coefficient vector the regexes match on
nmtxt <- if (length(rexctx) && !is.null(rexctx$coefnames))
  paste(esc(rexctx$coefnames), collapse = ", ") else
  "(no coefficients recorded)"
cat('
<div class="lhmap">
<style>
.lhmap {border:1px solid #d0d7de; border-radius:6px; margin:1em 0;}
.lhmap .hd {background:#f6f8fa; border-bottom:1px solid #d0d7de;
  padding:6px 10px; font-weight:600; font-size:90%;}
.lhmap .ct {background:#fffbdd; border-bottom:1px solid #d0d7de;
  padding:4px 10px; font-size:80%; color:#57606a; font-family:monospace;}
.lhmap .nm {border-bottom:1px solid #d0d7de; padding:4px 10px;
  font-size:80%; color:#24292e; font-family:monospace; line-height:1.5;}
.lhmap pre {margin:0; padding:10px 12px; background:#fff;
  font-size:85%; line-height:1.5; overflow-x:auto;}
.lhmap .lp {color:#005cc5; font-weight:600;}
.lhmap .vc {color:#24292e; font-weight:600;}
.lhmap .rx {color:#d73a49;}
.lhmap .dv {color:#24292e; font-style:normal;}
.lhmap .lg {padding:6px 10px; border-top:1px solid #d0d7de;
  background:#f6f8fa; font-size:80%; color:#57606a;}
</style>
<div class="hd">Linear-hypothesis vectors: nesting and the regex actually used</div>
<div class="ct">', esc(hdr), '</div>
<div class="nm"><b>names(coeffvec):</b> ', nmtxt, '</div>
<pre>
', paste(body, collapse = "\n"), '
</pre>
<div class="lg">
<b>bold</b> = vector built by regex &nbsp;|&nbsp;
<span style="color:#24292e">upright black</span> = derived (sum of others, no regex) &nbsp;|&nbsp;
<span style="color:#d73a49">red</span> = the regex matched against
<code>names(coeffvec)</code>, taken live from <code>rexlog</code>
</div>
</div>
', sep = "")

#### reshape confi Do This Mannually, lines 1338-1507    {r reshape confi Do This Mannually, eval = T, warning = F}    eval = T
confi <- data.table(unique(confi))
setnames(confi, c("FileName", "regtype", "num", "attributes", "experience",
  "ImpactType", "hv", "period", "estimate", "lb", "ub", "pvalue"))
numcols <- c("period", "estimate", "lb", "ub", "num", "pvalue")
confi[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
faccols <- c("FileName", "regtype", "attributes", "experience", "hv", "ImpactType")
confi[, (faccols) := lapply(.SD, as.factor), .SDcols = faccols]
#### Drop all num == 1 except for ConsumptionOLS, 
#### NumCows and NumCowsAdi, NumCowsNone, NumCowsOwn, 
confi <- confi[grepl("NumCows|OLS", FileName) | (!grepl("NumCows", FileName) & num != 1), ]
confi <- confi[!(grepl("NumCowsE", FileName) & num == 1), ]
table(confi[, .(FileName, num)])
confi[, ImpactType := factor(ImpactType, 
  levels = c(
    "level of reference trad",
    "level of trad in period X",
    "level of reference nontrad",
    "level of nontrad in each period",
    "reference nontrad - reference trad",
    "trad in each period - trad in period 2",
    "nontrad in each period - trad in period 2",
    "nontrad in each period - nontrad in period 2",
    "nontrad - trad, in each period",
    "sum of (nontrad - trad, in each period)",
    "j - trad, in each period",
    "j*g - trad, in period 2",
    "j*g - trad, in each period",
    "j*g - g, in each period",
    #### CLAUDE bug: 2026-07-16 "Poor + Arm.Poor = 0, time invariant"
    #### removed: zero rows carry it since the matP redesign, and the
    #### second factor(labels=) call rebuilds levels from present values
    #### only -> length mismatch error on knit
    "Poor + Poor.TimeX = 0, trad",
    "Poor + Arm.Poor + Poor.TimeX + Arm.Poor.TimeX = 0",
    "poor trad - nonpoor trad, in each period",
    "poor nontrad - nonpoor nontrad, in each period",
    "sum of (poor trad - nonpoor trad, in each period)",
    "sum of (poor nontrad - nonpoor nontrad, in each period)"
  ))]
confi[, ImpactType := factor(ImpactType, 
  labels = c(
    "level of reference trad",
    "level of trad in each period",
    "level of reference nontrad",
    "level of nontrad in each period",
    "reference nontrad - reference trad",
    "trad in each period - trad in period 2",
    "nontrad in each period - trad in period 2",
    "nontrad in each period - nontrad in period 2",
    "nontrad - trad, in each period",
    "sum of (nontrad - trad, in each period)",
    "j - trad, in each period",
    "j*g - trad in period 2",
    "j*g - trad, in each period",
    "j*g - g, in each period",
    "poor - nonpoor, trad, in each period",
    "poor - nonpoor, in arm g, in each period",
    "poor trad - nonpoor trad, in each period",
    "poor nontrad - nonpoor nontrad, in each period",
    "sum of (poor trad - nonpoor trad, in each period)",
    "sum of (poor nontrad - nonpoor nontrad, in each period)"
))]


confi[grepl("Con.*OL", FileName) & regtype=="T" & grepl("ge$", attributes) & grepl("sum", ImpactType), ]



confi[, period := period + 1]
#### period is NA for non-timevarying regressions
confi[!grepl("T", regtype), period := NA]
confi[!is.na(period) & grepl("Con", FileName) & num == 2 & grepl("Ta", regtype), ][
order(attributes, ImpactType, period)]
####confi <- rbindlist(list(confi, confis), use.names = T, fill = T)
setcolorder(confi, c("FileName", "regtype", "num", "attributes", 
  "hv", "ImpactType", "period", "lb", "estimate", "ub"))
confi <- confi[!(grepl("Con", FileName) & estimate == 0), ]
confi[, attributes := factor(attributes, 
  levels = c("trad", "Large", "LargeGrace", "Cattle",
  "LargeSize", "WithGrace", "InKind"))]
confi[, attributes := factor(attributes, 
  labels = c("Traditional", "Large", "LargeGrace", "Cattle",
  "Upfront", "WithGrace", "InKind"))]
#### CLAUDE bug: 2026-07-16 poverty-test tags were absent from levels ->
#### 3,978 rows (matP 1,314 + dmatP*/matP*inT 2,664) coerced to NA
#### (StandingIssues: matP rows lose identity). All six tags added.
# confi[, experience := factor(experience,
#   levels = c("None", "dummyAdiCattle0", "dummyOwnCattle0"))]
# confi[, experience := factor(experience,
#   labels = c("None", "AdiCattle", "OwnCattle"))]
confi[, experience := factor(experience,
  levels = c("None", "dummyAdiCattle0", "dummyOwnCattle0",
    "poor", "poor + arm*poor", "dpoor.TimeX", "dpoor.Arm.TimeX",
    "poor.TimeX", "poor.Arm.TimeX"))]
confi[, experience := factor(experience,
  labels = c("None", "AdiCattle", "OwnCattle",
    "poor", "poor + arm*poor", "dpoor.TimeX", "dpoor.Arm.TimeX",
    "poor.TimeX", "poor.Arm.TimeX"))]
#### NumCows reg specs
#### 1. NA,
#### 2. "|NumCows0$",
#### 3. "|Head|Flood|HH",
#### 4. "|^dummyHadCows"
#### 5. "|TotalImp.*0$"
#### NumCowsByExperience reg specs
#### Own subsample (1=OLS, 2=ANCOVA, 3=ANCOVA with covariates, 4=ditto)
#### 1. NA,
#### 2. "|NumCows0$",
#### 3. "|Head|Flood|HH|^NetVa.*0$",
#### 4. "|NumC.*0$"
#### Adi, None subsamples (1=OLS, 3=OLS with covariates)
#### 1. NA, 
#### 2. "", # "|NumC.*0$" is NA if own=none, adi. Lead to error in linear hyp testing. Set to NA.
#### 3. "|Head|Flood|HH|^NetVa.*0$",
#### 4. ""
#### Comparable: 
####  NumCows 1 = 1 in all others
####  NumCows 2 = 2 in Own, 
####  NumCows 3 = 3 in Own, 3 in Adi, None (OLS, though)
confi[, AtType := "Arms (relative to Traditional)"]
confi[grepl("With|Kin", attributes), AtType := "Functional attributes (relative to Upfront, Upfront+WithGrace)"]
confi[grepl("^NinT$|TinT", hv), AtType := "Arms (relative to own in period 2)"]
confi[grepl("N2$|T0|N0", hv), AtType := "Levels"]
confi[, AtType := factor(AtType)]
confi[, regressand := "livestock"]
confi[grepl("Sch", FileName), regressand := "enrollment"]
confi[grepl("Cows$", FileName), regressand := "cattle"]
confi[grepl("CowsEx", FileName), regressand := "cattle, Experience"]
confi[grepl("CowsBy.*a$", FileName), regressand := "cattle, Adi"]
confi[grepl("CowsBy.*o$", FileName), regressand := "cattle, Own"]
confi[grepl("CowsBy.*n$", FileName), regressand := "cattle, None"]
confi[grepl("^NetAssets", FileName), regressand := "net assets"]
confi[grepl("^NetBroad.*ts$", FileName), regressand := "net broad assets"]
confi[grepl("^Net.*ea$", FileName), regressand := "net assets, Adi"]
confi[grepl("^Net.*eo$", FileName), regressand := "net assets, Own"]
confi[grepl("^Net.*en$", FileName), regressand := "net assets, None"]
confi[grepl("Net.*Pri", FileName), regressand := "net assets, AP"]
confi[grepl("tsEx", FileName), regressand := gsub("$", ", Experience", regressand)]
####confi[grepl("^AssetL", FileName), regressand := "broad total assets"]
confi[grepl("^NetNL", FileName), regressand := "net non-livestock assets"]
confi[grepl("Lan", FileName), regressand := "land"]
confi[grepl("Lab", FileName), regressand := "labour incomes"]
confi[grepl("Consumption$", FileName), regressand := "consumption"]
confi[grepl("ConsumptionO", FileName), regressand := "consumption, OLS"]
confi[, FileName := gsub("^Asset", "BroadAsset", FileName)]
confi[, FileName := gsub("ByExperiencea", "Adi", FileName)]
confi[, FileName := gsub("ByExperienceo", "Own", FileName)]
confi[, FileName := gsub("ByExperiencen", "None", FileName)]
confi[, regressand := factor(regressand)]
confi[, FileName := factor(FileName)]
####confi[grepl("far", FileName), regressand := "farm incomes"]
#### confi[grepl("sv", FileName), regressand := "net saving"]
#### confi[grepl("sv.[45]", FileName), regressand := "repayment"]
#### confi[grepl("sv.[78]", FileName), regressand := "effective repayment"]
#### confi[grepl("sc", FileName), regressand := "schooling"]
confi[, attributes := factor(attributes, levels = 
  c("Traditional", "Large", "LargeGrace", "Cattle", "Upfront", "WithGrace", "InKind"))]
confi[, regressand := factor(regressand, levels = 
  c("land", "livestock", #"broad total assets", 
    "net non-livestock assets", 
    paste0(rep(c("net assets", "cattle"), each = 5), 
      rep(c("", ", Adi", ", None", ", Own", ", Experience"), 2))
    , "net broad assets", "net assets, AP", 
    "enrollment", "consumption", "consumption, OLS", "labour incomes")
      )]
saveRDS(confi, paste0(pathsaveHere, "EstimatesCI.rds"))
qsave(confi, paste0(pathsaveHere, "EstimatesCI.qs"))
saveRDS(linhyp, paste0(pathsaveHere, "LinearHypothesis.rds"))

#### construct confis for diff schooling, lines 1570-1982    {r construct confis for diff schooling, eval = T, warning = F}    eval = T
library(car)
library(multcomp)
#### schooling
screglists <-  paste0("sc", 2:3)
schlevels <- c("primary", "junior", "high")
covadd <- covaddsch 
confis <- NULL
rr <- 4; s <- 2
r <- 1; g <- "Large"
for (rr in 1:length(reglists[[r]])) {
  # rr: regression type: "sc", "sca", "scP", "scT", "scTa", ....
  # regobj: e.g., "scT" with 7 regression specifications: 
  # specification s (from SchoolingCovariateSelectionANCOVA2.R):
  # 1. NA,
  # 2.  "|Enrolled0$",
    # add {dummyJunior/dummyHigh} and
    # {Arms}*{dummyJunior/dummyHigh} or
    # {Arms}*{dummyJunior/dummyHigh}*{Time.x}
  # 3.  "|^dummy[JH].*[rh]$|^dummy[CI].*[ed]\\.dummy[JH].*[rh]$|^dummy[LW].*[cgz]e\\.dummy[JH].*[rh]$|^dummy[JH].*[rh]\\.T|^dummy[CI].*[ed]\\.dummy[JH].*[rh]\\.T|^dummy[LW].*[cgz]e\\.dummy[JH].*[rh]\\.T",
  # 4.  "|ChildAge|Eldest|Head.*0|HHsize0|Flood",
    # add {Arms}*{dummyJunior/dummyHigh}*{Female}
    # {Arms}*{dummyJunior/dummyHigh}*{Female}*{Time.x}
  # 5. "|Female",
   #6. "|ChildAge|Eldest|Head.*0|HHsize0|Flood"
  regtype <- gsub("^..", "", reglists[[r]][rr])
  if (grepl("a", regtype)) 
    lattributes <- lattributeList[[2]] else
    lattributes <- lattributeList[[1]]
  regobj <- robj[[r]][[rr]]
  lmlist <- lapply(regobj, "[[", "lm")
  coefflist <- lapply(lmlist, "[[", "coefficients")
  # check NAs in coeff
  lapply(1:length(coefflist), function(i) c(i, names(coefflist[[i]])[is.na(coefflist[[i]])]))
  coefflist <- lapply(coefflist, function(x) x[!is.na(x)])
  Vlist <- lapply(lapply(regobj, "[[",  "robust"), "[[", "V")
  Mult <- 1
  covadd.trad <- lapply(covaddsch, function(x) x[1])
  # s-th specification
  for (s in 2:length(regobj)) {
    thisreg <- regobj[[s]]$lm
    coeffvec <- thisreg$coeff
    thisV <- regobj[[s]]$robust$V
    #### CLAUDE dea: 2026-04-29 thisres assigned but never read in any project file
    # thisres <- regobj[[s]]$robust$est
    for (g in lattributes) {
      # lattributes: 
      #  Arms (Large, LargeGrace, Cattle) or 
      #  functional attributes (Upfront, Grace, InKind)
      addcova <- lapply(covaddsch, function(x) gsub("XX", g, x))
      # covaddsch[1]: $MofT: \\(Intercept\\), ^dummyJunior$, ^dummyHigh$
      # covaddsch[2]: $FofT: ^Female$, ^dummyJunior.Female$, ^dummyHigh.Female$
      # covaddsch[4]: $FofN: ^dummyXX.Female$, ^dummyXX.dummyJunior.Female$,
      #   ^dummyXX.dummyHigh.Female$
      # covaddsch[7]: $MofNinT: ^dummyXX.TimeYY$, ^dummyXX.dummyJunior.TimeYY$,
      #   ^dummyXX.dummyHigh.TimeYY$
      # So, for example, g = Large gives
      # addcova[4]: $FofN: ^dummyLarge.Female$, ^dummyLarge.dummyJunior.Female$,
      #   ^dummyLarge.dummyHigh.Female$
      # addcova[7]: $MofNinT: ^dummyLarge.TimeYY$, ^dummyLarge.dummyJunior.TimeYY$,
      #   ^dummyLarge.dummyHigh.TimeYY$
      # names(addcova): MofT, FofT, MofN, FofN, MofTinT, FofTinT, MofNinT, FofNinT
      #   male of trad, female of trad, male of nontrad, female of nontrad, 
      #   male of trad in T, female of trad in T, male of nontrad in T, female of nontrad in T
      for (gg in names(addcova))
        assign(paste0("addcova", gg), addcova[[gg]])
      # i: school level
      # addcovaMofT: \\(Intercept\\), ^dummyJunior$, ^dummyHigh$
      for (i in 1:length(addcovaMofT)) {
        # hvMofTA: average change = 0 (of males in trad school i)
        #  Intercept + School 
        #  = MofT
        #  [[1]]"\\(Intercept\\)" "^dummyJunior$"   "^dummyHigh$"
        hvMofTA <- rep(0, length(coeffvec))
        # i picks up: 1 = primary, 2 = primary+dJunior, 3 = primary+dHigh
        hvMofTA[grepl(
             paste0(unique(addcovaMofT[c(1, i)]), collapse = "|")
            , names(coeffvec))] <- 1*Mult
        if (any(is.na(coeffvec))) hvMofTA <- hvMofTA[!is.na(coeffvec)]
        lhcow <- glht(model=thisreg, linfct = matrix(hvMofTA, byrow = T, nrow=1), 
          alternative="two.sided", vcov.=thisV)
        confis <- rbind(confis, 
           c(FileNames[r], regtype, s, "traditional", "male", schlevels[i], 
             "level of reference trad", "2-4", "MofTA", 
             confint(lhcow)$confint[1, ], 
             if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
           )
        # hvFofTA: average change = 0 (of females in trad school i)
        # addcovaFofT: ^Female$, ^dummyJunior.Female$, ^dummyHigh.Female$
        # Intercept+School+^Female$+Female.School
        # = hvMofTA + dFofT
        # For s < 4: There is no Female term. This copies male coefficient.
        # Just use male estimates and label them as "all".
        # After the loop, 
        #  drop s<4 & grepl("female", gender), rewrite "male" => "all"
        hvFofTA <- dFofT <- rep(0, length(coeffvec))
        dFofT[grepl(
             paste0(addcovaFofT[c(1, i)], collapse = "|")
            , names(coeffvec))] <- 1*Mult
        hvFofTA <- hvMofTA + dFofT
        lhcow <- glht(model=thisreg, linfct = matrix(hvFofTA, byrow = T, nrow=1), 
          alternative="two.sided", vcov.=thisV)
        confis <- rbind(confis, 
           c(FileNames[r], regtype, s, "traditional", "female", schlevels[i], 
             "level of reference female trad", "2-4", "FofTA", 
             confint(lhcow)$confint[1, ], 
             if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
           )
        # nontrad
        # construct coefficient names for attribute g
        covadd.nontrad <- lapply(covadd, function(x) gsub("XX", g, x))
        # gross = trad + delta.Arm
        # hvMofNA: average change = 0 (of nontrad arm g at school i)
        # addcovaMofT: \\(Intercept\\), ^dummyJunior$, ^dummyHigh$
        # addcovaMofN: ^dummyLarge$, ^dummyLarge.dummyJunior$, ^dummyLarge.dummyHigh$
        # intercept + School + Arm + Arm.School 
        # = hvMofTA + dMofN
        #  [[1]]"\\(Intercept\\)", "School", "dummyInKind.School"
        dMofNA <- rep(0, length(coeffvec))
        dMofNA[grepl(
             paste0(addcovaMofN[c(1, i)], collapse = "|")
            , names(coeffvec))] <- 1*Mult
        hvMofNA <- hvMofTA + dMofNA
        lhcow <- glht(model=thisreg, linfct = matrix(hvMofNA, byrow = T, nrow=1), 
          alternative="two.sided", vcov.=thisV)
        confis <- rbind(confis, 
             c(FileNames[r], regtype, s, g, "male", schlevels[i], 
               "level of nontrad at school", "2-4", "MofNA", 
               confint(lhcow)$confint[1, ], 
               if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
          )
        # hvFofNA: average change = 0 (of female nontrad Arm g at School i)
        #  \\(Intercept\\)+Arm+School+Female+Arm.School+Arm.Female+School.Female+Arm.School.Female
        #  \\(Intercept\\)+Arm+School+Arm.School + Female+School.Female + Arm.Female+Arm.School.Female
        # hvMofNA + dFofT + dFofNA
        dFofNA <- rep(0, length(coeffvec))
        dFofNA[grepl(
             paste0(addcovaFofN[c(1, i)], collapse = "|")
            , names(coeffvec))] <- 1*Mult
        hvFofNA <- hvMofNA + dFofT + dFofNA
        lhcow <- glht(model=thisreg, linfct = matrix(hvFofNA, byrow = T, nrow=1), 
          alternative="two.sided", vcov.=thisV)
        confis <- rbind(confis, 
             c(FileNames[r], regtype, s, g, "female", schlevels[i], 
               "level of female nontrad at school", "2-4", "FofNA", 
               confint(lhcow)$confint[1, ], 
               if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
          )
        # hvMofN: average difference = 0 (of nontrad Arm g relative to trad, at School i)
        #  hvMofNA - hvMofTA = Arm + Arm.School
        #  dummyInKind + dummyInKind.School
        hvMofN <- hvMofNA - hvMofTA
        lhcow <- glht(model=thisreg, linfct = matrix(hvMofN, byrow = T, nrow=1), 
          alternative="two.sided", vcov.=thisV)
        confis <- rbind(confis, 
             c(FileNames[r], regtype, s, g, "male", schlevels[i], 
               "nontrad - trad, at school", "2-4", "MofN", 
               confint(lhcow)$confint[1, ], 
               if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
          )
        # hvFofN: difference = 0 (of nontrad Arm g females to trad females, at School i)
        #  hvFofNA - hvFofTA = Arm + Arm.School + Arm.Female + Arm.School.Female
        #  = hvMofN + dFofNA
        hvFofN <- hvMofN + dFofNA
        lhcow <- glht(model=thisreg, linfct = matrix(hvFofN, byrow = T, nrow=1), 
          alternative="two.sided", vcov.=thisV)
        confis <- rbind(confis, 
             c(FileNames[r], regtype, s, g, "female", schlevels[i], 
               "female nontrad - female trad, at school", "2-4", "FofN", 
               confint(lhcow)$confint[1, ], 
               if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
          )
        for (tee in 2:4) {
          for (gg in names(addcova)) {
            # for gg (each comparison addcova), substitute period = 3, 4 to YY
            adc <- get(paste0("addcova", gg))
            assign(paste0("addtee", gg), gsub("YY", tee, adc))
            # addcovaMofNinT: ^dummyLarge.TimeYY$, ^dummyLarge.dummyJunior.TimeYY$,
            #   ^dummyLarge.dummyHigh.TimeYY$
            # So, for example, tee = 3 gives
            # addteeMofNinT: ^dummyLarge.Time3$, ^dummyLarge.dummyJunior.Time3$,
            #   ^dummyLarge.dummyHigh.Time3$
            # addcovaFofNinT: ^dummyInKind.Female.TimeYY, 
            #  ^dummyInKind.dummyJunior.Female.TimeYY$, 
            #  ^dummyInKind.dummyHigh.Female.TimeYY$
            # So, for example, tee = 3 gives
            # addteeFofNinT: ^dummyInKind.Female.Time3, 
            #  ^dummyInKind.dummyJunior.Female.Time3$, 
            #  ^dummyInKind.dummyHigh.Female.Time3$
          }
          # hvMofTinT: trad timeX - trad period 2, at school i
          #  School + TimeX + School.TimeX - School
          #  = TimeX + School.TimeX
          #  at tee == 2, (trad timeX - trad period 2, at school i) = 0
          hvMofTinT <- rep(0, length(coeffvec))
          if (tee > 2) 
            hvMofTinT[grepl(
               paste0(addteeMofTinT[c(1 ,i)], collapse = "|")
               , names(coeffvec))] <- 1*Mult
          lhcow <- glht(model=thisreg, linfct = matrix(hvMofTinT, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, "traditional", "male", schlevels[i], 
                 "trad in each period - trad in period 2, at school", tee, 
                 "MofTinT", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          # female
          # hvFofTinT: female trad timeX - female trad period 2, at school i
          #  School + TimeX + Female
          #   + School.TimeX + School.Female + Female.TimeX + School.Female.TimeX 
          #   - (School + Female + School.Female)
          #  = TimeX + School.TimeX  (hvMofTinT)
          #     +Female.TimeX + School.Female.TimeX ... dFofTinT
          #  = hvMofTinT                + dFofTinT
          #  at tee == 2, (female trad timeX - female trad period 2, at school i) = 0
          dFofTinT <- rep(0, length(coeffvec))
          if (tee > 2) 
            dFofTinT[grepl(
              paste0(addteeFofTinT[c(1 ,i)], collapse = "|")
              , names(coeffvec))] <- 1*Mult
          hvFofTinT <- hvMofTinT + dFofTinT
          lhcow <- glht(model=thisreg, linfct = matrix(hvFofTinT, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, "traditional", "female", schlevels[i], 
                 "female trad in each period - period 2 female trad, at school", tee, 
                 "FofTinT", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          # hvMofTinTL: cumulative change = 0 (of trad school i in period X)
          # Intercept+School+TimeX+School.TimeX
          #  = hvMofTA + hvMofTinT
          # if tee == 2
          #   Intercept+School = hvMofTA
          hvMofTinTL <- hvMofTA + hvMofTinT
          if (tee == 2) hvMofTinTL <- hvMofTA
          lhcow <- glht(model=thisreg, linfct = matrix(hvMofTinTL, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, "traditional", "male", schlevels[i], 
                 "level of trad in each period, at school", tee, 
                 "MofTinTL", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          hvFofTinTL <- hvFofTA + hvFofTinT
          if (tee == 2) hvFofTinTL <- hvFofTA
          lhcow <- glht(model=thisreg, linfct = matrix(hvFofTinTL, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, "traditional", "female", schlevels[i], 
                 "level of female trad in each period, at school", tee, 
                 "FofTinTL", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          # dMofNinT: diff = 0 (of nontrad change relative to concurrent trad change, at school iin period X)
          #  TimeX + Arm.TimeX + School.TimeX + Arm.School.TimeX 
          #    - (TimeX + School.TimeX)
          #  = Arm.TimeX + Arm.School.TimeX 
          # addteeMofNinT: ^dummyLarge.Time3$, ^dummyLarge.dummyJunior.Time3$,
          #   ^dummyLarge.dummyHigh.Time3$
          dMofNinT <- dFofNinT <- dFofNinT0 <- rep(0, length(coeffvec))
          dMofNinT[grepl(
            paste0(addteeMofNinT[c(1 ,i)], collapse = "|")
            , names(coeffvec))] <- 1*Mult
          # if tee == 2: Arm + Arm.School = hvMofN
          if (tee == 2) dMofNinT <- hvMofN
          lhcow <- glht(model=thisreg, linfct = matrix(dMofNinT, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, g, "male", schlevels[i], 
                 "nontrad change - trad change, in each period, at school", tee, 
                 "dMofNinT", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          # hvMofNinT: diff = 0 (of nontrad relative to concurrent trad, at school iin period X)
          #  Arm + School + TimeX + Arm.School + Arm.TimeX + School.TimeX + Arm.School.TimeX 
          #    - (School + TimeX + School.TimeX)
          #  = Arm + Arm.School + Arm.TimeX + Arm.School.TimeX
          #  = hvMofN + dMofNinT
          # addteeMofNinT: ^dummyLarge.Time3$, ^dummyLarge.dummyJunior.Time3$,
          #   ^dummyLarge.dummyHigh.Time3$
          #### hvMofNinT <- hvMofTinT + dMofNinT
          #### CLAUDE tpo: hvMofTinT ==should be==> hvMofN
          hvMofNinT <- hvMofN + dMofNinT
          # hvMofN (Arm + Arm.School)
          if (tee == 2) hvMofNinT <- hvMofN
          lhcow <- glht(model=thisreg, linfct = matrix(hvMofNinT, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, g, "male", schlevels[i], 
                 "nontrad - trad, in each period, at school", tee, 
                 "MofNinT", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          # dFofNinT: diff = 0 (of female nontrad change relative to concurrent female trad change, at school i in period X)
          #  TimeX + Arm.TimeX + Female.TimeX + School.TimeX + 
          #   + Arm.School.TimeX + Arm.Female.TimeX + Female.School.TimeX 
          #   + Arm.School.Female.TimeX 
          #    - (TimeX + Female.TimeX + School.TimeX + Female.School.TimeX)
          #  = Arm.TimeX + Arm.School.TimeX + Arm.Female.TimeX
          #   + Arm.School.Female.TimeX 
          #  = dMofNinT + Arm.Female.TimeX + Arm.School.Female.TimeX
          #  = dMofNinT + dFofNinT0
          # addteeFofNinT:  "^dummyInKind.Female.Time4$, 
          #  ^dummyInKind.dummyJunior.Female.Time4$, 
          #  ^dummyInKind.dummyHigh.Female.Time4$
          dFofNinT0[grepl(
            paste0(addteeFofNinT[c(1 ,i)], collapse = "|")
            , names(coeffvec))] <- 1*Mult
          dFofNinT <- dMofNinT + dFofNinT0
          # if tee == 2: Arm.TimeX + Arm.School.TimeX + Arm.Female.TimeX
          #   + Arm.School.Female.TimeX 
          #  =  Arm + Arm.School + Arm.Female+ Arm.School.Female
          #  = hvFofN
          if (tee == 2) dFofNinT <- hvFofN
          lhcow <- glht(model=thisreg, linfct = matrix(dFofNinT, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, g, "female", schlevels[i], 
                 "female nontrad change - female trad change, in each period, at school", tee, 
                 "dFofNinT", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          # hvFofNinT: diff = 0 (of female nontrad relative to concurrent female trad, at school iin period X)
          #  Arm + Arm.School + Arm.Female + Arm.TimeX + 
          #  Arm.School.Female + Arm.School.TimeX + Arm.Female.TimeX + 
          #  Arm.School.Female.TimeX
          #  = Arm + Arm.School  (MofN)
          #   + Arm.Female + Arm.School.Female  (dFofNA)
          #   + Arm.TimeX + Arm.School.TimeX (dMofNinT)
          #   +  Arm.Female.TimeX + Arm.School.Female.TimeX  (dFofNinT0)
          #  = hvMofN + dFofNA + dMofNinT + dFofNinT0
          #  = hvMofN + dFofNA + dFofNinT
          hvFofNinT <- hvMofN + dFofNA + dFofNinT
          # if tee == 2,
          #  = Arm + Arm.School  (MofN)
          #   + Arm.Female + Arm.School.Female  (dFofNA)
          #  = hvFofN
          if (tee == 2) hvFofNinT <- hvMofN + dFofNA
          lhcow <- glht(model=thisreg, linfct = matrix(hvFofNinT, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, g, "female", schlevels[i], 
                 "female nontrad - female trad, in each period, at school", 
                 tee, "FofNinT", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          # hvMofNinTL: cumulative change = 0 (of nontrad school i in period X)
          #  (intercept) + School + TimeX + School.TimeX 
          #    + Arm + Arm.School 
          #    + Arm.TimeX + Arm.School.TimeX 
          #  = hvMofTinTL + hvMofN + dMofNinT
          #### hvMofNinTL <- hvMofTinTL + hvMofN + hvMofNinT
          #### CLAUDE tpo: hvMofNinT ==should be==> dMofNinT
          hvMofNinTL <- hvMofTinTL + hvMofN + dMofNinT
          #  if tee == 2
          #  (intercept) + School + Arm + Arm.School
          #  = hvMofNA
          #### if (tee == 2) hvMofNinTL <- hvMofTA
          #### CLAUDE tpo: hvMofTA ==should be==> hvMofNA
          if (tee == 2) hvMofNinTL <- hvMofNA
          lhcow <- glht(model=thisreg, linfct = matrix(hvMofNinTL, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, g, "male", schlevels[i], 
                 "level of nontrad in each period at school", tee, 
                 "MofNinTL", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
          # hvFofNinTL: cumulative change = 0 (of female nontrad school i in period X)
          #  (intercept) + School + TimeX + School.TimeX 
          #   + Arm + Arm.School 
          #   + Arm.TimeX + Arm.School.TimeX 
          #   + Female + School.Female  (dFofT)
          #   + Female.TimeX + School.Female.TimeX (dFofTinT)
          #   + Arm.Female + Arm.School.Female  (dFofNA)
          #   +  Arm.Female.TimeX + Arm.School.Female.TimeX  (dFofNinT0)
          # = hvMofNinTL + dFofT + dFofTinT + dFofNA + dFofNinT0
          #### hvFofNinTL <- hvMofTinTL + dFofT + dFofTinT + dFofNA + dFofNinT0
          #### CLAUDE tpo: hvMofTinTL ==should be==> hvMofNinTL
          hvFofNinTL <- hvMofNinTL + dFofT + dFofTinT + dFofNA + dFofNinT0
          #  if tee == 2
          #  (intercept) + School + Arm + Arm.School  (hvMofNA)
          #   + Female + School.Female  (dFofT)
          #   + Arm.Female + Arm.School.Female  (dFofNA)
          #### if (tee == 2) hvFofNinTL <- hvMofTA + dFofT + dFofNA
          #### CLAUDE tpo: hvMofTA ==should be==> hvMofNA
          if (tee == 2) hvFofNinTL <- hvMofNA + dFofT + dFofNA
          lhcow <- glht(model=thisreg, linfct = matrix(hvFofNinTL, 
            byrow = T, nrow=1), alternative="two.sided", vcov.=thisV)
          confis <- rbind(confis, 
               c(FileNames[r], regtype, s, g, "female", schlevels[i], 
                 "level of female nontrad in each period at school", 
                 tee, "FofNinTL", 
                 confint(lhcow)$confint[1, ], 
                 if (sum(lhcow$linfct) != 0) summary(lhcow)$test$pvalues[1] else NA)
            )
        }  # end: TimeT tee loop
      } # end: school level i loop
    } # end: attribute g loop
  } # end: specification s loop
} # end: regression type rr loop

#### reshape confis, lines 1983-2066    {r reshape confis, eval = T, warning = F}    eval = T
confis <- data.table(confis)
setnames(confis, c("FileName", "regtype", "num", "attributes", 
  "gender", "school", "ImpactType", "period", "hv", "estimate", "lb", "ub", "pvalue"))
 # traditional has 4 same entries when computing interactions 
confis <- confis[!duplicated(confis), ]
#### drop: b, a, P have zero in time interaction tests (e.g., TimeX + Arm.TimeX for X == 2)
confis <- confis[estimate != 0, ]
numcols <- c("period", "estimate", "lb", "ub", "num", "pvalue")
confis[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
#### drop: females are only added for specification s >= 5 & regtype = T, rewrite "male" => "all"
confis <- confis[!(num < 5 & grepl("female", ImpactType)), ]
#### Note: rewrite "male" => "all" for num < 5, 
#### we are losing gender = "male" entries for specification < 5. 
#### Need to get gender = all entries for graphs. But this should not be a problem 
#### as we need "male" entries only when we contrast with females (num>=5).
confis[num < 5 & grepl("male", gender), gender := "all"]
confis[grepl("T", regtype) & num < 5 & grepl("ref", ImpactType), ][
  order(regtype, num, attributes, ImpactType, period), ]
faccols <- c("FileName", "regtype", "attributes", "ImpactType", "hv", "gender", "school")
confis[, (faccols) := lapply(.SD, as.factor), .SDcols = faccols]
confis[, ImpactType := factor(ImpactType, 
  levels = c(
    "level of reference trad",
    "level of reference female trad",
    "level of trad in each period, at school",
    "level of female trad in each period, at school",
    "trad in each period - trad in period 2, at school",
    "female trad in each period - period 2 female trad, at school",
    "level of nontrad at school",
    "level of female nontrad at school",
    "level of nontrad in each period at school",
    "level of female nontrad in each period at school",
    "nontrad - trad, at school",
    "female nontrad - female trad, at school",
    "nontrad change - trad change, in each period, at school",
    "female nontrad change - female trad change, in each period, at school",
    "nontrad - trad, in each period, at school",
    "female nontrad - female trad, in each period, at school"
  ))]
confis[, ImpactType := factor(ImpactType, 
  labels = c(
    "level of reference trad",
    "level of reference female trad",
    "level of trad in each period at school",
    "level of female trad in each period at school",
    "trad in each period - trad in period 2, at school",
    "female trad in each period - period 2 female trad, at school",
    "level of nontrad at school",
    "level of female nontrad at school",
    "level of nontrad in each period at school",
    "level of female nontrad in each period at school",
    "nontrad - trad, at school",
    "female nontrad - female trad, at school",
    "nontrad change - trad change, in each period, at school",
    "female nontrad change - female trad change, in each period, at school",
    "nontrad - trad, in each period, at school",
    "female nontrad - female trad, in each period, at school"
  ))]
confis[, hv := factor(hv, 
  levels = c(
  "MofTA", "FofTA", "MofNA", "FofNA", "MofN", "FofN",
   "MofTinT", "FofTinT", "MofTinTL", "FofTinTL",
   "dMofNinT", "dFofNinT", "MofNinT" , "FofNinT",
   "MofNinTL", "FofNinTL"
  ))]
confis[, school := factor(school, levels = c("primary", "junior", "high"))]
confis[!grepl("T", regtype), period := NA]
setcolorder(confis, c("FileName", "regtype", "num", "attributes", 
  "ImpactType", "period", "lb", "estimate", "ub"))
confis[, attributes := factor(attributes, 
  levels = c("traditional", "Large", "LargeGrace", "Cattle",
  "LargeSize", "WithGrace", "InKind"))]
confis[, attributes := factor(attributes, 
  labels = c("Traditional", "Large", "LargeGrace", "Cattle",
  "Upfront", "WithGrace", "InKind"))]
confis[, AtType := "Arms (relative to Traditional)"]
confis[grepl("With|Kin", attributes), AtType := "Functional attributes (relative to Upfront, Upfront+WithGrace)"]
confis[, AtType := factor(AtType)]
saveRDS(confis, paste0(pathsaveHere, "EstimatesCISchooling.rds"))
qsave(confis, paste0(pathsaveHere, "EstimatesCISchooling.qs"))
confis[grepl("^.ofNinT$", hv) & grepl("T$", regtype) & grepl("ge$", attributes) & 
  grepl("j", school) & num == 5, ][order(gender, period), ]

#### inline, line 2070    eval = T
gsub("\\\\|\\^|\\$", "", addcovaMofT)

#### inline, line 2072    eval = T
gsub("\\\\|\\^|\\$", "", addcovaFofT)

#### inline, line 2074    eval = T
gsub("\\\\|\\^|\\$", "", addcovaMofN)

#### inline, line 2076    eval = T
gsub("\\\\|\\^|\\$", "", addcovaFofN)

#### inline, line 2078    eval = T
gsub("\\\\|\\^|\\$", "", addteeMofTinT)

#### inline, line 2080    eval = T
gsub("\\\\|\\^|\\$", "", addteeFofTinT)

#### inline, line 2082    eval = T
gsub("\\\\|\\^|\\$", "", addteeMofNinT)

#### inline, line 2084    eval = T
gsub("\\\\|\\^|\\$", "", addteeFofNinT)

#### delete robj for memory management, lines 2087-2089    {r delete robj for memory management, echo = F}    eval = T
rm(robj)

#### read mean outcomes data, lines 2107-2183    {r read mean outcomes data, class.output="scroll-100"}    eval = T
#### Run ComputeNetAssetsANCOVA.R first to produce 
#### various "Figure" data. This is ran in 
#### ### Descriptive statistics as a knitr child file.
#### source(paste0(pathprogram, "ComputeNetAssetsANCOVA.R"))
NeA1R <- readRDS(paste0(pathsaveHere, 
  "NetAssetsANCOVATrimmed.rds"))
cpn <- NeA1R[(CompleteAssetPanel) & tee == 1,
  .(Arm, hhid, tee, 
   CP=CompleteAssetPanel, NLHAssetAmount, 
   PAssetAmount, NumCows)]
#### comple asset panel sample is only 220 HHs 
#### and only 35 HHs for traditional
lvoD <- readRDS(paste0(pathsaveHere, 
  "NumCowsFigure.rds"))
nAD <- readRDS(paste0(pathsaveHere, 
  "AllNetAssetsFigureMeanData.rds"))
cpn <- readRDS(paste0(pathsaveHere, 
  "CPNetAssetsFigureMeanData.rds"))
conD <- readRDS(paste0(pathsaveHere, 
  "ConsumptionFigure.rds"))
labDHH <- readRDS(paste0(pathsaveHere, 
  "HHLabourIncomeFigure.rds"))
labDpc <- readRDS(paste0(pathsaveHere, 
  "pcHHLabourIncomeFigure.rds"))
schD <- readRDS(paste0(pathsaveHere, 
  "SchoolingFigure.rds"))
schD[, file := Schooling]
#### break down various asset measures in nAD
nAD[grepl("cow", Arm), Arm := "cattle"]
armsC <- c("traditional", "large", "large grace", "cattle")
nAD[, Arm := factor(Arm, levels = armsC)]
na <- nAD[, .(Arm, tee, NetValue..mean, NetValue..upper, 
  NetValue..lower, NetValue..N)]
pal <- nAD[, .(Arm, tee, ProdValue..mean, ProdValue..upper, 
  ProdValue..lower, ProdValue..N)]
pa <- nAD[, .(Arm, tee, PAssetAmount..mean, 
  PAssetAmount..upper, PAssetAmount..lower, PAssetAmount..N)]
cpn <- cpn[, .(Arm, tee, NetValue..mean, NetValue..upper, 
  NetValue..lower, NetValue..N)]
setnames(na, grepout("Ne", colnames(na)), 
  c("mean", "upper", "lower", "N"))
setnames(pal, colnames(na))
setnames(pa, colnames(na))
setnames(cpn, colnames(na))
figD <- rbindlist(list(
  cbind(file = "NumCows", lvoD),
  cbind(file = "PAssetAmount", pa),
  cbind(file = "ProdValue", pal),
  cbind(file = "NetAssets", na),
  cbind(file = "CPNetAssets", cpn),
  cbind(file = "Consumption", conD),
  cbind(file = "HHLabourIncomes", labDHH),
  cbind(file = "pcHHLabourIncomes", labDpc),
  schD[, .(file, Arm, sex, tee, lower, mean, upper, N)]
  ), use.names = T, fill = T)
figD[, file := factor(file, levels = c(
  "NumCows", "PAssetAmount", "ProdValue",
  "NetAssets", "CPNetAssets", 
  "Consumption", "HHLabourIncomes", "pcHHLabourIncomes",
  "primary0512", "junior1315", "high1618"))]
figD[, file := factor(file, labels = c(
  "NumCows", "PAssetAmount", "ProdValue",
  "NetAssets", "CPNetAssets", 
  "Consumption", "HHIncomes", "pcIncomes",
  "Sch0512", "Sch1315", "Sch1618"))]
figD[is.na(sex), sex := "NA"]
figD[, sex := factor(sex, labels = c("M", "F", "NA"))]
figD[, Sex := factor(sex, labels = 
  c("Male", "Female", "Per household"))]
figD[grepl("tra", Arm), Arm := "Traditional"]
figD[grepl("ge$", Arm), Arm := "Large"]
figD[grepl("ce$", Arm), Arm := "LargeGrace"]
figD[grepl("^ca", Arm), Arm := "Cattle"]
figD[, Arm := factor(Arm, levels = c(Arms[-4], "Cattle"))]
saveRDS(figD, paste0(pathsaveHere, "figD.rds"))

#### figure raw outcomes, lines 2186-2270    {r figure raw outcomes, echo = F, warning = F, message = F, fig.align='center', fig.height = 12, fig.width = 12, fig.lp = 'Figure '}    eval = T
library(ggplot2)
figD <- readRDS(paste0(pathsaveHere, "figD.rds"))
Yfacet = c(
  `NumCows` = "number\nof cattle", 
  `PAssetAmount` = "productive\nassets", 
  `ProdValue` = "livestock and\nproductive\nassets", 
  `NetAssets` = "net assets", 
  `CPNetAssets` = "complete\npanel\nnet assets", 
####  `NarrowNetAssets` = "narrow\nnet assets", 
  `Consumption` = "consumption", 
  `HHIncomes` = "HH\nincomes", 
  `pcIncomes` = "per capita\nincomes", 
  `Sch0512` = "school\n05-12", 
  `Sch1315` = "school\n13-15", 
  `Sch1618` = "school\n16-18"
  )
Xfacet = c(`Traditional` = "Traditional", `Large` = "Large",
    `LargeGrace` = "Large grace", `Cattle` = "Cattle")
p <- ggplot(data = subset(figD, !grepl("^R|Br", file))
  , aes(x = factor(tee), y = mean, colour = Sex, 
     shape = Sex, group = interaction(Arm, Sex))
  , size = 0.1, stroke = 0) + 
  geom_pointrange(aes(
    ymin = lower, ymax = upper), 
    stat = "identity", fatten = 1.25, 
    position = position_dodge(width = .5))
p <- p + 
  scale_y_continuous(name = "values" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 1:4) +
  #scale_colour_viridis_d(guide = "legend") +
  scale_colour_manual(
  values = c("Male" = "green", "Female" = "red", 
    "Per household" = "blue")
  ) +
  scale_shape_manual(values = 
    c("Male" = 16, "Female" = 17, "Per household" = 15)
  ) +
  facet_grid(file ~ Arm, scales = "free_y", 
    labeller = labeller(file = Yfacet, Arm = Xfacet)) +
  theme(
    strip.text.x = element_text(color = "blue", size = 6, 
      margin = margin(0, 1.25, 0, 1.25, "cm")), 
    strip.text.y = element_text(color = "blue", size = 6, 
      margin = margin(t=1.65, r=.1, b=1.65, l=.1, "cm")),
    axis.text.x = element_text(size = 5, angle = 0, vjust = 1, hjust = 1), 
    axis.text.y = element_text(size = 6), 
    plot.title = element_text(size = 8), 
    axis.title = element_text(size = 8), 
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(.25, "cm"),    
    legend.margin = margin(t=-.5, r=0, b=0, l=0, "cm"), # 
    legend.box.margin = margin(t=.5, r=0, b=0, l=0, "cm"), # 
    legend.box.spacing = unit(.25, "cm"), # 
    legend.position="bottom") + 
  labs(
    x="Periods", 
    y = "values (in numbers or BDT or rates)", 
    colour = "",
    shape = ""
    )+ 
  guides(
    colour = guide_legend(title = "Groups", nrow = 1), 
    shape = guide_legend(title = "Groups", 
      override.aes = list(size = .125), nrow = 1)
    )
ggsave(
  paste0(pathprogram, 
    "figure/EstimationMemo/MeanOutcomesByArmAndPeriod.jpg")
  , p,
  width = 12*2, height = 15*2, units = "cm",
  dpi = 300
 )
par(lwd=.5)
pdf(
  paste0(pathprogram, 
    "figure/EstimationMemo/MeanOutcomesByArmAndPeriod.pdf")
  , width = 12/2.54, height = 15*2/2.54)
print(p)
whatever <- dev.off()
par(lwd=1)

#### figure assets only, lines 2272-2360    {r figure assets only, echo = F, warning = F, message = F, results = "hide", fig.align='center', fig.height = 12, fig.width = 12, fig.lp = 'Figure '}    eval = T
library(ggplot2)
#### read_cleaned_data.rnw(1113)
#### NarrowNetValue := 
####    TotalImputedValue + NarrowHAssetAmount + PAssetAmount 
####    - a2b(DebtOutstanding.before, NA, 0) - a2b(NonNGOBal, NA, 0)
#### NarrowHAssetAmount := 
####    sum(amount[type %in% completeAsset], na.rm = T)
#### Assets observed in all rounds. Some items 
#### (cabinets, residential land, vcr/vcp, motorcycle, jewelry, 
#### are not recorded in period 2) are observed from round 2. 
#### NarrowHAssets are
####   "television"      "bicycle"         "wrist watch"     "electric fan"   
####   "wall clock"      "radio"           "cassette player" "rickshaw/van"   
#### read_cleaned_data.rnw(1172)
#### NarrowAssetAmount := NarrowHAssetAmount+PAssetAmount
figD <- readRDS(paste0(pathsaveHere, "figD.rds"))
Yfacet = c(
  `NetAssets` = "net assets", 
  `ProdValue` = "livestock and\nproductive assets"
  )
Xfacet = c(`Traditional` = "Traditional", `Large` = "Large",
    `LargeGrace` = "Large grace", `Cattle` = "Cattle")
p <- ggplot(data = subset(figD, grepl("^NetA|^ProdV", file))
  , aes(x = factor(tee), y = mean, colour = Arm, shape = Arm, group = Arm)
  , size = 0.1, stroke = 0) + 
  geom_pointrange(aes(
    ymin = lower, ymax = upper), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .5))
p <- p + 
  scale_y_continuous(name = "values" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 1:4) +
  scale_colour_manual(
  values = c("Traditional" = "green", "Large" = "darkgreen", 
    "LargeGrace" = "blue", "Cattle" = "red")
  ) +
  scale_shape_manual(
  values = c("Traditional" = 16, "Large" = 17, 
    "LargeGrace" = 18, "Cattle" = 15)
  ) +
  facet_grid(file ~ Arm, scales = "free_y", 
    labeller = labeller(file = Yfacet, Arm = Xfacet)) +
  theme(
    plot.margin = margin(t=.0, r=0, b=1, l=0, "cm"), # 
    strip.text.x = element_text(color = "blue", size = 6, 
      margin = margin(0, 1.25, 0, 1.25, "cm")), 
    strip.text.y = element_text(color = "blue", size = 6, 
      margin = margin(1.5, 0, 1.5, 0, "cm")),
    axis.text.x = element_text(size = 5, angle = 0, vjust = 1, hjust = 1), 
    axis.text.y = element_text(size = 6), 
    plot.title = element_text(size = 8), 
    axis.title = element_text(size = 6), 
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(.25, "cm"),    
    legend.margin = margin(t=-.5, r=0, b=0, l=0, "cm"), # 
    legend.box.margin = margin(t=.5, r=0, b=0, l=0, "cm"), # 
    legend.box.spacing = unit(.25, "cm"), # 
    legend.position="bottom") + 
  labs(
    x="Periods", 
    y = "values (in BDT)", 
    colour = "",
    shape = ""
    )+ 
  guides(
    colour = guide_legend(title = "Arms", nrow = 1), 
    shape = guide_legend(title = "Arms", 
      override.aes = list(size = .125), nrow = 1)
    )
ggsave(
  paste0(pathprogram, 
    "figure/EstimationMemo/MeanAssetOutcomesByArmAndPeriod.png"),
  p,
  width = 9, height = 10, units = "cm",
  dpi = 300
 )
par(lwd=.5)
pdf(
  paste0(pathprogram, 
    "figure/EstimationMemo/MeanAssetOutcomesByArmAndPeriod.pdf"),
  , width = 14/2.54, height = 12/2.54)
print(p)
whatever <- dev.off()
par(lwd=1)

#### plot MeanAssetOutcomesByArmAndPeriod, lines 2362-2364    {r plot MeanAssetOutcomesByArmAndPeriod, echo = F}    eval = T
p

#### figure raw outcomes income consumption, lines 2371-2426    {r figure raw outcomes income consumption, echo = F, warning = F, message = F, results = "hide", fig.align='center', fig.height = 12, fig.width = 12, fig.lp = 'Figure '}    eval = T
library(ggplot2)
figD <- readRDS(paste0(pathsaveHere, "figD.rds"))
cl <- subset(figD, grepl("Con|Inc", file) & !is.na(Arm))
cl[, file := factor(file, labels = 
  c("Consumption", "Household income", "Per capita income"))]
cl <- cl[!grepl("Per ca.*in", file), ]
p <- ggplot(data = cl
  , aes(x = factor(tee), y = mean, group = Arm)
  , size = 0.1, stroke = 0) + 
  geom_pointrange(aes(
    ymin = lower, ymax = upper), colour = "blue", 
    stat = "identity", fatten = .75, 
    position = position_dodge(width = .5))
p <- p + 
  scale_y_continuous(name = "values" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 1:4) +
  facet_grid(file ~ Arm, scales = "free_y", 
    labeller = labeller(file = 
      c("Consumption" = "Per capita consumption", 
        "HHIncomes" = "Household labour income"
        #,"pcIncomes" = "per capita household labour income"
        ))) +
  theme(
    axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 1), 
    axis.text.y = element_text(size = 8), 
    axis.title = element_text(size = 8), 
    strip.text.x = element_text(color = "blue", size = 8, 
      margin = margin(.05, 1.25, .05, 1.25, "cm")), 
    strip.text.y = element_text(color = "blue", size = 8, 
      margin = margin(1.5, .05, 1.5, .05, "cm")),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(.25, "cm"),
    legend.margin = margin(t=-.5, r=0, b=0, l=0, "cm"), # 
    legend.box.margin = margin(t=.5, r=0, b=0, l=0, "cm"), # 
    legend.box.spacing = unit(.25, "cm"), # 
    legend.position="none") + 
  xlab("periods")
invisible(ggsave(
  paste0(pathprogram, 
    "figure/EstimationMemo/MeanConsumptionIncomeByArmAndPeriod.jpg"),
  p,
  width = 1, height = 1, units = "cm",
  dpi = 300
 ))
par(lwd=.1)
pdf(
  paste0(pathprogram, 
    "figure/EstimationMemo/MeanConsumptionIncomeByArmAndPeriod.pdf")
  , width = 2*12/2.54, height = 2*6/2.54)
print(p)
whatever <- dev.off()

#### plot MeanConsumptionIncomeByArmAndPeriod, lines 2428-2430    {r plot MeanConsumptionIncomeByArmAndPeriod, echo = F}    eval = T
p

#### figure raw outcomes income consumption dodge, lines 2431-2500    {r figure raw outcomes income consumption dodge, echo = F, warning = F, message = F, results = "hide", fig.align='center', fig.height = 12, fig.width = 12, fig.lp = 'Figure '}    eval = T
library(ggplot2)
figD <- readRDS(paste0(pathsaveHere, "figD.rds"))
cl <- subset(figD, grepl("Con|Inc", file) & !is.na(Arm))
cl[, file := factor(file, labels = 
  c("Consumption", "Household income", "Per capita income"))]
p <- ggplot(data = cl
  , aes(x = factor(tee), y = mean, colour = Arm, shape = Arm, group = Arm)
  , size = 0.1, stroke = 0) + 
  geom_pointrange(aes(
    ymin = lower, ymax = upper), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .5))
p <- p + 
  scale_y_continuous(name = "values" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 1:4) +
  scale_colour_manual(
  values = c("Traditional" = "green", "Large" = "darkgreen", 
    "LargeGrace" = "blue", "Cattle" = "red")
  ) +
  scale_shape_manual(
  values = c("Traditional" = 16, "Large" = 17, 
    "LargeGrace" = 18, "Cattle" = 15)
  ) +
  facet_grid(file ~ ., scales = "free_y") +
  theme(
    strip.text.x = element_text(color = "blue", size = 8, 
      margin = margin(0, 1.25, 0, 1.25, "cm")), 
    strip.text.y = element_text(color = "blue", size = 8, 
      margin = margin(1.5, 0, 1.5, 0, "cm")),
    axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 1), 
    axis.text.y = element_text(size = 8), 
    plot.title = element_text(size = 8), 
    axis.title = element_text(size = 8), 
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(.25, "cm"),    
    legend.margin = margin(t=-.5, r=0, b=0, l=0, "cm"), # 
    legend.box.margin = margin(t=.5, r=0, b=0, l=0, "cm"), # 
    legend.box.spacing = unit(.25, "cm"), # 
    legend.position="bottom") + 
  labs(
    x="Periods", 
    y = "values (in BDT)", 
    colour = "",
    shape = ""
    )+ 
  guides(
    colour = guide_legend(title = "Arms", nrow = 1), 
    shape = guide_legend(title = "Arms", 
      override.aes = list(size = .125), nrow = 1)
    )
invisible(ggsave(
  paste0(pathprogram, 
    "figure/EstimationMemo/MeanConsumptionIncomeDodge.jpg"),
  p,
  width = 12, height = 10, units = "cm",
  dpi = 300
 ))
par(lwd=.5)
pdf(
  paste0(pathprogram, 
    "figure/EstimationMemo/MeanConsumptionIncomeDodge.pdf")
  , width = 8*2/2.54, height = 10*2/2.54)
print(p)
whatever <- dev.off()
par(lwd=1)

#### net saving and cumulative repayment, lines 2505-2595    {r net saving and cumulative repayment, warning = F, message = F, results = "hide", fig.align='center', fig.height = 5, fig.width = 10, fig.cap = paste0("Weekly net saving and cumulative repayment by poverty status", ""), fig.lp = 'Figure '}    eval = T
library(ggplot2)
ga <- arA[!is.na(Date) & !is.na(DisDate1) & grepl("Yes", creditstatus), 
  .(Arm, hhid, povertystatus, MonthsElapsed, 
  DebtOutstanding, CumLoanAmount, value.repay, value.NetSaving, 
  CumPlannedInstallment, CumRepaidRate, CumEffectiveRepaidRate)]
ga <- arA[!is.na(Date) & !is.na(DisDate1) & grepl("Yes", creditstatus), 
  .(Arm, hhid, povertystatus, MonthsElapsed, 
  value.NetSaving, CumRepaid)]
ga1 <- ga[, !grepl("Ne", colnames(ga)), with = F]
ga1[, variable := "cumulative repayment"]
ga2 <- ga[, !grepl("Rep", colnames(ga)), with = F]
ga2[, variable := "weekly net saving"]
setnames(ga1, grepout("Re", colnames(ga1)), "amount")
setnames(ga2, grepout("Ne", colnames(ga2)), "amount")
ga <- rbindlist(list(ga1, ga2))
ga[, Arm := factor(Arm, labels = c(Arms[-4], "Cattle"))]
ga[, variable := factor(variable)]
ColourForPoints <- c("darkblue", "darkred")
g1 <- ggplot(ga[grepl("rep", variable), ], 
  aes(x = MonthsElapsed, y = amount, 
    colour = povertystatus, group = povertystatus)) +
  geom_point(aes(fill = povertystatus), size = .01, 
    position = position_dodge(width = .5), #colour = "transparent",
    alpha = .6) +
  geom_smooth(span = .5, size = .75,
    aes(colour = povertystatus, group = povertystatus)) +
  scale_colour_manual(values = ColourForPoints) +
  scale_fill_manual(values = c("blue", "red")) +
  theme(
    legend.position="none", 
    plot.margin = margin(t = 0, b = 0, l = 0, r = 0, "cm"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    strip.text.x = element_text(color = "blue", size = 6, 
      margin = margin(0, .5, 0, .5, "cm")), 
    strip.text.y = element_text(color = "blue", size = 6, 
      margin = margin(.5, 0, .5, 0, "cm"))
  ) + 
  scale_y_continuous(limits = c(0, 20000)) +
  scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, 12))+
  xlab("Months since 1st loan disbursement") + 
  ylab("Amount (BDT)") +
  facet_grid(variable ~ Arm)
g2 <- ggplot(ga[grepl("sav", variable), ], 
  aes(x = MonthsElapsed, y = amount, 
    colour = povertystatus, group = povertystatus)) +
  geom_point(aes(fill = povertystatus), size = .01, 
    position = position_dodge(width = .5), #colour = "transparent",
    alpha = .6) +
  geom_smooth(span = .5, size = .75,
    aes(colour = povertystatus, group = povertystatus)) +
  scale_colour_manual(values = ColourForPoints) +
  scale_fill_manual(values = c("blue", "red")) +
  theme(
    legend.position="bottom", 
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 9),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(.25, "cm"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    strip.text.x = element_text(color = "blue", size = 6, 
      margin = margin(0, .5, 0, .5, "cm")), 
    strip.text.y = element_text(color = "blue", size = 6, 
      margin = margin(.5, 0, .5, 0, "cm"))
  ) + 
  scale_y_continuous(limits = c(0, 800)) +
  scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, 12))+
  xlab("Months since 1st loan disbursement") + 
  ylab("Amount (BDT)") +
  facet_grid(variable ~ Arm)
library(gridExtra)
g <- grid.arrange(g1, g2, heights = c(2, 2.5), ncol=1)
ggsave(
  paste0(pathprogram, 
    "figure/ImpactEstimationOriginal1600Memo2/",
    "CumulativeWeeklyNetSavingAndRepayment.png"),
  g,
  width = 14, height = 10, units = "cm",
  dpi = 400
 )
library(grid)
cairo_pdf(
  paste0(pathprogram, 
    "figure/ImpactEstimationOriginal1600Memo2/",
    "CumulativeWeeklyNetSavingAndRepayment.pdf"),
  , width = 13/2.54, height = 10/2.54, pointsize = 10) # native unit: inch
grid.draw(g)
whatever <- dev.off()

#### plot CumulativeWeeklyNetSavingAndRepayment, lines 2600-2602    {r plot CumulativeWeeklyNetSavingAndRepayment, echo = F, fig.cap = "Weekly net saving and cumulative repayment by poverty status", warning = F}    eval = T
grid.draw(g)

#### Cumulative repaid rate original HHs, lines 2609-2674    {r Cumulative repaid rate original HHs, warning = F, message = F, results = "hide", fig.align='center', fig.height = 5, fig.width = 10, fig.cap = paste0("Cumulative repayment rates", ""), fig.lp = 'Figure '}    eval = T
library(ggplot2)
ga <- arA[!is.na(Date) & !is.na(DisDate1) & grepl("Yes", creditstatus) & 
  grepl("bo", BStatus) & o800 == 1L, 
  .(Arm, hhid, povertystatus, MonthsElapsed, 
  CumNetSaving, CumRepaid, CumRepaidRate, CumEffectiveRepaidRate)] 
####  ga1: amount
ga1 <- ga[, !grepl("Ne|Rate", colnames(ga)), with = F]
ga1[, variable := "repayment"]
####  ga2: rate
ga20 = copy(ga)
ga20 <- ga20[, grepout("Ne|Repaid$|variab", colnames(ga20)) := NULL]
ga20 <- ga20[!is.na(CumEffectiveRepaidRate) & 
  !is.na(CumEffectiveRepaidRate), ]
ga21 <- ga20[, .(Arm, hhid, povertystatus, MonthsElapsed, CumEffectiveRepaidRate)]
ga22 <- ga20[, .(Arm, hhid, povertystatus, MonthsElapsed, CumRepaidRate)]
setnames(ga21, "CumEffectiveRepaidRate", "value")
setnames(ga22, "CumRepaidRate", "value")
ga21[, variable := "Repay+net saving"]
ga22[, variable := "Repayment"]
ga2 <- rbindlist(list(ga21, ga22))
ga2[, variable := factor(variable, 
  levels = c("Repayment", "Repay+net saving"))]
setnames(ga1, grepout("Re", colnames(ga1)), "amount")
#setnames(ga2, grepout("Re", colnames(ga2)), "amount")
#ga <- rbindlist(list(ga1, ga2))
ColourForPoints <- c("darkblue", "darkred")
g <- ggplot(ga2, 
  aes(x = MonthsElapsed, y = value, 
    colour = povertystatus, group = povertystatus)) +
  geom_point(aes(fill = povertystatus), size = .01, 
    position = position_dodge(width = .5), #colour = "transparent",
    alpha = .6) +
  geom_smooth(span = .5, size = .5, #colour = "blue", 
    aes(colour = povertystatus, group = povertystatus)) +
  scale_colour_manual(values = ColourForPoints) +
  scale_fill_manual(values = c("blue", "red")) +
####   scale_shape_manual(values=c(21, 25)) +
  theme(
    legend.position="bottom", 
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 9),
    legend.key = element_rect(fill = "white"),
    legend.key.size = unit(.5, "cm"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    strip.text.x = element_text(color = "blue", size = 6, 
      margin = margin(0, .5, 0, .5, "cm")), 
    strip.text.y = element_text(color = "blue", size = 6, 
      margin = margin(.5, 0, .5, 0, "cm"))
  ) + 
  scale_y_continuous(limits = c(0, 2)) +
  scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, 12)) +
  xlab("Months since 1st loan disbursement") + 
  ylab("Cumulative repayment rates") +
  facet_grid(variable ~ Arm, scales = "free_y") +
  geom_hline(aes(yintercept = 1), colour = "lightgreen", data = ga2)
ggsave(
  paste0(pathprogram, 
    "figure/ImpactEstimationOriginal1600Memo2/",
    "CumulativeWeeklyRepaymentRateByPovertystatus.png"),
  g,
  width = 12, height = 6, units = "cm",
  dpi = 300
 )

#### plot CumulativeWeeklyRepaymentRateByPovertystatus, lines 2676-2678    {r plot CumulativeWeeklyRepaymentRateByPovertystatus, echo = F, warning = F, message = F, fig.cap = "Cumulative weekly net repayment rates by poverty status"}    eval = T
g

#### summary of 1600, lines 2686-2689    {r summary of 1600, warning = F, echo = F, eval = F}    eval = F
# summary(ar[tee == 1 & o1600 == 1L, .(MonthGap, Mstatus, 
#   Mgroup, BorrowerStatus, creditstatus)])

#### figure NetAssets relative to concurrent trad, lines 2702-2758    {r figure NetAssets relative to concurrent trad, warning = F, message = F, fig.align='center', fig.height = 12, fig.width = 10}    eval = T
library(ggplot2)
confi <- qread(paste0(pathsaveHere, "EstimatesCI.qs"))
confi1 <- confi[
  grepl("^NetAssets$", FileName) & 
  !grepl("None|Adi|Own", FileName) & 
  !grepl("Tr|Up", attributes) &
  grepl("^periN", hv) &
  grepl("Ta?$", regtype), ]
confi1[grepl("^Large$", attributes), attributes := "Large/Upfront"]
cols <- c("FileName", "regressand", "attributes", "regtype")
confi1[, (cols) := droplevels(.SD), .SDcols = cols]
confi1[, attributes := factor(attributes, levels =
  c("Large/Upfront", "LargeGrace", "Cattle", "WithGrace", "InKind"),
  labels = c("Large/Upfront", "Large grace", "Cattle", "WithGrace", "InKind"))]
confi1[, num := factor(num-1)]
p <- ggplot(data = confi1
  , aes(x = factor(period), y = estimate, 
      colour = num, shape = num, group = num)) + 
  geom_pointrange(aes(
    ymin = lb, ymax = ub), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .5))
p <- p + facet_grid( ~ attributes, scales = "free_y") + 
  scale_y_continuous(name = "impacts" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 2:4) +
  theme(
   axis.text.x = element_text(size = 5, angle = 0, vjust = 1, hjust = 1), 
   axis.text.y = element_text(size = 6), 
   axis.title = element_text(size = 6), 
   strip.text.x = element_text(color = "blue", size = 9, 
     margin = margin(.1, 1.25, .1, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 9, 
     margin = margin(1.5, .1, 1.5, .1, "cm")),
   panel.spacing.x = unit(c(.1, .1, .3, .1), units = "cm"),
   panel.spacing.y = unit(.1, units = "cm"),
   legend.position="bottom") + 
  xlab("periods") + 
  labs(color  = "regression specifications", shape = "regression specifications") +
  guides(colour = guide_legend(title = "regression specifications", nrow = 1)) +
  geom_hline(aes(yintercept = 0), data = confi1, colour = "lightgreen")
p <- p + ggh4x::facet_nested( ~ AtType+attributes, scales = "free_y")
ggsave(
  paste0(pathprogram, 
  "figure/EstimationMemo/NetAssetsEffects.jpg"),
  p,
  width = 13*2, height = 6*2, units = "cm",
  dpi = 600
 )
pdf(
  paste0(pathprogram, 
  "figure/EstimationMemo/NetAssetsEffects.pdf")
  , width = 2*12/2.54, height = 2*5/2.54)
print(p)
whatever <- dev.off()

#### figure BroadNet etc, lines 2759-2818    {r figure BroadNet etc, warning = F, message = F, fig.align='center', fig.height = 12, fig.width = 10}    eval = T
library(ggplot2)
confi <- qread(paste0(pathsaveHere, "EstimatesCI.qs"))
confi1 <- confi[
  grepl("Annual|^NetB|NetNL|ws$", FileName) & 
  !grepl("None|Adi|Own", FileName) & 
  !grepl("Tr|Up", attributes) &
  grepl("^periN", hv) &
  grepl("T$", regtype) & num > 1, ]
confi1[grepl("^Large$", attributes), attributes := "Large"]
cols <- c("FileName", "regressand", "attributes", "regtype")
confi1[, (cols) := droplevels(.SD), .SDcols = cols]
levels(confi1$attributes)[levels(confi1$attributes) == "LargeGrace"] <- "Large grace"
confi1[, regressand := factor(regressand, levels = 
  c("net assets, AP", "net non-livestock assets", "net broad assets", "cattle"))]
confi1[, regressand := factor(regressand, labels = 
  c("Net assets,\nannual price (BDT)", "Net non-livestock\n assets (BDT)", 
  "Net broad assets\n (BDT)", "Cattle (counts)"))]
table(confi1[,.(attributes, regressand)])
confi1[, num := factor(num-1)]
p <- ggplot(data = confi1
  , aes(x = factor(period), y = estimate, 
      colour = num, shape = num, group = num)) + 
  geom_pointrange(aes(
    ymin = lb, ymax = ub), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .5))
p <- p + facet_grid(regressand ~ attributes, scales = "free_y") + 
  scale_y_continuous(name = "impacts" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 2:4) +
  theme(
   axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 1), 
   axis.text.y = element_text(size = 8), 
   axis.title = element_text(size = 8), 
   strip.text.x = element_text(color = "blue", size = 9, 
     margin = margin(.1, 1.25, .1, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 9, 
     margin = margin(1.5, .1, 1.5, .1, "cm")),
   #panel.spacing.x = unit(c(.1, .1, .3, .1), units = "cm"),
   #panel.spacing.y = unit(.1, units = "cm"),
   legend.position="bottom") + 
  xlab("periods") + 
  labs(color  = "regression specifications", shape = "regression specifications") +
  guides(colour = guide_legend(title = "regression specifications", nrow = 1)) +
  geom_hline(aes(yintercept = 0), data = confi1, colour = "lightgreen")
ggsave(
  paste0(pathprogram, 
  "figure/EstimationMemo/NetAssetsNetBroadAssetsNLAssetsCattleEffects.jpg"),
  p,
  width = 13*1.5, height = 12*1.5, units = "cm",
  dpi = 300
 )
pdf(
  paste0(pathprogram, 
  "figure/EstimationMemo/NetAssetsNetBroadAssetsNLAssetsCattleEffects.pdf")
  , width = 2*12/2.54, height = 2*8/2.54)
print(p)
whatever <- dev.off()

#### plot NetAssetsEffects, lines 2823-2825    {r plot NetAssetsEffects, echo = F, fig.fullwidth=F, fig.width = 8}    eval = T
knitr::include_graphics("figure/EstimationMemo/NetAssetsEffects.jpg")

#### plot NetAssetsNetBroadAssetsNLAssetsCattleEffects., lines 2831-2833    {r plot NetAssetsNetBroadAssetsNLAssetsCattleEffects., echo = F, fig.fullwidth=F, fig.width = 8}    eval = T
knitr::include_graphics("figure/EstimationMemo/NetAssetsNetBroadAssetsNLAssetsCattleEffects.jpg")

#### figure sch arms concurrent, lines 2845-2912    {r figure sch arms concurrent, eval = T, warning = F, message = F, fig.align='center', fig.height = 12, fig.width = 14, fig.cap = paste0("Effects on schooling by arms per period", "\\\\ {\\footnotesize \\setlength{\\baselineskip}{8pt}}"), fig.lp = 'Figure '}    eval = T
library(ggplot2)
confis <- qread(paste0(pathsaveHere, "EstimatesCISchooling.qs"))
confi2 <- unique(confis)
confi2 <- confi2[!grepl("^Up", attributes), ]
confi2 <- confi2[grepl("^Large$", attributes), attributes := "Large/Upfront"]
confi2 <- confi2[grepl("T$", regtype), ]
cols <- c("FileName", "school", "attributes")
confi2[, (cols) := droplevels(.SD), .SDcols = cols]
confi2[, attributes := factor(attributes, levels = 
  c("Traditional", "Large/Upfront", "LargeGrace", "Cattle"))]
confi2[, attributes := factor(attributes, labels =
  c("Traditional", "Large", "Large grace", "Cattle"))]
#### Trad: Level in each period at school i
confi2t <- confi2[(grepl("^Tra", attributes) & grepl("lev.* trad.*ea", ImpactType)) & num >= 5, ]
#### Nontrad: nontrad - trad, in each period, at school i
confi2n <- confi2[grepl("change", ImpactType) & num >= 5, ][order(gender, period), ]
confi3 <- rbind(confi2t, confi2n)
confi3[, num := factor(num-4)]
faccol <- c("attributes", "gender", "ImpactType", "hv", "regtype", "num")
confi3[, (faccol) := lapply(.SD, droplevels), .SDcol = faccol]
table(confi3[,.(ImpactType, attributes)])
confi3[, numgen := paste0(gender, ", spec", num)]
confi3[, effects := "Level estimates"]
confi3[!grepl("Trad", attributes), effects := "Marginal impacts relative to Traditional"]
p <- ggplot(data = confi3
  , aes(x = factor(period), y = estimate, 
      colour = numgen, shape = numgen, group = numgen)) + 
  geom_pointrange(aes(
    ymin = lb, ymax = ub), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .25))
p <- p + facet_grid(school ~ attributes, scales = "free_y") + 
  scale_y_continuous(name = "impacts" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 2:4) +
  theme(
   axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 1), 
   axis.text.y = element_text(size = 7), 
   axis.title = element_text(size = 7), 
   strip.text.x = element_text(color = "blue", size = 9, 
     margin = margin(.1, 1.25, .1, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 9, 
     margin = margin(1.5, .1, 1.5, .1, "cm")),
   panel.spacing.x = unit(c(.3, .1, .1), units = "cm"),
   panel.spacing.y = unit(.1, units = "cm"),
   legend.position="bottom",
   legend.key.size = unit(0.1, "cm"),
   legend.text = element_text(size = 7)) + 
  xlab("periods") + 
  labs(color  = "gender, regression specifications", shape = "gender, regression specifications") +
  guides(colour = guide_legend(title = "gender, regression specifications", nrow = 1)) +
  geom_hline(aes(yintercept = 0), data = confi3, colour = "lightgreen")
p <- p + ggh4x::facet_nested(school ~ effects + attributes, scales = "free_y")
ggsave(
  paste0(pathprogram, 
  "figure/EstimationMemo/SchoolingEffectsConcurrentWithTradByArm.jpg"),
  p,
  width = 14*2, height = 12*2, units = "cm",
  dpi = 450
 )
pdf(
  paste0(pathprogram, 
  "figure/EstimationMemo/SchoolingEffectsConcurrentWithTradByArm.pdf"),
  , width = 12*2/2.54, height = 12*2/2.54)
print(p)
whatever <- dev.off()

#### figure sch func attributes concurrent, lines 2913-2980    {r figure sch func attributes concurrent, eval = T, warning = F, message = F, fig.align='center', fig.height = 12, fig.width = 14, fig.cap = paste0("Effects on schooling by functional attributes per period", "\\\\ {\\footnotesize \\setlength{\\baselineskip}{8pt}}"), fig.lp = 'Figure '}    eval = T
library(ggplot2)
confis <- qread(paste0(pathsaveHere, "EstimatesCISchooling.qs"))
confi2 <- unique(confis)
confi2 <- confi2[grepl("^Large$", attributes), attributes := "Large/Upfront"]
confi2 <- confi2[grepl("Ta$", regtype), ]
cols <- c("FileName", "school", "attributes")
confi2[, (cols) := droplevels(.SD), .SDcols = cols]
confi2[, attributes := factor(attributes, levels = 
  c("Traditional", "Upfront", "WithGrace", "InKind"))]
#### Trad: Level in each period at school i
confi2t <- confi2[(grepl("^Tra", attributes) & grepl("lev.* trad.*ea", ImpactType)) & num >= 5, ]
#### Nontrad: nontrad - trad, in each period, at school i
confi2n <- confi2[grepl("change", ImpactType) & num >= 5, ][order(gender, period), ]
confi3 <- rbind(confi2t, confi2n)
confi3[, num := factor(num-4)]
faccol <- c("attributes", "gender", "ImpactType", "hv", "regtype", "num")
confi3[, (faccol) := lapply(.SD, droplevels), .SDcol = faccol]
table(confi3[,.(ImpactType, attributes)])
confi3[, numgen := paste0(gender, ", spec", num)]
confi3[, effects := "Level estimates"]
confi3[!grepl("Trad", attributes), effects := "Marginal impacts relative to \nTraditional, Upfront, Upfront+WithGrace"]
confi3[, attributes := factor(attributes, label = c("Traditional", 
  "Upfront\nrelative to Traditional", "WithGrace\nrelative to Upfront", 
  "InKind\nrelative to Upfront+WithGrace"))]
p <- ggplot(data = confi3
  , aes(x = factor(period), y = estimate, 
      colour = numgen, shape = numgen, group = numgen)) + 
  geom_pointrange(aes(
    ymin = lb, ymax = ub), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .25))
p <- p + facet_grid(school * gender ~ attributes, scales = "free_y") + 
  scale_y_continuous(name = "impacts" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 2:4) +
  theme(
   axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 1), 
   axis.text.y = element_text(size = 7), 
   axis.title = element_text(size = 7), 
   strip.text.x = element_text(color = "blue", size = 8, 
     margin = margin(.1, 1.25, .1, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 9, 
     margin = margin(1.5, .1, 1.5, .1, "cm")),
   panel.spacing.x = unit(c(.3, .1, .1), units = "cm"),
   panel.spacing.y = unit(.1, units = "cm"),
   legend.position="bottom",
   legend.key.size = unit(0.1, "cm"),
   legend.text = element_text(size = 7)) + 
  xlab("periods") + 
  labs(color  = "regression specifications", shape = "regression specifications") +
  guides(colour = guide_legend(title = "regression specifications", nrow = 1)) +
  geom_hline(aes(yintercept = 0), data = confi3, colour = "lightgreen")
p <- p + ggh4x::facet_nested(school ~ effects + attributes, scales = "free_y")
ggsave(
  paste0(pathprogram, 
  "figure/EstimationMemo/SchoolingEffectsConcurrentWithTradByFunAttribute.jpg"),
  p,
  width = 14*2, height = 12*2, units = "cm",
  dpi = 450
 )
pdf(
  paste0(pathprogram, 
  "figure/EstimationMemo/SchoolingEffectsConcurrentWithTradByFunAttribute.pdf"),
  , width = 12*2/2.54, height = 10*2/2.54)
print(p)
whatever <- dev.off()

#### figure SchoolingEffectsWithTradByArm, lines 2985-2988    {r figure SchoolingEffectsWithTradByArm, echo = F, fig.fullwidth=F, fig.width = 8}    eval = T
knitr::include_graphics(
  "figure/EstimationMemo/SchoolingEffectsConcurrentWithTradByArm.jpg")

#### SchoolingEffectsWithTradByFunAttribute, lines 2994-2996    {r SchoolingEffectsWithTradByFunAttribute, echo = F, fig.fullwidth=F, fig.width = 8}    eval = T
knitr::include_graphics("figure/EstimationMemo/SchoolingEffectsConcurrentWithTradByFunAttribute.jpg")

#### figure income consumption, lines 3004-3073    {r figure income consumption, warning = F, message = F, fig.align='center', fig.height = 8, fig.width = 10, fig.cap = paste0("Effects on labour incomes and consumption", "\\\\ {\\footnotesize \\setlength{\\baselineskip}{8pt}}"), fig.lp = 'Figure '}    eval = T
library(ggplot2)
confi <- qread(paste0(pathsaveHere, "EstimatesCI.qs"))
confi2 <- confi[
  (grepl("Lab", FileName) |(grepl("OLS", FileName) & num <= 3)) &
  #grepl("^Ta?$", regtype) & For Arm, Fun Attribute panels
  grepl("^T$", regtype) & 
  # traditional gross level, nontrad cumulative relative to trad gross
  ((grepl("Tra", attributes) & grepl("lev.* trad.*d$", ImpactType)) |
    (!grepl("Tra", attributes) & grepl("sum", ImpactType))), ]
confi2[grepl("^Large$", attributes), attributes := "Large/Upfront"]
confi2[grepl("Fun", AtType), 
  AtType := "Functional attributes\n (relative to Upfront, Upfront+WithGrace)"]
confi2 <- confi2[!grepl("^Up", attributes), ]
confi2[, attributes := factor(attributes, levels =
  c("Traditional", "Large/Upfront", "LargeGrace", "Cattle",
    "WithGrace", "InKind"),
  labels = c("Traditional", "Large/Upfront", "Large grace", "Cattle",
    "WithGrace", "InKind"))]
confi3 <- confi2[!grepl("Tra", attributes), ]
cols <- c("FileName", "regressand", "attributes", "hv", "ImpactType", "AtType")
confi3[, (cols) := droplevels(.SD), .SDcols = cols]
confi3[grepl("lab", regressand), num := num-1]
confi3[, num := factor(num)]
#confi2[grepl("Lab", FileName), estimate := estimate*100]
#confi2[grepl("Lab", FileName), ub := ub*100]
#confi2[grepl("Lab", FileName), lb := lb*100]
p <- ggplot(data = confi3
  , aes(x = factor(period), y = estimate, 
      colour = num, shape = num, group = num)) + 
  geom_pointrange(aes(
    ymin = lb, ymax = ub), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .5))
p <- p + facet_grid(regressand ~ attributes, scales = "free_y",
   labeller = label_wrap_gen(multi_line = TRUE)) + 
  scale_y_continuous(name = "impacts" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 2:4) +
  theme(
   axis.text.x = element_text(size = 5, angle = 0, vjust = 1, hjust = 1), 
   axis.text.y = element_text(size = 6), 
   axis.title = element_text(size = 6), 
   strip.text.x = element_text(color = "blue", size = 9, 
     margin = margin(.1, 1.25, .1, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 9, 
     margin = margin(1.5, .1, 1.5, .1, "cm")),
   #panel.spacing.x = unit(c(.1, .1, .3, .1), units = "cm"), For Arm, Fun Attribute panels
   panel.spacing.x = unit(c(.1, .1), units = "cm"),
   panel.spacing.y = unit(.1, units = "cm"),
   legend.position="bottom") + 
  xlab("periods") + 
  labs(color  = "regression specifications", shape = "regression specifications") +
  guides(colour = guide_legend(title = "regression specifications", nrow = 1)) +
  geom_hline(aes(yintercept = 0), data = confi3, colour = "lightgreen")
#p <- p + ggh4x::facet_nested(regressand ~ AtType+attributes, scales = "free_y")
ggsave(
  paste0(pathprogram, 
  "figure/EstimationMemo/IncomeConsumptionEffects.jpg"),
  p,
  width = 13*2, height = 8*2, units = "cm",
  dpi = 300*4
 )
pdf(
  paste0(pathprogram, 
  "figure/EstimationMemo/IncomeConsumptionEffects.pdf"),
  , width = 13*2/2.54, height = 8*2/2.54)
print(p)
whatever <- dev.off()

#### plot IncomeConsumptionEffects, lines 3078-3080    {r plot IncomeConsumptionEffects, echo = F, fig.fullwidth=F, fig.width = 8}    eval = T
knitr::include_graphics("figure/EstimationMemo/IncomeConsumptionEffects.jpg")

#### figure NetAssets by experience relative to concurrent trad, lines 3086-3146    {r figure NetAssets by experience relative to concurrent trad, warning = F, message = F, fig.align='center', fig.height = 12, fig.width = 8, fig.lp = 'Figure '}    eval = T
library(ggplot2)
confi <- qread(paste0(pathsaveHere, "EstimatesCI.qs"))
cie1 <- confi[
  grepl("^NetA.*ts", FileName) & grepl("^non.*-.*\\,", ImpactType) & 
  grepl("Adi|None|Own", regressand) & 
  grepl("^T$", regtype), ]
cols <- c("FileName", "regressand", "attributes", "ImpactType")
cie1[, (cols) := droplevels(.SD), .SDcols = cols]
levels(cie1$attributes)[levels(cie1$attributes) == "LargeGrace"] <- "Large grace"
cie1[, SubGroup := factor(gsub(".*ts\\, ", "", regressand))]
cie1[, SubGroup := factor(SubGroup, levels = c("Own", "Adi", "None"))]
cie1[, SubGroup := factor(SubGroup, labels = c("Owner", "Adi", "None"))]
cie1[, num := factor(num-1)]
cie1[, num := gsub("^", "Spec ", num)]
p <- ggplot(data = cie1
  , aes(x = factor(period), y = estimate, 
      colour = SubGroup, shape = SubGroup, group = SubGroup), size = .1) + 
  geom_pointrange(aes(
    ymin = lb, ymax = ub), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .5))
p <- p + facet_grid(num ~ attributes) + 
  scale_y_continuous(name = "impacts" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 2:4) +
  theme(
   axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 1), 
   axis.text.y = element_text(size = 8), 
   axis.title = element_text(size = 8), 
   strip.text.x = element_text(color = "blue", size = 9, 
     margin = margin(.1, 1.25, .1, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 9, 
     margin = margin(1.5, .1, 1.5, .1, "cm")),
   #panel.spacing.x = unit(c(.1, .1, .3, .1), units = "cm"),
   #panel.spacing.y = unit(.1, units = "cm"),
   legend.text = element_text(size = 7),
   legend.title = element_text(size = 7),
   legend.key = element_rect(fill = "white"),
   legend.key.size = unit(.25, "cm"),
   legend.position="bottom") + 
  xlab("periods") + 
  labs(color  = "Group by experience", shape = "Group by experience") +
  guides(
    colour = guide_legend(title = "Group by experience", nrow = 1),
    shape = guide_legend(override.aes = list(size = .125))) +
  geom_hline(aes(yintercept = 0), data = cie1, colour = "lightgreen")
ggsave(
  paste0(pathprogram, 
  "figure/EstimationMemo/NetAssetsByExperienceEffects.jpg")
  , p,
  width = 12*2, height = 6*2, units = "cm",
  dpi = 300*4
 )
pdf(
  paste0(pathprogram, 
  "figure/EstimationMemo/NetAssetsByExperienceEffects.pdf")
  , width = 2*12/2.54, height = 2*6/2.54)
print(p)
whatever <- dev.off()

#### plot NetAssetsByExperienceEffects, lines 3151-3154    {r plot NetAssetsByExperienceEffects, echo = F, fig.fullwidth=F, fig.width = 8}    eval = T
knitr::include_graphics(
  "figure/EstimationMemo/NetAssetsByExperienceEffects.jpg")

#### figure NumCows by experience relative to concurrent trad, lines 3159-3226    {r figure NumCows by experience relative to concurrent trad, warning = F, message = F, fig.align='center', fig.height = 12, fig.width = 8, fig.lp = 'Figure '}    eval = T
library(ggplot2)
confi <- qread(paste0(pathsaveHere, "EstimatesCI.qs"))
confie1 <- confi[
  grepl("^Num", FileName) & 
  !grepl("Exp", FileName) & 
  grepl("^periN", hv) & 
  grepl("^T$", regtype) & 
  !grepl("Trad", attributes), ]
cols <- c("FileName", "regressand", "attributes")
confie1[, (cols) := droplevels(.SD), .SDcols = cols]
levels(confie1$attributes)[levels(confie1$attributes) == "LargeGrace"] <- "Large grace"
confie1[, SubGroup := factor(gsub(".*tle\\, ", "", regressand))]
confie1[, SubGroup := factor(SubGroup, levels = c("cattle", "Own", "Adi", "None"))]
confie1[, SubGroup := factor(SubGroup, labels = c("All members", "Owner", "Adi", "None"))]
addmargins(table(confie1[, .(SubGroup, num)]))
#### Comparable: 
####  NumCows (All) 1 = 1 in all others (not included in confi)
####  NumCows (All) 2 = 2 in Own, 
####  NumCows (All) 3 = 3 in Own, 3 in Adi, None (OLS, so not strictly comparable)
####  NumCows (All) 4: does not exist in Own, Adi, None 
confie2 <- confie1[grepl("1", num), ]
p <- ggplot(data = confie2
  , aes(x = factor(period), y = estimate, 
      colour = SubGroup, shape = SubGroup, group = SubGroup), size = .1) + 
  geom_pointrange(aes(
    ymin = lb, ymax = ub), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .5))
p <- p + facet_grid( ~ attributes) + 
  scale_y_continuous(name = "impacts" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 2:4) +
  theme(
   axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust = 1), 
   axis.text.y = element_text(size = 8), 
   axis.title = element_text(size = 8), 
   strip.text.x = element_text(color = "blue", size = 9, 
     margin = margin(.1, 1.25, .1, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 9, 
     margin = margin(1.5, .1, 1.5, .1, "cm")),
   #panel.spacing.x = unit(c(.1, .1, .3, .1), units = "cm"),
   #panel.spacing.y = unit(.1, units = "cm"),
   legend.text = element_text(size = 7),
   legend.title = element_text(size = 7),
   legend.key = element_rect(fill = "white"),
   legend.key.size = unit(.25, "cm"),
   legend.position="bottom") + 
  xlab("periods") + 
  labs(color  = "Group by experience", shape = "Group by experience") +
  guides(
    colour = guide_legend(title = "Group by experience", nrow = 1),
    shape = guide_legend(override.aes = list(size = .125))) +
  geom_hline(aes(yintercept = 0), data = confie2, colour = "lightgreen")
ggsave(
  paste0(pathprogram, 
  "figure/EstimationMemo/NumCowsByExperienceEffects.jpg")
  , p,
  width = 12*2, height = 4*2, units = "cm",
  dpi = 300
 )
pdf(
  paste0(pathprogram, 
  "figure/EstimationMemo/NumCowsByExperienceEffects.pdf")
  , width = 2*12/2.54, height = 2*4/2.54)
print(p)
whatever <- dev.off()

#### plot NumCowsByExperienceEffects, lines 3231-3234    {r plot NumCowsByExperienceEffects, echo = F, fig.fullwidth=F, fig.width = 8}    eval = T
knitr::include_graphics(
  "figure/EstimationMemo/NumCowsByExperienceEffects.jpg")

#### figure poverty consumption, lines 3240-3295    {r figure poverty consumption, warning = F, message = F, fig.align='center', fig.height = 8, fig.width = 10, fig.cap = paste0("Effects on labour incomes and consumption", "\\\\ {\\footnotesize \\setlength{\\baselineskip}{8pt}}"), fig.lp = 'Figure '}    eval = T
library(ggplot2)
confi <- qread(paste0(pathsaveHere, "EstimatesCI.qs"))
confi2 <- confi[
  (grepl("Lab", FileName) |(grepl("OLS", FileName) & num <= 3)) &
  grepl("^TP$", regtype) & grepl("sum.*poor non", ImpactType), ]
confi2[grepl("on", regressand), regressand := "Per capita consumption\n(BDT)"]
confi2[!grepl("on", regressand), regressand := "Labour income\n(BDT)"]
cols <- c("FileName", "regressand", "attributes", "hv", "ImpactType", "AtType")
confi2[, (cols) := droplevels(.SD), .SDcols = cols]
levels(confi2$attributes)[levels(confi2$attributes) == "LargeGrace"] <- "Large grace"
confi2[grepl("lab", regressand), num := num-1]
confi2[, num := factor(num)]
p <- ggplot(data = confi2
  , aes(x = factor(period), y = estimate, 
      colour = num, shape = num, group = num)) + 
  geom_pointrange(aes(
    ymin = lb, ymax = ub), 
    stat = "identity", fatten = 1.75, 
    position = position_dodge(width = .5))
p <- p + facet_grid(regressand ~ attributes, scales = "free_y",
   labeller = label_wrap_gen(multi_line = TRUE)) + 
  scale_y_continuous(name = "impacts" #,limits = c(-.35, .15)
  ) +
  scale_x_discrete(name = "periods", breaks = 2:4) +
  theme(
   axis.text.x = element_text(size = 5, angle = 0, vjust = 1, hjust = 1), 
   axis.text.y = element_text(size = 6), 
   axis.title = element_text(size = 6), 
   strip.text.x = element_text(color = "blue", size = 9, 
     margin = margin(.1, 1.25, .1, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 9, 
     margin = margin(1.5, .1, 1.5, .1, "cm")),
   #panel.spacing.x = unit(c(.1, .1, .3, .1), units = "cm"), For Arm, Fun Attribute panels
   panel.spacing.x = unit(c(.1, .1), units = "cm"),
   panel.spacing.y = unit(.1, units = "cm"),
   legend.position="bottom") + 
  xlab("periods") + 
  labs(color  = "regression specifications", shape = "regression specifications") +
  guides(colour = guide_legend(title = "regression specifications", nrow = 1)) +
  geom_hline(aes(yintercept = 0), data = confi2, colour = "lightgreen")
#p <- p + ggh4x::facet_nested(regressand ~ AtType+attributes, scales = "free_y")
ggsave(
  paste0(pathprogram, 
  "figure/EstimationMemo/IncomeConsumptionPovertyEffects.jpg"),
  p,
  width = 13*2, height = 6*2, units = "cm",
  dpi = 300
 )
pdf(
  paste0(pathprogram, 
  "figure/EstimationMemo/IncomeConsumptionPovertyEffects.pdf"),
  , width = 13*2/2.54, height = 6*2/2.54)
print(p)
whatever <- dev.off()

#### plot IncomeConsumptionPovertyEffects, lines 3300-3303    {r plot IncomeConsumptionPovertyEffects, echo = F, fig.fullwidth=F, fig.width = 8}    eval = T
knitr::include_graphics(
  "figure/EstimationMemo/IncomeConsumptionPovertyEffects.jpg")

