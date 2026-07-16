#### ============================================================
#### example_usage.R -- replaying EstimationGUK_Tufte.Rmd in the R GUI
#### folder: C:/data/GUK/analysis/.claude/.scratch/extract_chunks/
#### paste step by step; nothing below runs the pipeline until step 3
#### ============================================================

#### 1. prerequisites: paths, setwd, libraries, helper functions
source("C:/data/GUK/analysis/.claude/.scratch/extract_chunks/guk_preamble.R")

#### 2. load the runchunks() helper (defines the function only;
####    runs no pipeline code)
source("C:/data/GUK/analysis/.claude/.scratch/extract_chunks/runchunks.R")

#### 3. replay slices of the linearised document.
####    Targets are matched by SUBSTRING against the #### headers in
####    EstimationGUK_Tufte_chunks.R -- open it to see chunk names.
####    eval = F entries (dead chunks, inert .rnw children) are skipped.

## everything from the top up to (NOT including) the confi construction:
runchunks(to = "construct confi")

## a middle slice: from "set parameters" up to "linhyp rexlog init":
# runchunks(to = "linhyp rexlog init", from = "set parameters")

## everything up to a child document (children before it will run):
# runchunks(to = "subsection_Incomes.rmd")

## the whole pipeline, top to end (several minutes):
# runchunks()

#### 4. after runchunks(), all objects (confi, s1, arA, ...) sit in the
####    global workspace for inspection:
# dim(confi); confi[hv == "matP"][1:5]

#### 5. regenerate after editing the .Rmd (run in a terminal, not R):
## /mnt/c/seiro/languages/R/R-4.4.1/bin/Rscript.exe \
##   'C:/seiro/languages/claude/.claude/extract_chunks.R' \
##   'C:/data/GUK/analysis/program/EstimationGUK_Tufte.Rmd' \
##   'C:/data/GUK/analysis/.claude/.scratch/extract_chunks/EstimationGUK_Tufte_chunks.R' \
##   'C:/data/GUK/analysis/.claude/.scratch/extract_chunks/guk_preamble.R'
