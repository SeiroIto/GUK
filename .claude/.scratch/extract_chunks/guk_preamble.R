#### GUK prerequisites (mirrors knit_guk.R render harness)
source("c:/seiro/settings/Rsetting/functions.R")   # grepout() etc.
path0 <- "c:/data/GUK/"
path  <- paste0(path0, "analysis/")
setwd(path)
library(rmarkdown)
library(knitr)
