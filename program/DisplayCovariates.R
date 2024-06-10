# display covariates
jay2 <- length(Regressands)
GroupVar <- rep("^hhid$", jay2)
# list of length = jay2
ListCovariates <- vector(mode = "list", length = jay2)
# replicate this list by length(listheader): list[[1:length(listheader)]][[1:jay2]]
#   listheader: regression types (base, grace, poverty, attributes, ...)
#   jay2: number of specifications in each regression type
ListCovariates <- replicate(length(listheader), ListCovariates, simplify = FALSE)
# use functions in GetCovariatesFunctions.R
ListCovariates <- GetCovariates(listheader, ListCovariates, Jay = jay2) 
ListCovariates <- AttachSymbolsToLC(ListCovariates, bracket = "[", CutBy = 3)
# If asset regressions, select only HHAssets regresssions (PAssets regressions have same specifications)
if (any(grepl("^as", listheader))) 
  ListCovariates <- lapply(ListCovariates, function(x) x[1:3])
ListCovariates <- lapply(ListCovariates, function(x) paste(x, collapse = "\n"))
cat(unlist(ListCovariates), "\n\n")
