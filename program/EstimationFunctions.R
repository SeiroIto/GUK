clx <-function(fm, dfcw = 1, cluster, returnV = F, dfc2 = F, DFC = F,
  Estfun = "sandwich", deviation = F)
#  1 way clustering robust covariance matrix
#  following Arai 2010
#  fm: fitted lm model
#  cluster: 1 column matrix
#  dfcw: degree of freedom adjustment when using deviation data
#   Note: When using the FE estimator with N obs and M groups, 
#   lm uses dof as N-K, but it should be N-M-K so dfcw = (N-K-M)/(N-K).
#   In FD estimator, there is no need of dof correction.
#  dfc2: some suggests this dfc, but not sure if this is correct
#  DFC: User provided dof
#  returnV: if T, aslo returns cluster-robust covariance
#  ESTfun: use estfun provided by "sandwich" package or "seiro"
#  deviation: T if using deviations data, if F, dfcw is set to 1
{
  require(sandwich); require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M / (M - 1)) * ((N - 1) / (N - fm$rank))
  if (dfc2) dfc <- (N -  fm$rank) / (N - M - fm$rank)
  if (DFC) dfc <- DFC
  #  switch estfun according to specified by estfun = "sandwich" (default) or "seiro"
  esfun <- function(fm, ESTfun) switch(ESTfun, sandwich = estfun(fm), seiro = estfun2(fm))
  #u <- apply(estfun2(fm), 2, function(x) tapply(x, cluster, sum))
  # This sums estimating function [resid*x (in FOC of OLS)] by cluster.
  u <- apply(esfun(fm, Estfun), 2, function(x) tapply(x, cluster, sum))
  # There is no attribute called "df": lm$df.residual, glm$residual.
  # But leave it for backward compatibility.
  if (any(grepl("^df$", names(fm))))  
    dfcw <-  fm$df /(fm$df - (M-1)) else
  if (deviation)  
    dfcw <-  fm$df.residual /(fm$df.residual - (M-1))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(u) / N) * dfcw
  #diag(vcovCL)^.5
  if (returnV) list(est = lmtest::coeftest(fm, vcov. = vcovCL), 
      ci = lmtest::coefci(fm, vcov. = vcovCL),
      V = vcovCL, dfc = dfc, dfcw = dfcw, reg = fm, 
      clusters.M = M, cluster = cluster, N = length(fm$fit)) else
  coeftest(fm, vcovCL) 
}
T234 <- function(teedata) 
  as.numeric(crossprod(teedata, 1:3))

FirstDiffPanelData <- function(X, Group, TimeVar = "time", Cluster = NULL, 
  LevelCovariates = NULL, drop.if.NA.in.differencing = T,
  LevelPeriodToKeep = "first", print.messages = T)
# First difference time-varying variables while keep time-invariant variables
# Data preparation:
#   Group is to set ID. It can be more than one variable.
#   TimeVar sets time column. It must be a single column. 
#   Cluster: A factor variable of cluster index will be used for SE. 
#     Group, TimeVar, Cluster are set aside from first differencing.
#   LevelCovariates is set to add as covariates, including factor variables. 
#     Since FD reduces time dimension from T to T-1, 
#     need to merge x[-T, ] or x[-1, ] matrix.
#     LevelPeriodToKeep = "first" uses x[-T, ], "last" uses x[-1, ]
#   drop.if.NA.in.differencing: if T, drops rows with NA in variables other
#     than Group, Time, LevelCovariates (Cluster included) after first differencing.
#  -- Arguments --
#  (UpperCaseArguments specify variables, lower.case.arguments set T/F.)
#  X: data object
#  Group: regexp for the grouping variable (must be a single numeric column)
#  TimeVar: regexp for time index variable.
#  Cluster: regexp for clustering variable, if NULL no clustring of SE.
#  LevelCovariates: regexp for covariates in X to be added as a level (x_{t-1} level for Delta y_{t})
#  drop.if.NA.in.differencing: if T, drops rows with NA.
#  LevelPeriodToKeep: if "first", last period of group is dropped in level matrix. "last" drops the 1st.
#    Time index variable will be kept as dropping 1st period: 1, 2, 3 => 2, 3.
#  print.messages: if T, "dropped X obs due to NA" is printed.
#  -- Outputs --
#  data: Data matrix with differencing
#  original.data: Original data matrix
#  LevelPeriod: "first" or "last"
#  groupname: Group string.
#  dropped.rows: Dropped row numbers in level.data due to NA.
{
  require("data.table")
  X <- data.table(X)
  # en and group index matrix is for recording which are dropped
  # en must not to be differenced.
  X[, en := 1:.N]
  X0 = copy(X)
  if (is.null(LevelCovariates)) 
    LevelCovariates <- paste("^en$", Cluster, sep = "|") else
    LevelCovariates <- paste(LevelCovariates, "^en$", Cluster, sep = "|")
  # FD all columns other than ID, cluster, and levels
  # To do so, find ID, cluster, and levels column location
  # internal index variables
  if (length(Group) > 1) Group <- paste(paste0("^", Group, "$"), collapse = "|")
  groupstring <- grepout(Group, colnames(X))
  if (is.null(Cluster)) {
    clusterstring <- clustercols <- NULL
    # Get rid of "|" at the end when Cluster = NULL
    LevelCovariates <- substr(LevelCovariates, 1, nchar(LevelCovariates)-1)
  } else {
    if (!is.null(Cluster)) 
      clusterstring <- grepout(Cluster, colnames(X)) else
      clusterstring <- NULL
    clustercols <- grep(Cluster, colnames(X))
  }
  timestring <- grepout(TimeVar, colnames(X))
  grouptime <- X[, eval(c(groupstring, timestring)), with = F]
  IDcols <- grep(Group, colnames(X))
  IDstring <- grepout(Group, colnames(X))
  timecol <- grep(TimeVar, colnames(X))
  levelcols <- grep(LevelCovariates, colnames(X))
  levelstring <- grepout(LevelCovariates, colnames(X))
  IDCTLcols <- unique(c(IDcols, timecol, levelcols, clustercols))
  IDCTLstring <- colnames(X)[IDCTLcols]
  # Time-variant variates
  FDThese <- colnames(X)[-IDCTLcols]
  # Shift one period ahead of LevelCovariates and rename as L(original colname)
  # This takes time. Maybe too long.
  FDLVariates <- c(FDThese, levelstring)
  if (LevelPeriodToKeep == "first")
    X[, (paste0("L", FDLVariates)) := shift(.SD, 1L, type = "lag")
      , by = IDstring, .SDcols = FDLVariates] else
    X[, (paste0("L", FDLVariates)) := shift(.SD, 1L, type = "lag")
      , by = IDstring, .SDcols = FDLVariates]
  # Take an FD of time-variant variates, overwrite levels of time-variant variables
  for (i in 1:length(FDThese))
    set(X, j = grep(paste0("^", FDThese[i], "$"), colnames(X)), 
       value = X[[FDThese[i]]] - X[[paste0("L", FDThese[i])]])
  #     X[, (FDThese[i]) := 
  #       eval(parse(text =
  #         paste0(FDThese[i], "-", paste0("L", FDThese[i]))
  #       ))]
  # order by ID and time
  setkeyv(X, names(X)[c(IDcols, timecol)])
  droppedRows <- NULL
  #  drop obs with T < 2
  X[, Tee := .N, by = eval(parse(text=
    paste0("list(", paste(groupstring, collapse = ", "), ")")
    ))]
  table(X[, Tee])
  if (any(X[, Tee] == 1)) {
    if (print.messages) cat("Dropped", nrow(X[Tee == 1, ]), "obs due to T<2.\n")
    droppedRows <- X[Tee == 1, en]
    X <- X[Tee > 1, ]
  }
  X[, Tee := NULL]
  #  logical => numeric
  if (any(iil <- sapply(X, is.logical))) 
    X[, names(iil)[iil] := sapply(eval(parse(text=names(iil)[iil])), as.numeric)]
  X[, N := 1:.N, by = eval(names(X)[IDcols])]
  X[, maxN := .N, by = eval(names(X)[IDcols])]
  #  drop unbalanced portion among FDThese (obs with NAs): This is NAs as shifting 1 period (N) and genuine NAs.
  if (drop.if.NA.in.differencing) {
    if (any(iina <- is.na(rowSums(X[, FDThese, with = F])))) {
      if (print.messages) cat("Dropped", sum(iina), "obs due to NA.\n")
      droppedRows <- list(OnlyOnePeriod = droppedRows, NAInDiff = X[iina, en])
      X <- X[!iina, ]
    }
  } else droppedRows <- list(OnlyOnePeriod = droppedRows, ForNA = NA)
  #  turn cluster as a numeric vector
  if (!is.null(Cluster)) {
    qclusterstring <- quote(list(clusterstring))
    clusterNum <- X[, eval(quote(clusterstring)), with = F]
    clusterNum <- as.numeric(factor(asc(clusterNum)))
  } else clusterNum <- NULL
  # keep IDcols, TimeVar, LevelCovariates, FDThese
  dX <- X[, c(IDCTLstring, FDThese), with = F]
  # Check if NAs due to differencing
  if (any(iina2 <- apply(is.na(dX[, FDThese, with = F]), 1, any))) {
    droppedRows[[2]] <- c(droppedRows[[2]], dX[iina2, en])
    droppedRows[[2]] <- droppedRows[[2]][order(droppedRows[[2]])]
    if (drop.if.NA.in.differencing) {
      dX <- dX[!(iina2), ]
      if (print.messages) cat("Dropped another", sum(iina2), "obs due to NA in differencing.\n")
    }
  }
  list(diff = dX, original.data = X0,
      groupname = groupstring,
      droppedRows = droppedRows, clusterIndex = clusterNum,
      LevelPeriod = LevelPeriodToKeep, FDvars = FDThese)
}


