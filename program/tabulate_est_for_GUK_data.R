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

