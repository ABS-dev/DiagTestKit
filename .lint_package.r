################################################################################
############ Utilities to check the code for style and other issues ############
################################################################################

library(data.table)
library(stringr)

# Lint the package to check for formatting errors
# Path should be the root of the package directory.
path <- getwd()
linter <- N <- NULL

# Variables that don't follow stylistic guidelines, which we are not changing.
okay_vars <- c("H", "N", "X", "Sn", "Sp", "SnE", "SnR", "SpE", "SpR", "Prev",
               "Xneg", "Xpos", "Xsus", "CP.Sn", "CP.Sp", "N.vec", "N_mat",
               "cellN", "cellP", "p.neg", "p.pos", "Sn.fig", "Sn.ref", "Sp.fig",
               "Sp.ref", "est.Sn", "iter.n", "Sn.sims", "SnR.vec", "Sp.sims",
               "SpR.vec", "X.short", "calcVal", "NEWinput", "Sn.distn",
               "Sp.distn", "infal_Sn", "infal_Sp", "n.states", "newAlpha",
               "prev.pop", "prev.vec", "sus.perc", "Sn.spread", "Sp.spread",
               "count.vec", "detailOut", "pop.names", "prev.sims", "step.size",
               "NEWcalcVal", "blood_SnSp", "prev.distn", "sens.final",
               "spec.final", "test.names", "updatedOut", "SnR.current",
               "SpR.current", "blood2_SnSp", "current.con", "current.fit",
               "prev.spread", "updateAlpha", "NEWdetailOut", "cloppearSnSp",
               "current.ests", "estimateSnSp", "prev.current", "blood2_a_SnSp",
               "ex1_detailOut", "ex2_detailOut", "ex3_detailOut",
               "ex4_detailOut", "ex5_detailOut", "ex6_detailOut",
               "ex7_detailOut", "ex8_detailOut", "message.current",
               "estimateSnSpControl")

## Change This Value ##
# Determine whose files should be linted.  Change the number in the [.]

excluded_files <- included_files <- NULL

idx <- 1
excluded_linters <- c("indentation_linter",
                      "cyclocomp_linter",
                      "commented_code_linter",
                      "object_usage_linter"
                      )[idx]

# Run lintr on the selected files.
tmp <- codeDiagnostics::lint_package_extended(
  path = path,
  okay_vars = okay_vars,
  excluded_files = excluded_files,
  included_files = included_files,
  excluded_linters = excluded_linters)

# Send results to Markers screen
if (length(tmp) == 0) {
  message("Sucess!  No lintr issues!")
} else {
  tmp
}

## Run to this point to see lint results.
# CTRL-SHIFT-B

# Format results as a data.table for further exploration
dt <- data.table::as.data.table(tmp)
dt[, .N, linter][order(N)]
dt[, .N, filename]
dt[, .N, keyby = .(filename, linter)]


tmp[dt[, which(grepl("name", linter))]]



lf <- paste0("(", paste0(lindsay_files, collapse = "|"), ")")
dt[!str_detect(filename, lf), .N, filename]
tmp[!dt[, str_detect(filename, lf)]]


?assign


tmp <- dt[str_detect(linter, "name"),
          str_extract(trimws(line), "^[^ ]*"), line][, .N, keyby = V1]
cat(tmp[, paste0("\"", V1, "\"", collapse = ", ")])
