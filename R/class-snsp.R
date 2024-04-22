#' @title snsp-class
#' @export
snsp <- setRefClass(
  "snsp",
  fields = list(calcVal = "list", detailOut = "list", input = "list"),
  methods = list(
    show = function() {
      cat(paste(calcVal$Nsim, " simulations \n"))
      cat(paste(calcVal$Confidence * 100, "% Interval Estimates\n\n"))

      summary <- data.frame("Point Estimate" = c(calcVal$SnPE,
                                                 calcVal$SpPE),
                            "Lower" = c(calcVal$SnInterval[1],
                                        calcVal$SpInterval[1]),
                            "Upper" = c(calcVal$SnInterval[2],
                                        calcVal$SpInterval[2]))
      if (length(calcVal) > 6) {
        summary <- rbind(summary,
                         c(calcVal$SusDisPosPE,
                           as.vector(calcVal$SusDisPosInterval)),
                         c(calcVal$SusDisNegPE,
                           as.vector(calcVal$SusDisNegInterval)))
        rownames(summary) <- c("Sn = P(T+|D+)", "Sp = P(T-|D-)",
                               "P(T?|D+)", "P(T?|D-)")
      } else {
        rownames(summary) <- c("Sn = P(T+|D+)", "Sp = P(T-|D-)")
      }

      print(summary)
    }
  ))
