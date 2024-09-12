#' @title cp-class
#' @export
cp <- setRefClass("cp",
                    fields = list(calcVal = "list", data = 'list', alpha = "numeric"),
                    methods = list(
                      show = function(){
                        if(names(data)[1] == "Test.Positive"){
                          out <- paste('Sn = P(T+|D+): ', round(as.numeric(calcVal$Sn), digits = 6), " (",
                                       (1 - alpha) * 100, '% CI: ', round(as.numeric(calcVal$Sn.LL), digits = 6), ', ',
                                       round(as.numeric(calcVal$Sn.UL), digits = 6), ")", sep = "")
                        } else {
                          out <- paste('Sp = P(T-|D-): ', round(as.numeric(calcVal$Sp), digits = 6), " (",
                                       (1 - alpha) * 100, '% CI: ', round(as.numeric(calcVal$Sp.LL), digits = 6), ', ',
                                       round(as.numeric(calcVal$Sp.UL), digits = 6), ")", sep = "")
                        }


                        print(out)
                      }
                    ))
