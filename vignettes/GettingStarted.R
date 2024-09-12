## ----setup, include=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE, tidy = FALSE)
require(plyr)
options(width = 50)


## ----echo=TRUE, warning=FALSE, message=FALSE----
library(ggplot2)
library(DiagTestKit)
data("ExampleData")
dat <- dat_dichot
infallible <- dat_infal

## ----echo=TRUE,eval=T---------------------------
#Blood Samples
blood <- ddply(.data = subset(dat, specimen == "wholeblood"),
                  .variables = .(visual_read, ref_result),
                  .drop = FALSE,
                  .fun = summarize,
                  Count = length(deviceID))

## ---- echo=T,eval=T-----------------------------
names(blood) <- c("Exp", "Ref1", "Count")

## ---- echo=T,eval=T-----------------------------
blood2 <- data.frame(table(Exp = dat$visual_read[dat$specimen == "wholeblood"],
                          Ref1 = dat$ref_result[dat$specimen == "wholeblood"]))

## ----echo=TRUE,eval=T,results="hide"------------
blood_SnSp <- estimateSnSp(dat = blood,
                           Sn.ref = data.frame(Ref1 = c(0.95, 0)),
                           Sp.ref = data.frame(Ref1 = c(0.98, 0)),
                           prev.pop = c(A = 0.70))

## ----echo=T,eval=T------------------------------
blood_SnSp


## ----echo=TRUE,eval=T---------------------------
blood2_SnSp <- estimateSnSp(dat = blood2,
                           Sn.ref = c(Ref1 = 0.95),
                           Sp.ref = c(Ref1 = 0.98),
                           prev.pop = c(A = 0.70),
                           control = estimateSnSpControl(iter.n = 250))
blood2_SnSp

## ---- echo=T,eval=T-----------------------------
blood_SnSp$input$seed
blood2_SnSp$input$seed

## ---- echo=T, eval=T----------------------------
blood2_a_SnSp <-
  estimateSnSp(dat = blood2,
      Sn.ref = c(Ref1 = 0.95),
      Sp.ref = c(Ref1 = 0.98),
      prev.pop = c(A = 0.70),
      control = estimateSnSpControl(seed = blood_SnSp$input$seed,
                                      rep.iter = FALSE))
blood2_a_SnSp

## ---- echo=T,eval=T-----------------------------
unique(blood_SnSp$detailOut$Converge)
unique(blood_SnSp$detailOut$Message)

## ---- echo=F,eval=T-----------------------------
Sn.fig <- ggplot() +
          geom_histogram(aes(blood_SnSp$detailOut$Exp.Sn), binwidth = 0.005) +
          labs(x = "Experimental Test Sensitivity Estimates", y = "Count")
Sn.fig


## ----echo=F,eval=T------------------------------
Sp.fig <- ggplot() +
          geom_histogram(aes(blood_SnSp$detailOut$Exp.Sp), binwidth = 0.005) +
          labs(x = "Experimental Test Specificity Estimates", y = "Count")
Sp.fig

## ---- echo=F,eval=T-----------------------------
Sn.ref <- ggplot() +
          geom_histogram(aes(blood_SnSp$input$Sn.sim[, 1]), binwidth = 0.025) +
          labs(x = "Simulated Sensitivity for Reference", y = "Count")
Sn.ref

## ---- echo=F,eval=T-----------------------------
Sp.ref <- ggplot() +
          geom_histogram(aes(blood_SnSp$input$Sp.sim[, 1]), binwidth = 0.05) +
          labs(x = "Simulated Specificity for Reference", y = "Count")
Sp.ref

## ---- echo=T,eval=T-----------------------------
infal_Sn <- cloppearSnSp(dat = infallible)
infal_Sn

## ---- echo = T, eval = T------------------------
infal_Sp <- cloppearSnSp(dat = infallible,
                         est.Sn = FALSE)
infal_Sp
