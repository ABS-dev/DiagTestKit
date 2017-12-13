## ----setup, include=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE, tidy=FALSE)
options(width = 50)
library(DiagTestKit)
data('ExampleData')

## ----echo = T, eval = T-------------------------
ex1 <- estimateSnSp(dat = data1,
          Sn.ref = data.frame(ref = c(0.90, 0)),
          Sp.ref = data.frame(ref = c(0.99, 0)),
          prev.pop = c(A = 0.80),
          control = estimateSnSpControl(seed = 64725, rep.iter = F))
unique(ex1$detailOut$Converge)
unique(ex1$detailOut$Message)
ex1


## ---- echo = T, eval = T------------------------
ex1_update <- updateAlpha(ex1, newAlpha = 0.01)
ex1_update

## ---- echo = T, eval = T------------------------
ex2 <- estimateSnSp(dat = data2,
         Sn.ref = c(ref_result=0.90),
         Sp.ref = c(ref_result=0.94),
         prev.pop = c(A = 0.92, B = 0.20, C = 0.50),
         control = estimateSnSpControl(seed=4902342, rep.iter = F))
unique(ex2$detailOut$Converge)
unique(ex2$detailOut$Message)
ex2


## ---- echo = T, eval = T------------------------
ex3 <- estimateSnSp(dat = data3,
        Sn.ref = data.frame(ref = c(0.99, 0.75)),
        Sp.ref = data.frame(ref = c(0.85, 0.67)),
        prev.pop = c(A = 0.92),
        control = estimateSnSpControl(seed = 896421, rep.iter = F))

unique(ex3$detailOut$Converge)
unique(ex3$detailOut$Message)
ex3


## ---- echo = T, eval = T------------------------
ex4 <- estimateSnSp(dat = data4,
        Sn.ref = data.frame(ref = c(0.95, 0.55)),
        Sp.ref = data.frame(ref = c(0.93, 0.48)),
        prev.pop = c(A = 0.97, B = 0.25, C = 0.68),
        control=estimateSnSpControl(seed = 6589732, rep.iter = F))

unique(ex4$detailOut$Converge)
unique(ex4$detailOut$Message)
ex4

## ---- echo = T, eval = T------------------------
ex5 <- estimateSnSp(dat = data5,
         Sn.ref = c(Ref1 = 0.90),
         Sp.ref = c(Ref1 = 0.99),
         prev.pop = c(A = 0.80, B = 0.90),
         control = estimateSnSpControl(seed = 1249856, rep.iter = F))

unique(ex5$detailOut$Converge)
unique(ex5$detailOut$Message)
ex5


## ---- echo = T, eval = T------------------------
ex6 <- estimateSnSp(dat = data6,
          Sn.ref = c(Ref1_result = 0.95, ref2 = 0.91),
          Sp.ref = c(Ref1_result = 0.85, ref2 = 0.98),
          prev.pop = c(A = 0.82),
          control = estimateSnSpControl(seed = 2948217, rep.iter = F))

unique(ex6$detailOut$Converge)
unique(ex6$detailOut$Message)
ex6


## ---- echo = T, eval = T------------------------
ex7 <- estimateSnSp(dat = data7,
          Sn.ref = data.frame(ref1 = c(0.88, 0.75), ref2 = c(0.90 ,0.55)),
          Sp.ref = data.frame(ref1 = c(0.97, 0.6), ref2 = c(0.95, 0.5)),
          prev.pop = c(A = 0.87, B = 0.35),
          control = estimateSnSpControl(seed=1937457, rep.iter = F))

unique(ex7$detailOut$Converge)
unique(ex7$detailOut$Message)
ex7


## ---- echo = T, eval = T------------------------

ex8<-estimateSnSp(dat = data8,
       Sn.ref = c(ref1_result = 0.92, ref2_result = 0.88, ref3_result = 0.85),
       Sp.ref = c(ref1_result = 0.86, ref2_result = 0.90, ref3_result = 0.92),
       prev.pop = c(A = 0.95, B = 0.62, C = 0.18),
       control = estimateSnSpControl(seed = 865213, rep.iter = F))

unique(ex8$detailOut$Converge)
unique(ex8$detailOut$Message)
ex8


