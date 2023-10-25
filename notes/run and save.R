devtools::load_all()

ex1 <- suppressWarnings(
  estimateSnSp(dat = DiagTestKit::data1,
               Sn.ref = data.frame(ref = c(0.90, 0)),
               Sp.ref = data.frame(ref = c(0.99, 0)),
               prev.pop = c(A = 0.80),
               control = estimateSnSpControl(seed = 64725,
                                             rep.iter = FALSE))
)

lst <- as.list(environment())
save(lst, file = "notes/version4.3.1.optmr.rdata")
lst
ex1

S
