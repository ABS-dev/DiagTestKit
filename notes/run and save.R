devtools::load_all()

t0 <- proc.time()[3]
ex1 <- suppressWarnings(
  estimateSnSp(dat = DiagTestKit::data1,
               Sn.ref = data.frame(ref = c(0.90, 0)),
               Sp.ref = data.frame(ref = c(0.99, 0)),
               prev.pop = c(A = 0.80),
               control = estimateSnSpControl(seed = 64725,
                                             rep.iter = FALSE))
)
print(proc.time()[3] - t0)
ex1




lst <- as.list(environment())
save(lst, file = "notes/version4.1.3.optmr.rdata")

lst
ex1

S

estimateSnSpControl(64725, rep.iter = FALSE)
