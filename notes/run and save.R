devtools::load_all()

t0 <- proc.time()[3]
ex1 <- suppressWarnings(
  estimateSnSp(dat = DiagTestKit::data1,
               Sn.ref = data.frame(ref = c(0.90, 0)),
               Sp.ref = data.frame(ref = c(0.99, 0)),
               prev.pop = c(A = 0.80),
               control = estimateSnSpControl(seed = 64725,
                                             rep.iter = FALSE), nsim = 1000)
)
print(proc.time()[3] - t0)
ex1

# 1000  simulations
# 95 % Interval Estimates
#
#               Point.Estimate     Lower Upper
# Sn = P(T+|D+)      0.9449821 0.9019639     1
# Sp = P(T-|D-)      0.9062769 0.7523346     1

# 117, 111, 128 w/o names
# 85, 106, 124, w/ names

lst <- as.list(environment())
save(lst, file = "notes/version4.1.3.optmr.rdata")
lst
ex1

S

estimateSnSpControl(64725, rep.iter = FALSE)

