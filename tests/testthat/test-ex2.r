context("ex2")

test_that("values", {
  ex2_detailOut <-  ex2_prevsims <- NULL
  load("./data/testdata.rda")
  ex2 <- estimateSnSp(dat = data2,
                      Sn.ref = c(ref_result = 0.90),
                      Sp.ref = c(ref_result = 0.94),
                      prev.pop = c(A = 0.92,
                                   B = 0.20,
                                   C = 0.50),
                      control = estimateSnSpControl(seed = 4902342,
                                                    rep.iter = FALSE))

  # calcVal
  expect_equal(ex2$calcVal$Nsim, expected = 1000)
  expect_equal(ex2$calcVal$Confidence, expected = 0.95)
  expect_equal(ex2$calcVal$SnPE, expected = 0.8614248,
               tolerance = 0.000001)
  expect_equal(ex2$calcVal$SnInterval,
               expected = round(c("1.2%" = 0.8171083,
                                  "96.2%" = 0.9162939),
                                digits = 7),
               tolerance = 0.000001)
  expect_equal(ex2$calcVal$SpPE, expected = 0.9818038,
               tolerance = 0.000001)
  expect_equal(ex2$calcVal$SpInterval,
               expected = round(c("5%" = 0.9260871,
                                  "100%" = 1),
                                digits = 7),
               tolerance = 0.000001)

  # detailOut
  expect_equal(ex2$detailOut$Exp.Sn,
               ex2_detailOut$Exp.Sn,
               tolerance = 0.000001)
  expect_equal(ex2$detailOut$Exp.Sp,
               ex2_detailOut$Exp.Sp,
               tolerance = 0.000001)
  expect_equal(ex2$detailOut$Converge,
               ex2_detailOut$Converge,
               tolerance = 0)
  expect_equal(ex2$detailOut$Message,
               ex2_detailOut$Message)

  # input
  expect_equal(ex2$input$seed, expected = 4902342)
  # TODO: look at ex2$input$Sn.sims after naming fixed
  dimnames(ex2$input$prev.sims) <- NULL
  dimnames(ex2_prevsims) <- NULL
  expect_equal(ex2$input$prev.sims,
               ex2_prevsims)
})
