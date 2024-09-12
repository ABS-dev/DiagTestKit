context("ex6")
test_that("values", {
  ex6_detailOut <-  ex6_prevsims <- NULL
  load("./data/testdata.rda")

  ex6 <- suppressWarnings(
    estimateSnSp(dat = data6,
                 Sn.ref = c(Ref1_result = 0.95,
                            ref2 = 0.91),
                 Sp.ref = c(Ref1_result = 0.85,
                            ref2 = 0.98),
                 prev.pop = c(A = 0.82),
                 control = estimateSnSpControl(seed = 2948217,
                                               rep.iter = FALSE))
  )

  # calcVal
  expect_equal(ex6$calcVal$Nsim,
               expected = 1000)
  expect_equal(ex6$calcVal$Confidence,
               expected = 0.95)
  expect_equal(ex6$calcVal$SnPE,
               expected = 0.9589171,
               tolerance = 0.000001)
  expect_equal(ex6$calcVal$SnInterval,
               expected = round(c("5%" = 0.9106663,
                                  "100%" = 1),
                                digits = 7),
               tolerance = 0.000001)
  expect_equal(ex6$calcVal$SpPE,
               expected = 0.9456264,
               tolerance = 0.000001)
  expect_equal(ex6$calcVal$SpInterval,
               expected = round(c("5%" = 0.7962622,
                                  "100%" = 1),
                                digits = 7),
               tolerance = 0.000001)

  # detailOut
  expect_equal(ex6$detailOut$Exp.Sn,
               ex6_detailOut$Exp.Sn,
               tolerance = 0.000001)
  expect_equal(ex6$detailOut$Exp.Sp,
               ex6_detailOut$Exp.Sp,
               tolerance = 0.000001)
  expect_equal(ex6$detailOut$Converge,
               ex6_detailOut$Converge,
               tolerance = 0)
  expect_equal(ex6$detailOut$Message,
               ex6_detailOut$Message)

  # input
  expect_equal(ex6$input$seed,
               expected = 2948217)
  # TODO: look at ex6$input$Sn.sims after naming fixed
  dimnames(ex6$input$prev.sims) <- NULL
  dimnames(ex6_prevsims) <- NULL
  expect_equal(ex6$input$prev.sims,
               ex6_prevsims)
})
