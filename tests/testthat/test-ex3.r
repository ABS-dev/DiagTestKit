context("ex3")
test_that("values", {
  ex3_detailOut <- ex3_prevsims <- NULL
  load("./data/testdata.rda")
  ex3 <- suppressWarnings(
    estimateSnSp(dat = data3,
                 Sn.ref = data.frame(ref = c(0.99, 0.75)),
                 Sp.ref = data.frame(ref = c(0.85, 0.67)),
                 prev.pop = c(A = 0.92),
                 control = estimateSnSpControl(seed = 896421,
                                               rep.iter = FALSE))
  )

  # calcVal
  expect_equal(ex3$calcVal$Nsim,
               expected = 1000)
  expect_equal(ex3$calcVal$Confidence,
               expected = 0.95)
  expect_equal(ex3$calcVal$SnPE,
               expected = .9195444, tolerance = 0.000001)
  expect_equal(ex3$calcVal$SnInterval,
               expected = c("0.3%" = 0.8882768,
                            "95.3%" = 0.9792288),
               tolerance = 0.000001)
  expect_equal(ex3$calcVal$SpPE,
               expected = 0.9211223, tolerance = 0.000001)
  expect_equal(ex3$calcVal$SpInterval,
               expected = round(c("5%" = .6734354,
                                  "100%" = 1),
                                digits = 7),
               tolerance = 0.000001)

  # detailOut
  expect_equal(ex3$detailOut$Exp.Sn,
               ex3_detailOut$Exp.Sn,
               tolerance = 0.000001)
  expect_equal(ex3$detailOut$Exp.Sp,
               ex3_detailOut$Exp.Sp,
               tolerance = 0.000001)
  expect_equal(ex3$detailOut$Converge,
               ex3_detailOut$Converge,
               tolerance = 0)
  expect_equal(ex3$detailOut$Message,
               ex3_detailOut$Message)

  # input
  expect_equal(ex3$input$seed,
               expected = 896421)
  # TODO: look at ex3$input$Sn.sims after naming fixed
  dimnames(ex3$input$prev.sims) <- NULL
  dimnames(ex3_prevsims) <- NULL
  expect_equal(ex3$input$prev.sims,
               ex3_prevsims)
})
