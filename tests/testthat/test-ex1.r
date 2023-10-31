context("ex1")

test_that("initial values", {
  ex1_detailOut <- ex1_prevsims <- NULL
  load("./data/testdata.rda")


  ex1 <- suppressWarnings(
    estimateSnSp(dat = data1,
                 Sn.ref = data.frame(ref = c(0.90, 0)),
                 Sp.ref = data.frame(ref = c(0.99, 0)),
                 prev.pop = c(A = 0.80),
                 control = estimateSnSpControl(seed = 64725,
                                               rep.iter = FALSE))
  )
  ex1_update <- updateAlpha(ex1, newAlpha = 0.01)

  save(ex1, ex1_update, file = "./data/ex1.rdata")

  #calcVal
  expect_equal(ex1$calcVal$Nsim,
               expected = 1000,
               tolerance = 0)
  expect_equal(ex1$calcVal$Confidence,
               expected = 0.95,
               tolerance = 0)
  expect_equal(round(ex1$calcVal$SnPE, digits = 7),
               expected = 0.9449821,
               tolerance = 0.000001)
  expect_equal(ex1$calcVal$SnInterval,
               expected = round(c("5%" = 0.9019639,
                                  "100%" = 1),
                                digits = 7),
               tolerance = 0.000001)
  expect_equal(ex1$calcVal$SpPE,
               expected = 0.9062769,
               tolerance = 0.000001)
  expect_equal(ex1$calcVal$SpInterval,
               expected = round(c("5%" = 0.7523346,
                                  "100%" = 1),
                                digits = 7),
               tolerance = 0.000001)

  #detailOut
  expect_equal(ex1$detailOut$Exp.Sn,
               ex1_detailOut$Exp.Sn,
               tolerance = 0.000001)
  expect_equal(ex1$detailOut$Exp.Sp,
               ex1_detailOut$Exp.Sp,
               tolerance = 0.000001)
  expect_equal(ex1$detailOut$Converge,
               ex1_detailOut$Converge,
               tolerance = 0)
  expect_equal(ex1$detailOut$Message,
               ex1_detailOut$Message)

  #input
  expect_equal(ex1$input$seed, expected = 64725)
  # TODO: look at ex1$input$Sn.sims after naming fixed
  dimnames(ex1$input$prev.sims) <- NULL
  dimnames(ex1_prevsims) <- NULL
  expect_equal(ex1$input$prev.sims, ex1_prevsims)

})

test_that("update values", {
  load("./data/ex1.rdata")
  ### values that haven"t changed with update
  # calcVal
  expect_equal(ex1_update$calcVal$Nsim,
               expected = ex1$calcVal$Nsim)
  expect_equal(ex1_update$calcVal$SnPE,
               expected = ex1$calcVal$SnPE)
  expect_equal(ex1_update$calcVal$SpPE,
               expected = ex1$calcVal$SpPE)
  # detailOut and input
  attributes(ex1_update$detailOut) <- NULL
  attributes(ex1$detailOut) <- NULL
  expect_equal(ex1_update$detailOut,
               expected = ex1$detailOut)

  attributes(ex1_update$input[[4]]) <- NULL
  attributes(ex1$input[[4]]) <- NULL

  expect_equal(ex1_update$input,
               expected = ex1$input)

  ### values that have changed
  # calcVal
  expect_equal(ex1_update$calcVal$Confidence, expected = 0.99)
  expect_equal(ex1_update$calcVal$SnInterval,
               expected = round(c("1%" = 0.8848417,
                                  "100%" = 1),
                                digits = 7),
               tolerance = 0.000001)
  expect_equal(ex1_update$calcVal$SpInterval,
               expected = round(c("1%" = 0.7111645,
                                  "100%" = 1),
                                digits = 7),
               tolerance = 0.000001)
})

test_that("warnings and messages", {
  load("./data/ex1.rdata")
  expect_warning(estimateSnSp(dat = data1,
                              Sn.ref = data.frame(ref = c(0.90, 0)),
                              Sp.ref = data.frame(ref = c(0.99, 0)),
                              prev.pop = c(A = 0.80),
                              control = estimateSnSpControl(seed = 64725,
                                                            rep.iter = FALSE)),
                 "The data suggests a single population was tested")
  # TODO: check message once it has been converted from cat statement to message
})
