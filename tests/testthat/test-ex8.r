context("ex8")
load("./data/testdata.rda")
ex8 <-estimateSnSp(dat = data8,
                  Sn.ref = c(ref1_result = 0.92, ref2_result = 0.88, ref3_result = 0.85),
                  Sp.ref = c(ref1_result = 0.86, ref2_result = 0.90, ref3_result = 0.92),
                  prev.pop = c(A = 0.95, B = 0.62, C = 0.18),
                  control = estimateSnSpControl(seed = 865213, rep.iter = FALSE))

test_that("values", {
  #calcVal
  expect_equal(ex8$calcVal$Nsim, expected = 1000)
  expect_equal(ex8$calcVal$Confidence, expected = 0.95)
  expect_equal(ex8$calcVal$SnPE, expected = 0.965417, tolerance = 0.000001)
  expect_equal(ex8$calcVal$SnInterval,
               expected = round(c('5%' = 0.8879949, '100%' = 1), digits = 7),
               tolerance = 0.000001)
  expect_equal(ex8$calcVal$SpPE, expected = 0.9835192, tolerance = 0.000001)
  expect_equal(ex8$calcVal$SpInterval,
               expected = round(c('5%' = 0.9016964, '100%' = 1), digits = 7),
               tolerance = 0.000001)

  #detailOut
  expect_equal(ex8$detailOut$Exp.Sn, ex8_detailOut$Exp.Sn, tolerance = 0.000001)
  expect_equal(ex8$detailOut$Exp.Sp, ex8_detailOut$Exp.Sp, tolerance = 0.000001)
  expect_equal(ex8$detailOut$Converge, ex8_detailOut$Converge, tolerance = 0)
  expect_equal(ex8$detailOut$Message, ex8_detailOut$Message)

  #input
  expect_equal(ex8$input$seed, expected = 865213)
  # TODO: look at ex8$input$Sn.sims after naming fixed
  expect_equal(ex8$input$prev.sims, ex8_prevsims)
})
