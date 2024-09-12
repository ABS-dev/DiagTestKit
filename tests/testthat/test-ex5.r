context("ex5")

ex5 <- estimateSnSp(dat = data5,
                    Sn.ref = c(Ref1 = 0.90),
                    Sp.ref = c(Ref1 = 0.99),
                    prev.pop = c(A = 0.80, B = 0.90),
                    control = estimateSnSpControl(seed = 1249856, rep.iter = FALSE))

test_that("values", {
  #calcVal
  expect_equal(ex5$calcVal$Nsim, expected = 1000)
  expect_equal(ex5$calcVal$Confidence, expected = 0.95)
  expect_equal(ex5$calcVal$SnPE, expected = 0.9412681, tolerance = 0.000001)
  expect_equal(ex5$calcVal$SnInterval,
               expected = round(c('5%' = 0.8937435, '100%' = 1), digits = 7),
               tolerance = 0.000001)
  expect_equal(ex5$calcVal$SpPE, expected = 0.8721353, tolerance = 0.000001)
  expect_equal(ex5$calcVal$SpInterval,
               expected = round(c('5%' = 0.7169586, '100%' = 1), digits = 7),
               tolerance = 0.000001)

  #detailOut
  expect_equal(ex5$detailOut$Exp.Sn, ex5_detailOut$Exp.Sn, tolerance = 0.000001)
  expect_equal(ex5$detailOut$Exp.Sp, ex5_detailOut$Exp.Sp, tolerance = 0.000001)
  expect_equal(ex5$detailOut$Converge, ex5_detailOut$Converge, tolerance = 0)
  expect_equal(ex5$detailOut$Message, ex5_detailOut$Message)

  #input
  expect_equal(ex5$input$seed, expected = 1249856)
  # TODO: look at ex5$input$Sn.sims after naming fixed
  expect_equal(ex5$input$prev.sims, ex5_prevsims)
})
