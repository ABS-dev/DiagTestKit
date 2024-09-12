context("ex7")

ex7 <- estimateSnSp(dat = data7,
                    Sn.ref = data.frame(ref1 = c(0.88, 0.75), ref2 = c(0.90 ,0.55)),
                    Sp.ref = data.frame(ref1 = c(0.97, 0.6), ref2 = c(0.95, 0.5)),
                    prev.pop = c(A = 0.87, B = 0.35),
                    control = estimateSnSpControl(seed=1937457, rep.iter = FALSE))

test_that("values", {
  #calcVal
  expect_equal(ex7$calcVal$Nsim, expected = 1000)
  expect_equal(ex7$calcVal$Confidence, expected = 0.95)
  expect_equal(ex7$calcVal$SnPE, expected = 0.966133, tolerance = 0.000001)
  expect_equal(ex7$calcVal$SnInterval,
               expected = round(c('5%' = 0.9157368, '100%' = 1), digits = 7),
               tolerance = 0.000001)
  expect_equal(ex7$calcVal$SpPE, expected = 0.9215106, tolerance = 0.000001)
  expect_equal(ex7$calcVal$SpInterval,
               expected = round(c('5%' = 0.8715217, '100%' = 1), digits = 7),
               tolerance = 0.000001)

  #detailOut
  expect_equal(ex7$detailOut$Exp.Sn, ex7_detailOut$Exp.Sn, tolerance = 0.000001)
  expect_equal(ex7$detailOut$Exp.Sp, ex7_detailOut$Exp.Sp, tolerance = 0.000001)
  expect_equal(ex7$detailOut$Converge, ex7_detailOut$Converge, tolerance = 0)
  expect_equal(ex7$detailOut$Message, ex7_detailOut$Message)

  #input
  expect_equal(ex7$input$seed, expected = 1937457)
  # TODO: look at ex7$input$Sn.sims after naming fixed
  expect_equal(ex7$input$prev.sims, ex7_prevsims)
})
