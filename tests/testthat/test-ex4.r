context("ex4")

ex4 <- estimateSnSp(dat = data4,
                    Sn.ref = data.frame(ref = c(0.95, 0.55)),
                    Sp.ref = data.frame(ref = c(0.93, 0.48)),
                    prev.pop = c(A = 0.97, B = 0.25, C = 0.68),
                    control=estimateSnSpControl(seed = 6589732, rep.iter = FALSE))

test_that("values", {
  #calcVal
  expect_equal(ex4$calcVal$Nsim, expected = 1000)
  expect_equal(ex4$calcVal$Confidence, expected = 0.95)
  expect_equal(ex4$calcVal$SnPE, expected = 0.9101593, tolerance = 0.000001)
  expect_equal(ex4$calcVal$SnInterval,
               expected = c('1.5%' = 0.8722713, '96.5%' = 0.9675238),
               tolerance = 0.000001)
  expect_equal(ex4$calcVal$SpPE, expected = 0.8802044, tolerance = 0.000001)
  expect_equal(ex4$calcVal$SpInterval,
               expected = c('0.2%' = 0.8221522, '95.2%' = 0.9373587),
               tolerance = 0.000001)

  #detailOut
  expect_equal(ex4$detailOut$Exp.Sn, ex4_detailOut$Exp.Sn, tolerance = 0.000001)
  expect_equal(ex4$detailOut$Exp.Sp, ex4_detailOut$Exp.Sp, tolerance = 0.000001)
  expect_equal(ex4$detailOut$Converge, ex4_detailOut$Converge, tolerance = 0)
  expect_equal(ex4$detailOut$Message, ex4_detailOut$Message)

  #input
  expect_equal(ex4$input$seed, expected = 6589732)
  # TODO: look at ex4$input$Sn.sims after naming fixed
  expect_equal(ex4$input$prev.sims, ex4_prevsims)
})
