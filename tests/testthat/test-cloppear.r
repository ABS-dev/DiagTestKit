context('cloppear')

CP.Sn <- cloppearSnSp(dat = dat_infal, est.Sn = TRUE)
CP.Sp <- cloppearSnSp(dat = dat_infal, est.Sn = FALSE)

test_that("CP.Sn", {
  expect_equal(CP.Sn$calcVal,
               expected = list('Sn' = 0.987013, 'Sn.LL' = 0.9538755, 'Sn.UL' = 0.9984233),
               tolerance = 0.000001)
  expect_equal(CP.Sn$data,  expected = list('Test.Positive' = 152, 'Total.Positive' = 154))
  expect_equal(CP.Sn$alpha, expected = 0.05)
})

test_that("CP.Sp", {
  expect_equal(CP.Sp$calcVal,
               expected = list('Sp' = 0.970297, 'Sp.LL' = 0.9156431, 'Sp.UL' = 0.9938321),
               tolerance = 0.000001)
  expect_equal(CP.Sp$data,  expected = list('Test.Negative' = 98, 'Total.Negative' = 101))
  expect_equal(CP.Sp$alpha, expected = 0.05)
})
