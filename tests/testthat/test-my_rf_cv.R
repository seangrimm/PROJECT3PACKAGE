test_that("Proper cross-validation returned", {
  expect_equal(my_rf_cv(5), 120000, tolerance = .1)
  expect_equal(my_rf_cv(2), 125000, tolerance = .15)
})

test_that("Appropriate errors given", {
  expect_error(my_rf_cv(.1))
  expect_error(my_rf_cv("string"))
  expect_error(my_rf_cv(c(1, 2, 3)))
  expect_error(my_rf_cv(1))
})
