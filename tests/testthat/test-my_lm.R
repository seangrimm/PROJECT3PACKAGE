test_that("my_lm returns correct coefficients", {
  expect_equal(my_lm(lifeExp ~ gdpPercap, my_gapminder)[1, 1], 53.956,
               tolerance = .001)
  expect_equal(my_lm(lifeExp ~ gdpPercap, my_gapminder)[2, 1], .0007,
               tolerance = .001)
})

test_that("my_lm returns correct number of coefficients", {
  expect_equal(nrow(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)), 6)
  expect_equal(nrow(my_lm(lifeExp ~ gdpPercap, my_gapminder)), 2)
})

test_that("my_lm returns errors when appropriate", {
  expect_error(my_lm("string", my_gapminder))
  expect_error(my_lm(lifeExp ~ continent, "string"))
  expect_error(my_lm(lifeExp ~ continnent, my_gapminder))
})
