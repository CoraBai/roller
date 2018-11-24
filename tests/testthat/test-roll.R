context("Test if functions of roll work as expected")

test_that("roll works as expected", {
  fair_coin <- device()
  set.seed(123)
  fair_coin50rolls <- roll(fair_coin, times = 50)
  fair_coin50rolls_correct = c(2,1,2,1,1,2,1,1,1,2,1,2,1,1,2,1,2,2,2,
                          1,1,1,1,1,1,1,1,1,2,2,1,1,1,1,2,2,1,2,2,2,
                          2, 2, 2, 2, 2, 2, 2, 2, 2, 1)
  expect_equal(fair_coin50rolls$rolls, fair_coin50rolls_correct)
  expect_s3_class(fair_coin50rolls, "rolls" )
  expect_equal(names(fair_coin50rolls), c("rolls", "sides", "prob", "total"))
  expect_equal(fair_coin50rolls$sides, c(1, 2))
  expect_equal(fair_coin50rolls$prob, c(0.5, 0.5))
  expect_equal(fair_coin50rolls$total, 50)


  fair_die <- device(sides = 1:6, prob = rep(1/6, 6))
  set.seed(123)
  fair_die50rolls <- roll(fair_die, times = 50)
  fair_coin50rolls_correct = c(3, 6, 4, 1, 1, 2, 5, 1, 5, 4, 1, 4, 6,
                               5, 2, 1, 3, 2, 3, 1, 1, 6, 5, 1, 5, 6,
                               5, 5, 3, 2, 1, 1, 6, 6, 2, 4, 6, 3, 3,
                               3, 2, 4, 4, 4, 2, 2, 3, 4, 3, 1)
  expect_equal(fair_die50rolls$rolls, fair_coin50rolls_correct)
  expect_s3_class(fair_die50rolls, "rolls")
  expect_equal(names(fair_die50rolls), c("rolls", "sides", "prob", "total"))

  str_die <- device(
    sides = c('a', 'b', 'c', 'd', 'e', 'f'),
    prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
  set.seed(123)
  str_rolls = roll(str_die, times = 20)
  expect_equal(names(str_rolls), c("rolls", "sides", "prob", "total"))
  expect_equal(str_rolls$rolls, c("f", "c", "e", "b", "a", "f", "e",
                                  "b", "d", "e", "a", "e", "d", "d", "f",
                                  "b", "f", "f", "f", "a"))
})

test_that("check_times() works as expected", {
  expect_error(check_times(-1))
  expect_true(check_times(2))
})
