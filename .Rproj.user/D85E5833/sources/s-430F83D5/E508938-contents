context("Test if functions of device work as expected")

test_that("device works as expected", {
  weird_die <- device(sides = c('i', 'ii', 'iii', 'iv'), prob = rep(1/4, 4))
  sides_correct = c('i', 'ii', 'iii', 'iv')
  expect_equal(weird_die$sides, sides_correct)
  prob_correct = rep(1/4, 4)
  expect_equal(weird_die$prob, prob_correct)
  expect_s3_class(weird_die, "device")

  loaded_die <- device(sides = 1:6, prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
  sides_correct_loaded = 1:6
  prob_correct_loaded = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35)
  expect_equal(loaded_die$sides, sides_correct_loaded)
  expect_equal(loaded_die$prob, prob_correct_loaded)
  expect_s3_class(loaded_die, "device")

  expect_error(device(sides = c('a')))
  expect_error(device(sides = c('heads', 'heads')))
  expect_error(device( sides = c('a', 'b'), prob = c(0.2, 0.1)))
  expect_error(device( sides = c('a', 'b', 'c'), prob = c(0.2, 0.8)))
})

test_that("check_sides works as expected", {
  check_sides_1 <- check_sides(c(2,3))
  expect_true(check_sides_1)
  expect_error(check_sides(5))
  expect_error(check_sides(c(3,3)))
})

test_that("check_prob works as expected", {
  expect_true(check_prob(c(0.5, 0.5)))
  expect_error(check_prob(c(0.5, 0.1)))
  expect_error(check_prob(c(2,2)))
  expect_error(check_prob(1))
  expect_error(check_prob("not numeric"))
})

test_that("is.device works as expected", {
  expect_true(is.device(weird_die))
  expect_true(is.device(loaded_die))
  expect_false(is.device(c(1,2,3)))
})
