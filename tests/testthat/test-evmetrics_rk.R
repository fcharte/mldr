context("test-evmetrics_rk.R")

test_that("ranking labels correctly ranks labels", {
  my_labels <- matrix(c(
    0.3, 0.9, 0.7, 0.1,
    0.8, 0.6, 0.7, 0.2,
    0, 0, 0, 0
  ), nrow = 3, byrow = T)
  my_rankings <- matrix(c(
    3, 1, 2, 4,
    1, 3, 2, 4,
    1, 1, 1, 1
  ), nrow = 3, byrow = T)

  expect_equal(rank_labels(my_labels, ties.method = "min"), my_rankings)
})

test_that("true labels are correctly identified", {
  my_labels <- matrix(c(
    0, 1, 1, 0,
    1, 1, 1, 0,
    0, 0, 0, 0
  ), nrow = 3, byrow = T)
  identified <- list(
    c(2, 3),
    c(1, 2, 3),
    numeric(0)
  )

  expect_equal(relevant_labels(my_labels), identified)
})

test_that("average precision is calculated", {
  my_probs <- matrix(c(
    0.3, 0.9, 0.7, 0.1,
    0.8, 0.6, 0.7, 0.2,
    0, 0, 0, 0
  ), nrow = 3, byrow = T)
  my_labels <- matrix(c(
    0, 1, 1, 0,
    1, 1, 1, 0,
    0, 0, 0, 0
  ), nrow = 3, byrow = T)
  my_labels2 <- matrix(c(
    0, 1, 0, 1,
    1, 1, 1, 0,
    0, 0, 1, 0
  ), nrow = 3, byrow = T)

  expect_equal(average_precision(my_labels, my_probs), 1)
  expect_equal(average_precision(my_labels2, my_probs, ties.method = "min"), 2.75/3)
  expect_equal(average_precision(my_labels2, my_probs, ties.method = "average"), 2.15/3)
})
