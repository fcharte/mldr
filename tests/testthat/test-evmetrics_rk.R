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

  expect_equal(rank_labels(my_labels, ties_method = "min"), my_rankings)
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
  my_labels2 <- matrix(c(
    0, 1, 1, 0,
    1, 1, 0, 0,
    0, 0, 1, 1
  ), nrow = 3, byrow = T)
  identified2 <- list(
    c(2, 3),
    c(1, 2),
    c(3, 4)
  )

  expect_equivalent(relevant_labels(my_labels), identified)
  expect_equivalent(relevant_labels(my_labels2), identified2)
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
  expect_equal(average_precision(my_labels2, my_probs, ties_method = "min"), 2.75/3)
  expect_equal(average_precision(my_labels2, my_probs, ties_method = "average"), 2.15/3)
})

test_that("one error is calculated", {
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
    0, 1, 1, 0,
    0, 0, 1, 0
  ), nrow = 3, byrow = T)

  expect_equal(one_error(my_labels, my_probs), 1/3)
  expect_equal(one_error(my_labels2, my_probs), 2/3)
})

test_that("coverage is calculated", {
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
    0, 1, 1, 0,
    0, 0, 1, 0
  ), nrow = 3, byrow = T)

  expect_equal(coverage(my_labels, my_probs), 1)
  expect_equal(coverage(my_labels2, my_probs), 2)
  expect_equal(coverage(my_labels2, my_probs, ties_method = "average"), 6.5/3)
})

test_that("ranking loss is calculated", {
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
    0, 1, 1, 0,
    0, 0, 1, 0
  ), nrow = 3, byrow = T)

  expect_equal(ranking_loss(my_labels, my_probs), 0)
  expect_equal(ranking_loss(my_labels2, my_probs), 2/3)
})
