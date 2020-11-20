
test_that("pairwise contingency table is correct for a simple example", {
  pred_pairs <- rbind(c(1, 2), c(1, 3), c(4, 5))
  true_pairs <- rbind(c(1, 2), c(1, 5))
  result <- contingency_table_pairs(pred_pairs, true_pairs, num_pairs = 25)
  true_result <- rbind("TRUE" = c("TRUE" = 1,"FALSE" = 1), "FALSE" = c("TRUE" = 2, "FALSE" = 21))
  true_result <- as.table(true_result)
  names(dimnames(true_result)) <- c("Prediction", "Truth")
  expect_equal(result, true_result)
})


# Examples to test
make_pairs_identical <- function() {
  true <- rbind(c(1, 2), c(1, 3), c(2, 3), c(4, 5))
  pred <- rbind(c(1, 2), c(1, 3), c(2, 3), c(4, 5))
  num_pairs <- 10
  measures <- list(
    "precision_pairs" = 1.0,
    "recall_pairs" = 1.0,
    "specificity_pairs" = 1.0,
    "sensitivity_pairs" = 1.0,
    "f_measure_pairs" = 1.0,
    "accuracy_pairs" = 1.0,
    "balanced_accuracy_pairs" = 1.0,
    "fowlkes_mallows_pairs" = 1.0
  )
  list("true" = true, "pred" = pred, "num_pairs" = num_pairs, "true_measures" = measures,
       "description" = "pairs in complete agreement")
}

make_pairs_distinct <- function() {
  true <- rbind(c(1, 2), c(1, 3), c(2, 3))
  pred <- rbind(c(1, 4), c(2, 4), c(3, 4))
  num_pairs <- 6
  measures <- list(
    "precision_pairs" = 0.0,
    "recall_pairs" = 0.0,
    "specificity_pairs" = 0.0,
    "sensitivity_pairs" = 0.0,
    "f_measure_pairs" = 0.0,
    "accuracy_pairs" = 0.0,
    "balanced_accuracy_pairs" = 0.0,
    "fowlkes_mallows_pairs" = 0.0
  )
  list("true" = true, "pred" = pred, "num_pairs" = num_pairs, "true_measures" = measures,
       "description" = "pairs in complete disagreement")
}

make_pairs_no_pred <- function() {
  true <- rbind(c(1, 2), c(1, 3), c(2, 3))
  pred <- matrix(0L, nrow = 0, ncol = 2)
  num_pairs <- 3
  measures <- list(
    "precision_pairs" = NaN,
    "recall_pairs" = 0.0,
    "specificity_pairs" = NaN,
    "sensitivity_pairs" = 0.0,
    "f_measure_pairs" = NaN,
    "accuracy_pairs" = 0.0,
    "balanced_accuracy_pairs" = NaN,
    "fowlkes_mallows_pairs" = NaN
  )
  list("true" = true, "pred" = pred, "num_pairs" = num_pairs, "true_measures" = measures,
       "description" = "pairs with zero recall")
}

make_pairs_one_fp <- function() {
  true <- rbind(c(1, 2), c(1, 3), c(2, 3), c(4, 5))
  pred <- rbind(c(1, 2), c(1, 3), c(2, 3), c(4, 5), c(1, 4))
  num_pairs <- 10
  measures <- list(
    "precision_pairs" = 4/5,
    "recall_pairs" = 1.0,
    "specificity_pairs" = 5/6,
    "sensitivity_pairs" = 1.0,
    "f_measure_pairs" = 8/9,
    "accuracy_pairs" = 9/10,
    "balanced_accuracy_pairs" = 11/12,
    "fowlkes_mallows_pairs" = 2/sqrt(5)
  )
  list("true" = true, "pred" = pred, "num_pairs" = num_pairs, "true_measures" = measures,
       "description" = "pairs with one false positive error")
}

make_pairs_no_true <- function() {
  true <- matrix(0L, nrow = 0, ncol = 2)
  pred <- rbind(c(1, 2), c(1, 3), c(2, 3))
  num_pairs <- 3
  measures <- list(
    "precision_pairs" = 0.0,
    "recall_pairs" = NaN,
    "specificity_pairs" = 0.0,
    "sensitivity_pairs" = NaN,
    "f_measure_pairs" = NaN,
    "accuracy_pairs" = 0.0,
    "balanced_accuracy_pairs" = NaN,
    "fowlkes_mallows_pairs" = NaN
  )
  list("true" = true, "pred" = pred, "num_pairs" = num_pairs, "true_measures" = measures,
       "description" = "pairs with zero precision")
}

examples_to_test <- list(make_pairs_identical,
                         make_pairs_distinct,
                         make_pairs_no_pred,
                         make_pairs_no_true,
                         make_pairs_one_fp)


context("Precision of Linked Pairs")
for (example in examples_to_test) {
  example <- example()
  test_that(paste("measure is correct for", example$description), {
    true <- example$true
    pred <- example$pred
    expect_equal(precision_pairs(true, pred),
                 example$true_measures[["precision_pairs"]])
  })
}

context("Recall of Linked Pairs")
for (example in examples_to_test) {
  example <- example()
  test_that(paste("measure is correct for", example$description), {
    true <- example$true
    pred <- example$pred
    expect_equal(recall_pairs(true, pred),
                 example$true_measures[["recall_pairs"]])
  })
}

context("Specificity of Linked Pairs")
for (example in examples_to_test) {
  example <- example()
  test_that(paste("measure is correct for", example$description), {
    true <- example$true
    pred <- example$pred
    num_pairs <- example$num_pairs
    expect_equal(specificity_pairs(true, pred, num_pairs),
                 example$true_measures[["specificity_pairs"]])
  })
}

context("Sensitivity of Linked Pairs")
for (example in examples_to_test) {
  example <- example()
  test_that(paste("measure is correct for", example$description), {
    true <- example$true
    pred <- example$pred
    expect_equal(sensitivity_pairs(true, pred),
                 example$true_measures[["sensitivity_pairs"]])
  })
}

context("F-Measure of Linked Pairs")
for (example in examples_to_test) {
  example <- example()
  test_that(paste("measure is correct for", example$description), {
    true <- example$true
    pred <- example$pred
    expect_equal(f_measure_pairs(true, pred),
                 example$true_measures[["f_measure_pairs"]])
  })
}

context("Accuracy of Linked Pairs")
for (example in examples_to_test) {
  example <- example()
  test_that(paste("measure is correct for", example$description), {
    true <- example$true
    pred <- example$pred
    num_pairs <- example$num_pairs
    expect_equal(accuracy_pairs(true, pred, num_pairs),
                 example$true_measures[["accuracy_pairs"]])
  })
}

context("Balanced Accuracy of Linked Pairs")
for (example in examples_to_test) {
  example <- example()
  test_that(paste("measure is correct for", example$description), {
    true <- example$true
    pred <- example$pred
    num_pairs <- example$num_pairs
    expect_equal(balanced_accuracy_pairs(true, pred, num_pairs),
                 example$true_measures[["balanced_accuracy_pairs"]])
  })
}

context("Fowlkes-Mallows Index of Linked Pairs")
for (example in examples_to_test) {
  example <- example()
  test_that(paste("measure is correct for", example$description), {
    true <- example$true
    pred <- example$pred
    expect_equal(fowlkes_mallows_pairs(true, pred),
                 example$true_measures[["fowlkes_mallows_pairs"]])
  })
}
