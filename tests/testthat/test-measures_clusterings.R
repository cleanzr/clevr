
# Examples to test
make_clusterings_identical <- function() {
  true <- c(1,1,1,2,2)
  pred <- c(1,1,1,2,2)
  measures <- list(
    "rand_index" = 1.0,
    "adj_rand_index" = 1.0,
    "fowlkes_mallows" = 1.0,
    "homogeneity" = 1.0,
    "completeness" = 1.0,
    "v_measure" = 1.0,
    "variation_info" = 0.0,
    "mutual_info" = 0.6730116670092563
  )
  list("true" = true, "pred" = pred, "true_measures" = measures,
       "description" = "clusterings in perfect agreement")
}

make_clusterings_distinct <- function() {
  true <- c(1,2,3,4,5)
  pred <- c(1,1,1,1,1)
  measures <- list(
    "rand_index" = 0.0,
    "adj_rand_index" = 0.0,
    "fowlkes_mallows" = 0.0,
    "homogeneity" = 0.0,
    "completeness" = 1.0,
    "v_measure" = 0.0,
    "variation_info" = 1.6094379124341003,
    "mutual_info" = 0.0
  )
  list("true" = true, "pred" = pred, "true_measures" = measures,
       "description" = "clusterings in complete disagreement")
}

make_clusterings_one_difference <- function() {
  true <- c(1,1,2,2,2)
  pred <- c(1,1,1,2,2)
  measures <- list(
    "rand_index" = 0.6,
    "adj_rand_index" = 0.16666666666666666,
    "fowlkes_mallows" = 0.5,
    "homogeneity" = 0.43253806776631243,
    "completeness" = 0.43253806776631243,
    "v_measure" = 0.43253806776631243,
    "variation_info" = 0.7638170019537754,
    "mutual_info" = 0.2911031660323686
  )
  list("true" = true, "pred" = pred, "true_measures" = measures,
       "description" = "clusterings with one difference")
}

make_clusterings_anticorrelated <- function() {
  true <- c(1,1,1,2,3)
  pred <- c(1,2,3,4,4)
  measures <- list(
    "rand_index" = 0.6,
    "adj_rand_index" = -0.176470588235294,
    "fowlkes_mallows" = 0.0,
    "homogeneity" = 0.7082316448032829,
    "completeness" = 0.5051961085524235,
    "v_measure" = 0.5897275217561567,
    "variation_info" = 0.936426245424844,
    "mutual_info" = 0.6730116670092563
  )
  list("true" = true, "pred" = pred, "true_measures" = measures,
       "description" = "clusterings that are anti-correlated")
}

examples_to_test <- list(make_clusterings_identical,
                         make_clusterings_distinct,
                         make_clusterings_one_difference,
                         make_clusterings_anticorrelated)

measures_to_test <- c("Rand Index" = "rand_index",
                      "Adjusted Rand Index" = "adj_rand_index",
                      "Fowlkes-Mallows Index" = "fowlkes_mallows",
                      "Homogeneity" = "homogeneity",
                      "Completeness" = "completeness",
                      "V-Measure" = "v_measure",
                      "Variation Information" = "variation_info",
                      "Mutual Information" = "mutual_info")


for (measure_name in names(measures_to_test)) {
  context(measure_name)
  measure <- measures_to_test[measure_name]
  for (example in examples_to_test) {
    example <- example()
    test_that(paste(measure_name, "is correct for", example$description), {
      true <- example$true
      pred <- example$pred
      expect_equal(eval(parse(text=paste0(measure, "(true, pred)"))),
                   example$true_measures[[measure]])
    })
  }
}
