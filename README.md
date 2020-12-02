
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clevr: Clustering and Link Prediction Evaluation in R

<!-- badges: start -->

<!-- badges: end -->

clevr implements functions for evaluating link prediction and clustering
algorithms in R. It includes efficient implementations of common
performance measures, such as:

  - pairwise precision, recall, F-measure;
  - homogeneity, completeness and V-measure;
  - (adjusted) Rand index;
  - variation of information; and
  - mutual information.

While the current focus is on supervised (a.k.a. external) performance
measures, unsupervised (internal) measures are also in scope for future
releases.

## Installation

You can install the latest release from
[CRAN](https://CRAN.R-project.org) by entering:

``` r
install.packages("clevr")
```

The development version can be installed from GitHub using `devtools`:

``` r
# install.packages("devtools")
devtools::install_github("cleanzr/clevr")
```

## Example

Several functions are included which transform between different
clustering representations.

``` r
library(clevr)
# A clustering of four records represented as a membership vector
pred_membership <- c("Record1" = 1, "Record2" = 1, "Record3" = 1, "Record4" = 2)

# Represent as a set of record pairs that appear in the same cluster
pred_pairs <- membership_to_pairs(pred_membership)
print(pred_pairs)
#>      [,1]      [,2]     
#> [1,] "Record1" "Record2"
#> [2,] "Record1" "Record3"
#> [3,] "Record2" "Record3"

# Represent as a list of record clusters
pred_clusters <- membership_to_clusters(pred_membership)
print(pred_clusters)
#> $`1`
#> [1] "Record1" "Record2" "Record3"
#> 
#> $`2`
#> [1] "Record4"
```

Performance measures are available for evaluating linked pairs:

``` r
true_pairs <- rbind(c("Record1", "Record2"), c("Record3", "Record4"))

pr <- precision_pairs(true_pairs, pred_pairs)
print(pr)
#> [1] 0.3333333

re <- recall_pairs(true_pairs, pred_pairs)
print(re)
#> [1] 0.5
```

and for evaluating clusterings:

``` r
true_membership <- c("Record1" = 1, "Record2" = 1, "Record3" = 2, "Record4" = 2)

ari <- adj_rand_index(true_membership, pred_membership)
print(ari)
#> [1] 0

vi <- variation_info(true_membership, pred_membership)
print(vi)
#> [1] 0.8239592
```
