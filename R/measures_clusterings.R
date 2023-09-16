
#' @importFrom stats xtabs
#' @importFrom Matrix rowSums colSums crossprod
#' @noRd
pair_contingency_table_clusters <- function(true, pred) {
  if (length(true) != length(pred))
    stop("`true` and `pred` must have the same length")

  # TODO: NA treatment
  data <- data.frame("Truth" = true, "Prediction" = pred,
                     stringsAsFactors = FALSE)
  ct <- xtabs(~ pred + true, data = data, sparse = TRUE)

  sizes_true <- colSums(ct)
  sizes_pred <- rowSums(ct)
  sum_squares <- sum(ct^2)
  num_items <- length(true)
  pair_ct <- matrix(nrow = 2, ncol = 2, data = NA_integer_)
  pair_ct[1,1] <- sum_squares - num_items # TP
  pair_ct[2,1] <- sum(ct %*% sizes_true) - sum_squares # FP
  pair_ct[1,2] <- sum(crossprod(ct, sizes_pred)) - sum_squares # FN
  pair_ct[2,2] <- num_items^2 - pair_ct[1,2] - pair_ct[2,1] - sum_squares # TN
  dimnames(pair_ct) <- list("Prediction" = c("TRUE", "FALSE"), "Truth" = c("TRUE", "FALSE"))
  return(as.table(pair_ct))
}


#' Contingency Table for Clusterings
#'
#' @description Compute the contingency table for a _predicted_ clustering
#'   given a _ground truth_ clustering.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#' @return Returns a table \eqn{C} (stored as a sparse matrix) such that
#'    \eqn{C_{ij}}{C_ij} counts the number of elements assigned to
#'    cluster \eqn{i} in `pred` and cluster \eqn{j} in `true`.
#'
#' @seealso
#' [`eval_report_clusters`] computes common evaluation measures derived
#' from the output of this function.
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' contingency_table_clusters(true, pred)
#'
#' @export
#' @importFrom stats xtabs
contingency_table_clusters <- function(true, pred) {
  if (length(true) != length(pred))
    stop("`true` and `pred` must have the same length")

  # TODO: NA treatment
  data <- data.frame("true" = true, "pred" = pred,
                     stringsAsFactors = FALSE)
  ct <- xtabs(~ pred + true, data = data, sparse = TRUE)
  ct
}


#' Evaluation Report for Clustering
#'
#' @description Compute various evaluation measures for a predicted
#'   clustering using a ground truth clustering as a reference.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#' @return Returns a list containing the following measures:
#'   \describe{
#'     \item{homogeneity}{see [`homogeneity`]}
#'     \item{completeness}{see [`completeness`]}
#'     \item{v_measure}{see [`v_measure`]}
#'     \item{rand_index}{see [`rand_index`]}
#'     \item{adj_rand_index}{see [`adj_rand_index`]}
#'     \item{variation_info}{see [`variation_info`]}
#'     \item{mutual_info}{see [`mutual_info`]}
#'     \item{fowlkes_mallows}{see [`fowlkes_mallows`]}
#'   }
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' eval_report_clusters(true, pred)
#'
#' @export
eval_report_clusters <- function(true, pred) {
  pair_ct <- pair_contingency_table_clusters(true, pred)
  ct <- contingency_table_clusters(true, pred)
  list("homogeneity" = homogeneity_ct(ct),
       "completeness" = completeness_ct(ct),
       "v_measure" = v_measure_ct(ct),
       "rand_index" = rand_index_ct(pair_ct),
       "adj_rand_index" = adj_rand_index_ct(pair_ct),
       "variation_info" = variation_info_ct(ct),
       "mutual_info" = mutual_info_ct(ct),
       "fowlkes_mallows" = fowlkes_mallows_ct(ct))
}


#' Rand Index Between Clusterings
#'
#' @description Computes the Rand index (RI) between two clusterings, such
#'    as a predicted and ground truth clustering.
#'
#' @details The Rand index (RI) can be expressed as:
#'   \deqn{\frac{a + b}{{n \choose 2}}.}{(a + b)/binom(n, 2).}
#'   where
#'   * \eqn{n} is the number of elements,
#'   * \eqn{a} is the number of pairs of elements that appear in the
#'   same cluster in both clusterings, and
#'   * \eqn{b} is the number of pairs of elements that appear in distinct
#'   clusters in both clusterings.
#'
#'   The RI takes on values between 0 and 1, where 1 denotes exact agreement
#'   between the clusterings and 0 denotes disagreement on all pairs of
#'   elements.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#'
#' @references
#' Rand, W. M. "Objective Criteria for the Evaluation of Clustering Methods."
#' _Journal of the American Statistical Association_ 66(336), 846-850 (1971).
#' \doi{10.1080/01621459.1971.10482356}
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' rand_index(true, pred)
#'
#' @export
rand_index <- function(true, pred) {
  pair_ct <- pair_contingency_table_clusters(true, pred)
  rand_index_ct(pair_ct)
}


#' Adjusted Rand Index Between Clusterings
#'
#' @description Computes the adjusted Rand index (ARI) between two clusterings,
#'    such as a predicted and ground truth clustering.
#'
#' @details The adjusted Rand index (ARI) is a variant of the Rand index (RI)
#'   which is corrected for chance using the Permutation Model for
#'   clusterings. It is related to the RI as follows:
#'   \deqn{\frac{RI - E(RI)}{1 - E(RI)},}{(RI - E(RI))/(1 - E(RI)),}
#'   where \eqn{E(RI)} is the expected value of the RI under the Permutation
#'   Model.
#'   Unlike the RI, the ARI takes values in the range -1 to 1. A value
#'   of 1 indicates that the clusterings are identical, while a value of
#'   0 indicates the clusterings are drawn randomly independent of one
#'   another.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' adj_rand_index(true, pred)
#'
#' @references
#' Hubert, L., Arabie, P. "Comparing partitions." _Journal of Classification_
#' **2**, 193–218 (1985). \doi{10.1007/BF01908075}
#'
#' @export
adj_rand_index <- function(true, pred) {
  pair_ct <- pair_contingency_table_clusters(true, pred)
  adj_rand_index_ct(pair_ct)
}


#' Fowlkes-Mallows Index Between Clusterings
#'
#' @description Computes the Fowlkes-Mallows index between two clusterings,
#'    such as a predicted and ground truth clustering.
#'
#' @details The Fowlkes-Mallows index is defined as the geometric mean of
#'    precision and recall, computed with respect to pairs of elements.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#'
#' @references
#' Fowlkes, E. B. and Mallows, C. L. "A Method for Comparing Two Hierarchical
#' Clusterings." _Journal of the American Statistical Association_ **78:383**,
#' 553-569, (1983). \doi{10.1080/01621459.1983.10478008}
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' fowlkes_mallows(true, pred)
#'
#' @export
fowlkes_mallows <- function(true, pred) {
  ct <- contingency_table_clusters(true, pred)
  fowlkes_mallows_ct(ct)
}



#' Homogeneity Between Clusterings
#'
#' @description Computes the homogeneity between two clusterings, such
#'    as a predicted and ground truth clustering.
#'
#' @details Homogeneity is an entropy-based measure of the similarity
#'    between two clusterings, say \eqn{t} and \eqn{p}. The homogeneity
#'    is high if clustering \eqn{t} only assigns members of a cluster to
#'    a single cluster in \eqn{p}. The homogeneity ranges between 0
#'    and 1, where 1 indicates a perfect homogeneity.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#'
#' @references
#' Rosenberg, A. and Hirschberg, J. "V-measure: A conditional entropy-based external cluster evaluation measure." _Proceedings of the 2007 Joint Conference on Empirical Methods in Natural Language Processing and Computational Natural Language Learning_ (EMNLP-CoNLL), (2007).
#'
#' @seealso [`completeness`] evaluates the _completeness_, which is a dual
#' measure to _homogeneity_. [`v_measure`] evaluates the harmonic mean of
#' _completeness_ and _homogeneity_.
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' homogeneity(true, pred)
#'
#' @export
homogeneity <- function(true, pred) {
  ct <- contingency_table_clusters(true, pred)
  homogeneity_ct(ct)
}


#' Completeness Between Clusterings
#'
#' @description Computes the completeness between two clusterings, such
#'    as a predicted and ground truth clustering.
#'
#' @details Completeness is an entropy-based measure of the similarity
#'    between two clusterings, say \eqn{t} and \eqn{p}. The completeness
#'    is high if _all_ members of a given cluster in \eqn{t} are assigned
#'    to a single cluster in \eqn{p}. The completeness ranges between 0
#'    and 1, where 1 indicates perfect completeness.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#'
#' @references
#' Rosenberg, A. and Hirschberg, J. "V-measure: A conditional entropy-based external cluster evaluation measure." _Proceedings of the 2007 Joint Conference on Empirical Methods in Natural Language Processing and Computational Natural Language Learning_ (EMNLP-CoNLL), (2007).
#'
#' @seealso [`homogeneity`] evaluates the _homogeneity_, which is a dual
#' measure to _completeness_. [`v_measure`] evaluates the harmonic mean of
#' _completeness_ and _homogeneity_.
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' completeness(true, pred)
#'
#' @export
completeness <- function(true, pred) {
  ct <- contingency_table_clusters(true, pred)
  completeness_ct(ct)
}


#' V-measure Between Clusterings
#'
#' @description Computes the V-measure between two clusterings, such
#'    as a predicted and ground truth clustering.
#'
#' @details V-measure is defined as the \eqn{\beta}{β}-weighted harmonic
#'    mean of homogeneity \eqn{h} and completeness \eqn{c}:
#'    \deqn{(1 + \beta)\frac{h \cdot c}{\beta \cdot h + c}.}{(1 + β)·h·c/(β·h + c).}
#'    The range of V-measure is between 0 and 1, where 1 corresponds to a
#'    perfect match between the clusterings. It is equivalent to the
#'    normalised mutual information, when the aggregation function is the
#'    arithmetic mean.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#' @param beta non-negative weight. A value of 0 assigns no weight to
#'   completeness (i.e. the measure reduces to homogeneity), while larger
#'   values assign increasing weight to completeness. A value of 1 weights
#'   completeness and homogeneity equally.
#'
#' @references
#' Rosenberg, A. and Hirschberg, J. "V-measure: A conditional entropy-based external cluster evaluation measure." _Proceedings of the 2007 Joint Conference on Empirical Methods in Natural Language Processing and Computational Natural Language Learning_ (EMNLP-CoNLL), (2007).
#'
#' Becker, H. "Identification and characterization of events in social media."
#' _PhD dissertation_, Columbia University, (2011).
#'
#' @seealso [`homogeneity`] and [`completeness`] evaluate the component
#' measures upon which this measure is based.
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' v_measure(true, pred)
#'
#' @export
v_measure <- function(true, pred, beta=1) {
  ct <- contingency_table_clusters(true, pred)
  v_measure_ct(ct, beta=beta)
}


#' Variation of Information Between Clusterings
#'
#' @description Computes the variation of information between two
#'    clusterings, such as a predicted and ground truth clustering.
#'
#' @details Variation of information is an entropy-based distance metric
#'    on the space of clusterings. It is unnormalized and varies between
#'    \eqn{0} and \eqn{\log(N)}{log(N)} where \eqn{N} is the number of
#'    clustered elements. Larger values of the distance metric correspond
#'    to greater dissimilarity between the clusterings.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#' @param base base of the logarithm. Defaults to `exp(1)`.
#'
#' @references
#' Arabie, P. and Boorman, S. A. "Multidimensional scaling of measures of
#' distance between partitions." _Journal of Mathematical Psychology_ **10:2**,
#' 148-203, (1973). \doi{10.1016/0022-2496(73)90012-6}
#'
#' Meilă, M. "Comparing Clusterings by the Variation of Information." In:
#' Learning Theory and Kernel Machines, Lecture Notes in Computer Science
#' **2777**, Springer, Berlin, Heidelberg, (2003).
#' \doi{10.1007/978-3-540-45167-9_14}
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' variation_info(true, pred)
#'
#' @export
variation_info <- function(true, pred, base=exp(1)) {
  ct <- contingency_table_clusters(true, pred)
  variation_info_ct(ct, base=base)
}


#' Mutual Information Between Clusterings
#'
#' @description Computes the mutual information between two
#'    clusterings, such as a predicted and ground truth clustering.
#'
#' @details Mutual information is an entropy-based measure of the similarity
#'    between two clusterings.
#'
#' @param true ground truth clustering represented as a membership
#'    vector. Each entry corresponds to an element and the value identifies
#'    the assigned cluster. The specific values of the cluster identifiers
#'    are arbitrary.
#' @param pred predicted clustering represented as a membership
#'    vector.
#' @param base base of the logarithm. Defaults to `exp(1)`.
#'
#' @examples
#' true <- c(1,1,1,2,2)  # ground truth clustering
#' pred <- c(1,1,2,2,2)  # predicted clustering
#' mutual_info(true, pred)
#'
#' @export
mutual_info <- function(true, pred, base=exp(1)) {
  ct <- contingency_table_clusters(true, pred)
  mutual_info_ct(ct, base=base)
}


# Definition of clustering measures in terms of contingency tables
rand_index_ct <- function(pair_ct) {
  correct <- sum(diag(pair_ct))
  total <- sum(pair_ct)

  if (correct == total || total == 0)
    # Special cases: no clustering since the data is not split;
    # or trivial clustering where each item is assigned a unique
    # cluster. These are perfect matches hence return 1.0.
    return(1.0)

  return(correct / total)
}

adj_rand_index_ct <- function(pair_ct) {
  tp <- pair_ct["TRUE", "TRUE"]
  fp <- pair_ct["TRUE", "FALSE"]
  fn <- pair_ct["FALSE", "TRUE"]
  tn <- pair_ct["FALSE", "FALSE"]

  # Special cases: empty data or full agreement
  if (fn == 0 && fp == 0) return(1.0)

  return(2 * (tp * tn - fn * fp) /
           ((tp + fn) * (fn + tn) + (tp + fp) * (fp + tn)))
}


#' @param ct contingency table represented as a sparse matrix, specifically
#'   an object of S4 class [`Matrix::dgCMatrix-class`]
#' @importFrom Matrix colSums
#' @noRd
homogeneity_ct <- function(ct) {
  true_counts <- colSums(ct)
  entropy <- entropy_counts(true_counts)
  if (entropy == 0) return(1.0)
  mi <- mutual_info_ct(ct)
  mi / entropy
}


#' @param ct contingency table represented as a sparse matrix, specifically
#'   an object of S4 class [`Matrix::dgCMatrix-class`]
#' @importFrom Matrix rowSums
#' @noRd
completeness_ct <- function(ct) {
  pred_counts <- rowSums(ct)
  entropy <- entropy_counts(pred_counts)
  if (entropy == 0) return(1.0)
  mi <- mutual_info_ct(ct)
  mi / entropy
}


#' @param ct contingency table represented as a sparse matrix, specifically
#'   an object of S4 class [`Matrix::dgCMatrix-class`]
#' @importFrom Matrix rowSums colSums which
#' @noRd
fowlkes_mallows_ct <- function(ct) {
  n <- sum(ct)
  tk <- sum(ct^2) - n
  pk <- sum(rowSums(ct)^2) - n
  qk <- sum(colSums(ct)^2) - n
  ifelse(tk == 0, 0.0, sqrt(tk / pk) * sqrt(tk / qk))
}


#' @param ct contingency table represented as a sparse matrix, specifically
#'   an object of S4 class [`Matrix::dgCMatrix-class`]
#' @importFrom Matrix rowSums colSums which
#' @noRd
v_measure_ct <- function(ct, beta=1.0) {
  true_counts <- colSums(ct)
  pred_counts <- rowSums(ct)
  entropy_true <- entropy_counts(true_counts)
  entropy_pred <- entropy_counts(pred_counts)
  mi <- mutual_info_ct(ct)
  homogeneity <- ifelse(entropy_true==0, 1.0, mi / entropy_true)
  completeness <- ifelse(entropy_pred==0, 1.0, mi / entropy_pred)
  alpha <- 1/(1 + beta^2)
  1 / (alpha / homogeneity + (1 - alpha) / completeness)
}


#' @param ct contingency table represented as a sparse matrix, specifically
#'   an object of S4 class [`Matrix::dgCMatrix-class`]
#' @param base base of the logarithm. Defaults to `exp(1)`.
#' @importFrom Matrix rowSums colSums
#' @noRd
variation_info_ct <- function(ct, base=exp(1)) {
  # Get array indices of non-zero elements
  true_counts <- colSums(ct)
  pred_counts <- rowSums(ct)
  entropy_true <- entropy_counts(true_counts, base=base)
  entropy_pred <- entropy_counts(pred_counts, base=base)
  mi <- mutual_info_ct(ct, base=base)
  vi <- entropy_true + entropy_pred - 2 * mi
  ifelse(vi >= 0, vi, 0.0)
}


#' @param counts numeric vector of counts for categories
#' @param base base of the logarithm. Defaults to `exp(1)`.
#' @noRd
entropy_counts <- function(counts, base=exp(1)) {
  counts <- counts[counts > 0]
  total <- sum(counts)
  - sum(counts / total * (log(counts, base=base) - log(total, base=base)))
}


#' @param ct contingency table represented as a sparse matrix, specifically
#'   an object of S4 class [`Matrix::dgCMatrix-class`]
#' @param base base of the logarithm. Defaults to `exp(1)`.
#' @importFrom Matrix rowSums colSums which
#' @noRd
mutual_info_ct <- function(ct, base=exp(1)) {
  # Get array indices of non-zero elements
  nz_ind <- which(ct > 0, arr.ind = TRUE, useNames = FALSE)
  total <- sum(ct)
  row_totals <- rowSums(ct)
  col_totals <- colSums(ct)
  ct_nz <- ct@x                # non-zero entries
  ct_nz_norm <- ct_nz / total  # normalized non-zero entries
  log_ct_nz <- log(ct_nz, base=base)
  outer <- row_totals[nz_ind[,1]] * col_totals[nz_ind[,2]]
  log_outer <- - log(outer, base=base) + 2 * log(total, base=base)
  mi <- sum(ct_nz_norm * (log_ct_nz  - log(total, base=base)) + ct_nz_norm * log_outer)
  ifelse(mi >= 0, mi, 0.0)
}
