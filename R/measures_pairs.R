#' @include transformations.R
NULL

#' Binary Contingency Table for Linked Pairs
#'
#' @description Compute the binary contingency table for a set of _predicted_
#'   coreferent (linked) pairs given a set of _ground truth_ coreferent pairs.
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param num_pairs the total number of coreferent and non-coreferent pairs,
#'   excluding equivalent pairs with reversed ids. If not provided,
#'   the true negative cell will be set to `NA`.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#' @return Returns a \eqn{2 \times 2}{2×2} contingency table of the form:
#' \preformatted{
#'              Truth
#'    Prediction   TRUE  FALSE
#'         TRUE      TP     FP
#'         FALSE     FN     TN
#' }
#'
#' @seealso
#' The [`membership_to_pairs`] and [`clusters_to_pairs`] functions can be
#' used to transform other clustering representations into lists of pairs,
#' as required by this function.
#' The [`eval_report_pairs`] function computes common evaluation measures
#' derived from binary contingency matrices, like the ones output by this
#' function.
#'
#' @examples
#' ### Example where pairs/edges are undirected
#' # ground truth is 3-clique
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3))
#' # prediction misses one edge
#' pred_pairs <- rbind(c(1,2), c(2,3))
#' # total number of pairs assuming 3 elements
#' num_pairs <- 3 * (3 - 1) / 2
#' eval_report_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' ### Example where pairs/edges are directed
#' # ground truth is a 3-star
#' true_pairs <- rbind(c(2,1), c(3,1), c(4,1))
#' # prediction gets direction of one edge incorrect
#' pred_pairs <- rbind(c(2,1), c(3,1), c(1,4))
#' # total number of pairs assuming 4 elements
#' num_pairs <- 4 * 4
#' eval_report_pairs(true_pairs, pred_pairs, num_pairs, ordered = TRUE)
#'
#' @export
contingency_table_pairs <- function(true_pairs, pred_pairs, num_pairs=NULL, ordered=FALSE) {
  if (!is.null(num_pairs)) {
    if (length(num_pairs) != 1 | num_pairs <= 0)
      stop("num_pairs must be a positive scalar or NULL")
  }

  # Binding pairs ensures that they are coerced to the same type
  comb_pairs <- rbind(true_pairs, pred_pairs)
  true_pairs <- comb_pairs[seq_len(nrow(true_pairs)),]
  pred_pairs <- comb_pairs[nrow(true_pairs) + seq_len(nrow(pred_pairs)),]

  # Canonicalize pairs
  pred_pairs <- as.data.frame(canonicalize_pairs(pred_pairs, ordered = ordered))
  true_pairs <- as.data.frame(canonicalize_pairs(true_pairs, ordered = ordered))

  # Standardize column names
  colnames(pred_pairs) <- c("ID.x", "ID.y")
  colnames(true_pairs) <- c("ID.x", "ID.y")

  # Allow for empty data frames
  pred_pairs[["PRED_MATCH"]] <- rep(TRUE, times=nrow(pred_pairs))
  true_pairs[["MATCH"]] <- rep(TRUE, times=nrow(true_pairs))

  # Perform a full outer join on the two data frames.
  merged_pairs <- merge(pred_pairs, true_pairs, by=c("ID.x", "ID.y"), all=TRUE)

  # An NA in PRED_MATCH or MATCH represents 'FALSE'
  merged_pairs$PRED_MATCH[is.na(merged_pairs$PRED_MATCH)] <- FALSE
  merged_pairs$MATCH[is.na(merged_pairs$MATCH)] <- FALSE

  # Convert to factors so we can use built-in table function
  prediction = factor(merged_pairs$PRED_MATCH, levels = c(TRUE, FALSE))
  truth = factor(merged_pairs$MATCH, levels = c(TRUE, FALSE))

  ct <- table(prediction, truth, dnn = c("Prediction", "Truth"))

  if (is.null(num_pairs)) {
    ct[2,2] <- NA # number of true negatives is unknown since links are incomplete
  } else {
    ct[2,2] <- num_pairs - nrow(merged_pairs)
  }

  return(ct)
}


#' Evaluation Report for Linked Pairs
#'
#' @description Compute various evaluation measures for a set of _predicted_
#'   coreferent (linked) pairs given a set of _ground truth_ coreferent pairs.
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param num_pairs the total number of coreferent and non-coreferent pairs,
#'   excluding equivalent pairs with reversed ids. If not provided,
#'   measures that depend on the number of true negatives will be returned
#'   as `NA`.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#'
#' @return Returns a list containing the following measures:
#'   \describe{
#'     \item{precision}{see [`precision_pairs`]}
#'     \item{recall}{see [`recall_pairs`]}
#'     \item{specificity}{see [`specificity_pairs`]}
#'     \item{sensitivity}{see [`sensitivity_pairs`]}
#'     \item{f1score}{see [`f_measure_pairs`]}
#'     \item{accuracy}{see [`accuracy_pairs`]}
#'     \item{balanced_accuracy}{see [`balanced_accuracy_pairs`]}
#'     \item{fowlkes_mallows}{see [`fowlkes_mallows_pairs`]}
#'   }
#'
#' @seealso The [`contingency_table_pairs`] function can be used to compute
#'   the contingency table for entity resolution or record linkage problems.
#'
#' @examples
#' ### Example where pairs/edges are undirected
#' # ground truth is 3-clique
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3))
#' # prediction misses one edge
#' pred_pairs <- rbind(c(1,2), c(2,3))
#' # total number of pairs assuming 3 elements
#' num_pairs <- 3 * (3 - 1) / 2
#' eval_report_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' ### Example where pairs/edges are directed
#' # ground truth is a 3-star
#' true_pairs <- rbind(c(2,1), c(3,1), c(4,1))
#' # prediction gets direction of one edge incorrect
#' pred_pairs <- rbind(c(2,1), c(3,1), c(1,4))
#' # total number of pairs assuming 4 elements
#' num_pairs <- 4 * 4
#' eval_report_pairs(true_pairs, pred_pairs, num_pairs, ordered = TRUE)
#'
#' @export
eval_report_pairs <- function(true_pairs, pred_pairs, num_pairs = NULL, ordered=FALSE)
{
  ct <- contingency_table_pairs(true_pairs, pred_pairs, num_pairs = num_pairs, ordered = ordered)
  list("precision" = precision_pairs_ct(ct),
       "recall" = recall_pairs_ct(ct),
       "specificity" = specificity_pairs_ct(ct),
       "sensitivity" = recall_pairs_ct(ct),
       "f1score" = f_measure_pairs_ct(ct),
       "accuracy" = accuracy_pairs_ct(ct),
       "balanced_accuracy" = balanced_accuracy_pairs_ct(ct))
}


#' Precision of Linked Pairs
#'
#' @description Computes the precision of a set of _predicted_ coreferent
#'   (linked) pairs given a set of _ground truth_ coreferent pairs.
#'
#' @details The precision is defined as:
#'   \deqn{\frac{|T \cap P|}{|P|}}{|T ∩ P|/|P|}
#'   where \eqn{T} is the set of true coreferent pairs and \eqn{P} is the
#'   set of predicted coreferent pairs.
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#'
#' @examples
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3)) # ground truth is 3-clique
#' pred_pairs <- rbind(c(1,2), c(2,3))         # prediction misses one edge
#' num_pairs <- 3                              # assuming 3 elements
#' precision_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' @export
precision_pairs <- function(true_pairs, pred_pairs, ordered=FALSE) {
  ct <- contingency_table_pairs(true_pairs, pred_pairs, ordered = ordered)
  precision_pairs_ct(ct)
}


#' Recall of Linked Pairs
#'
#' @description Computes the precision of a set of _predicted_ coreferent
#'   (linked) pairs given a set of _ground truth_ coreferent pairs.
#'
#' @details The recall is defined as:
#'   \deqn{\frac{|T \cap P|}{|T|}}{|T ∩ P|/|T|}
#'   where \eqn{T} is the set of true coreferent pairs and \eqn{P} is the
#'   set of predicted coreferent pairs.
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#'
#' @examples
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3)) # ground truth is 3-clique
#' pred_pairs <- rbind(c(1,2), c(2,3))         # prediction misses one edge
#' num_pairs <- 3                              # assuming 3 elements
#' recall_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' @rdname recall_pairs
#' @export
recall_pairs <- function(true_pairs, pred_pairs, ordered=FALSE) {
  ct <- contingency_table_pairs(true_pairs, pred_pairs, ordered = ordered)
  recall_pairs_ct(ct)
}


#' @note `sensitivity_pairs` is an alias for `recall_pairs`.
#'
#' @rdname recall_pairs
#' @export
sensitivity_pairs <- function(true_pairs, pred_pairs, ordered=FALSE) {
  recall_pairs(true_pairs, pred_pairs)
}


#' F-measure of Linked Pairs
#'
#' @description Computes the F-measure (a.k.a. F-score) of a set of
#'   _predicted_ coreferent (linked) pairs given a set of _ground truth_
#'   coreferent pairs.
#'
#' @details The \eqn{\beta}{β}-weighted F-measure is defined as the weighted
#'   harmonic mean of precision \eqn{P} and recall \eqn{R}:
#'   \deqn{(1 + \beta^2)\frac{P \cdot R}{\beta^2 \cdot P + R}.}{(1 + β^2)·P·R/(β^2·P + R).}
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param beta non-negative weight. A value of 0 assigns no weight to recall
#'   (i.e. the measure reduces to precision), while larger values assign
#'   increasing weight to recall. A value of 1 weights precision and recall
#'   equally.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#'
#' @references
#' Van Rijsbergen, C. J. "Information Retrieval." (2nd ed.).
#' Butterworth-Heinemann, USA, (1979).
#'
#' @examples
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3)) # ground truth is 3-clique
#' pred_pairs <- rbind(c(1,2), c(2,3))         # prediction misses one edge
#' num_pairs <- 3                              # assuming 3 elements
#' f_measure_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' @export
f_measure_pairs <- function(true_pairs, pred_pairs, beta=1, ordered=FALSE) {
  ct <- contingency_table_pairs(true_pairs, pred_pairs, ordered = ordered)
  f_measure_pairs_ct(ct, beta)
}


#' Specificity of Linked Pairs
#'
#' @description Computes the specificity of a set of _predicted_ coreferent
#'   (linked) pairs given a set of _ground truth_ coreferent pairs.
#'
#' @details The specificity is defined as:
#'   \deqn{\frac{|P' \cap T'|}{|P'|}}{|P' ∩ T'|/|P'|}
#'   where \eqn{T'} is the set of true non-coreferent pairs, \eqn{P} is the
#'   set of predicted non-coreferent pairs.
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param num_pairs the total number of coreferent and non-coreferent pairs,
#'   excluding equivalent pairs with reversed ids.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#'
#' @examples
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3)) # ground truth is 3-clique
#' pred_pairs <- rbind(c(1,2), c(2,3))         # prediction misses one edge
#' num_pairs <- 3                              # assuming 3 elements
#' specificity_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' @export
specificity_pairs <- function(true_pairs, pred_pairs, num_pairs, ordered=FALSE) {
  ct <- contingency_table_pairs(true_pairs, pred_pairs, num_pairs = num_pairs, ordered = ordered)
  specificity_pairs_ct(ct)
}


#' Accuracy of Linked Pairs
#'
#' @description Computes the accuracy of a set of _predicted_ coreferent
#'   (linked) pairs given a set of _ground truth_ coreferent pairs.
#'
#' @details The accuracy is defined as:
#'   \deqn{\frac{|T \cap P| + |T' \cap P'|}{N}}{(|T ∩ P| + |T' ∩ P'|)/N}
#'   where:
#'   * \eqn{T} is the set of true coreferent pairs,
#'   * \eqn{P} is the set of predicted coreferent pairs,
#'   * \eqn{T'} is the set of true non-coreferent pairs,
#'   * \eqn{P'} is the set of predicted non-coreferent pairs, and
#'   * \eqn{N} is the total number of coreferent and non-coreferent pairs.
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param num_pairs the total number of coreferent and non-coreferent pairs,
#'   excluding equivalent pairs with reversed ids.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#'
#' @examples
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3)) # ground truth is 3-clique
#' pred_pairs <- rbind(c(1,2), c(2,3))         # prediction misses one edge
#' num_pairs <- 3                              # assuming 3 elements
#' accuracy_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' @export
accuracy_pairs <- function(true_pairs, pred_pairs, num_pairs, ordered=FALSE) {
  ct <- contingency_table_pairs(true_pairs, pred_pairs, num_pairs = num_pairs, ordered = ordered)
  accuracy_pairs_ct(ct)
}


#' Balanced Accuracy of Linked Pairs
#'
#' @description Computes the balanced accuracy of a set of _predicted_
#'   coreferent (linked) pairs given a set of _ground truth_ coreferent
#'   pairs.
#'
#' @details The balanced accuracy is defined as:
#'   \deqn{\frac{\frac{|T \cap P|}{|P|} + \frac{|T' \cap P'|}{|P'|}}{2}}{|T ∩ P|/(2|P|) + |T' ∩ P'|/(2|P'|)}
#'   where:
#'   * \eqn{T} is the set of true coreferent pairs,
#'   * \eqn{P} is the set of predicted coreferent pairs,
#'   * \eqn{T'} is the set of true non-coreferent pairs, and
#'   * \eqn{P'} is the set of predicted non-coreferent pairs.
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param num_pairs the total number of coreferent and non-coreferent pairs,
#'   excluding equivalent pairs with reversed ids.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#'
#' @examples
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3)) # ground truth is 3-clique
#' pred_pairs <- rbind(c(1,2), c(2,3))         # prediction misses one edge
#' num_pairs <- 3                              # assuming 3 elements
#' balanced_accuracy_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' @export
balanced_accuracy_pairs <- function(true_pairs, pred_pairs, num_pairs, ordered=FALSE) {
  ct <- contingency_table_pairs(true_pairs, pred_pairs, num_pairs = num_pairs, ordered = ordered)
  balanced_accuracy_pairs_ct(ct)
}


#' Fowlkes-Mallows Index of Linked Pairs
#'
#' @description Computes the Fowlkes-Mallows index for a set of _predicted_
#'   coreferent (linked) pairs given a set of _ground truth_ coreferent pairs.
#'
#' @details The Fowlkes-Mallows index is defined as the geometric mean of
#'   precision \eqn{P} and recall \eqn{R}:
#'   \deqn{\sqrt{P R}.}{√(P·R).}
#'
#' @param true_pairs set of true coreferent pairs stored in a matrix or
#'   data.frame, where rows index pairs and columns index the ids of the
#'   constituents. Any pairs not included are assumed to be _non-coreferent_.
#'   Duplicate pairs (including equivalent pairs with reversed ids) are
#'   automatically removed.
#' @param pred_pairs set of predicted coreferent pairs, following the same
#'   specification as `true_pairs`.
#' @param ordered whether to treat the element pairs as ordered---i.e. whether
#'   pair \eqn{(x, y)} is distinct from pair \eqn{(y, x)} for \eqn{x \neq y}.
#'   Defaults to FALSE, which is appropriate for clustering, undirected link
#'   prediction, record linkage etc.
#'
#' @references
#' Fowlkes, E. B. and Mallows, C. L. "A Method for Comparing Two Hierarchical
#' Clusterings." _Journal of the American Statistical Association_ **78:383**,
#' 553-569, (1983). \doi{10.1080/01621459.1983.10478008}.
#'
#' @examples
#' true_pairs <- rbind(c(1,2), c(2,3), c(1,3)) # ground truth is 3-clique
#' pred_pairs <- rbind(c(1,2), c(2,3))         # prediction misses one edge
#' num_pairs <- 3                              # assuming 3 elements
#' fowlkes_mallows_pairs(true_pairs, pred_pairs, num_pairs)
#'
#' @export
fowlkes_mallows_pairs <- function(true_pairs, pred_pairs, ordered=FALSE) {
  ct <- contingency_table_pairs(true_pairs, pred_pairs, ordered = ordered)
  fowlkes_mallows_pairs_ct(ct)
}


# Definition of measures in terms of contingency table
precision_pairs_ct <- function(ct) {
  tp <- ct["TRUE", "TRUE"]
  fp <- ct["TRUE", "FALSE"]
  pp <- tp + fp
  return(tp / pp)
}

recall_pairs_ct <- function(ct) {
  tp <- ct["TRUE", "TRUE"]
  fn <- ct["FALSE", "TRUE"]
  p <- tp + fn
  return(tp / p)
}

f_measure_pairs_ct <- function(ct, beta=1.0) {
  if (beta < 0)
    stop("`beta` must be non-negative")
  P <- precision_pairs_ct(ct)
  R <- recall_pairs_ct(ct)
  alpha <- 1/(1 + beta^2)
  1 / (alpha / P + (1 - alpha) / R)
}

specificity_pairs_ct <- function(ct) {
  fp <- ct["TRUE", "FALSE"]
  tn <- ct["FALSE", "FALSE"]
  n <- tn + fp
  tn / n
}

accuracy_pairs_ct <- function(ct) {
  tp <- ct["TRUE", "TRUE"]
  fp <- ct["TRUE", "FALSE"]
  fn <- ct["FALSE", "TRUE"]
  tn <- ct["FALSE", "FALSE"]
  correct <- tp + tn
  total <- tp + fp + tn + fn
  correct/total
}

balanced_accuracy_pairs_ct <- function(ct) {
  sensitivity <- recall_pairs_ct(ct)
  specificity <- specificity_pairs_ct(ct)
  (sensitivity + specificity) / 2
}

fowlkes_mallows_pairs_ct <- function(ct) {
  P <- precision_pairs_ct(ct)
  R <- recall_pairs_ct(ct)
  sqrt(P) * sqrt(R)
}
