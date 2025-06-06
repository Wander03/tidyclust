#' Cut Height
#'
#' Used in most `tidyclust::hier_clust()` models.
#'
#' @inheritParams dials::Laplace
#' @examples
#' cut_height()
#' @export
cut_height <- function(range = c(0, dials::unknown()), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(cut_height = "Cut Height"),
    finalize = NULL
  )
}

#' The agglomeration Linkage method
#'
#' @param values A character string of possible values. See `linkage_methods`
#'  in examples below.
#'
#' @details
#' This parameter is used in `tidyclust` models for `hier_clust()`.
#' @examples
#' values_linkage_method
#' linkage_method()
#' @export
linkage_method <- function(values = values_linkage_method) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(activation = "Linkage Method"),
    finalize = NULL
  )
}

#' @rdname linkage_method
#' @export
values_linkage_method <- c(
  "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median",
  "centroid"
)

#' Min Support
#'
#' Used in all `tidyclust::freq_itemsets()` models.
#'
#' @inheritParams dials::Laplace
#' @examples
#' min_support()
#' @export
min_support <- function(range = c(0.1, 0.5), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(min_support = "Minimum Support Value"),
    finalize = NULL # Add to look at data and create a CI-like range around some min support value
  )
}
