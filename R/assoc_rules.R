#' Association Rules Mining
#'
#' @description
#'
#' `assoc_rules()` defines a model that finds association rules based on
#' specified minimum support and confidence.
#'
#' The method of estimation is chosen by setting the model engine. The
#' engine-specific pages for this model are listed below.
#'
#' - \link[=details_assoc_rules_stats]{arules}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "association".
#' @param engine A single character string specifying the computational engine
#'   to use for fitting. The default for this model is `"arules"`.
#' @param method A single character string specifying the algorithm to use for
#'   fitting. Possible algorithms are `"apriori"` and `"eclat"`. The default for
#'   this model is `"apriori"`.
#' @param min_support Positive double, minimum support for a rule (between 0 and 1).
#' @param min_confidence Positive double, minimum confidence for a rule (between 0 and 1).
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' WORK IN PROGRESS
#'
#' @return A `assoc_rules` association specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("assoc_rules")
#'
#' assoc_rules()
#' @export
assoc_rules <-
  function(mode = "partition",
           engine = "arules",
           method = "apriori",
           min_support = NULL,
           min_confidence = NULL) {
    args <- list(
      min_support = enquo(min_support),
      min_confidence = enquo(min_confidence)
    )

    new_cluster_spec(
      "assoc_rules",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.assoc_rules <- function(x, ...) {
  cat("Association Rules Mining Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update assoc_rules
#' @rdname tidyclust_update
#' @export
update.assoc_rules <- function(object,
                              parameters = NULL,
                              min_support = NULL,
                              min_confidence = NULL,
                              fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args,
    fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    min_support = enquo(min_support),
    min_confidence = enquo(min_confidence)
  )

  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args)) {
      args <- args[!null_args]
    }
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  new_cluster_spec(
    "assoc_rules",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# # ----------------------------------------------------------------------------

#' @export
check_args.assoc_rules <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$min_support)) && any(args$min_support >= 0) && any(args$min_support <= 1)) {
    rlang::abort("The minimum support should be between 0 and 1.")
  }

  if (all(is.numeric(args$min_confidence)) && any(args$min_confidence >= 0) && any(args$min_confidence <= 1)) {
    rlang::abort("The minimum confidence should be between 0 and 1.")
  }

  invisible(object)
}

#' @export
translate_tidyclust.assoc_rules <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around arules functions
#'
#' This wrapper prepares the data and parameters to send to either `arules::apriori`
#' or `arules::eclat` for association rules mining, depending on the chosen method.
#'
#' @param data A transaction data set.
#' @param support Minimum support threshold.
#' @param confidence Minimum confidence threshold.
#' @param mining_method Algorithm to use for mining frequent itemsets. Either "apriori" or "eclat".
#'
#' @return A set of association rules based on the specified parameters.
#' @keywords internal
#' @export
.assoc_rules_fit_arules <- function(x,
                                    support = NULL,
                                    confidence = NULL,
                                    mining_method = "apriori") {

  if (is.null(support)) {
    rlang::abort(
      "Please specify `min_support` to be able to fit specification.",
      call = call("fit")
    )
  }

  if (is.null(confidence)) {
    rlang::abort(
      "Please specify `min_confidence` to be able to fit specification.",
      call = call("fit")
    )
  }

  if (mining_method == "apriori") {
    res <- arules::apriori(data, parameter = list(support = support, confidence = confidence, target = "rules"))
  } else if (mining_method == "eclat") {
    # Run Eclat first to get frequent itemsets
    frequent_itemsets <- arules::eclat(data, parameter = list(support = support))
    # Generate association rules from frequent itemsets
    res <- arules::ruleInduction(frequent_itemsets, confidence = confidence, method = "ptree")
  } else {
    stop("Invalid engine specified. Choose 'apriori' or 'eclat'.")
  }

  attr(res, "items") <- colnames(data)
  return(res)
}
