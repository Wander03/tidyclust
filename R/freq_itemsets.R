#' Frequent Itemsets Mining
#'
#' @description
#'
#' `freq_itemsets()` defines a model that finds frequent itemsets based on
#' specified minimum support.
#'
#' The method of estimation is chosen by setting the model engine. The
#' engine-specific pages for this model are listed below.
#'
#' - \link[=details_freq_itemsets_arules]{arules}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "association".
#' @param engine A single character string specifying the computational engine
#'   to use for fitting. The default for this model is `"arules"`.
#' @param mining_method A single character string specifying the algorithm to use for
#'   fitting. Possible algorithms are `"apriori"` and `"eclat"`. The default for
#'   this model is `"apriori"`.
#' @param min_support Positive double, minimum support for an itemset (between 0 and 1).
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' WORK IN PROGRESS
#'
#' @return A `freq_itemsets` association specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("freq_itemsets")
#'
#' freq_itemsets()
#' @export
freq_itemsets <-
  function(mode = "partition", # will add other modes
           engine = "arules",
           min_support = NULL,
           mining_method = "apriori") {
    args <- list(
      min_support = enquo(min_support),
      mining_method = enquo(mining_method)
    )

    new_cluster_spec(
      "freq_itemsets",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.freq_itemsets <- function(x, ...) {
  cat("Frequent Itemsets Mining Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update freq_itemsets
#' @rdname tidyclust_update
#' @export
update.freq_itemsets <- function(object,
                                 parameters = NULL,
                                 min_support = NULL,
                                 mining_method = NULL,
                                 fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args,
    fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    min_support = enquo(min_support)
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
    "freq_itemsets",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# # ----------------------------------------------------------------------------

#' @export
check_args.freq_itemsets <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$min_support)) && any(args$min_support < 0) && any(args$min_support > 1)) {
    rlang::abort("The minimum support should be between 0 and 1.")
  }

  invisible(object)
}

#' @export
translate_tidyclust.freq_itemsets <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around arules functions
#'
#' This wrapper prepares the data and parameters to send to either `arules::apriori`
#' or `arules::eclat` for frequent itemsets mining, depending on the chosen method.
#'
#' @param data A transaction data set.
#' @param min_support Minimum support threshold.
#' @param mining_method Algorithm to use for mining frequent itemsets. Either "apriori" or "eclat".
#'
#' @return A set of frequent itemsets based on the specified parameters.
#' @keywords internal
#' @export
.freq_itemsets_fit_arules <- function(data,
                                      min_support = NULL,
                                      mining_method = "apriori") {
  if (mining_method == "apriori") {
    res <- arules::apriori(data, parameter = list(support = min_support, target = "frequent itemsets"))
  } else if (mining_method == "eclat") {
    res <- arules::eclat(data, parameter = list(support = min_support))
  } else {
    stop("Invalid method specified. Choose 'apriori' or 'eclat'.")
  }

  # attr(res, "items") <- data.frame(items = dimnames(data)[[2]])
  return(res)
}
