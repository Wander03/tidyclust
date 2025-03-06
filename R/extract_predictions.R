#' Returns recommender predictions with predicted values imputed into dataset
#' Notes: currently imputes raw probabilities

extract_predictions <- function(object, new_data, cutoff = 0.5, ..., prefix = "Cluster_") {
  new_data <- as.data.frame(new_data)

  # Extract frequent itemsets and their supports
  items <- attr(object, "item_names")
  itemsets <- arules::inspect(object)
  frequent_itemsets <- lapply(strsplit(gsub("[{}]", "", itemsets$items), ","), stringr::str_trim)
  supports <- itemsets$support

  for (i in seq_len(nrow(new_data))) {
    observed_items <- colnames(new_data)[which(new_data[i, ] == 1)]
    missing_items <- colnames(new_data)[which(is.na(new_data[i, ]))]

    for (item in missing_items) {
      # Find relevant itemsets and supports
      relevant_indices <- which(sapply(frequent_itemsets, function(x) item %in% x && any(observed_items %in% x)))
      relevant_itemsets <- frequent_itemsets[relevant_indices]
      relevant_supports <- supports[relevant_indices]

      # Compute confidence for each relevant itemset
      probabilities <- sapply(seq_along(relevant_itemsets), function(idx) {
        itemset <- relevant_itemsets[[idx]]
        itemset_without_item <- setdiff(itemset, item)

        # Find support values using indices
        support_full <- relevant_supports[idx]
        support_without <- supports[which(sapply(frequent_itemsets, function(x) identical(x, itemset_without_item)))]

        if (length(support_without) > 0) {
          return(support_full / support_without[1])
        } else {
          return(NA)
        }
      }, USE.NAMES = FALSE)

      # Aggregate probabilities (using mean)
      prob_estimate <- ifelse(length(na.omit(probabilities)) > 0, mean(na.omit(probabilities)), NA)

      # Replace NA with probability estimate
      new_data[i, item] <- prob_estimate
    }
  }

  return(new_data)
}
