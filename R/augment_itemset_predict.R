#' Extract Predictions from Observation Data Frames
#'
#' This function processes a data frame containing observation data frames and extracts non-NA values.
#'
#' Returns recommender predictions with predicted values imputed into dataset
#' Notes: currently imputes thresholded probabilities
#'
#' @param pred_output A data frame with one column, where each cell contains a data frame.
#' @return A data frame with items as columns and non-NA values as rows.
#' @export

augment_itemset_predict <- function(pred_output, truth_output) {
  # Extract all predictions (bind all .pred_cluster dataframes)
  preds_df <- dplyr::bind_rows(pred_output$.pred_cluster, .id = "row_id") %>%
    dplyr::filter(!is.na(.pred_item)) %>%  # Keep only rows with predictions
    dplyr::mutate(item = stringr::str_remove_all(item, "`")) %>% # Remove backticks from item names
    dplyr::select(row_id, item, preds = .pred_item)  # Standardize column names

  # Pivot truth data to long format (to match predictions)
  truth_long <- truth_output %>%
    tibble::rownames_to_column("row_id") %>%
    tidyr::pivot_longer(
      cols = -row_id,
      names_to = "item",
      values_to = "truth_value"
    )

  # Join predictions with truth (inner join to keep only predicted items)
  result <- preds_df %>%
    dplyr::inner_join(truth_long, by = c("row_id", "item"))

  # Return simplified output (preds vs truth)
  dplyr::select(result, item, row_id, preds, truth = truth_value)
}





random_na_with_truth <- function(df, na_prob = 0.3) {
  # Create a copy of the original dataframe to store truth values
  truth_df <- df

  # Create a mask of NAs (TRUE = becomes NA)
  na_mask <- matrix(
    sample(
      c(TRUE, FALSE),
      size = nrow(df) * ncol(df),
      replace = TRUE,
      prob = c(na_prob, 1 - na_prob)
    ),
    nrow = nrow(df)
  )

  # Apply the mask to create NA values
  na_df <- df
  na_df[na_mask] <- NA

  # Return both the NA-filled dataframe and the truth
  list(
    na_data = na_df,
    truth = truth_df
  )
}


set.seed(123)
na_result <- random_na_with_truth(groceries[1:5,], na_prob = 0.3)
pred_output <- predict(fi_fit, na_result$na_data)
comparison_df <- augment_itemset_predict(pred_output, na_result$truth)

comparison_df %>%
  dplyr::mutate(truth = factor(truth, levels=c(0, 1)), preds = factor(preds, levels=c(0, 1))) %>%
  yardstick::accuracy(truth, preds)

#----------------------------------

set.seed(123)
na_result <- random_na_with_truth(groceries[1:5,], na_prob = 0.3)
pred_output <- predict(fi_fit, na_result$na_data, type = 'raw')
comparison_df <- augment_itemset_predict(pred_output, na_result$truth)

comparison_df %>%
  dplyr::mutate(truth = factor(truth, levels=c(0, 1))) %>%
  yardstick::pr_curve(truth, preds) %>%
  autoplot()


#----------------------------------

set.seed(123)
na_result <- random_na_with_truth(groceries[1:5,], na_prob = 0.3)
pred_output <- predict(fi_fit, na_result$na_data)
comparison_df <- augment_itemset_predict(pred_output, na_result$truth)

comparison_df %>%
  dplyr::mutate(truth = factor(truth, levels=c(0, 1)), preds = factor(preds, levels=c(0, 1))) %>%
  yardstick::roc_auc(truth, preds)
