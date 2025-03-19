#' Extract Predictions from Observation Data Frames
#'
#' This function processes a data frame containing observation data frames and extracts non-NA values.
#'
#' @param pred_output A data frame with one column, where each cell contains a data frame.
#' @return A data frame with items as columns and non-NA values as rows.
#' @export
#' Returns recommender predictions with predicted values imputed into dataset
#' Notes: currently imputes thresholded probabilities

extract_predictions <- function(pred_output) {
  # Extract the list of data frames from the single column
  data_frames <- pred_output$.pred_cluster

  # Process each observation and combine results using reduce
  result_df <- data_frames %>%
    purrr::reduce(.f = ~ {
      # Process each observation (data frame)
      processed <- .y %>%
        dplyr::mutate(value = ifelse(!is.na(.obs_item), .obs_item, .pred_item)) %>%
        dplyr::select(item, value) %>%
        tidyr::pivot_wider(names_from = item, values_from = value)

      # Combine the processed data frame with the accumulated results
      dplyr::bind_rows(.x, processed)
    }, .init = NULL)

  return(result_df)
}
