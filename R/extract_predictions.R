#' Returns recommender predictions with predicted values imputed into dataset
#' Notes: currently imputes thresholded probabilities

extract_predictions <- function(pred_output) {

  result_df <- pred_output %>%
    purrr::map(~ {
      print(.x)
      .x %>%
        dplyr::mutate(value = ifelse(!is.na(.obs_item), .obs_item, .pred_item)) %>%
        dplyr::select(item, value) %>%
        tidyr::pivot_wider(names_from = item, values_from = value)
    }) %>%
    purrr::list_rbind()  # Combine the list of data frames into a single data frame

}

