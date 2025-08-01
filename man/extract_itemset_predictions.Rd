% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_itemset_predictions.R
\name{extract_itemset_predictions}
\alias{extract_itemset_predictions}
\title{Extract Predictions from Observation Data Frames}
\usage{
extract_itemset_predictions(pred_output)
}
\arguments{
\item{pred_output}{A data frame with one column, where each cell contains a data frame.}
}
\value{
A data frame with items as columns and non-NA values as rows.
}
\description{
This function processes a data frame containing observation data frames and extracts non-NA values.
}
\details{
Returns recommender predictions with predicted values imputed into dataset
Notes: currently imputes thresholded probabilities
}
\examples{
toy_df <- data.frame(
"beer"    = c(FALSE, TRUE, TRUE, TRUE, FALSE),
"milk"    = c(TRUE, FALSE, TRUE, TRUE, TRUE),
"bread"   = c(TRUE, TRUE, FALSE, TRUE, TRUE),
"diapers" = c(TRUE, TRUE, TRUE, TRUE, TRUE),
"eggs"    = c(FALSE, TRUE, FALSE, FALSE, FALSE)
)

new_data <- data.frame(
"beer"    = NA,
"milk"    = TRUE,
"bread"   = TRUE,
"diapers" = TRUE,
"eggs"    = FALSE
)

fi_spec <- freq_itemsets(
 min_support = 0.05,
 mining_method = "eclat"
 ) |>
 set_engine("arules") |>
 set_mode("partition")

fi_fit <- fi_spec |>
 fit(~ .,
    data = toy_df
 )

fi_fit |>
 predict(new_data) |>
 extract_itemset_predictions()

}
