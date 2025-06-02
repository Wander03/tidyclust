toy_df <- data.frame(
  'beer'    = c(F, T, T, T, F),
  'milk'    = c(T, F, T, T, T),
  'bread'   = c(T, T, F, T, T),
  'diapers' = c(T, T, T, T, T),
  'eggs'    = c(F, T, F, F, F)
)

toy_pred <- data.frame(
  'beer'    = c(F),
  'milk'    = c(NA),
  'bread'   = c(T),
  'diapers' = c(T),
  'eggs'    = c(F)
)

test_that("fitting", {
  set.seed(1234)
  spec <- freq_itemsets(min_support = 0.5) %>%
    set_engine("arules")

  expect_no_error(
    res <- fit(spec, ~., toy_df)
  )
})

test_that("predicting", {
  set.seed(1234)
  spec <- freq_itemsets(min_support = 0.5) %>%
    set_engine("arules")

  res <- fit(spec, ~., toy_df)

  preds <- predict(res, toy_pred)$.pred_cluster[[1]]$.pred_item

  expect_identical(
    preds,
    c(NA, 1, NA, NA, NA)
  )
})

test_that("extract_centroids works", {
  set.seed(1234)
  fi_fit <- freq_itemsets(min_support = 0.5) %>%
    set_engine("arules") %>%
    fit(~., toy_df %>% dplyr::mutate(across(everything(), as.numeric)))

  expect_snapshot(error = TRUE, extract_centroids(fi_fit))
})

test_that("extract_cluster_assignment() works", {
  set.seed(1234)
  fi_fit <- freq_itemsets(min_support = 0.5, mining_method = "eclat") %>%
    set_engine("arules") %>%
    fit(~., toy_df %>% dplyr::mutate(across(everything(), as.numeric)))

  set.seed(1234)
  ref_res <- arules::eclat(data = toy_df,
                           parameter = list(support = 0.5),
                           control = list(verbose = FALSE))

  ref_itemsets <- arules::DATAFRAME(ref_res)
  ref_clusts <- c(1, 2, 2, 2, 0)
  ref_outliers <- "eggs"

  expect_equal(
    arules::DATAFRAME(fi_fit$fit),
    ref_itemsets
  )

  expect_equal(
    ref_clusts,
    extract_cluster_assignment(fi_fit)$.cluster %>% as.numeric() - 1
  )
})
