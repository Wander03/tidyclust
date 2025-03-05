make_predictions <- function(x, prefix, n_clusters) {
  levels <- seq_len(n_clusters)
  factor(x, levels = levels, labels = paste0(prefix, levels))
}

.k_means_predict_stats <- function(object, new_data, prefix = "Cluster_") {
  res <- object$centers
  res <- flexclust::dist2(res, new_data)
  res <- apply(res, 2, which.min)

  make_predictions(res, prefix, length(object$size))
}

.k_means_predict_ClusterR <- function(object, new_data, prefix = "Cluster_") {
  clusters <- predict(object, new_data)
  n_clusters <- length(object$obs_per_cluster)

  make_predictions(clusters, prefix, n_clusters)
}

.k_means_predict_clustMixType <- function(object, new_data, prefix = "Cluster_") {
  clusters <- predict(object, new_data)$cluster
  n_clusters <- length(object$size)

  make_predictions(clusters, prefix, n_clusters)
}

.k_means_predict_klaR <- function(object, new_data, prefix = "Cluster_",
                                  ties = c("first", "last", "random")) {
  ties <- rlang::arg_match(ties)

  modes <- object$modes
  n_modes <- nrow(modes)

  clusters <- integer(nrow(new_data))

  modes <- as.matrix(modes)
  new_data <- as.matrix(new_data)

  for (i in seq_along(clusters)) {
    misses <- rowSums(new_data[rep(i, n_modes), ] != modes)

    which_min <- which(misses == min(misses))


    if (length(which_min) == 1) {
      clusters[i] <- which_min
    } else {
      clusters[i] <- switch(
        ties,
        first = which_min[1],
        last = which_min[length(which_min)],
        random = sample(which_min, 1)
      )
    }
  }

  make_predictions(clusters, prefix, n_modes)
}

.hier_clust_predict_stats <- function(object, new_data, ..., prefix = "Cluster_") {
  linkage_method <- object$method

  new_data <- as.matrix(new_data)

  training_data <- as.matrix(attr(object, "training_data"))
  clusters <- extract_cluster_assignment(
    object,
    ...,
    prefix = prefix,
    call = call("predict")
  )

  if (linkage_method %in% c("single", "complete", "average", "median")) {
    ## complete, single, average, and median linkage_methods are basically the
    ## same idea, just different summary distance to cluster

    cluster_dist_fun <- switch(linkage_method,
      "single" = min,
      "complete" = max,
      "average" = mean,
      "median" = stats::median
    )

    # need this to be obs on rows, dist to new data on cols
    dists_new <- Rfast::dista(xnew = training_data, x = new_data, trans = TRUE)

    cluster_dists <- dplyr::bind_cols(data.frame(dists_new), clusters) %>%
      dplyr::group_by(.cluster) %>%
      dplyr::summarize_all(cluster_dist_fun)

    pred_clusts_num <- cluster_dists %>%
      dplyr::select(-.cluster) %>%
      map_dbl(which.min)
  } else if (linkage_method == "centroid") {
    ## Centroid linkage_method, dist to center

    cluster_centers <- extract_centroids(object) %>% dplyr::select(-.cluster)
    dists_means <- Rfast::dista(new_data, cluster_centers)

    pred_clusts_num <- apply(dists_means, 1, which.min)
  } else if (linkage_method %in% c("ward.D", "ward", "ward.D2")) {
    ## Ward linkage_method: lowest change in ESS
    ## dendrograms created from already-squared distances
    ## use Ward.D2 on these plain distances for Ward.D

    cluster_centers <- extract_centroids(object)
    n_clust <- nrow(cluster_centers)
    cluster_names <- cluster_centers[[1]]
    cluster_centers <- as.matrix(cluster_centers[, -1])

    d_means <- map(
      seq_len(n_clust),
      ~ t(
        t(training_data[clusters$.cluster == cluster_names[.x], ]) -
          cluster_centers[.x, ]
      )
    )

    d_new_list <- map(
      seq_len(nrow(new_data)),
      function(new_obs) {
        map(
          seq_len(n_clust),
          ~ t(t(training_data[clusters$.cluster == cluster_names[.x], ])
          - new_data[new_obs, ])
        )
      }
    )

    n <- nrow(training_data)

    change_in_ess <- map(
      d_new_list,
      function(v) {
        map2_dbl(
          d_means, v,
          ~ sum((n * .x + .y)^2 / (n + 1)^2 - .x^2)
        )
      }
    )

    pred_clusts_num <- map_dbl(change_in_ess, which.min)
  } else {
    rlang::abort(
      glue::glue(
        "linkage_method {linkage_method} is not supported for prediction."
      )
    )
  }
  pred_clusts <- unique(clusters$.cluster)[pred_clusts_num]

  pred_clusts
}

.freq_itemsets_predict_arules <- function(object, new_data, cutoff = 0.5, ..., prefix = "Cluster_") {
  new_data <- as.data.frame(new_data)

  # Extract frequent itemsets and their supports
  items <- attr(object, "item_names")
  itemsets <- arules::inspect(object)
  frequent_itemsets <- lapply(strsplit(gsub("[{}]", "", itemsets$items), ","), stringr::str_trim)
  supports <- itemsets$support

  # Transform the output dataframe format (make wide)
  result_list <- lapply(1:nrow(new_data), function(i) {
    row_data <- new_data[i, ]
    data.frame(
      item = items,
      prob = as.vector(as.matrix(row_data)),
      pred = is.na(as.vector(as.matrix(row_data)))
    )
  })

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

  # Apply cutoff to each dataframe in result_list
  result_list <- lapply(result_list, function(df) {
    row_data <- new_data[i, ]
    df$prob <- ifelse(df$pred & as.vector(as.matrix(row_data)) >= cutoff, 1, 0)
    df
  })

  return(result_list)
}
