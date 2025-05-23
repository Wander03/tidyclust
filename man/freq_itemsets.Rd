% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq_itemsets.R
\name{freq_itemsets}
\alias{freq_itemsets}
\title{Frequent Itemsets Mining}
\usage{
freq_itemsets(
  mode = "partition",
  engine = "arules",
  min_support = NULL,
  mining_method = "eclat"
)
}
\arguments{
\item{mode}{A single character string for the type of model. The only
possible value for this model is "partition".}

\item{engine}{A single character string specifying the computational engine
to use for fitting. The default for this model is \code{"arules"}.}

\item{min_support}{Positive double, minimum support for an itemset (between 0 and 1).}

\item{mining_method}{A single character string specifying the algorithm to use for
fitting. Possible algorithms are \code{"apriori"} and \code{"eclat"}. The default for
this model is \code{"eclat"}.}
}
\value{
A \code{freq_itemsets} association specification.
}
\description{
\code{freq_itemsets()} defines a model that finds frequent itemsets based on
specified minimum support.

The method of estimation is chosen by setting the model engine. The
engine-specific pages for this model are listed below.
\itemize{
\item \link[=details_freq_itemsets_arules]{arules}
}
}
\details{
\subsection{What does it mean to predict?}{

WORK IN PROGRESS
}
}
\examples{
# Show all engines
modelenv::get_from_env("freq_itemsets")

freq_itemsets()
}
