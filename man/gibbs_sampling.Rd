% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{gibbs_sampling}
\alias{gibbs_sampling}
\alias{gibbs_sampling_iter}
\title{Gibbs sampling iteration}
\usage{
gibbs_sampling_iter(last_sample, sigma, mu)
}
\arguments{
\item{last_sample}{A numeric vector representing the last sample}

\item{sigma}{Covariance matrix for the bivariate normal distribution}

\item{mu}{Mean vector for the bivariate normal distribution}
}
\value{
A numeric vector representing the new sample
}
\description{
Perform one iteration of Gibbs sampling
}
