% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_xT.R
\name{compute_action_prob}
\alias{compute_action_prob}
\title{Computes bin-wise probabilites for moving ball/shooting/scoring}
\usage{
compute_action_prob(df, bins_x = 16, bins_y = 12)
}
\arguments{
\item{df}{Event data set in SPADL format with bin memberships for coordinates,
as returned by \link{prepare_data}.}

\item{bins_x}{Number of bins in \code{x} direction}

\item{bins_y}{Number of bins in \code{x} direction}
}
\value{
A tibble with a probability of shooting, scoring and moving the ball
at a given bin on the grid, for all possible combinations of bins.
}
\description{
Computes bin-wise probabilites for moving ball/shooting/scoring
}
\examples{
data(spadlData)
prep_dat <- prepare_data(spadlData)
compute_action_prob(prep_dat$prep_data)
}
