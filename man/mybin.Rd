% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mybin.R
\name{mybin}
\alias{mybin}
\title{Simulate binomial trials and plot results}
\usage{
mybin(iter = 100, n = 10, p = 0.5)
}
\arguments{
\item{iter}{Number of iterations to simulate (default = 100)}

\item{n}{Number of trials per iteration (default = 10)}

\item{p}{Probability of success for each trial (default = 0.5)}
}
\value{
A table of relative frequencies for number of successes, normalized by iterations
}
\description{
Simulate binomial trials and plot results
}
\examples{
# Simulate 100 iterations of 10 trials with p=0.5
mybin()

# Simulate with different parameters
mybin(iter=1000, n=20, p=0.3)
}
