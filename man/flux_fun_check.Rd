% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flux_fun_check.R
\name{flux_fun_check}
\alias{flux_fun_check}
\title{checking that arguments and columns are in the correct class}
\usage{
flux_fun_check(args, fn, msg, origdf = NA)
}
\arguments{
\item{args}{list of arguments or dataframe to check}

\item{fn}{list of functions used to check (\code{is.numeric}, \code{is.character}, ...)}

\item{msg}{list of messages to return in case of failed check}

\item{origdf}{in case args is a df with selected columns to check origdf
is the orginal df to take the name from for a more obvious error message}
}
\description{
checking that arguments and columns are in the correct class
}
\author{
Adam Klimes
}
