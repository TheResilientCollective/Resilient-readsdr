% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xmile_helpers.R
\name{modify_one_constant_node}
\alias{modify_one_constant_node}
\title{Modify one constant \code{<eqn>} (node with numeric value)}
\usage{
modify_one_constant_node(new_row, xmile)
}
\arguments{
\item{new_row}{Row in the modified dataframe from which to take values.}

\item{xmile}{Full xmile object to be modified.}
}
\description{
Find constant in xmile, matched by \code{name}, \code{dimensions}, and
\code{elements}.  Modify \code{value} field to match input row.
}
\details{
\emph{WARNING} As above, this does not handle multi dimensional arrays or
integer indexed arrays.
}
