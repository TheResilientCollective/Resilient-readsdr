% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_xmile_constants.R
\name{merge_xmile_constants}
\alias{merge_xmile_constants}
\title{Read only the constants from an XMILE file into R}
\usage{
merge_xmile_constants(xmile, constants)
}
\arguments{
\item{xmile}{The original xml object from which parameters were extracted.}

\item{constants}{The modified dataframe of constants.}
}
\value{
Modified xmile object with the constant values replaced.
}
\description{
\code{merge_xmile_constants} inserts constant parameter values into xmile
}
\details{
Paired with \code{\link{read_xmile_constants}}, this function is designed
to merge changed constant parameter values back into the original xmile.

\emph{NOTE} that this method modifies the original xmile in place, and also
returns the object.
}
\seealso{
Other Constant Parameter Manipulators: 
\code{\link{read_xmile_constants}()}
}
\concept{Constant Parameter Manipulators}
