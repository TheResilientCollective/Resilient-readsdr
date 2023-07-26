#' Read only the constants from an XMILE file into R
#'
#'\code{merge_xmile_constants} inserts constant parameter values into xmile
#'
#' Paired with \code{\link{read_xmile_constants}}, this function is designed
#' to merge changed constant parameter values back into the original xmile.
#'
#' \emph{NOTE} that this method modifies the original xmile in place, and also
#' returns the object.
#'
#' @family Constant Parameter Manipulators
#'
#' @param xmile The original xml object from which parameters were extracted.
#' @param constants The modified dataframe of constants.
#' @returns Modified xmile object with the constant values replaced.
#'
#' @export
#'
merge_xmile_constants <- function(xmile, constants) {
  constants %>%
    split(1:nrow(constants)) %>%
    purrr::map(modify_one_constant_node, xmile)

  return(xmile)
}
