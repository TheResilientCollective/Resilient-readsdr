#' Read only the constants from an XMILE file into R
#'
#'\code{read_xmile_constants} parses xmile file and returns constant terms
#'
#' This lightweight version of the \code{\link{read_xmile}} function extracts
#' just the constant terms from the xml from the file specified via
#' \code{filepath}.  Returned dataframe may be manipulated and then merged back
#' to the xmile file via \code{\link{write_xmile_constants}}.
#'
#' A \emph{constant} in this context is an \code{<eqn>} node of numeric value
#' contained within an \code{<aux>} node under \code{<variables>}.
#'
#' This function extracts a tibble with one row for each constant and columns:
#' - \emph{name} is the name of the constant (from \code{<aux name='...'>})
#'   - NOTE: names are NOT sanitized, just the raw string in the xmile file
#' - \emph{dimensions} displays dimension name for array constants
#' - \emph{subscript} displays element subscript for array constants
#' - \emph{value} is the numeric value of the constant cast as a float
#' - \emph{units} units for this constant if specified
#' - \emph{doc} documentation for this constant if specified
#'
#'
#' @param filepath A string that indicates a path to a file with extension .stmx
#'   or .xmile. Vensim files (.mdl) are not xmile files. They must be exported
#'   from Vensim with extension .xmile
#' @returns a tibble with information on all constants in the model
#'
#' @export
#'
#' @examples
#' path <- system.file("models", "SIR.stmx", package = "readsdr")
#' read_xmile_constants(path)
read_xmile_constants <- function(filepath) {
  safe_read(filepath) %>%
    xml2::xml_find_all(".//d1:variables//d1:aux//d1:eqn[number(.) = .]") %>%
    lapply(extract_parameters_one_constant) %>%
    dplyr::bind_rows()
}
