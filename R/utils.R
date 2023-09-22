#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom deSolve timestep
#' @export
deSolve::timestep

remove_NULL  <-  function(x.list) {
  x.list[unlist(lapply(x.list, length) != 0)]
}

get_names <- function(obj_list, name_var = "name") {
  sapply(obj_list, function(obj) obj[[name_var]])
}

get_raw_names <- function(obj_list, name_var) {

  purrr::map_chr(obj_list, function(obj) {

    name <- obj[[name_var]]

    if("par_trans" %in% names(obj)) {

      name <- stringr::str_remove(name, paste0(obj$par_trans, "_"))
    }

    name
  })
}

as_row_list <- function(df) do.call(function(...) Map(list,...), df)

execute_trans <- function(val, trans_type, return_type = "numeric") {

  if(trans_type == "inv" & return_type == "numeric") return(1/val)

  if(trans_type == "inv" & return_type == "text") return(paste0("1/", val))
}

is_string_numeric <- function(x) suppressWarnings(ifelse(!is.na(as.numeric(x)),
                                                         TRUE, FALSE))

# This function makes consts & auxs to have the same properties

format_consts_as_vars <- function(constants) {

  lapply(constants, function(const) {
    list(name = const$name, equation = const$value)
  })
}

df2list <- function(df) do.call(function(...) Map(list,...), df)

#' Extract one function call from a string.
#'
#' Finds the given function name (not as part of another word), followed by
#' a matched set of parentheses.
#'
#' @param x A string of xmile.
#' @param function_name The name of the function to find.
#' @returns List with `match` being the full match and `args` just the part
#'   inside the parentheses.
extract_function_call <- function(x, function_name) {
  pat <- paste0("\\b", function_name, "(\\((?:[^()]+|(?-1))*+\\))")
  call_match <- regmatches(x, regexpr(pat, x, perl = TRUE, ignore.case = TRUE))
  if (length(call_match) == 0) {
    return(NA)
  }
  func_len <- stringr::str_length(function_name)
  list(
    match = call_match,
    args = stringr::str_sub(call_match, func_len + 2, -2))
}
