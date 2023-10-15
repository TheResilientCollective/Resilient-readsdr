array_equations <- function(aux_obj, dims_obj, dim_names, vendor) {

  dims_dict <- dims_obj$dictionary
  glob_dims <- dims_obj$global_dims

  n_dims <- length(dim_names)

  dims_list        <- lapply(dim_names, function(dim_name) glob_dims[[dim_name]])
  names(dims_list) <- dim_names
  elems            <- combine_dims(dims_list)

  raw_equation <- aux_obj$equation
  aux_name     <- aux_obj$name

  if(vendor == "Vensim") {

    vector_pattern <- create_array_pattern(dims_list)
    is_an_array    <- stringr::str_detect(raw_equation, vector_pattern)

    if(is_an_array) {

      clean_equation <- raw_equation %>%
         stringr::str_replace_all(";",",")

      clean_equation <- substr(clean_equation,1, nchar(clean_equation) - 1)
      equations      <- stringr::str_split(clean_equation, ",")[[1]]
      are_const      <- !is.na(suppressWarnings(as.numeric(equations)))
    }

    if(!is_an_array) {

      devec_eqs <- devectorise_equation(raw_equation, dims_list)

      equations <- sapply(devec_eqs, sanitise_aux_equation, vendor,
                          USE.NAMES = FALSE)

      are_const <- !is.na(suppressWarnings(as.numeric(equations)))
    }
  }

  if(vendor == "isee") {

    equations    <- sanitise_aux_equation(raw_equation, vendor)
    are_const    <- !is.na(suppressWarnings(as.numeric(equations)))

    if(!are_const) {

      eq_vars <- extract_variables(lhs = aux_name, equations)

      arrayed_vars <- names(dims_dict)

      for(var_in_eq in eq_vars) {

        if(var_in_eq %in% arrayed_vars) {

          var_dims <- dims_dict[[var_in_eq]]
          dims_idx <- paste(var_dims, collapse = ",")

          replacement       <- stringr::str_glue("{var_in_eq}[{dims_idx}]")
          pattern           <- stringr::str_glue("\\b{var_in_eq}\\b")
          unvectorised_eq   <- stringr::str_replace_all(equations, pattern,
                                                  replacement)
          devec_eqs <- devectorise_equation(unvectorised_eq, dims_list)

          equations <- sapply(devec_eqs, sanitise_aux_equation, vendor,
                              USE.NAMES = FALSE)

          are_const <- !is.na(suppressWarnings(as.numeric(equations)))

        }

      }
    }

  }

  list(equations  = equations,
       are_const  = are_const,
       elems      = elems)
}

devectorise_equation <- function(raw_equation, dims_list) {

  dim_names   <- names(dims_list)
  pattern     <- paste0("\\[", paste(dim_names, collapse = ","), "\\]")

  elems       <- combine_dims(dims_list)
  replacement <- paste0("_", elems)

  stringr::str_replace_all(raw_equation, pattern, replacement)

}

combine_dims <- function(dims_list) {

  rev_dims_list <- rev(dims_list)
  rev_combs_df  <- expand.grid(rev_dims_list, stringsAsFactors = FALSE)
  combs_df      <- rev(rev_combs_df)
  do.call(paste, c(combs_df, sep = "_"))

}

create_array_pattern <- function(dims_list) {

  n_dims      <- length(dims_list)
  dim1_length <- length(dims_list[[1]])
  rgx_elems   <- rep(".+?", dim1_length)
  rgx_array   <- paste(rgx_elems, collapse = ",")

  if(n_dims == 2) {
    rgx_row    <- paste0(rgx_array, ";")
    rgx_matrix <- rep(rgx_row, length(dims_list[[2]]))
    rgx_array  <- paste(rgx_matrix, collapse = "")
  }

  rgx_array

}

#' Expand a variable on it's dimensions.
#'
#' If the variable has dimensions, return a list of new variables expanded on
#' the dimensions.
#'
#' It is assumed that `dims_obj` contains
#' - `dictionary` which maps variables to lists of dimension names
#' - `global_dims` which maps dimension names to dimenion elements/categories
#'
#' @param var_name name of a variable (\emph{flow}, \emph{aux}, etc)
#' @param dims_obj object describing model dimensions
#' @returns list of expanded variable names, one for each combo of dimensions
#'
expand_dimensions <- function(var_name, dims_obj) {
  res <- lapply(var_name, list)
  calc_idx <- var_name %in% names(dims_obj$dictionary)

  dim_names <- dims_obj$dictionary[var_name[calc_idx]]
  dims_list <- lapply(dim_names,
                      function(dim_name) dims_obj$global_dims[[dim_name]])
  res[calc_idx] <- lapply(
    var_name[calc_idx],
    function(vn) lapply(
      combine_dims(dims_list[vn]),
      function(d) paste(vn, d, sep="_")))

  res
}

#' Generate a full data frame with dimensions for all dimensional variables
#'
#' This is a table with columns:
#' - name: the sanitized name of one variable (stock, flow, aux)
#' - dimension.1: the name of the first dimension (NA if non-dimensional)
#' - elem.1: name of the element for dimension 1
#' - ...
#'
#' There are enough dimension.n and elem.n columns to cover the highest-
#' dimensional variable.  There is one row for each combination of dimensional
#' elements for each variable.
#'
#' @param dims_obj Dimension dictionary
#' @returns tibble with columns defined as above
#'
dimension_tibble <- function(dims_obj) {
  maxdims <- max(sapply(dims_obj$dictionary, length))
  gdims <- tibble::as_tibble(
    list(dimension=names(dims_obj$global_dims), elem=dims_obj$global_dims)
  ) %>%
    tidyr::unnest(elem)

  vdims <- tibble::as_tibble(
    list(name=names(dims_obj$dictionary), dimension=dims_obj$dictionary)
  ) %>%
    tidyr::unnest_wider(dimension, names_sep=".")


  # I know there's a better way to do this...
  for (idx in 1:maxdims) {
    #dimcol <- paste("dimension", idx, sep=".")
    dim_map <- setNames(c("dimension"), paste("dimension", idx, sep="."))
    elcol <- paste("elem", idx, sep=".")
    vdims <- vdims %>%
      dplyr::left_join(gdims,
                       by=dim_map,
                       relationship="many-to-many") %>%
      dplyr::rename(!!elcol:=elem)

  }
  vdims
}

#' Simple map of sanitized name to all extensions (enumerated dimensions)
#'
dimension_extensions <- function(dims_obj = NA, dims_tibble = NA) {
  if (is.na(dims_tibble) && is.na(dims_obj)) {
    stop("dimension_extensions -- Need one of dims_obj or dims_tibble!")
  }
  if (is.na(dims_tibble)) {
    dims_tibble <- dimension_tibble(dims_obj)
  }
  dims_tibble %>%
    tidyr::unite(ext, starts_with("elem."), sep="_", na.rm=TRUE) %>%
    dplyr::select(name, ext)
}
