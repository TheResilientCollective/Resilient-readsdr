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

#' Simple map of sanitized name to all extensions (enumerated dimensions)
#'
#' Start by generating a table (vdims) with columns:
#' - name: the sanitized name of one variable (stock, flow, aux)
#' - dim.1: the name of the first dimension
#' - elem.1: name of the element for dimension 1
#' - ...
#'
#' There are enough dimension.n and elem.n columns to cover the highest-
#' dimensional variable.  There is one row for each combination of dimensional
#' elements for each variable (same order as found in the xml file).
#'
#' Next we unite all the elem.* columns with an "_" separator into the "ext"
#' column
#'
#' @param dims_obj original dimensions object (will generate `dims_tibble`)
#' @returns tibble with `name` and `ext` (dimension-based name extension)
dimension_extensions <- function(dims_obj) {
  if (all(is.na(dims_obj$dictionary)) || all(is.na(dims_obj$global_dims))) {
    return (tibble::tibble(name=character(), ext=character()))
  }

  maxdims <- max(sapply(dims_obj$dictionary, length))

  gdims <- stack(dims_obj$global_dims) %>% dplyr::rename(dim=ind, elem=values)

  vdims <- tibble::as_tibble(
    list(name=names(dims_obj$dictionary), dim=dims_obj$dictionary)
  ) %>%
    tidyr::unnest_wider(dim, names_sep=".")

  for (idx in 1:maxdims) {
    dim_map <- setNames(c("dim"), paste("dim", idx, sep="."))
    elcol <- paste("elem", idx, sep=".")
    vdims <- vdims %>%
      dplyr::left_join(gdims,
                       by=dim_map,
                       relationship="many-to-many") %>%
      dplyr::rename(!!elcol:=elem)
  }

  vdims %>%
    tidyr::unite(ext, starts_with("elem."), sep="_", na.rm=TRUE) %>%
    dplyr::select(name, ext)
}
