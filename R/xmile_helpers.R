# inits_vector is only used for sd_bayes()
extract_structure_from_XMILE <- function(filepath, inits_vector = NULL,
                                         const_list = NULL) {

  raw_xml <- safe_read(filepath)
  vendor  <- which_vendor(raw_xml)

  sim_specs  <- xml2::xml_find_all(raw_xml, ".//d1:sim_specs")
  parameters <- create_param_obj_xmile(sim_specs)

  # time_aux is meant for calculating init values of stocks
  time_aux   <- list(name     = "time",
                     equation = as.character(parameters$start))

  #-----------------------------------------------------------------------------
  dims_obj <- NULL

  cld_xml      <- xml2::xml_children(raw_xml)
  child_names  <- xml2::xml_name(cld_xml)

  if("dimensions" %in% child_names) {
    dims_obj <- create_dims_obj(raw_xml)
  }
  #-----------------------------------------------------------------------------

  variables_xml   <- xml2::xml_find_first(raw_xml, ".//d1:variables")

  auxs_xml        <- xml2::xml_find_all(variables_xml, ".//d1:flow|.//d1:aux")
  stocks_xml      <-  xml2::xml_find_all(variables_xml, ".//d1:stock")

  vars_and_consts <- create_vars_consts_obj_xmile(auxs_xml, vendor, dims_obj,
                                                  const_list, inits_vector)

  if(vendor == "Vensim") {
    vars_and_consts <- extract_vars_in_stocks(stocks_xml, vars_and_consts,
                                              inits_vector)
  }

  vars_and_consts <- expand_conveyors(variables_xml,
                                      vars_and_consts,
                                      parameters$dt,
                                      dims_obj,
                                      inits_vector,
                                      vendor)

  variables       <- arrange_variables(vars_and_consts$variables)
  constants       <- vars_and_consts$constants

  args_fun       <- list(stocks_xml = stocks_xml,
                         variables  = variables,
                         constants  = constants,
                         dims_obj   = dims_obj,
                         time_aux   = time_aux,
                         vendor     = vendor)

  if("builtin_stocks" %in% names(vars_and_consts)) {
    args_fun$builtin_stocks <- vars_and_consts$builtin_stocks
  }

  if(!is.null(inits_vector)) args_fun$fixed_inits <- inits_vector

  levels         <- do.call("create_level_obj_xmile", args_fun)

  list(parameters = parameters,
       levels = levels,
       variables = variables,
       constants = constants)
}

#' Parse one constant \code{<eqn>} (node with numeric value)
#'
#' Pull out \emph{name}, \emph{dimensions}, \emph{element} for each dimensional
#' element, \emph{units}, and \emph{doc}.
#'
#' \emph{WARNING} this does not handle multi dimensional arrays or integer
#' indexed arrays.  Only single dimensional arrays with named subscripts.
#'
#' @param node_eqn constant \code{eqn} node to extract info for
#'
#' @return A tibble
#'
extract_parameters_one_constant <- function(node_eqn) {
  subscript <- node_eqn %>%
    xml2::xml_parent() %>%
    xml2::xml_attr("subscript")
  if (xml2::xml_name(xml2::xml_parent(node_eqn)) == "element") {
    node_aux <- node_eqn %>%
      xml2::xml_parent() %>%
      xml2::xml_parent()
    if (is.na(subscript)) {
      warning("Can't handle integer-indexed arrays yet:",
              xml2::xml_attr(node_aux, "name"))
      return(dplyr::tibble())
    }
  } else {
    node_aux <- xml2::xml_parent(node_eqn)
  }
  dplyr::tibble(
    name = xml2::xml_attr(node_aux, "name"),
    dimensions = node_aux %>%
      xml2::xml_find_first("./d1:dimensions/d1:dim") %>%
      xml2::xml_attr("name"),
    subscript = subscript,
    value = xml2::xml_double(node_eqn),
    units = node_aux %>%
      xml2::xml_find_first("./d1:units") %>%
      xml2::xml_text(),
    doc = node_aux %>%
      xml2::xml_find_first("./d1:doc") %>%
      xml2::xml_text()
  )
}

#' Modify one constant \code{<eqn>} (node with numeric value)
#'
#' Find constant in xmile, matched by \code{name}, \code{dimensions}, and
#' \code{elements}.  Modify \code{value} field to match input row.
#'
#' \emph{WARNING} As above, this does not handle multi dimensional arrays or
#' integer indexed arrays.
#'
#' @param new_row Row in the modified dataframe from which to take values.
#' @param xmile Full xmile object to be modified.
#'
modify_one_constant_node <- function(new_row, xmile) {
  xpath <- sprintf('.//d1:variables//d1:aux[@name="%s"]', new_row["name"])
  if (!is.na(new_row["dimensions"])) {
    xpath = paste0(
      xpath,
      sprintf('/d1:element[@subscript="%s"]', new_row["subscript"])
    )
  }

  xpath <- paste0(xpath, "/d1:eqn")
  xml_node <- xml2::xml_find_first(xmile, xpath)
  if(is.na(xml_node)) {
    warning(sprintf('Failed to find xml node: "%s"', xpath))
    return();
  }
  xml2::xml_text(xml_node) <- as.character(new_row["value"])
}

compute_init_value <- function(var_name, equation, auxs, fixed_inits) {

  tryCatch(
    error = function(cnd) {
     stop(stringr::str_glue("Can't compute the init value of '{var_name}'"),
           call. = FALSE)
    }, {

      vars_in_equation <- extract_variables(var_name, equation)
      newEquation      <- equation
      auxs_names       <- sapply(auxs, function(aux) aux$name)

      if(!is.null(fixed_inits)) {

          vars_in_equation <- vars_in_equation[!vars_in_equation %in% fixed_inits]
          if(length(vars_in_equation) == 0) return (newEquation)
      }

      for(var_in_equation in vars_in_equation) {

        pos_aux     <- which(auxs_names == var_in_equation)
        aux_obj     <- auxs[[pos_aux]]
        rpl_val     <- aux_obj$equation # replacement value


        if(!is.null(aux_obj$graph)){

          input_equation <- stringr::str_match(rpl_val, "f.+\\((.+)\\)")[[2]]
          input          <- compute_init_value("", input_equation, auxs, fixed_inits)
          assign(aux_obj$graph_fun$name, aux_obj$graph_fun$fun)
          rpl_val        <- do.call(aux_obj$graph_fun$name, list(input))
        }

        replacement <- paste0("(", rpl_val, ")")
        pattern     <- paste0("\\b", var_in_equation, "\\b(?!')")
        newEquation <- gsub(pattern, replacement, newEquation, perl = TRUE)
      }

      env <- environment()
      env$.memory <- data.frame() # for sd_fixed_delay
      newEquation <- safe_eval(newEquation, env)

      if(is.character(newEquation)) {
        initValue <- compute_init_value(var_name, newEquation, auxs, fixed_inits)
        return(initValue)
      }

      if(is.numeric(newEquation)) {
        initValue   <- newEquation
      }

      initValue
   }
  )
}

sanitise_elem_name <- function(elem_name) {
  elem_name %>%
    stringr::str_replace_all("\n|\t|~","") %>%
    stringr::str_replace_all(" |\\\\n", "_")
}

sanitise_init_value <- function(init_value, vendor, is_arrayed) {

  clean_init <- init_value %>%
    stringr::str_replace_all("\\{.*?\\}", "") %>%  # removes commentaries
    stringr::str_replace_all("\n|\t|~","")

  if(is_arrayed) clean_init <- purrr::map_chr(clean_init, sanitise_arrays,
                                              vendor)

  clean_init
}

sanitise_aux_equation <- function(equation, vendor) {
  sanitised_equation <- sanitise_arrays(equation, vendor)

  sanitised_equation %>%
    translate_if_else_functions(vendor) %>%
    stringr::str_replace_all("\n|\t|~| ","") %>%
    stringr::str_replace_all("\\{.*?\\}", "") %>%  # removes commentaries
    translate_extrema() %>%
    translate_math_funs() %>%
    translate_comparison_operators() %>%
    translate_stat_funs(vendor) %>%
    translate_logical_operators(vendor) %>%
    translate_time_builtins() %>%
    eval_constant_expr() # Must go at the end
}

#' Evaluate constant expression
#'
#' eval_constant_expr(3 + 3) # returns 6
#' eval_constant_expr(a + 3) # returns a + 3
#'
#' @param equation A string
#'
#' @return A string
#' @noRd
eval_constant_expr <- function(equation) {
  tryCatch(
    error = function(cnd) equation,
    {
      evaluated_expr <- eval(parse(text = equation), envir = baseenv())
      as.character(evaluated_expr)
    }
  )
}

check_elem_name <- function(elem_name) {
  is_valid <- make.names(elem_name) == elem_name

  if(!is_valid) {
    error_message <- paste0(elem_name , " is not a valid name for a variable")
    stop(error_message, call. = FALSE)
  }

  elem_name
}

which_vendor <- function(raw_xml) {

  vendor_raw <- xml2::xml_find_first(raw_xml, ".//d1:vendor") %>%
    xml2::xml_text()

  is_Vensim <- stringr::str_detect(vendor_raw, "Ventana")
  is_isee   <- stringr::str_detect(vendor_raw, "isee")
  is_Simlin <- stringr::str_detect(vendor_raw, "Simlin")

  if(is_Vensim) vendor <- "Vensim"
  if(is_isee)   vendor <- "isee"
  if(is_Simlin) vendor <- "isee"

  vendor
}

safe_read <- function(filepath) {

  tryCatch(
    error = function(cnd) {

      tryCatch(
        error = function(cnd) {
          stop("Invalid XML file", call. = FALSE)

        },
        readChar(filepath, file.info(filepath)$size) %>%
          sanitise_xml() %>% xml2::read_xml()
      )

    },

    xml2::read_xml(filepath)
  )
}

sanitise_arrays <- function(equation, vendor) {

 if(vendor == "isee") {
   pattern        <- "\\[(.+?)\\]"
   there_is_array <- stringr::str_detect(equation, pattern)

   if(there_is_array) {
     equation <- stringr::str_replace(equation, "\\[(.+?)\\]", "_\\1")
     equation <- sanitise_arrays(equation, vendor)
   }
 }

  equation
}

safe_eval <- function(equation, env) {
  tryCatch(
    error = function(cnd) equation,
    eval(parse(text = equation), envir = env)
  )
}
