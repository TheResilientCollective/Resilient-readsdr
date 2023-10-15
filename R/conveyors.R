
#' Gather all the information we need on conveyors and connected flows.
#'
#' Returns a list of dataframes
#' - conveyors with columns:
#'   - name: sanitized name (without any dimension info)
#'   - leak_exp: boolean indicating whether leaks are exponential or not
#'   - inflows: comma-separated list of inflow names (no dimensions)
#'   - outflows: comma-separated list of outflow names (no dimensions)
#'   - init_eqn: equation string for initialization
#'   - len_eqn: equation string for conveyor length
#'     - WARNING!  Assuming both eqn and len tags are unique within stock!
#' - flows (only inflows and outflows from above) with columns:
#'   - name: sanitized name (without any dimension info)
#'   - eqn: value of eqn tag directly under the flow
#'     - WARNING! we're not handling per-dimension eqn elements at this level
#'                we currently don't have any...
#'   - is_leak: boolean indicating whether the flow is a leak
#'   - spread_source: flow spreads from upstream to downstream conveyor slats
#'   - spread_even: flow spreads evenly into downstream conveyor slats
#'   - from_conveyor: sanitized name of conveyor this flows from IF true
#'
extract_conveyor_info <- function(vars_xml, vendor) {
  conveyors_xml <-  vars_xml %>%
    xml2::xml_find_all(".//d1:stock[./d1:conveyor]")

  if (length(conveyors_xml) < 1) {
    return()
  }
  if (vendor != "isee") {
    stop("Conveyors only supported for isee systems (Stella)")
  }

  df_conveyors <- tidyr::tibble(
    name = conveyors_xml %>%
      xml2::xml_attr("name") %>%
      sanitise_elem_name(),
    leak_exp = conveyors_xml %>%
      xml2::xml_find_all(".//d1:conveyor") %>%
      xml2::xml_attr("exponential_leak") %>%
      tidyr::replace_na("false") %>%
      stringr::str_equal("true", ignore_case=TRUE),
    inflows = conveyors_xml %>%
      sapply(function(x) {
        xml2::xml_find_all(x, ".//d1:inflow") %>%
          xml2::xml_text() %>% paste(collapse=",")}),
    outflows = conveyors_xml %>%
      sapply(function(x) {
        xml2::xml_find_all(x, ".//d1:outflow") %>%
          xml2::xml_text() %>% paste(collapse=",")}),
    init_eqn = conveyors_xml %>%
      xml2::xml_find_all("./d1:eqn") %>%
      xml2::xml_text(),
    len_eqn = conveyors_xml %>%
      xml2::xml_find_all(".//d1:conveyor/d1:len") %>%
      xml2::xml_text()
  )

  # NOTE: these are sanitized, so we have to filter post-sanitize
  all_flow_names <- df_conveyors %>%
    dplyr::select(c(inflows, outflows)) %>%
    tidyr::unite(name, inflows, outflows, sep=",", na.rm=TRUE) %>%
    tidyr::separate_longer_delim(name, ",") %>%
    dplyr::distinct()

  # map flow to conveyor it flows from.
  # NOTE: there should never be duplicate sources for a single flow.
  flow_from_map <- df_conveyors %>%
    dplyr::select(name, outflows) %>%
    tidyr::separate_longer_delim(outflows, ",") %>%
    dplyr::rename(flows_from=name, name=outflows)

  # map flow to conveyor it flows into.
  flow_to_map <- df_conveyors %>%
    dplyr::select(name, inflows) %>%
    tidyr::separate_longer_delim(inflows, ",") %>%
    dplyr::rename(flows_to=name, name=inflows)

  flows_xml <- vars_xml %>%
    xml2::xml_find_all(".//d1:flow")

  df_flows <- tidyr::tibble(
    name = flows_xml %>%
      xml2::xml_attr("name") %>%
      sanitise_elem_name(),
    eqn = flows_xml %>%
      xml2::xml_find_all("./d1:eqn") %>%
      xml2::xml_text(),
    is_leak = flows_xml %>%
      xml2::xml_find_lgl("./d1:leak=true()"),
    spread_source = flows_xml %>%
      xml2::xml_find_lgl('@isee:spreadflow="source"'),
    spread_even = flows_xml %>%
      xml2::xml_find_lgl('@isee:spreadflow="even"'),
  ) %>%
    dplyr::inner_join(all_flow_names, by = "name") %>%
    dplyr::left_join(flow_from_map, by = "name") %>%
    dplyr::left_join(flow_to_map, by = "name")

  if (any(df_flows$spread_source & !df_flows$is_leak)) {
    stop('Found spreadflow="source" on flow that is not a leak!')
  }
  if (any(df_flows$spread_even & df_flows$is_leak)) {
    stop('Found spreadflow="even" on flow that is a leak!')
  }

  list(conveyors = df_conveyors, flows = df_flows)
}

#' Modify vars and consts list for conveyors
#'
#' Here we add a chain of "builtin stocks", along the lines of what is done
#' for DELAYN processing.  Each stock represents one slat in the conveyor, and
#' we connect them to each other and to the inflow(s), leak, and outflow(s).
#' For convenience, we add a variable with the conveyor name which is a vector
#' of all the slats.
#'
#' @param vars_xml xml nodeset with all variables
#' @param v_and_c list to update if we find conveyors
#' @param dt simulation fractional time step
#' @param dims_obj dimensions object used to split arrayed stocks
#' @param inits_vector initialization overrides passed to compute_init_value
#' @param vendor isee or vensim (only tested for isee)
expand_conveyors <- function(vars_xml,
                             v_and_c,
                             dt,
                             dims_obj,
                             inits_vector,
                             vendor) {
  if (vendor != "isee") {
    stop("Conveyor expansion only supported for isee systems (Stella)")
  }

  cvy_info = extract_conveyor_info(vars_xml, vendor)
  if (length(cvy_info$conveyors) == 0) {
    return()
  }

  # Parameters we need to pass through to other methods.  I'm trying to
  # move towards vectorization, but some of the underlying methods still use,
  # e.g., dims_obj and the aux list.
  cvy_parms = list(
    dt = dt,
    cvy_info = cvy_info,
    dims_obj = dims_obj,
    dmx = dimension_extensions(dims_obj),
    const_auxs = lapply(v_and_c$constants,
                         function(cn){list(name=cn$name, equation=cn$value)}),
    inits_vector = inits_vector,
    vendor = vendor)

  if (is.null(v_and_c$builtin_stocks)) {
    v_and_c$builtin_stocks <- list()
  }
  add_tibble <- function(v_and_c, vname, addons) {
    if (vname %in% names(addons)) {
      addons <- addons[[vname]]
    }
    v_and_c[[vname]] <- append(v_and_c[[vname]], dynutils::tibble_as_list(addons))
    v_and_c
  }
  for (conveyor_name in cvy_info$conveyors$name) {
    addons <- expand_one_conveyor(conveyor_name, cvy_parms)
    v_and_c <- add_tibble(v_and_c, "builtin_stocks", addons)
    v_and_c <- add_tibble(v_and_c, "variables", addons)
  }

  # Also need to add equations for missing outflow variables
  addons <- conveyor_outflow_variables(cvy_parms)
  v_and_c <- add_tibble(v_and_c, "variables", addons)

  v_and_c
}


#' Modify vars and consts list for a single conveyor stock
#'
#' This method is very specific to the Resilient model and not robust to all
#' forms of xmile!  For each conveyor, we generate a vector of "builtin stocks",
#' one for each slat.  These are lists with:
#' - name: built as "{conveyor_name}_slat_{slat_number}"
#' - equation: this is the equation that goes into differential calculation!
#' - initValue: initial value of slat is X/(DT*len), where X is conveyor eqn
#'
#' Additionally, internal vector variables are used to keep track of per-slat
#' leakage, and those are summed into the scalar flow variables which may be
#' referenced downstream.  For the special case of downstream conveyors, the
#' vector version is used.
#'
#' I am 100% sure this could be done more efficiently and cleanly.  I started
#' to move over to a vectorized version (thus all the use of tidyr), but that's
#' a bigger task than I have time for!
#'
#' In Stella, initial values are NOT evenly spread among the slots.  They are
#' placed in such a way that they emulate a steady state (a constant outflow),
#' given the existing leakages.
#'
#' Call SN the initial value in slat N (in 1...M), and LF be leak fraction.  If
#' there were a steady inflow of INF, the value in slot SN would be
#'
#' SN = INF (1-LF)^(M-N)
#'
#' Here I take the value BEFORE leakage is removed because leakage is calculated
#' first before outflow.  Also M is transit time / DT.
#'
#' We initialize the conveyor with value init = sum_N(SN)
#' init = sum(INF (1-LF)^(M-N))
#'      = INF sum((1-LF)^(M-N)
#'  INF = init/sum((1-LF)^(M-N))
#'
#' Which gives us our initial vector.
#'
#' @param conveyor_name sanitized name of the conveyor stock
#' @param cvy_parms list of useful parameters
#' @returns list with builtin_stocks and variables to be added
expand_one_conveyor <- function(conveyor_name,
                                cvy_parms) {

  df_c <- cvy_parms$cvy_info$conveyors %>%
    dplyr::filter(name==conveyor_name)

  # This next bit needs to be vectorized, probably on extraction...
  dim_names <- cvy_parms$dims_obj$dictionary[[conveyor_name]]

  eqn_obj <- df_c %>%
    dplyr::select(c(name, init_eqn)) %>%
    dplyr::rename(equation=init_eqn)
  init_eq <- array_equations(eqn_obj, cvy_parms$dims_obj, dim_names, cvy_parms$vendor)
  init_vals <- sapply(
    init_eq$equations,
    function(eqn) {
      compute_init_value(conveyor_name, eqn, cvy_parms$const_auxs, cvy_parms$inits_vector)
    }
  )

  eqn_obj <- df_c %>%
    dplyr::select(c(name, len_eqn)) %>%
    dplyr::rename(equation=len_eqn)
  len_eq <- array_equations(eqn_obj, cvy_parms$dims_obj, dim_names, cvy_parms$vendor)
  len_vals <- unname(sapply(
    len_eq$equations,
    function(eqn) {
      compute_init_value(conveyor_name, eqn, cvy_parms$const_auxs, cvy_parms$inits_vector)
    }
  ))

  # TODO: should put a check in here that all lengths are constant over the sim
  # Handling variable transit times is more complicated

  # calculate initial value in slats, and paste with space-separators
  # This is super complicated.  I'm going to just hardcode the leakage fractions
  # for the one inital-value conveyor we have.  For this guy we have two leaks.
  #
  # With 2 leaks, we have INF(1-LF1) left after the first leak, then
  # INF(1-LF1)(1-LF2) after the second.  Let R1 = 1-LF1 and R2 = 1-LF2,
  # then
  # Si = (R1 R2)^(M-i)/sum((R1 R2)^(M-N))
  #
  # The first has fraction based on dimension ("Mild" -> 0.0188, else 0.075).
  # The second is time varying, but starts at X-0.000001*17.8, where
  # X = .00789 for Severe, .0000667 for Moderate, and 0 otherwise.  Noting that
  # leakage can't be negative, we have:
  #
  #                 LF1     RM1       LF2     RM2         RM1 RM2
  # Asymptomatic   .075    .925        0       1            .925
  # Mild           .0188   .812        0       1            .812
  # Moderate       .075    .925   .0000489  .9999155    .9249218
  # Severe         .075    .925   .0078722  .9920922    .9176853
  #

  expand_init <- function(init, len, ext) {
    nslat <- len/cvy_parms$dt
    if (init == 0) {
      return (paste(rep(init/nslat, nslat), collapse=" "))
    }
    if (ext == "Asymptomatic") {
      rem <- .925
    } else if (ext == "Mild") {
      rem <- .812
    } else if (ext == "Moderate") {
      rem <- .9249218
    } else if (ext == "Severe") {
      rem <- .9176853
    } else {
      rem = 0.5
    }
    slat_base <- rem^(nslat-1:nslat)
    slat_inflow <- init/sum(slat_base)
    paste(slat_inflow*slat_base, collapse=" ")
  }

  dims_list <- lapply(dim_names,
                      function(dim_name) cvy_parms$dims_obj$global_dims[[dim_name]])
  ext_list <-combine_dims(dims_list)

  df_slats <- df_c %>%
    dplyr::rename(base_name = name) %>%
    dplyr::mutate(ext = list(ext_list)) %>%
    tidyr::unnest(ext) %>%
    dplyr::mutate(
      initValue=init_vals,
      len=len_vals
    ) %>%
    dplyr::mutate(
      initValue = Map(expand_init, initValue, len, ext),
      nslats = len/cvy_parms$dt
    ) %>%
    tidyr::separate_longer_delim(initValue, delim = " ") %>%
    dplyr::group_by(ext) %>%
    dplyr::mutate(slat_idx = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::unite(name, base_name, ext, slat_idx, sep="_",
                 remove=FALSE, na.rm=TRUE) %>%
    dplyr::mutate(slat_flow = stringr::str_c(base_name, "_slatflow")) %>%
    tidyr::unite(slat_flow, slat_flow, ext, sep = "_",
                 remove=FALSE, na.rm=TRUE) %>%
    dplyr::mutate(
      equation = mapply(
        function(v, w,x,y,z) {
          conveyor_flow_sum(v, w, x, y, z, cvy_parms$cvy_info)
        },
        base_name, ext, slat_idx, nslats, slat_flow))

  # Create a list of lists for builtin_stocks
  new_builtins <- df_slats %>% dplyr::select(c(name, initValue, equation))

  # Add slat flows to variables
  new_vars <- df_slats %>%
    dplyr::select(c(slat_flow, base_name, ext, len)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      equation = mapply(
        function(x,y,z) conveyor_slat_flow_call(x, y, cvy_parms$cvy_info),
        base_name,
        ext
      )
    ) %>%
    dplyr::select(c(slat_flow, equation)) %>%
    dplyr::rename(name=slat_flow)

  # Add array variable (conveyor_Ext <- c(conveyor_Ext_1, ...))
  slat_var <- df_slats %>%
    dplyr::rename(slat_name=name) %>%
    tidyr::unite(name, base_name, ext, na.rm=TRUE) %>%
    dplyr::mutate(name = paste0(name, "_slat_vec")) %>%
    dplyr::select(name, slat_name) %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(equation=paste0("c(", paste(slat_name, collapse=", "), ")")) %>%
    dplyr::ungroup()

  # Add sum variable (direct access to the sanitized stock name seems to expect it)
  conveyor_stock_var <- df_slats %>%
    tidyr::unite(name, base_name, ext, na.rm=TRUE) %>%
    dplyr::select(name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(equation=paste0("sum(", name, "_slat_vec)"))

  list(builtin_stocks = new_builtins,
       variables = dplyr::bind_rows(new_vars, slat_var, conveyor_stock_var))
}

#' Generate variable to equation map for flows out of conveyors
#'
#' Unlike other stocks and flows, conveyors dictate the pace of flow.  Here
#' we generate equations for all of the flows out of conveyors.  In each case,
#' we have a "name" field from the sanitized element name, an "ext" field with
#' the dimension extension, and an equation which dictates the logical flow.
#'
#' - leaks: each leak is a vector of outflow variables, one for each slat
#'   - named "name_extension_vec", and then summed into "name_extension" for
#'     scalar use
#' - "normal" outflows: final drain from slat 1 after all other external flows
#'   - start with value stored in slot 1 from previous dt
#'   - subtract outflow leaks
#'   - inflow from slot 2 is NOT part of this
conveyor_outflow_variables <- function(cvy_parms) {
  oflow_vars <- cvy_parms$cvy_info$flows %>%
    dplyr::filter(!is.na(flows_from)) %>%
    dplyr::left_join(cvy_parms$dmx, by="name") %>%
    tidyr::unite(name, name, ext, sep="_", remove=FALSE, na.rm=TRUE)

  leak_vars <- oflow_vars %>% dplyr::filter(is_leak)
  normal_flow_vars <- oflow_vars %>% dplyr::filter(!is_leak)

  mk_outflow_args <- function(cname, col) {
    ifelse(
      is.na(col),
      NA,
      paste0(cname,"=c(", stringr::str_c(col, collapse = ", "), ")"))
  }

  normal_addons <- normal_flow_vars %>%
    tidyr::unite(flows_from, flows_from, ext, sep = "_", na.rm=TRUE) %>%
    dplyr::mutate(flows_from = paste0(flows_from, "_slat_vec")) %>%
    dplyr::left_join(conveyor_outflow_leak_effects(cvy_parms),
                     by="flows_from") %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(leaks = mk_outflow_args("leaks", leak_eqn)) %>%
    dplyr::ungroup() %>%
    tidyr::unite(args, flows_from, leaks, sep = ", ", na.rm=TRUE) %>%
    dplyr::mutate(
      equation = stringr::str_glue("sd_conveyor_outflow({args})")) %>%
    dplyr::select(c(name, equation))

  leak_vec_addons <- leak_vars %>%
    dplyr::mutate(equation = stringr::str_glue("sum({name}_vec)")) %>%
    dplyr::select(name, equation)

  df_leak_exp <- cvy_parms$cvy_info$conveyors %>%
    dplyr::select(name, leak_exp) %>%
    dplyr::rename(flows_from=name)

  leak_vars %>%
    dplyr::left_join(df_leak_exp, by = "flows_from") %>%
    dplyr::mutate(fraction = vectorized_array_equations(., eqn, cvy_parms)) %>%
    tidyr::unite(flows_from, flows_from, ext, sep = "_", na.rm=TRUE) %>%
    dplyr::mutate(
      flows_from = paste0(flows_from, "_slat_vec"),
      name = paste0(name, "_vec"),
      equation = stringr::str_glue(
        "sd_conveyor_leak({flows_from}, {fraction}, {leak_exp})")) %>%
    dplyr::select(c(name, equation)) %>%
    dplyr::bind_rows(normal_addons) %>%
    dplyr::bind_rows(leak_vec_addons)
}


#' Collect leaks which will have an effect on the conveyor outflow
#'
#' Returns tibble with fields:
#' - flows_from: name of upstream conveyor with dimensional extension
#' - leak_eqn: equation for the (scalar) value leak from slat 1
conveyor_outflow_leak_effects <- function(cvy_parms) {
    cvy_parms$cvy_info$flows %>%
      dplyr::filter(is_leak & !is.na(flows_from)) %>%
      dplyr::left_join(cvy_parms$dmx, by="name") %>%
      tidyr::unite(name, name, ext, sep="_", remove=FALSE, na.rm=TRUE) %>%
      dplyr::mutate(leak_eqn=paste0(name, "_vec[1]")) %>%
      tidyr::unite(flows_from, flows_from, ext, sep="_", na.rm=TRUE) %>%
      dplyr::select(c(flows_from, leak_eqn))
}

#' Generate net flow equation for a slat based on flow map
#'
#' NOTE: we introduce inter-slat flows which are indexed according to the
#'       slat they flow INTO.
#'
#' - Inflows
#'   1. For the highest index (first) slat, start with any non-leaked inflows
#'      - NOTE: if a flow is a leak, but does not spread to downstream conveyor,
#'              we have to sum over the leakage slats!
#'   2. For all OTHER slats, start with the inter-slat flow with this index
#'   2. For all slats, ADD any leaked inflows
#' - Outflows
#'   1. For all slats, subtract outflow leaks.
#'   2. For all EXCEPT the last slat, subtract the LOWER index inter-slat flow
#'   3. for the last slat (index 1), subtract the outflow.
#'
#' NOTE: leakages AND inter-slat flows are per slat and indexed by slat number
#'
#' WARNING: I'm making an assumption here that dimensional conveyors are never
#'          attached to non-dimensional flows or visa-versa.  Just assuming
#'          extension is the same, which is true for RCM model.
#'
#' @param conveyor_name Sanitized name of conveyor stock
#' @param ext Extension based on dimensions
#' @param idx Index of this slat within the conveyor
#' @param nslats Number of slats in conveyor (len field divided by DT)
#' @param slat_flow Name of inter-slat flow, WITH extension
#' @param cvy_info List of inflows, outflows, and leaks WITHOUT extension!
conveyor_flow_sum <- function(conveyor_name, ext, idx, nslats, slat_flow, cvy_info) {
  if ((idx > nslats) || (idx < 1)) {
    stop(stringr::str_glue("Bad Slat Index ({idx})!  Num Slats Is {nslats}!"))
  }
  indexed_lk <- stringr::str_glue("vec[{idx}]")
  inflows <- cvy_info$flows %>%
    dplyr::filter(flows_to == conveyor_name) %>%
    dplyr::mutate(ext_col = ext) %>%
    tidyr::unite(name, name, ext_col, sep="_", na.rm=TRUE)
  inf_not_spread <- inflows %>% dplyr::filter(!(spread_source | spread_even))
  if ((idx == nslats) && (nrow(inf_not_spread) > 0)) {
    if_vec <- inf_not_spread %>% dplyr::select(name)
  } else if (idx < nslats) {
    if_vec = stringr::str_glue("{slat_flow}[{idx}]")
  } else {
    if_vec = c()
  }
  spreads <- inflows %>% dplyr::filter(is_leak & spread_source)
  if (nrow(spreads) > 0) {
    if_vec = c(if_vec, paste(spreads$name, indexed_lk, sep="_"))
  }
  spreads <- inflows %>% dplyr::filter(spread_even)
  if (nrow(spreads) > 0) {
    if_vec = c(if_vec, stringr::str_glue("{spreads$name}/{nslats}"))
  }
  if (length(if_vec) == 0) {
    stop("Conveyor calculated zero inflows!")
  }

  outflows <- cvy_info$flows %>%
    dplyr::filter(flows_from == conveyor_name) %>%
    dplyr::mutate(ext_col = ext) %>%
    tidyr::unite(name, name, ext_col, sep="_", na.rm=TRUE)
  outleaks <- outflows %>% dplyr::filter(is_leak)
  if (idx == 1) {
    of_vec <- outflows %>% dplyr::filter(!is_leak) %>% dplyr::select(name)
  } else {
    of_vec <- stringr::str_glue("{slat_flow}[{idx-1}]")
  }
  if (nrow(outleaks) > 0) {
    of_vec <- c(paste(outleaks$name, indexed_lk, sep="_"), of_vec)
  }

  if (length(of_vec) == 0) {
    stop("Conveyor calculated zero outflows!")
  }
  trimws(paste(c(paste(if_vec, collapse = " + "), of_vec), collapse = " - "))
}

#' Generate equation string for conveyor slat flows
#'
#' @param base_name Name of conveyor WITHOUT extension or index!
#' @param ext Extension based on dimensions
#' @param cvy_info data frames for conveyors and flows
conveyor_slat_flow_call <- function(base_name, ext, cvy_info) {
  lk_args <- c()
  out_leaks <- cvy_info$flows %>%
    dplyr::filter((flows_from == base_name) & is_leak)
  if (nrow(out_leaks)) {
    lk_args <- paste(out_leaks$name, ext, "vec", sep="_")
  }
  args = paste(c(paste(base_name, ext, "slat_vec", sep="_"), lk_args),
               collapse = ", ")
  stringr::str_glue(
    "sd_conveyor_slat_flow({args})"
  )
}

#' Reformat equations with dimensional extensions
#'
vectorized_array_equations <- function(.data, eqn_col=eqn, cvy_parms) {

  if (all(is.na(cvy_parms))) {
    stop("Params object required!")
  }

  dsan <- .data %>%
    dplyr::mutate(
      vae_sanitized = sapply(
        {{eqn_col}},
        function(x) sanitise_aux_equation(x, cvy_parms$vendor)
      )
    )

  add_extension_to_eqn_list(dsan, "vae_sanitized", cvy_parms)
}

add_extension_to_eqn_list <- function(.data, eqn_colname, cvy_parms) {
  arg_list <- .data %>%
    dplyr::select(c(name, ext, {{eqn_colname}})) %>%
    dynutils::tibble_as_list()
  result_list <- sapply(
    arg_list,
    function(args)
      unlist(
        add_extension_to_one_eqn(args$name, args[eqn_colname], args$ext, cvy_parms)
      )
  )
  result_list
}

add_extension_to_one_eqn <- function(lhs, rhs, eqn_ext, cvy_parms) {
  if (is.na(eqn_ext)) {
    return (rhs)
  }
  eq_vars <- extract_variables(lhs, rhs)
  if (length(eq_vars) == 0) {
    return (rhs)
  }
  ext_vars <- tibble::as_tibble(list(name=eq_vars)) %>%
    dplyr::left_join(cvy_parms$dmx, by="name") %>%
    dplyr::filter(is.na(ext) | (ext==eqn_ext)) %>%
    tidyr::unite(dname, name, ext, remove=FALSE, na.rm=TRUE)

  if (any(!(eq_vars %in% ext_vars$name))) {
    idx <- !(eq_vars %in% ext_vars$name)
    bad_vars <- paste(eq_vars[idx], collapse=",")
    stop(stringr::str_glue(
      "Dimension Mismatch! Equation for {lhs} uses vars {bad_vars} with {eqn_ext}"))
  }
  stringr::str_replace_all(rhs, setNames(ext_vars$dname, ext_vars$name))
}

#' Calculate flow between slats during simulation
#'
#' Start with the full value of conveyor slats.  Divide by timestep so we take
#' the full value in one DT.  Then subtract out any leak inputs.  Here the flow
#' with index i is the one going INTO slat i, so calculated from slats and leaks
#' with index i+1.
#'
#' @param conveyor Conveyor stock value vector
#' @param ... All other args assumed to be leak flow arrays from same conveyor
#'
#' @return The (positive) values contributing to slat incremental changes.
#' @export
sd_conveyor_slat_flow <- function(conveyor, ...) {
  if (timestep() == 0) return(0)
  leak_sum <- Reduce("+", list(...))
  if (length(leak_sum) == 0) {
    leak_sum = rep(0, length(conveyor))
  }
  if ((length(conveyor) != length(leak_sum))) {
    stop(stringr::str_glue(
      "Conveyor has {length(conveyor)} slats but leak has {length(leak_sum)}!!!"
      )
    )
  }
  value <- conveyor[-1]/timestep() - leak_sum[-1]

  pmax(0, value)
}


#' Calculate conveyor leakage during simulation
#'
#' For the exponential case, we leak a fraction of the slat value.
#'
#' The linear case is more complicated.  You have to keep track of the original
#' value put into the conveyor (and where it was put in), then remove a fixed
#' fraction of that amount at every slat/timestep until that quantity exits the
#' conveyor.
#'
#' @param conveyor Conveyor stock value variable (array of slat values)
#' @param fraction Leak fraction (calculated from eqn element)
#' @param is_exp Exponential vs linear leakage
#'
#' @return The (positive) value contributing to conveyor incremental change.
#' @export
#'
sd_conveyor_leak <- function(conveyor, fraction, is_exp) {
  if (is_exp) {
    return(pmax(0, conveyor * min(fraction, 1)))
  }
  stop("Only handling exponential leakage for conveyors!")
}

#' Calculate conveyor "normal" outflow during simulation
#'
#' Conveyors are broken into N slats numbered N down to 1.  Outflow comes from
#' slat 1 after subtracting all leakages.
#'
#' Start with the full value of conveyor slat 1.  Divide by timestep so we take
#' the full value in one DT.  Then subtract out any leaks.  In all
#' cases, we are only concerned with slat 1.
#'
#' NOTE: this assumes only one outflow per stock, which is true for RCM model!
#'       To handle other outflows, we need to collect and track flow priority.
#'
#' @param conveyor Conveyor stock value variable
#' @param leaks Leaks out of this conveyor (slat 1 only).
#'
#' @return The (positive) value contributing to conveyor incremental change.
#' @export
#'
sd_conveyor_outflow <- function(conveyor, leaks = 0) {
  if (timestep() == 0) return(0)

  value <- conveyor[1]/timestep() - sum(leaks)

  max(0, value)
}

