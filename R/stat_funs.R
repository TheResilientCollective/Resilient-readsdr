translate_stat_funs <- function(equation, vendor) {
  new_equation <- translate_NORMAL(equation, vendor)
  new_equation <- translate_GAMMA(new_equation, vendor)
  translate_LOGNORMAL(new_equation, vendor)
}

translate_NORMAL <- function(equation, vendor) {
  new_equation <- equation

  if(vendor == "isee") {
    detection_pattern <- "\\bNORMAL\\b"
    pattern_found     <- stringr::str_detect(equation, detection_pattern)

    if(pattern_found) {

      validation_pattern <- stringr::regex("NORMAL\\((.+?)\\)",
                                           dotall = TRUE, ignore_case = TRUE)

      validation_match  <- stringr::str_match(equation, validation_pattern)
      params            <- stringr::str_split(validation_match[[2]], ",")
      n_params          <- length(params[[1]])

      if(n_params != 2) {
        stop("readsdr is restricted to translate NORMAL functions with only two parameters: mean, std_dev.",
             call. = FALSE)
      }

      pattern_normal <- stringr::regex("NORMAL\\((.+?),(.+?)\\)",
                                       dotall = TRUE, ignore_case = TRUE)

      string_match <- stringr::str_match(equation, pattern_normal)
      norm_mean    <- string_match[[2]]
      norm_sd      <- string_match[[3]]
      replacement  <- stringr::str_glue("rnorm(1,{norm_mean},{norm_sd})")

      new_equation <- stringr::str_replace(equation, pattern_normal,
                                           replacement)
    }
  }

  if(vendor == "Vensim") {

    detection_pattern <- "\\bRANDOM_NORMAL\\b"
    pattern_found     <- stringr::str_detect(equation, detection_pattern)

    if(pattern_found) {

      pattern_normal <- stringr::regex("RANDOM_NORMAL\\((.+?),(.+?),(.+?),(.+?),(.+?)\\)",
                                       dotall = TRUE, ignore_case = TRUE)

      string_match <- stringr::str_match(equation, pattern_normal)
      min_val      <- string_match[[2]]
      max_val      <- string_match[[3]]
      norm_mean    <- string_match[[4]]
      norm_sd      <- string_match[[5]]
      replacement  <- stringr::str_glue("truncnorm::rtruncnorm(1,{min_val},{max_val},{norm_mean},{norm_sd})")

      new_equation <- stringr::str_replace(equation, pattern_normal,
                                           replacement)

    }
  }

  new_equation
}

translate_GAMMA <- function(equation, vendor) {
  if(vendor != "isee") {
    return(equation)
  }

  detection_pattern <- "\\bGAMMA\\b"
  pattern_found     <- stringr::str_detect(equation, detection_pattern)

  if(!pattern_found) {
    return(equation)
  }

  extracted_call <- extract_function_call(equation, "GAMMA")
  if (all(is.na(extracted_call))) {
    stop(paste("ERROR Parsing GAMMA function call in:", equation))
  }
  args_list <- stringr::str_split(extracted_call$args, ",")[[1]]

  if (length(args_list) > 3) {
    # NOTE: this can't handle commas inside function calls!  So, e.g.,
    #       you can't do `GAMMA(shape, f(x1,x2,x3))`
    stop("Expected GAMMA to be called with 1-3 arguments!")
  }

  if (length(args_list) == 3) {
    warning(paste("Ignoring seed argument to GAMMA function:",
                  extracted_call$match))
  }

  gamma_shape <- args_list[1]
  if (length(args_list) > 1) {
    gamma_scale <- args_list[2]
  } else {
    gamma_scale <- 1
  }

  repl <- stringr::str_glue("rgamma(1, {gamma_shape}, scale={gamma_scale})")

  stringr::str_replace(equation, stringr::fixed(extracted_call$match), repl)
}

translate_LOGNORMAL <- function(equation, vendor) {
  if(vendor != "isee") {
    return(equation)
  }

  detection_pattern <- "\\bLOGNORMAL\\b"
  pattern_found     <- stringr::str_detect(equation, detection_pattern)

  if(!pattern_found) {
    return(equation)
  }

  extracted_call <- extract_function_call(equation, "LOGNORMAL")
  if (all(is.na(extracted_call))) {
    stop(paste("ERROR Parsing LOGNORMAL function call in:", equation))
  }
  args_list <- stringr::str_split(extracted_call$args, ",")[[1]]

  if ((length(args_list) < 2) || (length(args_list) > 3)) {
    # NOTE: this can't handle commas inside function calls!  So, e.g.,
    #       you can't do `LOGNORMAL(shape, f(x1,x2,x3))`
    stop("Expected LOGNORMAL to be called with 2-3 arguments!")
  }

  if (length(args_list) == 3) {
    warning(paste("Ignoring seed argument to LOGNORMAL function:",
                  extracted_call$match))
  }

  mean_val <- args_list[1]
  sd_val <- args_list[2]

  repl <- stringr::str_glue("rlnorm(1, log({mean_val}), {sd_val}*{sd_val})")

  stringr::str_replace(equation, stringr::fixed(extracted_call$match), repl)
}
