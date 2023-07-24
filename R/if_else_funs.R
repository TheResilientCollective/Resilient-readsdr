
translate_if_else_functions <- function(equation, vendor) {
  translated_equation <- translate_ifelse(equation, vendor) %>%
    translate_step() %>%
    translate_pulse(vendor)

  if(vendor == "Vensim") {
    translated_equation <- translate_pulse_train(translated_equation)
  }

  translated_equation
}

translate_ifelse <- function(equation, vendor) {

  if(vendor == "isee") {
    detection_pattern     <- "\\bIF\\b"
    there_is_if_statement <- stringr::str_detect(equation, detection_pattern)

    n_ifs <- stringr::str_count(equation, detection_pattern)

    if(n_ifs > 1) stop("Only one IF-ELSE statement per variable is permitted")

    if(there_is_if_statement) {
      pattern      <- stringr::regex("IF(.+)THEN(.*)ELSE(.*)",
                                     dotall = TRUE)
      string_match <- stringr::str_match(equation, pattern)
      condition    <- if_else_condition(equation)
      if_true      <- string_match[[3]]
      if_false     <- string_match[[4]]
      body_ifelse  <- paste(condition, if_true, if_false, sep = ", ")
      equation     <- paste0("ifelse(", body_ifelse, ")")
      return(equation)
    }

  }

  if(vendor == "Vensim") {
    pattern      <- stringr::regex("IF_THEN_ELSE", ignore_case = TRUE)
    n_ifs        <- stringr::str_count(equation, pattern)

    if(n_ifs > 1) stop("Only one IF-ELSE statement per variable is permitted")

    equation     <- stringr::str_replace(equation, pattern, "ifelse")
  }

  equation
}

if_else_condition <- function(equation) {

  # pattern with parentheses
  p1 <- stringr::regex("IF\\((.+)\\).*THEN.*", dotall = TRUE)

  if(stringr::str_detect(equation, p1)) {
    string_match <- stringr::str_match(equation, p1)
    return(string_match[[2]])
  }

  p2 <- stringr::regex("IF(.+)THEN.*", dotall = TRUE)
  string_match <- stringr::str_match(equation, p2)
  string_match[[2]]
}

translate_step <- function(equation) {
  pattern_step  <- stringr::regex("STEP\\((.+?),(.+?)\\)",
                                  ignore_case = FALSE, dotall = TRUE)
  there_is_step <- stringr::str_detect(equation, pattern_step)


  if(there_is_step) {
    new_equation <- stringr::str_replace(equation, pattern_step,
                                         "ifelse(time >=\\2, \\1, 0)")

    new_equation <- translate_step(new_equation)
    equation     <- new_equation
  }

  equation
}

translate_pulse_train <- function(equation) {
  # pattern pulse train
  pattern_pt  <- stringr::regex("PULSE_TRAIN\\((.+?),(.+?),(.+?),(.+?)\\)",
                                ignore_case = TRUE, dotall = TRUE)
  # is there a pulse train?
  there_is_pt <- stringr::str_detect(equation, pattern_pt)


  if(there_is_pt) {
    n_pt        <- stringr::str_count(equation, pattern_pt)

    if(n_pt > 1) stop("Only one PULSE_TRAIN statement per variable is permitted")

    match_result <- stringr::str_match(equation, pattern_pt)
    start_pt     <- match_result[[2]]
    duration_pt  <- match_result[[3]]
    repeat_pt    <- match_result[[4]]
    end_pt       <- match_result[[5]]

    translation <- stringr::str_glue(
        "sd_pulse_train(time, {start_pt},{duration_pt},{repeat_pt},{end_pt})")

    new_equation <- stringr::str_replace(equation, pattern_pt, translation)
    return(new_equation)
  }

  equation
}

# Translate Pulse

translate_pulse <- function(equation, vendor) {

  # Screening
  pattern_screen  <- stringr::regex("PULSE\\(",
                                    dotall = TRUE, ignore_case = TRUE)

  n_pulses        <- stringr::str_count(equation, pattern_screen)

  if(n_pulses == 0) return(equation)
  if(n_pulses > 1) stop("Only one PULSE statement per variable is permitted")

  if(vendor == "Vensim") {
    pattern_pulse  <- stringr::regex("PULSE\\((.+?),(.+?)\\)",
                                     dotall = TRUE, ignore_case = TRUE)
    there_is_pulse <- stringr::str_detect(equation, pattern_pulse)

    if(there_is_pulse) {
      string_match <- stringr::str_match(equation, pattern_pulse)
      pulse_start  <- string_match[[2]]
      pulse_width  <- string_match[[3]]
      start_num    <- suppressWarnings(as.numeric(pulse_start))
      width_num    <- suppressWarnings(as.numeric(pulse_width))

      if(is.na(start_num) | is.na(width_num)) {
        replacement  <- stringr::str_glue(
          "sd_pulse_v(time,{pulse_start},{pulse_width})"
        )
        new_equation <- stringr::str_replace(equation, pattern_pulse,
                                             replacement)
        return(new_equation)
      }

      replacement  <- get_pulse_v_statement(start_num, width_num)
      new_equation <- stringr::str_replace(equation, pattern_pulse,
                                           replacement)
      return(new_equation)
    }
  }

  if(vendor == "isee") {

    pulse_call <- extract_function_call(equation, "PULSE")
    if (all(is.na(pulse_call))) {
      stop(paste("ERROR Parsing PULSE function call in:", equation))
    }

    args_list <- stringr::str_split(pulse_call$args, ",")[[1]]

    if ((length(args_list) < 1) | (length(args_list) > 3)) {
      stop("PULSE MUST be called with 1-3 arguments!")
    }

    volume_p     <- args_list[[1]] # volume pulse
    if (length(args_list) == 1) {
      # A PULSE with one arg magnifies the variable
      replacement  <- stringr::str_glue("{volume_p} / timestep()")
      new_equation <- stringr::str_replace(equation,
                                           stringr::fixed(pulse_call$match),
                                           replacement)
      return(new_equation)
    }

    start_p      <- args_list[[2]] # start pulse
    if(length(args_list) == 2) {
      # A PULSE with two args is a magnified step
      replacement <- stringr::str_glue(
        "ifelse(time >= {start_p}, {volume_p} / timestep(), 0)")
      new_equation <- stringr::str_replace(equation,
                                           stringr::fixed(pulse_call$match),
                                           replacement)
      return(new_equation)
    }


    if(length(args_list) == 3) {
      # A PULSE with three args is a pulse train
      interval     <- args_list[[3]]

      interval_num <- suppressWarnings(as.numeric(interval))

      if(is.na(interval_num)) {
        replacement <- stringr::str_glue(
          "sd_pulse_s(time, {volume_p},{start_p},{interval})"
        )
        new_equation <- stringr::str_replace(equation,
                                             stringr::fixed(pulse_call$match),
                                             replacement)
        return(new_equation)
      }

      replacement  <- get_pulse_s_statement(volume_p, start_p, interval_num)
      new_equation <- stringr::str_replace(equation,
                                           stringr::fixed(pulse_call$match),
                                           replacement)
      return(new_equation)
    }
  }
}

get_pulse_s_statement <- function(volume_p, start_p, interval_num) {

  if(interval_num == 0L) {
    statement <- stringr::str_glue(
      "ifelse(time =={start_p}, {volume_p} / timestep(), 0)")
    return(statement)
  }

  if(interval_num > 0) {
    pulse_points <- stringr::str_glue(
      "seq({start_p}, max(time, {start_p}), {interval_num})")
    statement <- stringr::str_glue(
      "ifelse(time %in% {pulse_points}, {volume_p} / timestep(), 0)")
  }
}

get_pulse_v_statement <- function(pulse_start, pulse_width) {
  end_pulse <- pulse_start  + pulse_width
  if_true   <- paste0('== ', pulse_start)
  if_false  <- stringr::str_glue(">= {pulse_start} & time < {end_pulse}")
  condition <- ifelse(pulse_width == 0L, if_true, if_false)
  statement <- stringr::str_glue("ifelse(time {condition}, 1, 0)")
}

#' Extract one function call from an xmile string.
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
  call_match <- regmatches(x, regexpr(pat, x, perl=TRUE, ignore.case = TRUE))
  if (length(call_match) == 0) {
    return(NA)
  }
  func_len <- stringr::str_length(function_name)
  list(
    match = call_match,
    args = stringr::str_sub(call_match, func_len + 2, -2))
}
