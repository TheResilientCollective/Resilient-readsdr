library(ggplot2)

#' Plot combined levels (sum of slat values) of one conveyor
#'
#' @param results results returned from sd_simulate
#' @param cname name of conveyor to examine
#' @param fname if NA (default), plot is returned, else saved to this file
#' @returns plot unless fname specified
plot_conveyor_levels <- function(results, cname, fname = NA) {
  plt <- res %>%
    dplyr::select(c(time, matches(paste(cname, "\\w+", "\\d+", sep="_")))) %>%
    tidyr::pivot_longer(cols=starts_with(cname),
                        names_to="slat_name",
                        values_to="level") %>%
    tidyr::separate_wider_regex(slat_name, c(conveyor=cname, "_",
                                             dimension="\\w+", "_",
                                             slat_idx="\\d+")) %>%
    dplyr::group_by(time, conveyor, dimension) %>%
    dplyr::summarize(level=sum(level), .groups="drop") %>%
    ggplot(aes(x=time, y=level, group=dimension, color=dimension)) +
    geom_line()

  if (is.na(fname)) {
    return(plt)
  }
  ggplot2::ggsave(plt, filename = fname, width = 1500, height = 1000,
                  units = "px")
}


#' Plot levels for all 8 of our conveyors
#'
#' This just spits all plots out to file, no option to display them locally.
#'
#' @param results results returned from sd_simulate
plot_all_conveyor_levels <- function(results) {
  conveyor_names <- c("infected_not_contagious",
                      "tested_infected_not_contagious",
                      "asymptomatic_contagious",
                      "tested_asymptomatic_contagious",
                      "tested_symptomatic_contagious",
                      "symptomatic_contagious",
                      "symptomatic_not_contagious",
                      "tested_symptomatic_not_contagious")
  for (cname in conveyor_names) {
    fname <- paste0("LEVELS_", cname, ".png")
    plot_conveyor_levels(results, cname, fname)
  }
}
