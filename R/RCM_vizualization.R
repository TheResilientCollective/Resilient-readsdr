#' Plot combined levels (sum of slat values) of one conveyor
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
  ggplot2::ggsave(plt, filename = fname, width = 5000, height = 1000,
                  units = "px")
}


#' Plot levels for all 8 of our conveyors
plot_all_conveyor_levels <- function(results, write_to_file=FALSE) {
  conveyor_names <- c("infected_not_contagious",
                      "tested_infected_not_contagious",
                      "asymptomatic_contagious",
                      "tested_asymptomatic_contagious",
                      "tested_symptomatic_contagious",
                      "symptomatic_contagious",
                      "symptomatic_not_contagious",
                      "tested_symptomatic_not_contagious")
  for (cname in conveyor_names) {
    fname <- ifelse(write_to_file, paste0("LEVELS_", cname, ".png"), NA)
    plot_conveyor_levels(results, cname, fname)
  }
}
