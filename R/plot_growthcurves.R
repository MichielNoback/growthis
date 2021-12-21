
#' Plots varioscan data in growth curves
#'
#' @param varioscan_data all data in long format
#' @param only_average whether only the average of the three series should be displayed
#'
#' @export
#'
plot_growthcurves <- function(varioscan_data,
                              only_average = FALSE) {
    if (only_average) {
        varioscan_data %>%
            dplyr::filter(replicate == "Avg") %>%
            plot_all()
    } else {
        varioscan_data %>%
            dplyr::filter(
                replicate %in% c("1C", "2C", "3C")) %>%
            plot_all()
    }

}

#not exported helper function
plot_all <- function(data) {
    ggplot2::ggplot(data = data,
                    mapping = ggplot2::aes(x = duration, y = OD,
                                           color = as.factor(dilution))) +
        #ggplot2::geom_point(ggplot2::aes(shape = replicate), size = 0.5) +
        ggplot2::geom_line(ggplot2::aes(linetype = replicate)) +
        ggplot2::facet_wrap(ggplot2::vars(extract)) +
        ggplot2::scale_x_time() +
        ggplot2::xlab("Duration (h)") +
        ggplot2::labs(color = "dilution")
}

