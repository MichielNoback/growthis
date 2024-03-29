#' Plots varioscan data in growth curves.
#'
#' Support currently for three types of visualizations: Only average (line plot), all three replicates separately, and a ribbon plot with the average of the three replicates. In the ribbon plot the lower and upper bounds represent the minimum and maximum values of the replicates.
#' The upper panel shows the corrected OD values and the lower panel shows the Control series.
#'
#' @param varioscan_data all data in long format
#' @param plot_type a character, one of \code{c("replicates", "avg", "ribbon")}. See details
#'
#' @details
#' \code{plot_type} The `plot_type` parameter can be one of \code{c("replicates", "avg", "limits")} and controls which data will be displayed:
#' \describe{
#'   \item{ribbon}{the average as line and the maximum and minimum as ribbon}
#'   \item{replicates}{the individual three replicates}
#'   \item{average}{only the average of each series}
#' }
#'
#' @md
#' @export
#'
plot_growthcurves <- function(varioscan_data,
                              plot_type = "ribbon") {
    if (nrow(varioscan_data) == 0) {
        stop("an empty dataset can not be plotted")
    }
    #print(varioscan_data)
    controls_plot <- create_controls_plot(
                        filter_data(data = varioscan_data, replicates = "C"))

    if (plot_type == "average") {
        exp_plot <- plot_all(filter_data(data = varioscan_data,
                             replicates = "Avg"),
                             plot_type)
    } else if (plot_type == "replicates") {
        exp_plot <- plot_all(filter_data(varioscan_data,
                             replicates = c("1C", "2C", "3C")),
                             plot_type)
    } else if (plot_type == "ribbon"){
        exp_plot <- create_ribbon_plot(filter_data(varioscan_data,
                             replicates = c("1C", "2C", "3C", "Avg")))
    } else {
        stop(paste0("unknown plot ype requested: ", plot_type))
    }

    common_legend <- extract_legend(exp_plot)
    all <- gridExtra::grid.arrange(
        gridExtra::arrangeGrob(exp_plot + ggplot2::theme(legend.position="none"),
                    controls_plot + ggplot2::theme(legend.position="none"),
                    nrow=2, heights = c(3, 1)),
        common_legend, ncol=2, widths=c(10, 1))
    return(all)
}

#not exported helper function
create_controls_plot <- function(data) {
    controls_plot <- ggplot2::ggplot(data = data,
                mapping = ggplot2::aes(x = duration, y = OD,
                                       color = as.factor(dilution))) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(start_date, series, extract)) +
    #ggplot2::scale_x_time() +
    ggplot2::xlab("Duration (h)") +
    ggplot2::labs(color = "dilution") +
    ggplot2::scale_color_manual(values = get_series_palette()) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
                   #legend.key = ggplot2::element_rect()
                   #legend.spacing.y = ggplot2::unit(0.2, "cm"))
    return(controls_plot)
}

#not exported helper function
get_series_palette <- function() {
    #my_palette <- RColorBrewer::brewer.pal(8, "YlOrBr")[2:8]
    #my_palette <- viridis::magma(9)[-c(1, 9)]
    my_palette <- viridis::inferno(8)[-1]
    my_palette <- c("#3182BD", my_palette) # first one is blue for 0%
    return(my_palette)
}

#not exported helper function
create_ribbon_plot <- function(data) {
    #print(unique(data$replicate))

    #determine range per series
    data_wide <- data %>%
        dplyr::filter(replicate %in% c("1C", "2C", "3C", "Avg")) %>%
        tidyr::pivot_wider(names_from = replicate, values_from = OD) %>%
        dplyr::mutate(minimum = pmin(`1C`, `2C`, `3C`, na.rm = T),
                      maximum = pmax(`1C`, `2C`, `3C`, na.rm = T))

    exp_plot <- ggplot2::ggplot(data = data_wide,
                    mapping = ggplot2::aes(x = duration, y = Avg)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = minimum,
                                          ymax = maximum,
                                          fill = as.factor(dilution)),
                             alpha = 0.3) +
        ggplot2::geom_line(ggplot2::aes(color = as.factor(dilution))) +
        #ggplot2::scale_x_time() +
        ggplot2::scale_color_manual(values = get_series_palette()) +
        ggplot2::scale_fill_manual(values = get_series_palette()) +
        ggplot2::labs(fill = "dilution", color = "dilution") +
        #ggplot2::xlab("Duration (h)") +
        ggplot2::ylab("OD") +
        ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
        ) +
        ggplot2::facet_wrap(ggplot2::vars(start_date, series, extract))
    return(exp_plot)
}

#not exported helper function
#creates a line plot of all replicates, split over extracts
plot_all <- function(data, plt_type) {
    omitlegend <- if (plt_type == "average") TRUE  else FALSE
    exp_plot <- ggplot2::ggplot(data = data,
                    mapping = ggplot2::aes(x = duration, y = OD,
                                           color = as.factor(dilution))) +
        ggplot2::geom_line(ggplot2::aes(linetype = replicate)) +
        ggplot2::facet_wrap(ggplot2::vars(start_date, series, extract)) +
        #ggplot2::scale_x_time() +
        #ggplot2::xlab("Duration (h)") +
        ggplot2::labs(color = "dilution") +
        ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank()) +
        ggplot2::scale_color_manual(values = get_series_palette()) +
        {if (omitlegend) ggplot2::guides(linetype = "none")}
    return(exp_plot)
}

#not exported helper function
#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
extract_legend<-function(a.gplot){
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

