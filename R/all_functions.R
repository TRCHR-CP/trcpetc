

#' @export
show_surv <- function(surv_obj,
                      x_lab= 'Time',
                      y_lab= if (plot_cdf) 'The proportion of deceased subjects' else 'The freedom from death',
                      # x_lim= NULL,
                      y_lim= NULL,
                      x_break= NULL,
                      y_break= NULL,
                      color_scheme= c("brewer", "grey", "viridis", "manual"),
                      color_list= NULL, #required only if color_scheme= 'manual'. eg color_list= list(values= c('red', 'blue'))
                      plot_theme= ggplot2::theme_minimal(),

                      add_ci= TRUE,
                      add_atrisk= TRUE,
                      add_legend= FALSE,
                      add_pvalue= TRUE,
                      atrisk_init_pos= NULL,
                      pvalue_pos= c("topleft", "topright", "bottomleft", "bottomright", "left", "right", "top", "bottom"),
                      plot_cdf= FALSE,
                      print_fig = TRUE) {

  # no need to add pvalues for a single cohort
  add_pvalue<- if (all(names(surv_obj)!='strata')) FALSE else add_pvalue
  # no need to add legend if it is a single cohort or add risk (when it is >1 cohorts). The at-risk table will be color-coded to indicate cohort
  add_legend<- if (all(names(surv_obj)!='strata') | add_atrisk) FALSE else add_legend

  color_scheme<- match.arg(color_scheme)
  if (color_scheme=='manual' & is.null(color_list)) stop("Please provide a list of color value(s).")

  fill_fun <- switch(color_scheme,
                     'brewer' = quote(scale_fill_brewer(palette = "Set1")),
                     'grey'   = quote(scale_fill_grey(start= 0, end= 0.65)),
                     'viridis'= quote(scale_fill_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE)),
                     'manual' = match.call(do.call, call('do.call', what= 'scale_fill_manual', args= color_list)))
  color_fun<- switch(color_scheme,
                     'brewer' = quote(scale_color_brewer(palette = "Set1")),
                     'grey'   = quote(scale_color_grey(start= 0, end= 0.65)),
                     'viridis'= quote(scale_color_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE)),
                     'manual' = match.call(do.call, call('do.call', what= 'scale_color_manual', args= color_list)))

  if (!plot_cdf & !is.null(y_lim)) {
    y_lim <- c(0, 1)
    message("The parameter y_lim was reset to y_lim= c(0, 1) for survival function.")
  } else if (plot_cdf & !is.null(y_lim)) {
    y_lim <- c(0, max(y_lim, na.rm= TRUE))
    message("The lower limit of y-axis was reset to 0 for failure function.")
  } else if (!plot_cdf & is.null(y_lim)) {
    y_lim <- c(0, 1)
    message("The parameter y_lim was set to y_lim= c(0, 1) for survival function.")
  } else if (plot_cdf & is.null(y_lim)) {
    y_lim <- c(0, 1)
    message("The parameter y_lim was set to y_lim= c(0, 1) for failure function.")
  }

  #---- prepare survfit for plot ----
  surv_mat <- prepare_survfit(surv_obj)

  plot_prob_d <- surv_mat %>%
    dplyr::select(strata, plot_prob_d) %>%
    tidyr::unnest(cols = c(plot_prob_d)) %>%
    dplyr::mutate(prob= if (plot_cdf) 1-prob else prob)

  if (y_lim[2]< 1) {
    plot_prob_d<- plot_prob_d %>%
      dplyr::mutate(prob= pmin(prob, y_lim[2], na.rm= TRUE),
                    # prob= pmax(prob, min(y_lim, na.rm = TRUE), na.rm= TRUE)
      ) %>%
      dplyr::group_by(strata)
  }

  out <- ggplot2::ggplot() +
    ggplot2::geom_step(data= plot_prob_d,
              ggplot2::aes(x= time, y= prob, group= strata, color= strata),
              size= 1.1, show.legend = add_legend) +
    eval(color_fun) +
    ggplot2::scale_x_continuous(name  = x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                       expand= c(0.01, 0.005),
                       labels= function(x) scales::comma(x, accuracy = 1))

  if (add_ci) {
    plot_ci_d <- surv_mat %>%
      dplyr::select(strata, plot_ci_d) %>%
      tidyr::unnest(cols = c(plot_ci_d))

    if (plot_cdf) {
      plot_ci_d <- plot_ci_d %>%
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with('conf')), function(x) 1-x) %>%
        dplyr::rename(conf_high= conf_low,
               conf_low = conf_high)
    }

    # if (y_lim[2]< 1) {
    #   plot_ci_d<- plot_ci_d %>%
    #     dplyr::filter(conf_low <= y_lim[2],
    #            conf_high>= y_lim[1]) %>%
    #     dplyr::mutate(conf_high= replace(conf_high, conf_high> y_lim[2], y_lim[2]),
    #            conf_low = replace(conf_low, conf_low< y_lim[1], y_lim[1]))
    # }

    out <- out +
      ggplot2::geom_ribbon(data= plot_ci_d,
                  ggplot2::aes(x= time, ymin= conf_low, ymax= conf_high, fill= strata),
                  alpha= .2, show.legend = FALSE) +
      eval(fill_fun)
  }

  out <- out + ggplot2::scale_y_continuous(name  = y_lab,
                                  # name  = if (is.null(y_lab)) "Freedom from death" else y_lab,
                                  breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                                  # expand= c(0.01, 0.005),
                                  expand= c(0.005, 0),
                                  labels= function(x) scales::percent(x, accuracy = 1))

  out <- if (!is.null(y_lim)) out + ggplot2::coord_cartesian(ylim = y_lim, clip = "on") else out

  if (add_pvalue) {
    pval <- run_logrank_test(surv_obj) %>%
      format_pvalue()
    # pval<- format_pvalue(pval)
    pval <- ifelse(trimws(pval)=="<0.001", "Log-rank p< 0.001", paste0("Log-rank p= ", pval) )


    y_bottom<- min(ggplot2::layer_scales(out)$y$range$range[1], out$coordinates$limits$y[1], na.rm= TRUE)
    y_top   <- max(ggplot2::layer_scales(out)$y$range$range[2], out$coordinates$limits$y[2], na.rm= TRUE)
    y_mid   <- (y_top + y_bottom)/2

    tiny_nudge <- 0.01
    pvalue_pos <- match.arg(pvalue_pos)
    if (pvalue_pos %in% c("topleft")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[1]
      # pvalue.y<- y_top #ggplot2::layer_scales(out)$y$range$range[2]
      pvalue.x <- 0 + tiny_nudge
      pvalue.y <- 1 - tiny_nudge
      pvalue.hjust<- 0
      pvalue.vjust<- 1
    } else if (pvalue_pos %in% c("bottomleft")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[1]
      # pvalue.y<- y_bottom #ggplot2::layer_scales(out)$y$range$range[1]
      pvalue.x <- 0 + tiny_nudge
      pvalue.y <- 0 + tiny_nudge
      pvalue.hjust <- 0
      pvalue.vjust <- 0
    } else if (pvalue_pos %in% c("topright")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[2]
      # pvalue.y<- y_top #ggplot2::layer_scales(out)$y$range$range[2]
      pvalue.x <- 1 - tiny_nudge
      pvalue.y <- 1 - tiny_nudge
      pvalue.hjust <- 1
      pvalue.vjust <- 1
    } else if (pvalue_pos %in% c("bottomright")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[2]
      # pvalue.y<- y_bottom #ggplot2::layer_scales(out)$y$range$range[1]
      pvalue.x <- 1 - tiny_nudge
      pvalue.y <- 0 + tiny_nudge
      pvalue.hjust <- 1
      pvalue.vjust <- 0
    } else if (pvalue_pos %in% c("left")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[1]
      # pvalue.y<- y_mid #mean(ggplot2::layer_scales(out)$y$range$range)
      pvalue.x <- 0 + tiny_nudge
      pvalue.y <- 0.5
      pvalue.hjust <- 0
      pvalue.vjust <- 0.5
    } else if (pvalue_pos %in% c("right")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[2]
      # pvalue.y<- y_mid #mean(ggplot2::layer_scales(out)$y$range$range)
      pvalue.x <- 1 - tiny_nudge
      pvalue.y <- 0.5
      pvalue.hjust <- 1
      pvalue.vjust <- 0.5
    } else if (pvalue_pos %in% c("top")) {
      # pvalue.x<- mean(ggplot2::layer_scales(out)$x$range$range)
      # pvalue.y<- y_top #ggplot2::layer_scales(out)$y$range$range[2]
      pvalue.x <- 0.5
      pvalue.y <- 1 - tiny_nudge
      pvalue.hjust <- 0.5
      pvalue.vjust <- 1
    } else if (pvalue_pos %in% c("bottom")) {
      # pvalue.x<- mean(ggplot2::layer_scales(out)$x$range$range)
      # pvalue.y<- y_bottom #ggplot2::layer_scales(out)$y$range$range[1]
      pvalue.x <- 0.5
      pvalue.y <- 0 + tiny_nudge
      pvalue.hjust <- 0.5
      pvalue.vjust <- 0
    } else {
      pvalue.x <- NULL
      pvalue.y <- NULL
      pvalue.hjust <- NULL
      pvalue.vjust <- NULL
    }

    out<- out +
      ggplot2::annotation_custom(
        grob = grid::textGrob(label= pval,
                              x = pvalue.x,
                              hjust = pvalue.hjust,

                              y = pvalue.y,
                              vjust= pvalue.vjust,

                              gp   = grid::gpar (family  = "Inconsolata",
                                                 # fontface="bold.italic",
                                                 fontface = "italic",
                                                 # cex   = 1,
                                                 fontsize  = if (is.null(plot_theme$text$size)) 11 else plot_theme$text$size)))
    # ymin = pvalue.y,      # Vertical position of the grid::textGrob
    # ymax = pvalue.y,
    # xmin = pvalue.x,
    # xmax = pvalue.x)
  }

  if (add_atrisk) out <- add_atrisk(out,
                                    surv_obj = surv_obj,
                                    x_break = x_break,
                                    atrisk_init_pos= atrisk_init_pos,
                                    plot_theme = plot_theme)

  out<- out + plot_theme

  if (print_fig) print(out)
  # print(out, vp= viewport(width = unit(6.5, "inches"), height = unit(6.5, "inches")))
  return(out)
}



#' @export
run_gray_test <- function(surv_obj, evt_type= 1:2) {

  df<- as.list(eval(surv_obj$call$data, parent.frame()))
  df<- all.vars(surv_obj$call$formula) %>%
    stats::setNames (c("ftime", "fstatus", "group")) %>%
    lapply(function(x) df[[x]])

  test<- do.call('cuminc', df)
  nn<- rownames(test$Tests)
  pval<- test$Tests[ (if ( !is.null(evt_type) ) match(evt_type, nn) else -1), "pv"]
  pval
}

#' @title Plot Cumulative Incidence Function for Competing Risks
#' @description Displays the cumulative incidence function (CIF) for competing risks data, with optional stratification and customization.
#'
#' @details
#' This function visualizes the cumulative incidence of events in the presence of competing risks using a \code{survfit} object.
#' It supports customization of axis labels, plot limits, confidence intervals, legends, p-values, and at-risk tables.
#' @param surv_obj  A \code{survfit} object, such as one returned by \code{estimate_cif()}.
#' @param evt_type Integer or vector of integers; the event type(s) of interest to be plotted (default = 1).
#' @param evt_label A function to relabel event types for plotting (default uses \code{recode_factor()} to label 1 = "Event", 2 = "Competing event", others = "Event free").
#' @param add_ci Logical; if \code{TRUE}, adds confidence intervals to the CIF curves (default = TRUE).
#' @param add_atrisk Logical; if \code{TRUE}, adds an at-risk table below the plot (default = TRUE).
#' @param add_legend Logical; if \code{TRUE}, includes a legend in the plot (default = FALSE).
#' @param add_pvalue Logical; if \code{TRUE}, adds a p-value to the plot (default = TRUE).
#' @param atrisk_init_pos Character; position of the "At-risk N:" label.
#' @param pvalue_pos Character vector indicating where to place the p-value on the plot. Options include "bottomright", "topleft", "topright", "bottomleft", "left", "right", "top", "bottom" (default = all).
#' @param plot_theme A \code{ggplot2} theme object to customize the appearance of the plot (default = \code{ggplot2::theme_minimal()}).
#' @param x_lab Character; label for the x-axis (default = "Time").
#' @param y_lab Character; label for the y-axis (default = "Proportion of subjects").
#' @param x_lim Numeric vector of length 2 specifying x-axis limits.
#' @param y_lim Numeric vector of length 2 specifying y-axis limits.
#' @param x_break Numeric vector specifying x-axis tick positions.
#' @param y_break Numeric vector specifying y-axis tick positions.
#' @param color_scheme Character; color scheme to use. Options: "brewer", "grey", "viridis", "manual" (default = "brewer").
#' @param color_list A named list of colors to use when \code{color_scheme = "manual"} (e.g., \code{list(values = c("red", "blue"))}).
#' @param print_fig Logical; if \code{TRUE}, prints the plot (default = TRUE).
#' @param top.margin Numeric; top margin space for the at-risk table (default = 18).
#' @param right.margin Numeric; right margin space for the at-risk table (default = 18).
#' @param bottom.margin Numeric; bottom margin space for the at-risk table (default = 96).
#' @param left.margin Numeric; left margin space for the at-risk table (default = 96).
#'
#' @return A \code{ggplot} object representing the cumulative incidence function plot.
#' @export
show_cif <- function(surv_obj,
                     evt_type = 1,
                     # evt_label= identity, # identity function
                     evt_label= function(x) {
                       forcats::recode_factor(x,
                                     `1`= "Event",
                                     `2`= "Competing event",
                                     .default= "Event free")
                     },
                     add_ci= TRUE,
                     add_atrisk= TRUE,
                     add_legend= FALSE,
                     add_pvalue= TRUE,
                     atrisk_init_pos= NULL,
                     pvalue_pos= c("bottomright", "topleft", "topright", "bottomleft", "left", "right", "top", "bottom"),

                     plot_theme= ggplot2::theme_minimal(),
                     x_lab= 'Time',
                     y_lab= 'Proportion of subjects',
                     x_lim= NULL,
                     y_lim= NULL,
                     x_break= NULL,
                     y_break= NULL,
                     color_scheme= c("brewer", "grey", "viridis", "manual"),
                     color_list= NULL, #required only if color_scheme= 'manual'. eg color_list= list(values= c('red', 'blue'))

                     print_fig = TRUE,

                     top.margin = 18,
                     right.margin = 18,
                     bottom.margin = 96,
                     left.margin = 96

) {

  #---- prepare survfit for plot ----
  cmprisk_mat<- prepare_survfit(surv_obj)
  cmprisk_mat<- cmprisk_mat %>%
    dplyr::filter(state %in% evt_type) %>%
    dplyr::mutate(state_label = evt_label(state),
                  state_label = forcats::fct_drop(state_label),
                  state       = forcats::fct_drop((state)),
                  state_strata= interaction(state_label, strata, drop= TRUE, sep= ": "))

  plot_prob_d <- cmprisk_mat %>%
    dplyr::select(strata, state, state_label, state_strata, plot_prob_d) %>%
    tidyr::unnest(cols = c(plot_prob_d))

  add_pvalue <- if (nlevels(plot_prob_d$strata)==1) FALSE else add_pvalue
  add_legend <- if ((nlevels(plot_prob_d$strata)==1 &
                     nlevels(plot_prob_d$state) ==1 )) FALSE else add_legend

  color_scheme <- match.arg(color_scheme)
  if (color_scheme=='manual' & is.null(color_list)) stop("Please provide a list of color value(s) when a manual color scheme is specified.")

  fill_fun <- switch(color_scheme,
                     'brewer' = quote(scale_fill_brewer(palette = "Set1", guide_legend(title= ""))),
                     'grey'   = quote(scale_fill_grey(start= 0, end= 0.65, guide_legend(title= ""))),
                     'viridis'= quote(scale_fill_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE, guide_legend(title= ""))),
                     'manual' = match.call(do.call, call('do.call', what= 'scale_fill_manual', args= color_list)))
  color_fun<- switch(color_scheme,
                     'brewer' = quote(scale_color_brewer(palette = "Set1", guide_legend(title= ""))),
                     'grey'   = quote(scale_color_grey(start= 0, end= 0.65, guide_legend(title= ""))),
                     'viridis'= quote(scale_color_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE, guide_legend(title= ""))),
                     'manual' = match.call(do.call, call('do.call', what= 'scale_color_manual', args= color_list)))

  # x_lab<- if (is.null(x_lab)) "Time" else x_lab
  # y_lab<- if (is.null(y_lab)) "Proportion of subjects" else y_lab
  # x_break<- if (is.null(x_break)) scales::pretty_breaks(6) else x_break
  # y_break<- if (is.null(y_break)) scales::pretty_breaks(6) else y_break

  out<- ggplot2::ggplot()
  out<- if (nlevels(plot_prob_d$strata)==1 & nlevels(plot_prob_d$state)>1) {
    out +
      ggplot2::geom_step(data= plot_prob_d,
                ggplot2::aes(x= time, y= prob, group= state_label, color= state_label),
                size= 1.1, show.legend = add_legend)
  } else if (nlevels(plot_prob_d$strata)>1 & nlevels(plot_prob_d$state)==1) {
    out +
      ggplot2::geom_step(data= plot_prob_d,
                ggplot2::aes(x= time, y= prob, group= strata, color= strata),
                size= 1.1, show.legend = add_legend)
  } else {
    out +
      ggplot2::geom_step(data= plot_prob_d,
                ggplot2::aes(x= time, y= prob, group= state_strata, color= state_strata),
                size= 1.1, show.legend = add_legend)
  }
  out<- out +
    eval(color_fun) +
    ggplot2::scale_x_continuous(name  = x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                       expand= c(0.01, 0.005),
                       # limits = x_lim,
                       labels= function(x) scales::comma(x, accuracy = 1)) +
    ggplot2::scale_y_continuous(name  = y_lab,
                       breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                       expand= c(0.01, 0),
                       # limits= y_lim,
                       labels= function(x) scales::percent(x, accuracy = 1))

  out<- if (!is.null(x_lim) | !is.null(y_lim)) out + ggplot2::coord_cartesian(xlim= x_lim, ylim = y_lim, clip = "on") else out

  if (add_ci) {
    plot_ci_d <- cmprisk_mat %>%
      dplyr::select(strata, state, state_label, state_strata, plot_ci_d) %>%
      tidyr::unnest(cols = c(plot_ci_d))

    out <- if (nlevels(plot_prob_d$strata)==1 & nlevels(plot_prob_d$state)>1) {

      out +
        ggplot2::geom_ribbon(data= plot_ci_d,
                    ggplot2::aes(x   = time,
                        ymin= conf_low,
                        ymax= conf_high,
                        group= state_label,
                        fill= state_label),
                    alpha= .2,
                    show.legend = FALSE)

    } else if (nlevels(plot_prob_d$strata)>1 & nlevels(plot_prob_d$state)==1) {

      out +
        ggplot2::geom_ribbon(data= plot_ci_d,
                    ggplot2::aes(x= time,
                        ymin = conf_low,
                        ymax = conf_high,
                        group= strata,
                        fill = strata),
                    alpha= .2,
                    show.legend = FALSE)

    } else {

      out +
        ggplot2::geom_ribbon(data= plot_ci_d,
                    ggplot2::aes(x= time,
                        ymin= conf_low,
                        ymax= conf_high,
                        group= state_strata,
                        fill = state_strata),
                    alpha= .2,
                    show.legend = FALSE)

    }

    out <- out + eval(fill_fun)

  }

  if (add_pvalue) {
    pval <- run_gray_test(surv_obj, evt_type = evt_type) %>%
      format_pvalue()
    pval <- ifelse(trimws(pval)=="<0.001", "Gray's p< 0.001", paste0("Gray's p= ", pval) )

    tiny_nudge <- 0.01
    pvalue_pos <- match.arg(pvalue_pos)
    if (pvalue_pos %in% c("topleft")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[1]
      # pvalue.y<- y_top #ggplot2::layer_scales(out)$y$range$range[2]
      pvalue.x <- 0 + tiny_nudge
      pvalue.y <- 1 - tiny_nudge
      pvalue.hjust <- 0
      pvalue.vjust <- 1
    } else if (pvalue_pos %in% c("bottomleft")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[1]
      # pvalue.y<- y_bottom #ggplot2::layer_scales(out)$y$range$range[1]
      pvalue.x <- 0 + tiny_nudge
      pvalue.y <- 0 + tiny_nudge
      pvalue.hjust <- 0
      pvalue.vjust <- 0
    } else if (pvalue_pos %in% c("topright")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[2]
      # pvalue.y<- y_top #ggplot2::layer_scales(out)$y$range$range[2]
      pvalue.x <- 1 - tiny_nudge
      pvalue.y <- 1 - tiny_nudge
      pvalue.hjust <- 1
      pvalue.vjust <- 1
    } else if (pvalue_pos %in% c("bottomright")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[2]
      # pvalue.y<- y_bottom #ggplot2::layer_scales(out)$y$range$range[1]
      pvalue.x <- 1 - tiny_nudge
      pvalue.y <- 0 + tiny_nudge
      pvalue.hjust <- 1
      pvalue.vjust <- 0
    } else if (pvalue_pos %in% c("left")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[1]
      # pvalue.y<- y_mid #mean(ggplot2::layer_scales(out)$y$range$range)
      pvalue.x <- 0 + tiny_nudge
      pvalue.y <- 0.5
      pvalue.hjust <- 0
      pvalue.vjust <- 0.5
    } else if (pvalue_pos %in% c("right")) {
      # pvalue.x<- ggplot2::layer_scales(out)$x$range$range[2]
      # pvalue.y<- y_mid #mean(ggplot2::layer_scales(out)$y$range$range)
      pvalue.x <- 1 - tiny_nudge
      pvalue.y <- 0.5
      pvalue.hjust <- 1
      pvalue.vjust <- 0.5
    } else if (pvalue_pos %in% c("top")) {
      # pvalue.x<- mean(ggplot2::layer_scales(out)$x$range$range)
      # pvalue.y<- y_top #ggplot2::layer_scales(out)$y$range$range[2]
      pvalue.x <- 0.5
      pvalue.y <- 1 - tiny_nudge
      pvalue.hjust <- 0.5
      pvalue.vjust <- 1
    } else if (pvalue_pos %in% c("bottom")) {
      # pvalue.x<- mean(ggplot2::layer_scales(out)$x$range$range)
      # pvalue.y<- y_bottom #ggplot2::layer_scales(out)$y$range$range[1]
      pvalue.x <- 0.5
      pvalue.y <- 0 + tiny_nudge
      pvalue.hjust <- 0.5
      pvalue.vjust <- 0
    } else {
      pvalue.x <- NULL
      pvalue.y <- NULL
      pvalue.hjust <- NULL
      pvalue.vjust <- NULL
    }

    out <- out +
      ggplot2::annotation_custom(
        grob = grid::textGrob(label= pval,
                              x = pvalue.x,
                              hjust = pvalue.hjust,

                              y = pvalue.y,
                              vjust= pvalue.vjust,

                              gp   = grid::gpar (family  = "Inconsolata",
                                                 # fontface="bold.italic",
                                                 fontface="italic",
                                                 # cex   = 1,
                                                 fontsize  = if (is.null(plot_theme$text$size)) 11 else plot_theme$text$size)))
    # ymin = pvalue.y,      # Vertical position of the grid::textGrob
    # ymax = pvalue.y,
    # xmin = pvalue.x,
    # xmax = pvalue.x)
  }

  if (add_atrisk) out <- add_atrisk(out,
                                    surv_obj = surv_obj,
                                    x_break = x_break,
                                    atrisk_init_pos= atrisk_init_pos,
                                    plot_theme = plot_theme)

  out <- out + plot_theme

  if(add_atrisk) {
    p <- out + ggplot2::theme(plot.margin= grid::unit(c(top = top.margin, right = right.margin, bottom = bottom.margin, left= left.margin), "bigpts"))
    gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
    gt$layout$clip[gt$layout$name == 'panel'] <- "off"
    out <- gt
    #grid.draw(gt)

  }





  if (print_fig) print(out)
  return(out)
}










# Factor order -------------------------------------------------------------------------

#---- Order factor variables ----
#' @title factor_order
#'
#' @details Automatically orders the levels of a factor variable by descending frequency, improving clarity in summary tables
#'
#' @param var A factor or character vector to be reordered.
#' @return A factor vector with levels sorted in descending order of frequency.
#' @export
factor_order <- function(var){
  factor(var, levels = names(sort(table(var), decreasing = TRUE)))
}


# Fan util fun -------------------------------------------------------------------------

#---- utility functions ----

#' @title decimalplaces
#'
#' @details
#' An internal function that determines the number of digits in the summary of a continuous variable.
#'
#' @param x a continuous variable
#' @return the most frequent number of digits in the variable
#' @export
decimalplaces <- function(x, max_dec= 4L) {
  y<- x[!is.na(x)]
  y<- round((y %% 1), 10)

  if (length(y) == 0) {
    out<- 0L
  } else if (any((y %% 1) != 0)) {

    # remove the trailing zero's
    y<- gsub('0+$', '', as.character(y))

    # split each number into 2 parts as characters - one before the decimal and the other after the decimal
    # take the after-decimal part
    info <- strsplit(y, ".", fixed=TRUE)
    info <- info[ vapply(info, length, integer(1L) ) == 2]

    n_dec <- nchar(unlist(info))[ 2 * (1:length(y)) ]
    dec <- sort(table(n_dec))

    # return( pmin.int(max_dec, as.integer( names(dec)[length(dec)])) )
    out <- pmin.int(max_dec, as.integer( names(dec)[length(dec)]))

  } else {
    out<- 0L
  }
  out
}

#' @title format_pvalue
#'
#' @details An internal function that formats p-values according to the statistical guidelines of the Annals of Medicine.
#'
#' @param x Numeric variable
#' @return character variables reporting p-values
#' @export
format_pvalue <- function(x, eps = 0.001, trim = TRUE,
                          droptrailing0 = FALSE,
                          # tex = TRUE,
                          pad = FALSE, ...) {
  p<- vector("character", length = length(x))

  large<- !is.na(x) & x >= 0.1995 #Steve: if 0.2 then 0.196="0.200" and 0.201= "0.20"
  p[large]<- base::format.pval(x[large],
                               digits= 1,
                               eps= 0.1995,
                               na.form= "---",
                               nsmall= 2,
                               trim= trim,
                               drop0trailing= droptrailing0,
                               scientific = FALSE, ...)

  p[!large]<- base::format.pval(x[!large],
                                digits= 1,
                                eps= eps,
                                na.form= "---",
                                nsmall= 3,
                                trim= trim,
                                drop0trailing= droptrailing0,
                                scientific = FALSE, ...)

  if (pad) p <- gsub("^([^<])", "  \\1", p)
  p
}




#' @title Round a Vector of Percentages to 100
#'
#' @description Rounds a vector of values to a 100% value using the largest remainder method
#'
#' @param values A numeric vector of percentages that should approximately sum to 100
#' @param digits An integer indicating the number of decimal places to round to. Default is 1 (whole numbers).
#'
#' @return A numeric vector of the same length as `values`, rounded to the specified number of digits, and summing to exactly 100.
#'
#' @examples
#' exact_round_100(c(33.3, 33.3, 33.4), digits = 0)
#' # Returns: 33 33 34
#'
#' exact_round_100(c(33.33, 33.33, 33.34), digits = 1)
#' # Returns: 33.3 33.3 33.4
#'
#' @export

exact_round_100 <- function(values,digits = 1){
  # Based on the internalRoundFixedSum  from the nbc4va package but adding the option to round to select number of digits

  scaleFactor <- 10^digits
  values_scaled <- values * scaleFactor

  #If the total is already 100 no need to apply any rounding
  if (all(values_scaled%%1 == 0)) {
    out <- values_scaled
  }
  else {
    floor_values_scaled <- floor(values_scaled)
    difference <-  values_scaled - floor_values_scaled

    remainder <- 100*scaleFactor - sum(floor_values_scaled)
    i <- utils::tail(order(difference), remainder)
    floor_values_scaled[i] <- floor_values_scaled[i] + 1
    out <- floor_values_scaled
  }
  return(out/scaleFactor)

}




#' @title updateWorksheet
#'
#' @details
#' An internal function that adds a new worksheet or updates (remove and add) the existing worksheet in wb object.
#'
#' @param wb a \code{wb} object
#' @param sheetName a name of the sheet to be updated
#' @param x a dataframe to be write in the \code{wb} object
#' @return a \code{wb} object
#' @export
updateWorksheet<- function(wb, sheetName, x, ...) {
  if (!is.na(sheet_pos<- match(sheetName, names(wb), nomatch= NA))) {

    sheetOrder<- openxlsx::worksheetOrder(wb)
    names(sheetOrder)<- names(wb)
    openxlsx::removeWorksheet(wb, sheetName)
    openxlsx::addWorksheet(wb, sheetName );
    openxlsx::writeData(wb, sheetName, x)
    openxlsx::worksheetOrder(wb)<- sheetOrder[names(wb)]

  } else {
    openxlsx::addWorksheet(wb, sheetName );
    openxlsx::writeData(wb, sheetName, x)
  }
  wb
}


#' @title recode_missing
#'
#' @details
#' An internal function that replace missing value code with NA.
#'
#' @return input variable with NA
recode_missing <- function(x, na.value= NULL) {
  x[x %in% na.value]<- NA
  x
}





#' @title summarize_km
#'
#' @details
#' The function summarize the fitted KM at the time points specified by a user.
#'
#' @export
summarize_km <- function(fit, times= NULL, failure_fun= FALSE) {
  ss<- summary(fit, times= if (is.null(times)) pretty(fit$time) else times)
  if (failure_fun) {
    ff<- 1 - ss$surv
    ll<- 1 - ss$upper
    uu<- 1 - ss$lower

    ss$surv<- ff
    ss$lower<- ll
    ss$upper<- uu
  }

  out<- if (any(names(fit)=="strata")) {

    ss %$%
      purrr::map2(.x= c('surv', 'conf_low', 'conf_high'),
           .y= list(surv= surv, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               dplyr::mutate(strata= strata,
                             times = time) %>%
               reshape2::melt(id.vars= c('strata', 'times'),
                    value.name = var) %>%
               dplyr::select(-variable)
           }) %>%
      purrr::reduce(dplyr::full_join, by = c('strata', 'times')) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::one_of('surv', 'conf_low', 'conf_high')),
                       function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      dplyr::mutate(stat= paste0(surv, " [", conf_low, ", ", conf_high, "]")) %>%
      reshape2::dcast(times ~ strata, value.var = 'stat')

  } else {

    ss %$%
      purrr::map2(.x= c('surv', 'conf_low', 'conf_high'),
           .y= list(surv= surv, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               dplyr::mutate(times = time) %>%
               reshape2::melt(id.vars= c('times'),
                    value.name = var) %>%
               dplyr::select(-variable)
           }) %>%
      purrr::reduce(dplyr::full_join, by = c('times')) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::one_of('surv', 'conf_low', 'conf_high')),
                       function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      dplyr::mutate(stat= paste0(surv, " [", conf_low, ", ", conf_high, "]")) %>%
      reshape2::dcast(times ~ 'Overall', value.var = 'stat')

  }
  out
}



#' @title summarize_cif
#'
#' @details
#' The function summarizes the fitted CIF at the time points specified by a user.
#'
#' @export
summarize_cif <- function(fit, times= NULL) {
  ss<- summary(fit, times= if (is.null(times)) pretty(fit$time) else times)
  colnames(ss$pstate)<- colnames(ss$lower)<- colnames(ss$upper)<- replace(ss$state, sapply(ss$states, nchar)==0, "0")
  # if (is.null(ss$prev)) ss$prev<- ss$pstate

  out<- if (any(names(fit)=="strata")) {

    ss %$%
      purrr::map2(.x= c('pstate', 'conf_low', 'conf_high'),
           .y= list(pstate= pstate, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               dplyr::mutate(strata= strata,
                             times = time) %>%
               reshape2::melt(id.vars= c('strata', 'times'),
                    value.name = var,
                    variable.name = 'states')
           }) %>%
      purrr::reduce(dplyr::full_join, by = c('strata', 'times', 'states')) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::one_of('pstate', 'conf_low', 'conf_high')),
                       function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      dplyr::mutate(stat= paste0(pstate, " [", conf_low, ", ", conf_high, "]")) %>%
      reshape2::dcast(times ~ states + strata, value.var = 'stat')

  } else {

    ss %$%
      purrr::map2(.x= c('pstate', 'conf_low', 'conf_high'),
           .y= list(pstate= pstate, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               dplyr::mutate(times = time) %>%
               reshape2::melt(id.vars= c('times'),
                    value.name = var,
                    variable.name = 'states')
           }) %>%
      purrr::reduce(dplyr::full_join, by = c('times', 'states')) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::one_of('pstate', 'conf_low', 'conf_high')),
                       function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      dplyr::mutate(stat= paste0(pstate, " [", conf_low, ", ", conf_high, "]")) %>%
      reshape2::dcast(times ~ states, value.var = 'stat')
  }
  out
}


#' @title summarize_coxph
#'
#' @details
#' The function summarizes the fitted cox model with the type 3 error based on Wald's statistics.
#'
#' @export
summarize_coxph <- function(mdl, exponentiate= TRUE, maxlabel= 100, alpha= 0.05) {

  if (!any(class(mdl) %in% c("coxph", "coxph.penal"))) stop("Not a coxph or coxph.penal object.")

  out<- summary(mdl, maxlabel= maxlabel)$coefficient %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term")

  if (any(class(mdl)== "coxph.penal")) {
    out<- dplyr::rename(out, se= 'se(coef)')
  } else if (all(class(mdl)== "coxph")) {
    out<- dplyr::rename(out, se= 'se(coef)', p= 'Pr(>|z|)')
    # names(out)[grep("^p", names(out), ignore.case = TRUE)]<- "p"
  }

  out<- out %>%
    dplyr::mutate(conf_low = coef - stats::qnorm(1-alpha/2) * se,
                  conf_high= coef + stats::qnorm(1-alpha/2) * se,
                  coef     = if (exponentiate) exp(coef) else coef,
                  conf_low = if (exponentiate) exp(conf_low) else conf_low,
                  conf_high= if (exponentiate) exp(conf_high) else conf_high,
                  stat= ifelse(is.na(coef), NA_character_,
                               paste0(formatC(coef, format= "f", digits= 3, flag= "#"), " [",
                                      formatC(conf_low, format= "f", digits= 3, flag= "#"), ", ",
                                      formatC(conf_high, format= "f", digits= 3, flag= "#"), "]")),
                  pval= format_pvalue(p)) %>%
    dplyr::select(dplyr::one_of(c("term", "stat", "pval")))

  type3_coxph<- function(mdl, beta_var= vcov(mdl)) {
    x<- stats::model.matrix(mdl)
    varseq <- attr(x, "assign")
    out<- lapply(unique(varseq),
                 function(i){
                   df<- sum(varseq==i)
                   # set out the contrast matrix
                   L<- matrix(0, nrow= df, ncol= ncol(x))
                   L[, varseq==i]<- diag(df)

                   #
                   vv<- L %*% beta_var %*% t(L)
                   cc<- L %*% coef(mdl)

                   # calculate Wald's test statistics and p-value
                   wald_stat<- as.numeric( t(cc) %*% solve(vv) %*% cc )
                   pval<- stats::pchisq(wald_stat,
                                 df= if (any(class(mdl)=="coxph.penal") && !is.na(mdl$df[i])) mdl$df[i] else df,
                                 lower.tail  = FALSE)

                   data.frame(df= round(df, 0), stat= wald_stat, chisq_p= pval)
                 })
    out<- do.call(rbind, out)
    # out<- cbind(variable= attr(mdl$terms, "term.labels"), out)

    # term_excld<- attr(mdl$terms, "response")
    # term_excld<- if (!is.null(attr(mdl$terms, "specials"))) c(term_excld, unlist(attr(mdl$terms, "specials")))
    # term_excld<- if (!is.null(attr(mdl$terms, "specials"))) c(term_excld, unlist(attr(mdl$terms, "specials")[c("strata", "cluster")]))
    # out<- cbind(variable= names(attr(mdl$terms, "dataClasses"))[-term_excld],
    #             out, stringsAsFactors= FALSE)
    # var_label<- attr(mdl$terms, "term.labels")[
    #   match(names(attr(mdl$terms, "dataClasses"))[-term_excld],
    #                   attr(mdl$terms, "term.labels"))
    #   ]
    var_label<- grep("(strata|cluster|tt|ridge|pspline|frailty)\\(.*\\)", attr(mdl$terms, "term.labels"), value = T, invert = T)
    out<- cbind(variable= var_label, out, stringsAsFactors= FALSE)
    out
  }

  type3_out<- type3_coxph(mdl)

  out<- type3_out %>%
     dplyr::filter(df> 1) %>%
    dplyr::mutate(pval= format_pvalue(chisq_p)) %>%
    dplyr::select(variable, pval) %>%
    dplyr::rename(term= variable) %>%
    dplyr::bind_rows(out) %>%
    dplyr::arrange(term) %>%
    dplyr::select(term, stat, pval)

  out
}

#' @title calculate_type3_mi
#'
#' @details
#' The function calculates the  3 p-values based on Wald's statistics.
#'
#' @export
calculate_type3_mi <- function(mira_obj, vcov_fun= NULL) {
  require(mitools)
  require(sandwich)

  # to calulate the type 3 error
  # Li, Meng, Raghunathan and Rubin. Significance levels from repeated p-values with multiply-imputed data. Statistica Sinica (1991)
  # x<- model.matrix(mira_obj$analyses[[1]])
  x<- stats::model.matrix(tmp <- mice::getfit(mira_obj, 1L))
  varseq<- attr(x, "assign")
  df<- sapply(split(varseq, varseq), length)
  m <- length(mira_obj$analyses)

  # coef estimate and its vcov for each MI model
  betas <- mice::MIextract(mira_obj$analyses, fun= coef)
  vars <- mice::MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)

  # average betas and vcov cross MI mdls
  mean_betas <- purrr::reduce(betas, .f= `+`)/m
  with_var <- purrr::reduce(vars, .f= `+`)/m # with MI
  # between-MI vcov
  btwn_var <- lapply(betas, function(cc) (cc - mean_betas) %*% t(cc - mean_betas)) %>%
    purrr::reduce(.f= `+`)/(m-1)

  out<- lapply(unique(varseq),
               function(i){
                 df<- sum(varseq==i)
                 # set out the contrast matrix
                 L<- matrix(0, nrow= df, ncol= ncol(x))
                 L[, varseq==i]<- diag(df)

                 cc<- L %*% mean_betas
                 vv<- L %*% with_var %*% t(L) # with-mi vcov for beta
                 v2<- L %*% btwn_var %*% t(L) # btwn-mi vcov for beta

                 # calcualte Wald's test statistics and p-value
                 rm<- (1 + 1/m) * sum(diag(v2 %*% solve(vv))) # eqn (1.18) without dividing by k
                 wald_stat<- as.numeric( t(cc) %*% solve(vv) %*% cc )/(df + rm) # eqn (1.17)
                 # expr (1.19)
                 nu<- df * (m-1)
                 df_denominator<- if (nu> 4) {
                   4 + (nu-4)*(1 + (1-2/nu)/(rm/df))^2
                 } else {
                   0.5*(m-1)*(df+1)*(1+1/(rm/df))^2
                 }

                 pval<- stats::pf(wald_stat, df1= df, df2= df_denominator, lower.tail = FALSE)

                 out<- data.frame(var= if (i==0) '(Intercept)' else attr(tmp$terms, "term.labels")[i],
                                  rid = i,
                                  df= round(df, 0),
                                  stat= wald_stat,
                                  chisq_p= pval)
                 attr(out, "col_in_X")<- data.frame(term= colnames(x)[i==varseq],
                                                    rid= i,
                                                    stringsAsFactors = FALSE)
                 out
               })
  out
}

#' @export
summarize_mi_glm <- function(mira_obj, exponentiate= FALSE, alpha= .05, vcov_fun= NULL) {

  out<- calculate_type3_mi(mira_obj, vcov_fun= vcov_fun)
  # # to calulate the type 3 error
  # # Li, Meng, Raghunathan and Rubin. Significance levels from repeated p-values with multiply-imputed data. Statistica Sinica (1991)
  # # x<- model.matrix(mira_obj$analyses[[1]])
  # x<- model.matrix(tmp<- getfit(mira_obj, 1L))
  # varseq<- attr(x, "assign")
  # df<- sapply(split(varseq, varseq), length)
  # m <- length(mira_obj$analyses)
  #
  # # coef estimate and its vcov for each MI model
  # betas<- mice::MIextract(mira_obj$analyses, fun= coef)
  # vars <- mice::MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)
  #
  # # average betas and vcov cross MI mdls
  # mean_betas<- purrr::reduce(betas, .f= `+`)/m
  # with_var<- purrr::reduce(vars, .f= `+`)/m # with MI
  # # between-MI vcov
  # btwn_var<- lapply(betas, function(cc) (cc - mean_betas) %*% t(cc - mean_betas)) %>%
  #   purrr::reduce(.f= `+`)/(m-1)
  #
  # out<- lapply(unique(varseq),
  #              function(i){
  #                df<- sum(varseq==i)
  #                # set out the contrast matrix
  #                L<- matrix(0, nrow= df, ncol= ncol(x))
  #                L[, varseq==i]<- diag(df)
  #
  #                cc<- L %*% mean_betas
  #                vv<- L %*% with_var %*% t(L) # with-mi vcov for beta
  #                v2<- L %*% btwn_var %*% t(L) # btwn-mi vcov for beta
  #
  #                # calcualte Wald's test statistics and p-value
  #                rm<- (1 + 1/m) * sum(diag(v2 %*% solve(vv))) # eqn (1.18) without dividing by k
  #                wald_stat<- as.numeric( t(cc) %*% solve(vv) %*% cc )/(df + rm) # eqn (1.17)
  #                # expr (1.19)
  #                nu<- df * (m-1)
  #                df_denominator<- if (nu> 4) {
  #                  4 + (nu-4)*(1 + (1-2/nu)/(rm/df))^2
  #                } else {
  #                  0.5*(m-1)*(df+1)*(1+1/(rm/df))^2
  #                }
  #
  #                pval<- stats::pf(wald_stat, df1= df, df2= df_denominator, lower.utils::tail= FALSE)
  #
  #                data.frame(rid = i,
  #                           df= round(df, 0),
  #                           stat= wald_stat,
  #                           chisq_p= pval)
  #              })
  # names(out)<- c('(Intercept)',
  #                attr(tmp$terms, "term.labels"))

  # the order of the variables in the output
  out_tmp<- out %>%
    lapply(function(x) attr(x, "col_in_X")) %>%
    dplyr::bind_rows()

  type3_out<- out %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(pval= format_pvalue(chisq_p))

  glm_out<- mice::MIcombine(mice::MIextract(mira_obj$analyses, fun= coef),
                      mice::MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)) %$%
    data.frame(est= coefficients,
               se = sqrt(diag(variance))) %>%
    tibble::rownames_to_column(var= "term") %>%
    dplyr::right_join(out_tmp, by= "term") %>%
    dplyr::mutate(conf_low = est - stats::qnorm(1-alpha/2) * se,
                  conf_high= est + stats::qnorm(1-alpha/2) * se,
                  est      = if (exponentiate) exp(est) else est,
                  conf_low = if (exponentiate) exp(conf_low) else conf_low,
                  conf_high= if (exponentiate) exp(conf_high) else conf_high,
                  stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
                  pval= type3_out$pval[charmatch(gsub("TRUE$", "", term), type3_out$var)]) %>%
    select(term, stat, pval, rid, dplyr::everything())  %>%
    dplyr::rename(var= term)

  # dplyr::mutate(var= as.character(term),
  #        # est      = if (exponentiate) exp(est) else est,
  #        # conf_low = if (exponentiate) exp(conf_low) else conf_low,
  #        # conf_high= if (exponentiate) exp(conf_high) else conf_high,
  #        stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
  #        pval= format_pvalue(pval)) %>%
  # dplyr::select(var, stat, pval, est, conf_low, conf_high) %>%
  # dplyr::full_join(out_tmp, by= c("var" = "term"))


  type3_out<- type3_out %>%
     dplyr::filter(df>1) %>%
    dplyr::select(var, pval, rid)

  glm_out %>%
    dplyr::bind_rows(type3_out) %>%
    dplyr::arrange(rid, var) %>%
    select(var, stat, pval, dplyr::everything())

  # type3_out<- out %>%
  #   dplyr::bind_rows(.id= "var") %>%
  #   dplyr::mutate(pval= format_pvalue(chisq_p)) %>%
  #    dplyr::filter(df>1) %>%
  #   dplyr::select(var, pval)
  #
  # type3_out$rid<- sapply(type3_out$var,
  #                        function(x) min(grep(x, glm_out$var, ignore.case = TRUE)))
  #
  # glm_out %>%
  #   dplyr::mutate(rid= 1:n()) %>%
  #   dplyr::bind_rows(type3_out) %>%
  #   dplyr::arrange(rid, var) %>%
  #   select(-rid)
}

#' @export
summarize_mi_coxph <- function(cox_mira, exponentiate= TRUE, alpha= .05) {

  # # to calulate the type 3 error
  # # Li, Meng, Raghunathan and Rubin. Significance levels from repated p-values with multiply-imputed data. Statistica Sinica (1991)
  # # x<- model.matrix(cox_mira$analyses[[1]])
  # x<- model.matrix(tmp<- getfit(cox_mira, 1L))
  # varseq<- attr(x, "assign")
  # df<- sapply(split(varseq, varseq), length)
  # m <- length(cox_mira$analyses)
  #
  # # coef estimate and its vcov for each MI model
  # betas<- mice::MIextract(cox_mira$analyses, fun= coef)
  # vars <- mice::MIextract(cox_mira$analyses, fun= vcov)
  #
  # # average betas and vcov cross MI mdls
  # mean_betas<- purrr::reduce(betas, .f= `+`)/m
  # with_var<- purrr::reduce(vars, .f= `+`)/m # within MI
  # # between-MI vcov
  # btwn_var<- lapply(betas, function(cc) (cc - mean_betas) %*% t(cc - mean_betas)) %>%
  #   purrr::reduce(.f= `+`)/(m-1)
  #
  # out<- lapply(unique(varseq),
  #              function(i){
  #                df<- sum(varseq==i)
  #                # set out the contrast matrix
  #                L<- matrix(0, nrow= df, ncol= ncol(x))
  #                L[, varseq==i]<- diag(df)
  #
  #                cc<- L %*% mean_betas
  #                vv<- L %*% with_var %*% t(L) # with-mi vcov for beta
  #                v2<- L %*% btwn_var %*% t(L) # btwn-mi vcov for beta
  #
  #                # calculate Wald's test statistics and p-value
  #                rm<- (1 + 1/m) * sum(diag(v2 %*% solve(vv))) # eqn (1.18) without dividing by k
  #                wald_stat<- as.numeric( t(cc) %*% solve(vv) %*% cc )/(df + rm) # eqn (1.17)
  #                # expr (1.19)
  #                nu<- df * (m-1)
  #                df_denominator<- if (nu> 4) {
  #                  4 + (nu-4)*(1 + (1-2/nu)/(rm/df))^2
  #                } else {
  #                  0.5*(m-1)*(df+1)*(1+1/(rm/df))^2
  #                }
  #
  #                pval<- stats::pf(wald_stat, df1= df, df2= df_denominator, lower.utils::tail= FALSE)
  #
  #                data.frame(rid = i,
  #                           df= round(df, 0),
  #                           stat= wald_stat,
  #                           chisq_p= pval)
  #              })
  out<- calculate_type3_mi(cox_mira)
  # names(out)<- attr(tmp$terms, "term.labels")[unique(varseq)]
  # the order of the variables in the output
  out_tmp<- out %>%
    lapply(function(x) attr(x, "col_in_X")) %>%
    dplyr::bind_rows()

  type3_out<- out %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(pval= format_pvalue(chisq_p)) %>%
     dplyr::filter(df>1) %>%
    dplyr::select(var, pval, rid)

  cox_out<- cox_mira %>%
    # mice::pool() %>%
    # see https://github.com/amices/mice/issues/246#
    mice::pool(dfcom = mice::getfit(., 1L)$nevent - length(coef(mice::getfit(., 1L)))) %>%
    summary(conf.int = TRUE,
            conf.level = 1-alpha,
            exponentiate= exponentiate) %>%
    # as.data.frame() %>%
    # tibble::rownames_to_column("var") %>%
    dplyr::rename(est      = estimate,
                 pval     = p.value,
                 conf_low = `2.5 %`,
                 conf_high= `97.5 %`) %>%
    dplyr::mutate(var= as.character(term),
                  stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
                  pval= format_pvalue(pval)) %>%
    dplyr::select(var, stat, pval, est, conf_low, conf_high) %>%
    dplyr::full_join(out_tmp, by= c("var" = "term"))

  cox_out %>%
    dplyr::bind_rows(type3_out) %>%
    dplyr::arrange(rid, var) %>%
    select(var, stat, pval, dplyr::everything())
  # dplyr::bind_rows(cox_out, type3_out) %>% dplyr::arrange(rid, var)
}

#' @export
generate_mi_glm_termplot_df <- function(mira_obj,
                                       terms= NULL,
                                       center_at= NULL,
                                       vcov_fun= NULL, ...) {
  require(mitools)
  dummy_mdl<- mice::getfit(mira_obj, 1L)
  tt<- stats::terms(dummy_mdl)
  terms<- if (is.null(terms)) 1:length(labels(tt)) else terms
  cn<- attr(tt, "term.labels")[terms]
  varseq<- attr(mm_orig<- stats::model.matrix(dummy_mdl), "assign")

  carrier.name <- function(term) {
    if (length(term) > 1L)
      carrier.name(term[[2L]])
    else as.character(term)
  }

  betas <- mice::MIextract(mira_obj$analyses,
                     fun = coef)

  vars  <- mice::MIextract(mira_obj$analyses,
                     fun = if (is.null(vcov_fun)) vcov else vcov_fun)

  mi_res<- mice::MIcombine(betas, vars)

  dummy_mdl$coefficients<- mi_res$coefficients

  plot_d<- stats::termplot(dummy_mdl, terms= terms, plot= FALSE)

  plot_d<- mapply(function(df, cc, var, tt) {

    mm<- matrix(apply(mm_orig, 2, mean),
                nrow = nrow(df),
                ncol = length(varseq),
                byrow = T)
    colnames(mm)<- colnames(mm_orig)
    rownames(mm)<- df$x

    # calculate fitted value
    df<- if (!is.null(cc)) {
      which_x<- if (is.factor(df$x)) {
        which(df$x==cc)
      } else if (is.logical(df$x)) {
        which(!df$x)
      } else {
        min(which(df$x>=cc))
      }
      dplyr::mutate(df, y = y - y[which_x])
    } else df

    # now calculate the standard error
    tmp<- df
    names(tmp)<- gsub("x", sapply(str2expression(var), carrier.name), names(tmp))
    mm[, tt == varseq]<- (stats::model.matrix(stats::as.formula(paste("~ ", var)), data= tmp)[,-1])
    df$se<- sqrt(diag(mm %*% mi_res$variance %*% t(mm)))

    df %>%
      dplyr::mutate(conf_low= y - stats::qnorm(0.975) * se,
                    conf_high= y + stats::qnorm(0.975) * se)
  },
  df= plot_d,
  cc= if (is.null(center_at)) vector("list", length(terms)) else center_at,
  var= cn,
  tt= terms,
  SIMPLIFY = FALSE)

  # the next two (commented out) lines to check if I constructed the design matrix correctly
  # they should equal plot_d$y
  # xx<- as.numeric(mm %*% mi_res$coefficients)
  # xx<- xx - xx[which_x]

  plot_d
}




