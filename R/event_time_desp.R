#' @title extract_atrisk
#'
#' @details
#' The function creates a dataframe containing the number of at risk patients at time-, overall or by strata
#'
#' @param fit a survfit object
#' @param time.list a numeric vector specifying the time points at which the number of at-risk subjects is calculated.
#' @return A dataframe containing the number of at risk patients at time-, overall or by strata
#' @export
extract_atrisk<- function(fit, time.list, time.scale= 1) {

  if (any(names(fit)=="strata")){
    strata_lab<- sapply(strsplit(names(fit$strata), "="), function(x) x[2])

    x<- data.frame(time = fit$time/time.scale,
                   n.risk= if (is.matrix(fit$n.risk)) apply(fit$n.risk, 1, sum) else fit$n.risk,
                   strata = factor(unlist(mapply(rep, x = seq_along(fit$strata), each = fit$strata, SIMPLIFY = FALSE)),
                                   seq_along(fit$strata),
                                   labels = strata_lab))

    # at risk process is right-continuous
    # Steve note: have to fix the following line `rule` for data with delayed entries.
    # approxfun(x= time, y= n.risk, method= "constant", rule = 1:1, f= 1)
    atRisk<- lapply(split(x, x$strata),
                    function(x) with(x,
                                     approxfun(x= time, y= n.risk, method= "constant", rule = 2:1, f= 1)
                    )
    )

    atRiskPts<- rapply(atRisk,
                       function(x) {
                         out<- x(time.list)
                         replace(out, is.na(out), 0)
                       },
                       how = "unlist")
    atRiskPts<-  matrix(atRiskPts, nrow= length(time.list), byrow= FALSE)
    rownames(atRiskPts)<- time.list
    colnames(atRiskPts)<- strata_lab
    atRiskPts<- as.data.frame(atRiskPts) %>%
      dplyr::mutate_all(as.integer) %>%
      # rownames_to_column("time") %>%
      dplyr::mutate(time= time.list) %>%
      dplyr::select(time, everything())
  }
  else {
    x<- data.frame(time = fit$time/time.scale,
                   n.risk=  if (is.matrix(fit$n.risk)) apply(fit$n.risk, 1, sum) else fit$n.risk,
                   strata= factor(1, 1, labels = "Overall"))

    # Steve note: have to fix the following line `rule` for data with delayed entries.
    # approxfun(x= time, y= n.risk, method= "constant", rule = 1:1, f= 1)
    atRisk<- with(x, approxfun(x= time, y= n.risk, method= "constant", rule = 2:1, f= 1))
    atRiskPts<- atRisk(time.list)
    atRiskPts<- data.frame(time= time.list, Overall= as.integer(atRiskPts))
  }

  atRiskPts<- if (any(names(fit)=="start.time")) filter(atRiskPts, time >= fit$start.time) else atRiskPts

  return(atRiskPts)
}




#' @export
prepare_survfit<- function(surv_obj) {

  prepare_cmprisk<- function(surv_obj) {

    # set up strata
    if (is.null(surv_obj$strata)) {
      nstrat<- 1
      stemp <- rep(1, length(surv_obj$time) * length(surv_obj$states)) # same length as stime
      stemp <- factor(stemp, 1, labels = c("Overall"))
    }
    else {
      nstrat<- length(surv_obj$strata)
      strata_lab<- sapply(strsplit(names(surv_obj$strata), "="), function(x) x[2])
      stemp <- rep(rep(1:nstrat, surv_obj$strata), length(surv_obj$states)) # same length as stime
      stemp <- factor(stemp, 1:nstrat, strata_lab)
    }

    out<- tibble(strata   = stemp,
                 # state    = rep(surv_obj$state, each= length(surv_obj$time)),
                 state    = rep(replace(surv_obj$state, nchar(surv_obj$state)==0 | grepl("0", surv_obj$state), "0"),
                                each= length(surv_obj$time)),
                 time     = rep(surv_obj$time, length(surv_obj$state)),
                 prob     = as.numeric(surv_obj$pstate),
                 conf_low = as.numeric(surv_obj$lower),
                 conf_high= as.numeric(surv_obj$upper))
    if (class(out$strata)!= 'factor') out$strata<- factor(out$strata)
    if (class(out$state) != 'factor') out$state <- relevel(factor(out$state), ref= '0')
    out
  }

  prepare_surv<- function(surv_obj) {

    # set up strata
    if (is.null(surv_obj$strata)) {
      nstrat <- 1
      # stemp <- rep(1, length(surv_obj$time)) # same length as stime
      stemp <- factor(rep(1, length(surv_obj$time)), 1, labels = c("Overall"))
    }
    else {
      nstrat <- length(surv_obj$strata)
      strata_lab<- sapply(strsplit(names(surv_obj$strata), "="), function(x) x[2])
      # stemp <- rep(1:nstrat, surv_obj$strata) # same length as stime
      stemp <- factor(rep(1:nstrat, surv_obj$strata), 1:nstrat, strata_lab)
    }

    tibble(strata   = stemp,
           time     = surv_obj$time,
           prob     = surv_obj$surv,
           conf_low = surv_obj$lower,
           conf_high= surv_obj$upper,

           n_risk   = surv_obj$n.risk, # immediately before time t
           n_event  = surv_obj$n.event,
           n_censor = surv_obj$n.censor)
  }





  out<- if (any(class(surv_obj)=="survfitms")) {

    surv_obj %>%
      prepare_cmprisk() %>%
      group_by(strata, state) %>%
      nest() %>%
      dplyr::mutate(plot_prob_d= map2(state, data,
                               function(state, df) {
                                 df<- df %>%
                                   dplyr::select(one_of("time", "prob")) %>%
                                   bind_rows(tribble(~time, ~prob,
                                                     0, as.numeric(state=="0")))

                                 df %>%
                                   arrange(time, if (state!="0") prob else desc(prob))
                               }))
  } else {

    surv_obj %>%
      prepare_surv() %>%
      group_by(strata) %>%
      nest() %>%
      dplyr::mutate(data= map(data,
                       function(df) {
                         remove<- duplicated(df$prob)
                         if (remove[length(remove)]) remove[length(remove)]<- FALSE
                         df[!remove,]
                       }),
             plot_prob_d= map(data,
                              function(df) {
                                df<- df %>%
                                  dplyr::select(one_of("time", "prob"))

                                df<- if (is.na(match(0, df$time))) {
                                  # if time 0 is not included in the estimate
                                  # time-prob (i.e., no events occur at time 0),
                                  # then add time = 0 and prob = 1

                                  df %>%
                                    bind_rows(
                                      tribble(~time, ~prob,
                                              0, 1)
                                    )
                                } else df

                                df %>%
                                  arrange(time)

                              }))
  }

  out<- out %>%
    dplyr::mutate(plot_ci_d= map(data,
                          function(df) {
                            nn<- nrow(df)
                            # check http://stackoverflow.com/questions/33874909/how-do-i-add-shading-and-color-to-the-confidence-intervals-in-ggplot-2-generated
                            ys<- rep( 1:nn, each = 2 )[ -2 * nn ]
                            xs<- c( 1, rep( 2:nn, each = 2))

                            df %$%
                              tibble(time     = time[xs],
                                     conf_low = conf_low[ys],
                                     conf_high= conf_high[ys]) #%>%
                              # filter(!(is.na(conf_low) & is.na(conf_high)))
                          }))
  return(out)
}

#' @export
add_atrisk <- function(p, surv_obj, x_break= NULL, atrisk_init_pos= NULL, plot_theme = NULL) {

  # ---- get font information ----
  if (is.null(plot_theme)) {
    font_family<- "Arial"
    font_face  <- "plain"
    font_size  <- 11
  } else {
    font_family<- if (is.null(plot_theme$text$family) | trimws(plot_theme$text$family) == "") "Arial" else plot_theme$text$family
    font_face  <- if (is.null(plot_theme$text$face) | trimws(plot_theme$text$face) == "") "plain" else plot_theme$text$face
    font_size  <- if (is.null(plot_theme$text$size)) 11 else plot_theme$text$size
  }

  #---- get parameters required for where to include the at-risk table ----#
  # atrisk_init_pos<- -0.25 * max(diff(ggplot2::layer_scales(p)$y$range$range),
  #                            diff(p$coordinates$limits$y))
  atrisk_init_pos<- if (is.null(atrisk_init_pos)) {
    -0.225 * max(diff(ggplot2::layer_scales(p)$y$range$range),
                 diff(p$coordinates$limits$y))
  } else atrisk_init_pos

  # I need to calculate the number of at-risk at the x_break
  x_break     <- if (is.null(x_break)) {
    ggplot2::layer_scales(p)$x$get_breaks(ggplot2::layer_scales(p)$x$range$range)
  } else {
    x_break[x_break >= min(ggplot2::layer_scales(p)$x$range$range) & x_break<= max(ggplot2::layer_scales(p)$x$range$range)]
  }

  x_break<- if (any(is.na(x_break))) x_break[!is.na(x_break)] else x_break


  risk_tbl<- extract_atrisk(surv_obj, time.list= x_break)
  nstrata <- ncol(risk_tbl)-1
  # as_tibble(risk_tbl)

  # to include the at-risk table in the existing figure, I specified the location
  # (x- and y-position) for the "At-risk N:" label and for each cell entry of the
  #  at-risk table by creating separate grid::textGrob object.

  # Step 1: add a bold 'At-risk N:' label at x= 0 and y= atrisk_init_pos
  out <- p + ggplot2::annotation_custom(
    grob = grid::textGrob(label= format("At-risk N:", width = 20),
                    vjust= 1, hjust = 1,
                    gp = grid::gpar (fontfamily= font_family,
                              fontsize  = font_size,
                              fontface  = "bold")),
    ymin = atrisk_init_pos,      # Vertical position of the grid::textGrob
    ymax = atrisk_init_pos,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0)


  # where there are no strata, ncol(risk_tbl)= 2; the number at-risk is displayed at the same level as 'At-risk N:'
  # otherwise, ncol(risk_tbl)>2, the number of at-risk is displayed below 'At-risk N:'
  if (nstrata==1) {
    # no strata #
    for (i in seq_along(risk_tbl$time))  {
      out<- out + ggplot2::annotation_custom(
        grob = grid::textGrob(label = formatC(risk_tbl[i, 2],
                                        digits = 0,
                                        format = "d",
                                        big.mark = ",",
                                        flag = "#"),
                        vjust= 1, hjust = 0.5,
                        gp = grid::gpar (fontfamily= font_family,
                                  fontsize  = font_size)),
        ymin = atrisk_init_pos,      # Vertical position of the grid::textGrob
        ymax = atrisk_init_pos,
        xmin = risk_tbl$time[i],         # Note: The grobs are positioned outside the plot area
        xmax = risk_tbl$time[i])
    }
  } else if (nstrata>1) {
    # strata #

    # when there are strata, atrisk_y_inc indicates the relative position from the initial at-risk y pos.
    # atrisk_y_inc<- -0.075 * diff(ggplot2::layer_scales(p)$y$range$range)
    atrisk_y_inc<- -0.05 * max(diff(ggplot2::layer_scales(p)$y$range$range),
                               diff(p$coordinates$limits$y))

    # extract the color code used in the plot for different strata
    strata_col<- unique(ggplot2::layer_data(p)$colour)
    strata_lty<- unique(ggplot2::layer_data(p)$linetype)

    strata_col<- if (length(strata_col)==1 & length(strata_col)< nstrata) rep(strata_col, nstrata) else strata_col
    strata_lty<- if (length(strata_lty)==1 & length(strata_lty)< nstrata) rep(strata_lty, nstrata) else strata_lty

    for (j in 2:ncol(risk_tbl)) {
      tmp_y<- atrisk_init_pos + (j-1)*atrisk_y_inc

      out <- out + ggplot2::annotation_custom(
        grob = grid::textGrob(label = format(paste0(colnames(risk_tbl)[j], ":"),
                                       width = nchar(colnames(risk_tbl)[j])+ (20 - nchar("At-risk N:") + 1)),
                        vjust= 1, hjust = 1,
                        gp = grid::gpar (fontfamily= font_family,
                                  fontsize  = font_size,
                                  col   = strata_col[j-1],
                                  lty   = strata_lty[j-1])),
        ymin = tmp_y,      # Vertical position of the grid::textGrob
        ymax = tmp_y,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0)

      for (i in seq_along(risk_tbl$time)) {
        tmp_x<- risk_tbl$time[i]

        out <- out + ggplot2::annotation_custom(
          grob = grid::textGrob(label = formatC(risk_tbl[i, j],
                                          digits = 0,
                                          format = "d",
                                          big.mark = ",",
                                          flag = "#"),
                          vjust= 1, hjust = 0.5,
                          gp = grid::gpar (fontfamily= font_family,
                                    fontsize  = font_size,
                                    col   = strata_col[j-1],
                                    lty   = strata_lty[j-1])),
          ymin = tmp_y,      # Vertical position of the grid::textGrob
          ymax = tmp_y,
          xmin = tmp_x,      # Note: The grobs are positioned outside the plot area
          xmax = tmp_x)
      }
    }
  }
  out
}

#' @title estimate_km
#' @description Computes Kaplan-Meier survival estimates from a dataset, optionally stratified by a grouping variable.
#' @param df A data frame containing the survival data.
#' @param evt_time A numeric vector representing the time to event or censoring.
#' @param evt A binary event indicator (1 = event occurred, 0 = censored).
#' @param group A grouping variable for stratified survival curves.
#' @param ... Additional arguments passed to \code{survival::survfit()}.
#' @return A \code{survfit} object containing the survival estimates.
#' @details
#' The function analyzes the data (df) using Kaplan-Meier survival method with pointwise 95% CI estimated using log-log
#' transformation (same as SAS's defualt). The function store the input data in the call(), which can be used in
#' run_logrank_test().
#'
#' @export
#'
#'
estimate_km <- function(df, evt_time, evt, group, ...) {

  evt_time<- enquo(evt_time)
  evt     <- enquo(evt)
  group   <- enquo(group)

  out<- if (quo_is_missing(group)) {
    substitute(survfit(Surv(evt_time, evt) ~ 1, data= df, conf.type= "log-log", ...),
               list(evt_time = quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    df      = df))
  } else {
    substitute(survfit(Surv(evt_time, evt) ~ grp, data= df, conf.type= "log-log", ...),
               list(evt_time = quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    grp     = quo_get_expr(group),
                    df      = df))
  }
  out<- eval(out)
  out
}


#' @title Run Log-Rank Test for Survival Differences
#' @description Performs a log-rank test to compare survival distributions between two or more groups.
#' @param surv_obj A \code{survfit} object, such as one returned by \code{estimate_km()}, containing grouped survival curves.
#' @return A numeric value representing the p-value from the log-rank test.
#' @export
run_logrank_test <- function(surv_obj) {

  tmp <- surv_obj$call
  tmp[[1]] <- as.name("survdiff")
  tmp$rho <- 0
  tmp$conf.type <- NULL
  test <- eval(tmp, parent.frame())

  pval<- pchisq(test$chisq, df= length(test$n) - 1, lower.tail = FALSE)
  pval
}


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
                     plot_theme= theme_minimal(),

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
    unnest(cols = c(plot_prob_d)) %>%
    dplyr::mutate(prob= if (plot_cdf) 1-prob else prob)

  if (y_lim[2]< 1) {
    plot_prob_d<- plot_prob_d %>%
      dplyr::mutate(prob= pmin(prob, y_lim[2], na.rm= TRUE),
             # prob= pmax(prob, min(y_lim, na.rm = TRUE), na.rm= TRUE)
      ) %>%
      group_by(strata)
  }

  out <- ggplot() +
    geom_step(data= plot_prob_d,
              aes(x= time, y= prob, group= strata, color= strata),
              size= 1.1, show.legend = add_legend) +
    eval(color_fun) +
    scale_x_continuous(name  = x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                       expand= c(0.01, 0.005),
                       labels= function(x) scales::comma(x, accuracy = 1))

  if (add_ci) {
    plot_ci_d <- surv_mat %>%
      dplyr::select(strata, plot_ci_d) %>%
      unnest(cols = c(plot_ci_d))

    if (plot_cdf) {
      plot_ci_d <- plot_ci_d %>%
        dplyr::mutate_at(vars(starts_with('conf')), function(x) 1-x) %>%
        rename(conf_high= conf_low,
               conf_low = conf_high)
    }

    # if (y_lim[2]< 1) {
    #   plot_ci_d<- plot_ci_d %>%
    #     filter(conf_low <= y_lim[2],
    #            conf_high>= y_lim[1]) %>%
    #     dplyr::mutate(conf_high= replace(conf_high, conf_high> y_lim[2], y_lim[2]),
    #            conf_low = replace(conf_low, conf_low< y_lim[1], y_lim[1]))
    # }

    out <- out +
      geom_ribbon(data= plot_ci_d,
                  aes(x= time, ymin= conf_low, ymax= conf_high, fill= strata),
                  alpha= .2, show.legend = FALSE) +
      eval(fill_fun)
  }

  out <- out + scale_y_continuous(name  = y_lab,
                                 # name  = if (is.null(y_lab)) "Freedom from death" else y_lab,
                                 breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                                 # expand= c(0.01, 0.005),
                                 expand= c(0.005, 0),
                                 labels= function(x) scales::percent(x, accuracy = 1))

  out <- if (!is.null(y_lim)) out + coord_cartesian(ylim = y_lim, clip = "on") else out

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




#' @title estimate_cif
#'
#' @details
#' The function analyzes the competing data (df) using Andersen-Johansen method in estimating cumulative incidence
#' function.The function store the input data in the call(), which can be used in run_gray_test().
#'
#'
#' @export
estimate_cif <- function(df, evt_time, evt, group, ...) {

  evt_time<- enquo(evt_time)
  evt     <- enquo(evt)
  group   <- enquo(group)

  # out<- if (quo_is_missing(group)) {
  #   substitute(survfit(Surv(evt_time, evt, type= "mstate") ~ 1, data= df, ...),
  #              list(evt_time= quo_get_expr(evt_time),
  #                   evt     = quo_get_expr(evt),
  #                   df      = df))
  # } else {
  #   substitute(survfit(Surv(evt_time, evt, type= "mstate") ~ grp, data= df, ...),
  #              list(evt_time= quo_get_expr(evt_time),
  #                   evt     = quo_get_expr(evt),
  #                   grp     = quo_get_expr(group),
  #                   df      = df))
  # }
  out<- if (quo_is_missing(group)) {
    substitute(survfit(Surv(evt_time, evt) ~ 1, data= df, ...),
               list(evt_time= quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    df      = df))
  } else {
    substitute(survfit(Surv(evt_time, evt) ~ grp, data= df, ...),
               list(evt_time= quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    grp     = quo_get_expr(group),
                    df      = df))
  }
  out<- eval(out)
  out
}


#' @export
run_gray_test <- function(surv_obj, evt_type= 1:2) {

  df<- as.list(eval(surv_obj$call$data, parent.frame()))
  df<- all.vars(surv_obj$call$formula) %>%
    setNames(c("ftime", "fstatus", "group")) %>%
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
#' @param plot_theme A \code{ggplot2} theme object to customize the appearance of the plot (default = \code{theme_minimal()}).
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
#'

#' @examples
#' my_plot_theme<- theme_bw() +
#' theme(axis.title  = element_text(size= 14, family="Arial"),
#'       axis.title.x= element_text(margin= unit(c(t= 1, r = 0, b = 0, l = 0), "lines"), family="Arial"),
#'       axis.title.y= element_text(margin= unit(c(t= 0, r = 1, b = 0, l = 0), "lines"), family="Arial"),
#'       axis.text   = element_text(size= 12, family="Arial"),
#'       # set the axis line (i.e. bty= "l" in traditional plot)
#'       axis.line.x = element_line(color="black", size = 1),
#'       axis.line.y = element_line(color="black", size = 1),
#'
#'       # specify the legend (e.g. position, color, justification)
#'       legend.title= element_blank(),
#'       legend.background= element_rect(fill= "transparent"),
#'       legend.text      = element_text(family= "Arial", size = 12),
#'       legend.margin    = margin(t = 0, r = 0, b = 0, l = 0,"bigpts"),
#'       legend.key       = element_rect(fill= "transparent", color= "transparent", linetype= "blank"),
#'       legend.key.size  = unit(24, "bigpts"),
#'       # legend.position= c(0.98, 0.02),
#'       # legend.justification= c(1, 0),
#'
#'       #eliminates background, gridlines, and chart border
#'       panel.border = element_blank(),
#'       # plot.background = element_blank(),
#'       # panel.grid.major = element_blank(),
#'       panel.grid.minor = element_blank(),
#'       panel.spacing = unit(1, "lines"),
#'
#'       strip.background = element_rect(fill = "black", color= "white"),
#'       strip.text = element_text(family = "Arial", color= "white", size = 12, face= "bold"),
#'
#'       # plot margin. Sufficient margins must be specified for printing at-risk patients.
#'       plot.margin= unit(c(top= 0.1, right= 0.05, bottom= .3, left= .2), "npc"))
#'
#' # read in the example data
#' cmprisk_df<- read.csv2("http://www.stat.unipg.it/luca/misc/bmt.csv")
#' admin_censor_cmprisk(cmprisk_df, ftime, status, evt_label = c("0"= "Event free", "1"= "Event", "2"= "Competing event"), adm_cnr_time= 10)
#' p<- cmprisk_df %>%
#' admin_censor_cmprisk(ftime, status,
#'                      adm_cnr_time= 30,
#'                      overwrite_var = TRUE) %>%
#'   estimate_cif(ftime, status) %>%
#'   show_cif(evt_type = 1,
#'            plot_theme= my_plot_theme,
#'            x_break= seq(0, 10, by= 1),
#'            y_break= seq(0, .8, by= .1),
#'            add_pvalue = TRUE,
#'            add_atrisk = TRUE,
#'            add_ci = TRUE,
#'            add_legend = FALSE,
#'            color_scheme = "manual",
#'            color_list = list(values= c('blue', 'blue')),
#'            pvalue_pos= "topleft")
#'
#' #---- To show the at risk information ----
#' gt <- ggplot_gtable(ggplot_build(p))
#' gt$layout$clip[gt$layout$name == 'panel'] <- "off"
#' grid.draw(gt)
#' @export
show_cif <- function(surv_obj,
                    evt_type = 1,
                    # evt_label= identity, # identity function
                    evt_label= function(x) {
                      recode_factor(x,
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

                    plot_theme= theme_minimal(),
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
    filter(state %in% evt_type) %>%
    dplyr::mutate(state_label = evt_label(state),
           state_label = fct_drop(state_label),
           state       = fct_drop((state)),
           state_strata= interaction(state_label, strata, drop= TRUE, sep= ": "))

  plot_prob_d <- cmprisk_mat %>%
    dplyr::select(strata, state, state_label, state_strata, plot_prob_d) %>%
    unnest(cols = c(plot_prob_d))

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

  out<- ggplot()
  out<- if (nlevels(plot_prob_d$strata)==1 & nlevels(plot_prob_d$state)>1) {
    out +
      geom_step(data= plot_prob_d,
                aes(x= time, y= prob, group= state_label, color= state_label),
                size= 1.1, show.legend = add_legend)
  } else if (nlevels(plot_prob_d$strata)>1 & nlevels(plot_prob_d$state)==1) {
    out +
      geom_step(data= plot_prob_d,
                aes(x= time, y= prob, group= strata, color= strata),
                size= 1.1, show.legend = add_legend)
  } else {
    out +
      geom_step(data= plot_prob_d,
                aes(x= time, y= prob, group= state_strata, color= state_strata),
                size= 1.1, show.legend = add_legend)
  }
  out<- out +
    eval(color_fun) +
    scale_x_continuous(name  = x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                       expand= c(0.01, 0.005),
                       # limits = x_lim,
                       labels= function(x) scales::comma(x, accuracy = 1)) +
    scale_y_continuous(name  = y_lab,
                       breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                       expand= c(0.01, 0),
                       # limits= y_lim,
                       labels= function(x) scales::percent(x, accuracy = 1))

  out<- if (!is.null(x_lim) | !is.null(y_lim)) out + coord_cartesian(xlim= x_lim, ylim = y_lim, clip = "on") else out

  if (add_ci) {
    plot_ci_d <- cmprisk_mat %>%
      dplyr::select(strata, state, state_label, state_strata, plot_ci_d) %>%
      unnest(cols = c(plot_ci_d))

    out <- if (nlevels(plot_prob_d$strata)==1 & nlevels(plot_prob_d$state)>1) {

      out +
        geom_ribbon(data= plot_ci_d,
                    aes(x   = time,
                        ymin= conf_low,
                        ymax= conf_high,
                        group= state_label,
                        fill= state_label),
                    alpha= .2,
                    show.legend = FALSE)

    } else if (nlevels(plot_prob_d$strata)>1 & nlevels(plot_prob_d$state)==1) {

      out +
        geom_ribbon(data= plot_ci_d,
                    aes(x= time,
                        ymin = conf_low,
                        ymax = conf_high,
                        group= strata,
                        fill = strata),
                    alpha= .2,
                    show.legend = FALSE)

    } else {

      out +
        geom_ribbon(data= plot_ci_d,
                    aes(x= time,
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
    p <- out + theme(plot.margin= unit(c(top = top.margin, right = right.margin, bottom = bottom.margin, left= left.margin), "bigpts"))
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == 'panel'] <- "off"
    out <- gt
    #grid.draw(gt)

  }





  if (print_fig) print(out)
  return(out)
}




