
# Event time desp -------------------------------------------------------------------------


#' @title extract_atrisk
#'
#' @details
#' The function creates a dataframe containing the number of at risk patients at time-, overall or by strata
#'
#' @param fit a survfit object
#' @param time.list a numeric vector specifying the time points at which the number of at-risk subjects is calculated.
#' @return A dataframe containing the number of at risk patients at time-, overall or by strata
#' @export
extract_atrisk <- function(fit, time.list, time.scale= 1) {

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
      # tibble::rownames_to_column("time") %>%
      dplyr::mutate(time= time.list) %>%
      dplyr::select(time, dplyr::everything())
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

  atRiskPts<- if (any(names(fit)=="start.time")) dplyr::filter(atRiskPts, time >= fit$start.time) else atRiskPts

  return(atRiskPts)
}




#' @export
prepare_survfit <- function(surv_obj) {

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

    out<- tibble::tibble(strata   = stemp,
                 # state    = rep(surv_obj$state, each= length(surv_obj$time)),
                 state    = rep(replace(surv_obj$state, nchar(surv_obj$state)==0 | grepl("0", surv_obj$state), "0"),
                                each= length(surv_obj$time)),
                 time     = rep(surv_obj$time, length(surv_obj$state)),
                 prob     = as.numeric(surv_obj$pstate),
                 conf_low = as.numeric(surv_obj$lower),
                 conf_high= as.numeric(surv_obj$upper))
    if (class(out$strata)!= 'factor') out$strata<- factor(out$strata)
    if (class(out$state) != 'factor') out$state <- stats::relevel(factor(out$state), ref= '0')
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

    tibble::tibble(strata   = stemp,
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
      dplyr::group_by(strata, state) %>%
      tidyr::nest() %>%
      dplyr::mutate(plot_prob_d= purrr::map2(state, data,
                                      function(state, df) {
                                        df<- df %>%
                                          dplyr::select(dplyr::one_of("time", "prob")) %>%
                                          dplyr::bind_rows(tibble::tribble(~time, ~prob,
                                                            0, as.numeric(state=="0")))

                                        df %>%
                                          dplyr::arrange(time, if (state!="0") prob else  dplyr::desc(prob))
                                      }))
  } else {

    surv_obj %>%
      prepare_surv() %>%
      dplyr::group_by(strata) %>%
      tidyr::nest() %>%
      dplyr::mutate(data= purrr::map(data,
                              function(df) {
                                remove<- duplicated(df$prob)
                                if (remove[length(remove)]) remove[length(remove)]<- FALSE
                                df[!remove,]
                              }),
                    plot_prob_d= purrr::map(data,
                                     function(df) {
                                       df<- df %>%
                                         dplyr::select(dplyr::one_of("time", "prob"))

                                       df<- if (is.na(match(0, df$time))) {
                                         # if time 0 is not included in the estimate
                                         # time-prob (i.e., no events occur at time 0),
                                         # then add time = 0 and prob = 1

                                         df %>%
                                           dplyr::bind_rows(
                                             tibble::tribble(~time, ~prob,
                                                     0, 1)
                                           )
                                       } else df

                                       df %>%
                                         dplyr::arrange(time)

                                     }))
  }

  out<- out %>%
    dplyr::mutate(plot_ci_d= purrr::map(data,
                                 function(df) {
                                   nn<- nrow(df)
                                   # check http://stackoverflow.com/questions/33874909/how-do-i-add-shading-and-color-to-the-confidence-intervals-in-ggplot-2-generated
                                   ys<- rep( 1:nn, each = 2 )[ -2 * nn ]
                                   xs<- c( 1, rep( 2:nn, each = 2))

                                   df %$%
                                     tibble::tibble(time     = time[xs],
                                            conf_low = conf_low[ys],
                                            conf_high= conf_high[ys]) #%>%
                                   # dplyr::filter(!(is.na(conf_low) & is.na(conf_high)))
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
  # tibble::as_tibble(risk_tbl)

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
               list(evt_time = rlang::quo_get_expr(evt_time),
                    evt     = rlang::quo_get_expr(evt),
                    df      = df))
  } else {
    substitute(survfit(Surv(evt_time, evt) ~ grp, data= df, conf.type= "log-log", ...),
               list(evt_time = rlang::quo_get_expr(evt_time),
                    evt     = rlang::quo_get_expr(evt),
                    grp     = rlang::quo_get_expr(group),
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

  pval<- stats::pchisq(test$chisq, df= length(test$n) - 1, lower.tail = FALSE)
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
  #              list(evt_time= rlang::quo_get_expr(evt_time),
  #                   evt     = rlang::quo_get_expr(evt),
  #                   df      = df))
  # } else {
  #   substitute(survfit(Surv(evt_time, evt, type= "mstate") ~ grp, data= df, ...),
  #              list(evt_time= rlang::quo_get_expr(evt_time),
  #                   evt     = rlang::quo_get_expr(evt),
  #                   grp     = rlang::quo_get_expr(group),
  #                   df      = df))
  # }
  out<- if (quo_is_missing(group)) {
    substitute(survfit(Surv(evt_time, evt) ~ 1, data= df, ...),
               list(evt_time= rlang::quo_get_expr(evt_time),
                    evt     = rlang::quo_get_expr(evt),
                    df      = df))
  } else {
    substitute(survfit(Surv(evt_time, evt) ~ grp, data= df, ...),
               list(evt_time= rlang::quo_get_expr(evt_time),
                    evt     = rlang::quo_get_expr(evt),
                    grp     = rlang::quo_get_expr(group),
                    df      = df))
  }
  out<- eval(out)
  out
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

#' @title construct_surv_var
#'
#' @details
#' The function creates time-to-event variables for a binary (survival) process.
#'
#' @param df input data
#' @param idx_dt the index date
#' @param evt_dt the date of the event occurrence. Its value should be NA for non-event subjects.
#' @param end_dt the date of the last follow-up
#' @param patid the variable indicating subject/patient id
#' @param surv_varname an option of character vector of length 2, the 1st of which is the name of the time variable; the other is the name of the event indicator.
#' @return A data frame with patid, evt_time and evt.
#' @export
construct_surv_var <- function(df, patid, idx_dt, evt_dt, end_dt, surv_varname = NULL, append = FALSE) {

  # date of origin in R:   1970-01-01
  # date of origin in SAS: 1960-01-01

  ## Excel is said to use 1900-01-01 as day 1 (Windows default) or
  ## 1904-01-01 as day 0 (Mac default), but this is complicated by Excel
  ## thinking 1900 was a leap year.
  ## So for recent dates from Windows Excel
  #       as.Date(35981, origin="1899-12-30") # 1998-07-05
  ## and Mac Excel
  #       as.Date(34519, origin="1904-01-01") # 1998-07-05

  idx_dt <- rlang::enquo(idx_dt)
  evt_dt <- rlang::enquo(evt_dt)
  end_dt <- rlang::enquo(end_dt)
  patid  <- rlang::enquo(patid)

  if (rlang::quo_is_missing(idx_dt)) stop("No index date (time zero).")
  if (rlang::quo_is_missing(evt_dt)) stop("No event date.")
  if (rlang::quo_is_missing(end_dt)) stop("No date of the end of follow-up.")
  if (rlang::quo_is_missing(patid))  stop("Please provide subject id")

  tmp_df <- df %>%
    dplyr::mutate(tmp_idx_dt= as.Date(as.character(!!idx_dt), origin= "1970-01-01"),
                  tmp_evt_dt= as.Date(as.character(!!evt_dt), origin= "1970-01-01"),
                  tmp_end_dt= as.Date(as.character(!!end_dt), origin= "1970-01-01"),
                  evt       = ifelse(is.na(tmp_evt_dt), 0L, 1L),
                  time2evt  = as.numeric(ifelse(is.na(tmp_evt_dt),
                                                tmp_end_dt - tmp_idx_dt,
                                                tmp_evt_dt - tmp_idx_dt)),
    ) %>%
    dplyr::select(!!patid, time2evt, evt, dplyr::matches("^tmp_(idx|evt|end)_dt$"))

  flag<- FALSE

  flag_df <- tmp_df %>%
     dplyr::filter(time2evt <= 0) %>%
    dplyr::mutate(flag_evt_time_zero = (time2evt == 0),
                  flag_evt_time_neg = (time2evt< 0))

  if (any(tmp_df$time2evt==0)) {
    warning("Event at time zero")
    tmp_df$time2evt <- replace(tmp_df$time2evt, tmp_df$time2evt==0, 0.5)
    flag<- TRUE
  }

  if (any(tmp_df$time2evt<0)) {
    warning("Negative time-to-event!?")
    tmp_df$time2evt <- replace(tmp_df$time2evt, tmp_df$time2evt<0, NA)
    flag <- TRUE
  }

  if (flag) print(as.data.frame(flag_df))

  tmp_df<- if (is.null(surv_varname)) {
    dplyr::rename(tmp_df,
                        evt_time= time2evt)
  } else {
    dplyr::rename(tmp_df,
                        !!surv_varname[1]:= time2evt,
                        !!surv_varname[2]:= evt)
  }

  if (append) {
    df %>%
      dplyr::inner_join(dplyr::select(tmp_df, -dplyr::matches("^tmp_(idx|evt|end)_dt$")), by= rlang::as_name(patid))
  } else {
    tmp_df %>%
      dplyr::select(-dplyr::matches("^tmp_(idx|evt|end)_dt$"))
  }

}

#' @title construct_cmprisk_var
#'
#' @details
#' The function creates time-to-event variables for competing risk data
#'
#' @export
construct_cmprisk_var <- function(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname= NULL, append= FALSE, ...) {
  patid <- rlang::enquo(patid)
  idx_dt <- rlang::enquo(idx_dt)
  evt_dt <- rlang::enquo(evt_dt)
  end_dt <- rlang::enquo(end_dt)
  cmp_evt_dt <- rlang::enquos(...)

  if (rlang::quo_is_missing(idx_dt)) stop("No index date (time zero).")
  if (rlang::quo_is_missing(evt_dt)) stop("No event date.")
  if (rlang::quo_is_missing(end_dt)) stop("No date of the end of follow-up.")
  if (rlang::quo_is_missing(patid))  stop("Please provide subject id")

  n_cmp_evt<- length(cmp_evt_dt)
  names(cmp_evt_dt)<- sapply(cmp_evt_dt, rlang::as_name) # without, dplyr::select(df, !!!cmp_evt_dt) changes the variable name in the output data

  cmp_evt_desc<- paste0('cmp_evt_', seq_len(n_cmp_evt) + 1)
  evt_desc<- c('evt_1', cmp_evt_desc, 'censored_0')

  tmp_df<- df %>%
    dplyr::select(!!patid, !!idx_dt, !!evt_dt, !!!cmp_evt_dt, !!end_dt) %>%
     dplyr::group_by(!!patid) %>%
    # dplyr::mutate(first_evt_dt= pmin(!!evt_dt, !!!cmp_evt_dt, !!end_dt, na.rm= TRUE))
    dplyr::mutate(first_evt= ifelse(rlang::is_empty((evt_desc[which.min(c(!!evt_dt, !!!cmp_evt_dt, !!end_dt))])),
                                    NA, (evt_desc[which.min(c(!!evt_dt, !!!cmp_evt_dt, !!end_dt))])),
                  first_evt_dt= pmin(!!evt_dt, !!!cmp_evt_dt, !!end_dt, na.rm= TRUE),

                  time2evt=dplyr::case_when(
                    is.infinite(first_evt_dt) | is.na(first_evt_dt) ~ NA_real_,
                    TRUE ~ as.numeric(first_evt_dt - !!idx_dt)),

                  # Steve: this is the part of the code that generates warning messages.
                  # evt=dplyr::case_when(
                  #   is.na(time2evt) ~ NA_integer_,
                  #   first_evt=='censored' ~ 0L,
                  #   first_evt=='evt' ~ 1L,
                  #   TRUE ~ as.integer(gsub('^cmp_evt_', '', first_evt))),

                  evt=dplyr::case_when(
                    is.na(time2evt) ~ NA_integer_,
                    TRUE ~ as.integer(gsub('^(cmp_evt|evt|censored)_', '', first_evt))),
    ) %>%
    dplyr::ungroup()

  # per documentation, evt will be treated as a factor with the 1st level as censoring
  # in the situtation in which no pts are censored, no observations have a value of zero.
  # need to convert evt to a factor forcing 0 as the first level of the factor.
  tmp_df<- tmp_df %>%
    dplyr::mutate(evt= factor(evt, 0:(n_cmp_evt + 1), labels = 0:(n_cmp_evt + 1)))


  flag<- FALSE
  flag_df<- tmp_df %>%
     dplyr::filter(time2evt<=0) %>%
    dplyr::mutate(flag_evt_time_zero= (time2evt==0),
                  flag_evt_time_neg = (time2evt< 0))

  if (any(tmp_df$time2evt==0)) {
    warning("Event at time zero")
    tmp_df$time2evt<- replace(tmp_df$time2evt, tmp_df$time2evt==0, 0.5)
    flag <- TRUE
  }

  if (any(tmp_df$time2evt<0)) {
    warning("Negative time-to-event!?")
    tmp_df$time2evt<- replace(tmp_df$time2evt, tmp_df$time2evt<0, NA)
    flag <- TRUE
  }

  if (flag) print(as.data.frame(flag_df))

  tmp_df <- if (is.null(cmprisk_varname)) {
    tmp_df %>%
      select(!!patid, time2evt, evt) %>%
      dplyr::rename(evt_time= time2evt)
  } else {
    tmp_df %>%
      select(!!patid, time2evt, evt) %>%
      dplyr::rename(!!cmprisk_varname[1]:= time2evt,
                   !!cmprisk_varname[2]:= evt)
  }

  # tmp_df<- dplyr::select(tmp_df, !!patid, dplyr::one_of(c(cmprisk_varname, 'evt_time', 'evt')))

  if (!append) tmp_df else {
    df %>%
      dplyr::inner_join(tmp_df, by= rlang::as_name(patid))
  }
}
# debug(construct_cmprisk_var)
# onstruct_cmprisk_var(df= test,
#                      patid= patid,
#                      idx_dt= idx_dt,
#                      evt_dt= evt1_dt,
#                      dth_dt= evt2_dt,
#                      rec_dt= evt3_dt,
#                      end_dt= end_dt,
#                      cmprisk_varname = c('day_evt', 'status'),
#                      append = FALSE)


#' @title admin_censor_surv
#'
#' @details
#' The function creates time-to-event variables with the application of administrative censoring for a binary (survival) process.
#' The newly created variables are named by the same variables names but with a suffix of '_adm' by default. The original
#' variables can be overwritten by specifying overwrite_var= TRUE. Overwriting the original variables is not recommended, but
#' it can be useful in some situations.
#'
#' @param df input data
#' @param evt_time a numeric vector recording the time points at which the event occurs.
#' @param evt an integer vector indicating right censoring (0= censored; 1= event).
#' @param adm_cnr_time a numeric scalar specifying the time point at which administrative censoring is applied.
#' @param overwrite_var a logical scalar (default= FALSE) indiciates if the existing time-to-event variables should be overwritten.
#' @return The input data plus censored time-to-event variables.
#' @export
admin_censor_surv <- function(df, evt_time, evt, adm_cnr_time= NULL, overwrite_var= FALSE) {
  ######################################################################################
  ## the function creates administrately censored version of event time and indicator ##
  ## for survival (binary) process                                                    ##
  ##   df - input dataframe                                                           ##
  ##   evt_time - continuous time to event                                            ##
  ##   evt - event indicator (1= event; 0= non-event)                                 ##
  ##   adm_cnr_time - time at which admin censoring is applied                        ##
  ######################################################################################

  evt_time<- rlang::enquo(evt_time)
  evt     <- rlang::enquo(evt)

  if (!is.null(adm_cnr_time)) {

    if (overwrite_var) {
      cnr_evt_time_name<- rlang::as_name(evt_time)
      cnr_evt_name     <- rlang::as_name(evt)
    } else {
      cnr_evt_time_name<- paste0(rlang::as_name(evt_time), "_adm")
      cnr_evt_name     <- paste0(rlang::as_name(evt), "_adm")
    }

    df<- df %>%
      dplyr::mutate(!!cnr_evt_name      := replace(!!evt, !!evt_time> adm_cnr_time & !!evt!=0, 0),
                    !!cnr_evt_time_name := replace(!!evt_time, !!evt_time>adm_cnr_time, adm_cnr_time))
  }

  df
}

#' @title admin_censor_cmprisk
#'
#' @details
#' The function creates time-to-event variables with the application of administrative censoring for a competing risk
#' analysis. The newly created variables are named by the same variables names but with a suffix of '_adm' by default.
#' The original variables can be overwritten by specifying overwrite_var= TRUE. Overwriting the original variables is
#' not recommended, but it can be useful in some situation.
#'
#' @param df input data
#' @param evt_time a numeric vector recording the time points at which the event occurs.
#' @param evt a factor vector indicating right censoring (0= censored; 1= event of interest; other= competing risk(s)).
#' @param adm_cnr_time a numeric vector specifying the time point at which administrative censoring is applied.
#' @param evt_label a numeric vector specifying the time point at which administrative censoring is applied.
#' @param overwrite_var a logical scalar (default= FALSE) indiciates if the existing time-to-event variables should be overwritten.
#' @return The input data plus censored time-to-event variables.
#' @export
#' @importFrom magrittr %>%
#' @importFrom data.table :=
admin_censor_cmprisk<- function(df, evt_time, evt, adm_cnr_time= NULL, evt_label= NULL, overwrite_var= FALSE) {

  evt_time<- rlang::enquo(evt_time)
  evt<- rlang::enquo(evt)

  if (is.null(adm_cnr_time)) {

    stop("No administrative censor time is given.")
    # if (!is.null(evt_label)) {
    #   # df<- df %>%
    #   #   dplyr::mutate(!!rlang::as_name(evt):= factor(!!evt, as.integer(names(evt_label)), labels = evt_label))
    #   # df[[rlang::as_name(evt)]]<- evt_label[levels(df[[rlang::as_name(evt)]])]
    #   df$rlang::as_name(evt)<- evt_label[levels(df$rlang::as_name(evt))]
    # }

  } else {

    if (overwrite_var) {
      cnr_evt_time_name<- rlang::as_name(evt_time)
      cnr_evt_name     <- rlang::as_name(evt)
    } else {
      cnr_evt_time_name<- paste0(rlang::as_name(evt_time), "_adm")
      cnr_evt_name     <- paste0(rlang::as_name(evt), "_adm")
    }

    df<- if (is.null(evt_label)) {
      df %>%
        dplyr::mutate(!!cnr_evt_name      := replace(!!evt, !!evt_time> adm_cnr_time & !!evt!= "0", "0"),
                      !!cnr_evt_time_name := replace(!!evt_time, !!evt_time>adm_cnr_time, adm_cnr_time))
    } else {
      df %>%
        dplyr::mutate(!!cnr_evt_name      := factor(replace(!!evt, !!evt_time> adm_cnr_time & !!evt!= "0", "0"),
                                                    # as.integer(names(evt_label)),
                                                    names(evt_label),
                                                    labels = evt_label),
                      !!cnr_evt_time_name := replace(!!evt_time, !!evt_time>adm_cnr_time, adm_cnr_time))
    }
  }

  # df<- if (overwrite_var | is.null(evt_label)) df else dplyr::mutate(df,
  #                                                             !!rlang::quo_name(evt):= factor(!!evt, as.integer(names(evt_label)), labels = evt_label))
  df
}


#' @title summarize_km
#'
#' @details
#' The function summarize the fitted KM at the time points specified by a user.
#'
#' @export
summarize_km<- function(fit, times= NULL, failure_fun= FALSE) {
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
summarize_cif<- function(fit, times= NULL) {
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
summarize_coxph<- function(mdl, exponentiate= TRUE, maxlabel= 100, alpha= 0.05) {

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
calculate_type3_mi<- function(mira_obj, vcov_fun= NULL) {
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
summarize_mi_glm<- function(mira_obj, exponentiate= FALSE, alpha= .05, vcov_fun= NULL) {

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
summarize_mi_coxph<- function(cox_mira, exponentiate= TRUE, alpha= .05) {

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
generate_mi_glm_termplot_df<- function(mira_obj,
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




