
# Fan util fun -------------------------------------------------------------------------

#---- utility functions ----

#' @title decimalplaces
#'
#' @details
#' An internal function that determines the number of digits in the summary of a continuous variable.
#'
#' @param x a continuous variable
#' @return the most frequent number of digits in the variable
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
updateWorksheet <- function(wb, sheetName, x, ...) {
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



## ===========================================================================
## ===                Time to event utility functions                      ===
## ===========================================================================



#' @title extract_atrisk
#'
#' @details
#' The function creates a dataframe containing the number of at risk patients at time-, overall or by strata
#'
#' @param fit a survfit object
#' @param time.list a numeric vector specifying the time points at which the number of at-risk subjects is calculated.
#' @return A dataframe containing the number of at risk patients at time-, overall or by strata
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


#' @title Run Log-Rank Test for Survival Differences
#' @description Performs a log-rank test to compare survival distributions between two or more groups.
#' @param surv_obj A \code{survfit} object, such as one returned by \code{estimate_cif_km()}, containing grouped survival curves.
#' @return A numeric value representing the p-value from the log-rank test.
run_logrank_test <- function(surv_obj) {
  tmp <- surv_obj$call
  tmp[[1]] <- quote(survival::survdiff)
  tmp$rho <- 0
  tmp$conf.type <- NULL
  test <- eval(tmp, parent.frame())
  pval <- stats::pchisq(test$chisq, df= length(test$n) - 1, lower.tail = FALSE)
  pval
}


#' @title Run Grays Test
#' @description Performs Grays test to compare survival distributions between two or more groups.
#' @param surv_obj A \code{survfit} object, such as one returned by \code{estimate_cif_km()}, containing grouped survival curves.
#' @param evt_type The numeric vector of interest. Default is 1:2
#' @return A numeric value representing the p-value from Grays test.
run_gray_test <- function(surv_obj, evt_type= 1:2) {

  df <- as.list(eval(surv_obj$call$data, parent.frame()))
  df <- all.vars(surv_obj$call$formula) %>%
    stats::setNames (c("ftime", "fstatus", "group")) %>%
    lapply(function(x) df[[x]])

  test <- do.call(cmprsk::cuminc, df)
  nn <- rownames(test$Tests)
  pval <- test$Tests[ (if ( !is.null(evt_type) ) match(evt_type, nn) else -1), "pv"]
  pval
}


#' @title prepare survfit for survival table
#' @importFrom magrittr %>%
prepare_survfit <- function(surv_obj) {

  prepare_cmprisk <- function(surv_obj) {

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

#' @title Create atrisk table
add_atrisk <- function(p, surv_obj, space = -0.15 ,x_break= NULL, atrisk_init_pos= NULL, plot_theme = NULL) {

  # ---- get font information ----
  if (is.null(plot_theme)) {
    font_family<- "sans"
    font_face  <- "plain"
    font_size  <- 11
  } else {
    font_family<- if (is.null(plot_theme$text$family) | trimws(plot_theme$text$family) == "") "sans" else plot_theme$text$family
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
    atrisk_y_inc <- space * max(diff(ggplot2::layer_scales(p)$y$range$range),
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
