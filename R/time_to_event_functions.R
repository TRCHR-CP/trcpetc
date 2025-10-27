


#' @title construct_surv_cmprisk_var
#'
#' @details
#' The function creates time-to-event variables for survival or competing risks process.
#'
#' @param df input data
#' @param patid the variable indicating subject/patient id
#' @param idx_dt the index date
#' @param evt_dt the date of the event occurrence. Its value should be NA for non-event subjects.
#' @param end_dt the date of the last follow-up
#' @param ... all competing event dates
#' @param cmprisk Logical; whether to create variables for competing risks, when FALSE creates for survival process. Default is `TRUE`.
#' @param units Character string specifying the unit of time for the time-to-event analysis. Accepted values are: "days", "weeks", "months", or "years".
#' @param surv_varname an option of character vector of length 2, the 1st of which is the name of the time variable; the other is the name of the event indicator.
#' @param adm_cnr_time  a numeric vector specifying the time point at which administrative censoring is applied (in the same units as units).
#' @return A data frame with patid, evt_time and evt.
#' @export

construct_surv_cmprisk_var <- function(df, patid, idx_dt, evt_dt, end_dt, cmprisk = FALSE ,cmprisk_varname = NULL, append = FALSE,
                                       units = "days",adm_cnr_time = NULL,
                                       ...) {

  patid <- rlang::enquo(patid)
  idx_dt <- rlang::enquo(idx_dt)
  evt_dt <- rlang::enquo(evt_dt)
  end_dt <- rlang::enquo(end_dt)
  cmp_evt_dt <- rlang::enquos(...)

  if (rlang::quo_is_missing(idx_dt)) stop("No index date (time zero).")
  if (rlang::quo_is_missing(evt_dt)) stop("No event date.")
  if (rlang::quo_is_missing(end_dt)) stop("No date of the end of follow-up.")
  if (rlang::quo_is_missing(patid))  stop("Please provide subject id")

  n_cmp_evt <- length(cmp_evt_dt)

  names(cmp_evt_dt) <- sapply(cmp_evt_dt, rlang::as_name)

  cmp_evt_desc <- paste0('cmp_evt_', seq_len(n_cmp_evt) + 1)
  evt_desc <- c('evt_1', cmp_evt_desc, 'censored_0')

  if(!cmprisk) {
    if(n_cmp_evt > 0) (warning(paste("Survival analyses being performed competing event(s)",paste0(names(cmp_evt_dt),collapse = ","),"will not be considered") ))
    cmp_evt_dt <- NULL

  }


  tmp_df <- df %>%
    dplyr::select(!!patid, !!idx_dt, !!evt_dt, !!!cmp_evt_dt, !!end_dt) %>%
    dplyr::group_by(!!patid) %>%
    dplyr::mutate(first_evt = ifelse(rlang::is_empty((evt_desc[which.min(c(!!evt_dt, !!!cmp_evt_dt))])), NA, (evt_desc[which.min(c(!!evt_dt, !!!cmp_evt_dt))])),
                  first_evt = ifelse(is.na(first_evt) &  !rlang::is_empty(!!end_dt) ,'censored_0', first_evt),
                  first_evt_dt =  dplyr::coalesce(pmin(!!evt_dt, !!!cmp_evt_dt, na.rm = TRUE), !!end_dt),
                  time2evt = dplyr::case_when(is.infinite(first_evt_dt) | is.na(first_evt_dt) ~ NA_real_,TRUE ~ as.numeric(first_evt_dt - !!idx_dt)),
                  evt = dplyr::case_when( is.na(time2evt) ~ NA_integer_, TRUE ~ as.integer(gsub('^(cmp_evt|evt|censored)_', '', first_evt))),
                  time2evt = dplyr::case_when(units == "days"  ~ time2evt,
                                              units == "weeks"  ~ time2evt/7,
                                              units == "months"  ~ time2evt/30.44,
                                              units == "years"  ~ time2evt/365.25)) %>%


    dplyr::ungroup()

  # ONLY FOR COMPETING RISKS
  # per documentation, evt will be treated as a factor with the 1st level as censoring
  # in the situtation in which no pts are censored, no observations have a value of zero.
  # need to convert evt to a factor forcing 0 as the first level of the factor.

  if(cmprisk) {
    tmp_df <- tmp_df %>%  dplyr::mutate(evt = factor(evt, 0:(n_cmp_evt + 1), labels = 0:(n_cmp_evt + 1)))

    tmp_df <- tmp_df  %>% dplyr::mutate(evt = replace(evt, time2evt > adm_cnr_time & evt != "0", "0"),
                                        time2evt = replace(time2evt, time2evt > adm_cnr_time, adm_cnr_time))

  }else{

    tmp_df <- tmp_df  %>% dplyr::mutate(evt = replace(evt, time2evt > adm_cnr_time & evt != 0, 0),
                                        time2evt = replace(time2evt, time2evt > adm_cnr_time, adm_cnr_time))

  }



  # flags -------------------------------------------------------------------

  flag <- FALSE

  # IF if the time to event is less than or equal to zero flag
  flag_df <- tmp_df %>%
    dplyr::filter(time2evt <= 0) %>%
    dplyr::mutate(flag_evt_time_zero = (time2evt == 0),
                  flag_evt_time_neg = (time2evt < 0))

  # Values equal to zero become 0.5 days
  if (any(tmp_df$time2evt==0)) {
    warning("Event at time zero")
    tmp_df$time2evt <- replace(tmp_df$time2evt, tmp_df$time2evt==0, 0.5)
    flag <- TRUE
  }

  # Values below zero are replaced with NA
  if (any(tmp_df$time2evt < 0)) {
    warning("Negative time-to-event!?")
    tmp_df$time2evt <- replace(tmp_df$time2evt, tmp_df$time2evt < 0, NA)
    flag <- TRUE
  }
  if (flag) print(as.data.frame(flag_df))

  tmp_df <- if (is.null(cmprisk_varname)) {
    tmp_df %>%
      select(!!patid, time2evt, evt) %>%
      dplyr::rename(evt_time = time2evt)
  } else {
    tmp_df %>%
      select(!!patid, time2evt, evt) %>%
      dplyr::rename(!!cmprisk_varname[1]:= time2evt,
                    !!cmprisk_varname[2]:= evt)
  }


  if (!append) tmp_df else {
    df %>%
      dplyr::inner_join(tmp_df, by = rlang::as_name(patid))
  }

}



#' @title estimate_cif_km
#' @description Computes Kaplan-Meier survival estimates and cumulative incidence (CIF) from a dataset, optionally stratified by a grouping variable.
#' @param df A data frame containing the survival data.
#' @param evt_time A numeric vector representing the time to event or censoring.
#' @param evt A binary event indicator (1 = event occurred, 0 = censored).
#' @param group A grouping variable for stratified survival curves.
#' @param conf.type The type of confidence interval used by   \code{survival::survfit()}. When "default" is inputted uses log-log for Kaplan-Meier and log for CIF
#' @param ... Additional arguments passed to \code{survival::survfit()}.
#' @return A \code{survfit} object containing the survival estimates.
#' @details
#' The function analyzes the data (df) using Kaplan-Meier survival method with pointwise 95% CI estimated using log-log
#' transformation (same as SAS's defualt). The function store the input data in the call(), which can be used in
#' run_logrank_test().
#'
#'The function analyzes the competing data (df) using Andersen-Johansen method in estimating cumulative incidence
#' function.The function store the input data in the call(), which can be used in run_gray_test().
#'
#' @export
#'
#'
estimate_cif_km <- function(df, evt_time, evt, group,conf.type = "default", ...){

  evt_time <- enquo(evt_time)
  evt     <- enquo(evt)
  group   <- enquo(group)

  cmp <- is.factor(df %>% dplyr::pull(!!evt))
  surv <- is.numeric(df %>% dplyr::pull(!!evt))

  if(cmp == TRUE & surv == TRUE) warning("evt must be either a factor or numeric")
  if(cmp == FALSE & surv == FALSE) warning("evt must be either a factor or numeric")

  conf.type = case_when(conf.type == "default" & cmp ~ "log",
                        conf.type == "default" & surv  ~ "log-log",
                        TRUE ~ conf.type)


  out <- if (rlang::quo_is_missing(group)) {
    substitute( survival::survfit(survival::Surv(evt_time, evt) ~ 1, data= df, conf.type= conf.type, ...),
                list(evt_time = rlang::quo_get_expr(evt_time),
                     evt     = rlang::quo_get_expr(evt),
                     df      = df))
  } else {
    substitute(survival::survfit(survival::Surv(evt_time, evt) ~ grp, data= df, conf.type= conf.type, ...),
               list(evt_time = rlang::quo_get_expr(evt_time),
                    evt     = rlang::quo_get_expr(evt),
                    grp     = rlang::quo_get_expr(group),
                    df      = df))
  }
  out <- eval(out)
  out

}






