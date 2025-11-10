


#' @title construct survival or competing risks process
#'
#' @description
#'
#' The function creates time-to-event variables for survival or competing risks process.
#'
#'See also \code{\link{estimate_cif_km}},\code{\link{summarize_km}}, \code{\link{summarize_cif}}, \code{\link{show_surv}}, and \code{\link{show_cif}}   for related functions.
#'
#' @param df input data
#' @param patid the variable indicating subject/patient id
#' @param idx_dt the index date
#' @param evt_dt the date of the event occurrence. Its value should be NA for non-event subjects.
#' @param end_dt the date of the last follow-up
#' @param cmprisk Logical; indicates whether to create variables for competing risks. Defaults to \code{FALSE} when no competing event is included, and \code{TRUE} when a competing event is present.
#' @param surv_varname 	an option of character vector of length 2, the 1st of which is the name of the time variable; the other is the name of the event indicator.
#' @param units Character string specifying the unit of time for the time-to-event analysis. Accepted values are: "days (default) ", "weeks", "months", or "years".
#' @param adm_cnr_time  a numeric vector specifying the time point at which administrative censoring is applied (in the same units as units).
#' @param ... all competing event dates
#' @examples
#'
#'
#' ## For survival
#'
#'survival_data <-  construct_surv_cmprisk_var(cardio_data,
#'                                             patid = PatientID,
#'                                             idx_dt = SurgeryDate,
#'                                             evt_dt = DeathDate,
#'                                             end_dt = LastVisitDate,
#'                                             append = TRUE,
#'                                             units = "months",
#'                                             adm_cnr_time = 24)
#'
#' KM <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time)
#' KM_Sex <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time,group = Sex)

#'
#'
#' # For competing risks
#'cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
#'                                            patid = PatientID,
#'                                            idx_dt = SurgeryDate,
#'                                            evt_dt = TransplantDate,
#'                                            end_dt = LastVisitDate,
#'                                            death_dt = DeathDate,
#'                                            append = TRUE,
#'                                            units = "months",
#'                                            adm_cnr_time = 24)
#'
#' CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)
#' CIF_Sex <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time,group = Sex)

#'
#' @return A data frame with patid, evt_time and evt.
#' @export
#' @importFrom magrittr %>%
#' @importFrom data.table :=
construct_surv_cmprisk_var <- function(df, patid, idx_dt, evt_dt, end_dt, cmprisk = NULL ,surv_varname = NULL,units = "days", append = FALSE,
                                       adm_cnr_time = NULL,
                                       ...) {

  patid <- rlang::enquo(patid)
  idx_dt <- rlang::enquo(idx_dt)
  evt_dt <- rlang::enquo(evt_dt)
  end_dt <- rlang::enquo(end_dt)
  cmp_evt_dt <- rlang::enquos(...)

  if(is.null(cmprisk)){

    cmprisk <-  length(cmp_evt_dt) > 0

  }


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

  tmp_df <- if (is.null(surv_varname)) {
    tmp_df %>%
      dplyr::select(!!patid, time2evt, evt) %>%
      dplyr::rename(evt_time = time2evt)
  } else {
    tmp_df %>%
      dplyr::select(!!patid, time2evt, evt) %>%
      dplyr::rename(!!surv_varname[1]:= time2evt,
                    !!surv_varname[2]:= evt)
  }


  if (!append) tmp_df else {
    df %>%
      dplyr::inner_join(tmp_df, by = rlang::as_name(patid))
  }

}



#' @title Kaplan-Meier survival and cumulative incidence (CIF) estimates
#' @description Computes Kaplan-Meier survival estimates and cumulative incidence (CIF) from a dataset, optionally stratified by a grouping variable. #See also \code{\link{construct_surv_cmprisk_var}},\code{\link{summarize_km}}, \code{\link{summarize_cif}}, \code{\link{show_surv}}, and \code{\link{show_cif}}   for related functions.

#' @param df A data frame containing the survival data.
#' @param evt_time A numeric vector representing the time to event or censoring.
#' @param evt For survival data: a numeric event indicator (1 = event occurred, 0 = censored).
#'            For competing risks data: a factor event indicator (0 = censored, 1 = Event 1, 2 = Event 2, ...).
#' @param group A grouping variable for stratified survival curves.
#' @param conf.type The type of confidence interval used by   \code{survival::survfit()}. When "default" is inputted uses log-log for Kaplan-Meier and log for CIF
#' @param ... Additional arguments passed to \code{survival::survfit()}.
#' @return A \code{survfit} object containing the survival estimates.
#' @details
#'
#' The function will produce  Kaplan-Meier survival estimates when evt is numeric and will produce CIF estimates when evt is a factor
#'
#' The function analyzes the data (df) using Kaplan-Meier survival method with pointwise 95% CI estimated using log-log
#' transformation (same as SAS's defualt). The function store the input data in the call(), which can be used in
#' run_logrank_test().
#'
#'The function analyzes the competing data (df) using Andersen-Johansen method in estimating cumulative incidence
#' function.The function store the input data in the call(), which can be used in run_gray_test().
#'
#' @examples
#'
#'
#' ## For survival
#'
#'survival_data <-  construct_surv_cmprisk_var(cardio_data,
#'                                             patid = PatientID,
#'                                             idx_dt = SurgeryDate,
#'                                             evt_dt = DeathDate,
#'                                             end_dt = LastVisitDate,
#'                                             append = TRUE,
#'                                             units = "months",
#'                                             adm_cnr_time = 24)
#'
#' KM <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time)
#' KM_Sex <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time,group = Sex)

#'
#'
#' # For competing risks
#'cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
#'                                            patid = PatientID,
#'                                            idx_dt = SurgeryDate,
#'                                            evt_dt = TransplantDate,
#'                                            end_dt = LastVisitDate,
#'                                            death_dt = DeathDate,
#'                                            append = TRUE,
#'                                            units = "months",
#'                                            adm_cnr_time = 24)
#'
#' CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)
#' CIF_Sex <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time,group = Sex)
#'
#' @export
#' @importFrom magrittr %>%
#'
estimate_cif_km <- function(df, evt_time, evt, group,conf.type = "default", ...){

  evt_time <- rlang::enquo(evt_time)
  evt     <- rlang::enquo(evt)
  group   <- rlang::enquo(group)

  cmp <- is.factor(df %>% dplyr::pull(!!evt))
  surv <- is.numeric(df %>% dplyr::pull(!!evt))

  if(cmp == TRUE & surv == TRUE) warning("evt must be either a factor or numeric")
  if(cmp == FALSE & surv == FALSE) warning("evt must be either a factor or numeric")

  conf.type = dplyr::case_when(conf.type == "default" & cmp ~ "log",
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




#' @title Summarize Kaplan-Meier Estimates
#'
#' @description
#' Summarizes a fitted Kaplan-Meier survival object at specified time points, optionally transforming to failure probabilities,
#' and formats the output as a table with confidence intervals. See also \code{\link{construct_surv_cmprisk_var}},\code{\link{estimate_cif_km}}, \code{\link{summarize_cif}}, \code{\link{show_surv}}, and \code{\link{show_cif}}   for related functions.
#'
#' @param fit A fitted survival object of class \code{survfit}.
#' @param times Optional numeric vector of time points at which to summarize the KM estimates. If \code{NULL}, uses \code{pretty(fit$time)}.
#' @param failure_fun Logical; if \code{TRUE}, returns failure probabilities (1 - survival).
#' @param kable_output Logical; if \code{TRUE}, formats the output using \code{kableExtra::kbl()}.
#' @param caption Optional character string for the table caption (used if \code{kable_output = TRUE}).
#' @param full_width Logical; passed to \code{kableExtra::kable_styling()} to control table width.
#' @param time_lab Character string; label to use for the time column in the output table.
#' @param overall_label Character string to label the overall summary column. Default is \code{"Overall"}.

#' @return A data frame or a formatted \code{kableExtra} table summarizing survival or failure probabilities with confidence intervals.
#'
#' @details
#' The function summarizes the fitted Kaplan-Meier survival estimates at user-specified time points. If the model includes strata,
#' the output is stratified accordingly. Confidence intervals are included, and results are formatted as percentages with one decimal place.
#' If \code{failure_fun = TRUE}, the function returns failure probabilities instead of survival.
#' @examples
#'
#'survival_data <-  construct_surv_cmprisk_var(cardio_data,
#'                                             patid = PatientID,
#'                                             idx_dt = SurgeryDate,
#'                                             evt_dt = DeathDate,
#'                                             end_dt = LastVisitDate,
#'                                             append = TRUE,
#'                                             units = "months",
#'                                             adm_cnr_time = 24)
#'
#' KM <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time)
#' summarize_km(KM,overall_label = "All patients",
#' time_lab = "Time since surgery (months)",caption = "Overall Survival by sex")

#'
#' KM_Sex <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time,group = Sex)
#' summarize_km(KM_Sex,overall_label = "All patients",
#' time_lab = "Time since surgery (months)",caption = "Overall Survival for all patients")
#'
#' @export
#' @importFrom magrittr %$%
#' @importFrom magrittr %>%
summarize_km <- function(fit, times= NULL, failure_fun= FALSE,
                         kable_output = TRUE,caption = NULL,full_width = NULL,time_lab = "Times",overall_label = "Overall") {
  ss <- summary(fit, times= if (is.null(times)) pretty(fit$time) else times)
  if (failure_fun) {
    ff <- 1 - ss$surv
    ll <- 1 - ss$upper
    uu <- 1 - ss$lower

    ss$surv<- ff
    ss$lower<- ll
    ss$upper<- uu
  }

  out <- if (any(names(fit)=="strata")) {

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
      reshape2::dcast(times ~ 'Overall', value.var = 'stat') %>% dplyr::rename({{ overall_label }} := Overall)

  }




  if(kable_output){

    names(out) <- sub("^[^=]*=", "", names(out))
    out <- out %>% dplyr::rename({{ time_lab }} := times)

    out <-  out   %>% kableExtra::kbl(caption = caption,booktabs=TRUE,escape = FALSE, align= c('l', rep(c('c', 'c'), dim(out)[2]-1), 'r')) %>%
      kableExtra::row_spec(row = 0, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                full_width = full_width)

  }

  out



}


#' @title Summarize Cumulative Incidence Function (CIF)
#'
#' @description
#' Summarizes a fitted Cumulative Incidence Function (CIF) survival object at specified time points, optionally transforming to failure probabilities,
#' and formats the output as a table with confidence intervals. See also \code{\link{construct_surv_cmprisk_var}},\code{\link{estimate_cif_km}}, \code{\link{summarize_km}}, \code{\link{show_surv}}, and \code{\link{show_cif}}   for related functions.
#'
#' @param fit A fitted survival object of class \code{survfit}.
#' @param times Optional numeric vector of time points at which to summarize the CIF estimates. If \code{NULL}, uses \code{pretty(fit$time)}.
#' @param kable_output Logical; if \code{TRUE}, formats the output using \code{kableExtra::kbl()}.
#' @param caption Optional character string for the table caption (used if \code{kable_output = TRUE}).
#' @param full_width Logical; passed to \code{kableExtra::kable_styling()} to control table width.
#' @param time_lab Character string; label to use for the time column in the output table.
#' @param evt_type Numeric; Indicate which events will be outputted in the kable table; default is all events
#' @param evt_label Character string; labels for the events in the kable table when include_event_type is TRUE. Default is c('0' = "Event free",'1' = "Event",'2'="Competing",'3' = "Other")
#' @return A data frame or a formatted \code{kableExtra} table summarizing survival or failure probabilities with confidence intervals.
#' @param overall_label Character string to label the overall summary column. Default is \code{"Overall"}.
#' @examples
#'
#'
#'cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
#'                                            patid = PatientID,
#'                                            idx_dt = SurgeryDate,
#'                                            evt_dt = TransplantDate,
#'                                            end_dt = LastVisitDate,
#'                                            death_dt = DeathDate,
#'                                            append = TRUE,
#'                                            units = "months",
#'                                            adm_cnr_time = 24)
#'
#' CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)
#'
#' ## Presenting one event for all patients
#' summarize_cif(CIF,time_lab = "Time since surgery (months)",evt_type = 1,
#' caption = "Time to transplant for all patients",
#'               evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))
#'
#' ## Presenting all events for all patients
#' summarize_cif(CIF,time_lab = "Time since surgery (months)",
#' caption = "Time to transplant for all patients",
#'               evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))
#'
#' CIF_Sex <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time,group = Sex)
#' ## Presenting only the events by a covariate
#'
#'summarize_cif(CIF_Sex,time_lab = "Time since surgery (months)",evt_type = 1,
#'caption = "Time to transplant by sex",
#'evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))
#'
#'
#'## Presenting all events by a covariate
#'
#'
#'summarize_cif(CIF_Sex, time_lab = "Time since surgery (months)",
#'caption = "Time to transplant by sex",
#'              evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))
#'
#' @details
#' The function summarizes the fitted Kaplan-Meier survival estimates at user-specified time points. If the model includes strata,
#' the output is stratified accordingly. Confidence intervals are included, and results are formatted as percentages with one decimal place.
#' @export
#' @importFrom magrittr %>%
#' @importFrom data.table :=
summarize_cif <- function(fit, times = NULL, kable_output = TRUE,caption = NULL,full_width = NULL,time_lab = "Time",evt_type = NULL,
                          evt_label = c('0' = "Event free",'1' = "Event",'2'="Competing",'3' = "Second Competing"), overall_label = "Overall") {


  names(evt_label)[names(evt_label)==0] <- '(s0)'

  ss <- summary(fit, times= if(is.null(times)) pretty(fit$time) else times)
  colnames(ss$pstate)<- colnames(ss$lower)<- colnames(ss$upper)<- replace(ss$state, sapply(ss$states, nchar)==0, "0")
  # if (is.null(ss$prev)) ss$prev<- ss$pstate

  out <- if(any(names(fit)=="strata")) {

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

  out <- out %>% dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("0.0% \\[NA%, NA%\\]", "0.0% [0.0%, 0.0%]", .)))

  if(kable_output){

    if(!is.null(evt_type)){
      evt_type <- as.character(evt_type)
      evt_type[evt_type=="0"] <- "^\\("



      out <- out %>%  dplyr::select( dplyr::matches(paste0("^(", paste(c("times",evt_type), collapse = "|"), ")")))


    }



    events  <- sub("_.*", "", names(out))[-1]
    events_clean <- dplyr::recode(events, !!!evt_label)


    if(any(names(fit)=="strata")) {
      column_names = sub("^[^=]*=", "", names(out))[-1]

      if (!all(events == sort(events))) {
        stop("Error: Elements at positions 2=3, 4=5, 6=7, ... do not all match.")
      }
    }
    else{

      if(length(evt_type) > 1 | is.null(evt_type)) {
        column_names <- events_clean
      }else{
        column_names <- overall_label
      }


    }




    out <- out %>% dplyr::rename({{ time_lab }} := times)

    names(out)[-1] <- column_names


    out <- out  %>% kableExtra::kbl(caption = caption,booktabs=TRUE,escape = FALSE, align= c('l', rep(c('c', 'c'), dim(out)[2]-1), 'r')) %>%
      kableExtra::row_spec(row = 0, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                full_width = full_width)



    if(any(names(fit)=="strata") & ((length(evt_type) > 1) | is.null(evt_type))){

      labels <- unique(events_clean)

      if (!all(labels == events_clean[seq(1, length(events_clean), by = length(unique(column_names)))])) {
        stop("Error: unique names do not match")
      }

      out <- out %>%  kableExtra::add_header_above(c("", stats::setNames(rep(length(unique(column_names)), length(labels)), labels)))


    }
  }
  out
}


#' @title Plot Survival or Cumulative Death Function
#' @description Displays either the survival function or the cumulative death function based on a \code{survfit} object, with optional customization and stratification. #See also \code{\link{construct_surv_cmprisk_var}},\code{\link{estimate_cif_km}}, \code{\link{summarize_km}}, \code{\link{summarize_cif}}, and \code{\link{show_cif}}   for related functions.

#'
#' @details
#' This function visualizes survival data using a \code{survfit} object. It supports plotting either the survival curve or the cumulative death function (CDF), with options for axis labels, plot limits, confidence intervals, legends, p-values, and at-risk tables.
#'
#' @param surv_obj A \code{survfit} object, typically created using survival analysis functions.
#' @param x_lab Character; label for the x-axis (default = "Time").
#' @param y_lab Character; label for the y-axis. If \code{plot_cdf = TRUE}, defaults to "The proportion of deceased subjects"; otherwise, "The freedom from death".
#' @param y_lim Numeric vector of length 2 specifying y-axis limits.
#' @param x_break Numeric vector specifying x-axis tick positions.
#' @param y_break Numeric vector specifying y-axis tick positions.
#' @param color_scheme Character; color scheme to use. Options: "brewer", "grey", "viridis", "manual" (default = "brewer").
#' @param color_list A named list of colors to use when \code{color_scheme = "manual"} (e.g., \code{list(values = c("red", "blue"))}).
#' @param plot_theme A \code{ggplot2} theme object to customize the appearance of the plot (default = \code{ggplot2::theme_minimal()}).
#' @param add_ci Logical; if \code{TRUE}, adds confidence intervals to the survival curves (default = TRUE).
#' @param add_atrisk Logical; if \code{TRUE}, adds an at-risk table below the plot (default = TRUE).
#' @param add_legend Logical; if \code{TRUE}, includes a legend in the plot (default = FALSE).
#' @param add_pvalue Logical; if \code{TRUE}, adds a p-value to the plot (default = TRUE).
#' @param atrisk_init_pos Character; position of the "At-risk N:" label.
#' @param pvalue_pos Character vector indicating where to place the p-value on the plot. Options include "bottomright", "topleft", "topright", "bottomleft", "left", "right", "top", "bottom" (default = all).
#' @param plot_cdf Logical; if \code{TRUE}, plots the cumulative death function instead of the survival curve (default = FALSE).
#' @param print_fig Logical; if \code{TRUE}, prints the plot (default = FALSE).
#' @param top.margin Numeric; top margin space for the at-risk table (default = 18).
#' @param right.margin Numeric; right margin space for the at-risk table (default = 18).
#' @param bottom.margin Numeric; bottom margin space for the at-risk table (default = 96).
#' @param left.margin Numeric; left margin space for the at-risk table (default = 96).
#' @return A \code{ggplot} object representing the survival or cumulative death function plot.
#' @examples
#'
#'## All patients
#'
#' survival_data <-  construct_surv_cmprisk_var(cardio_data,
#'                                             patid = PatientID,
#'                                             idx_dt = SurgeryDate,
#'                                             evt_dt = DeathDate,
#'                                             end_dt = LastVisitDate,
#'                                             append = TRUE,
#'                                             units = "months",
#'                                             adm_cnr_time = 24)
#'
#' KM <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time)
#' #show_surv(KM, print_fig = FALSE,pvalue_pos = "bottomleft",
#' #add_legend = TRUE,x_break = seq(0,24,by=3)) #%>%  grid::grid.draw()

#'## Including a covariate
#' KM_Sex <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time,group = Sex)
#' #show_surv(KM_Sex,print_fig = FALSE,pvalue_pos = "bottomleft",
#' #add_legend = TRUE,x_break = seq(0,24,by=3)) %>% grid::grid.draw()
#'
#'
#' @export
#' @importFrom magrittr %>%
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
                      print_fig = FALSE,

                      top.margin = 18,
                      right.margin = 18,
                      bottom.margin = 96,
                      left.margin = 96) {

  # no need to add pvalues for a single cohort
  add_pvalue<- if (all(names(surv_obj)!='strata')) FALSE else add_pvalue
  # no need to add legend if it is a single cohort or add risk (when it is >1 cohorts). The at-risk table will be color-coded to indicate cohort
  add_legend<- if (all(names(surv_obj)!='strata') | add_atrisk) FALSE else add_legend

  color_scheme<- match.arg(color_scheme)
  if (color_scheme=='manual' & is.null(color_list)) stop("Please provide a list of color value(s).")

  fill_fun <- switch(color_scheme,
                     'brewer' = quote(ggplot2::scale_fill_brewer(palette = "Set1")),
                     'grey'   = quote(ggplot2::scale_fill_grey(start= 0, end= 0.65)),
                     'viridis'= quote(viridis::scale_fill_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE)),
                     'manual' = match.call(do.call, call('do.call', what= 'scale_fill_manual', args= color_list)))
  color_fun<- switch(color_scheme,
                     'brewer' = quote(ggplot2::scale_color_brewer(palette = "Set1")),
                     'grey'   = quote(ggplot2::scale_color_grey(start= 0, end= 0.65)),
                     'viridis'= quote(viridis::scale_color_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE)),
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

  out <- out + plot_theme

  if(add_atrisk) {
    p <- out + ggplot2::theme(plot.margin= grid::unit(c(top = top.margin, right = right.margin, bottom = bottom.margin, left= left.margin), "bigpts"))
    gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
    gt$layout$clip[gt$layout$name == 'panel'] <- "off"
    out <- gt
    #grid.draw(gt)

  }

  if (print_fig) print(out)
  # print(out, vp= viewport(width = unit(6.5, "inches"), height = unit(6.5, "inches")))
  return(out)
}


#' @title Plot Cumulative Incidence Function for Competing Risks
#' @description Displays the cumulative incidence function (CIF) for competing risks data, with optional stratification and customization. #See also \code{\link{construct_surv_cmprisk_var}},\code{\link{estimate_cif_km}}, \code{\link{summarize_km}}, \code{\link{summarize_cif}}, and \code{\link{show_surv}}   for related functions.

#'
#' @details
#' This function visualizes the cumulative incidence of events in the presence of competing risks using a \code{survfit} object.
#' It supports customization of axis labels, plot limits, confidence intervals, legends, p-values, and at-risk tables.
#' @param surv_obj  A \code{survfit} object, such as one returned by \code{estimate_cif()}.
#' @param evt_type Integer or vector of integers; the event type(s) of interest to be plotted (default = 1).
#' @param evt_label A function to relabel event types for plotting (default uses \code{c('0' = "Event free",'1' = "Event",'2'="Competing",'3' = "Second Competing"}).
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
#' @param print_fig Logical; if \code{TRUE}, prints the plot (default = FALSE).
#' @param top.margin Numeric; top margin space for the at-risk table (default = 18).
#' @param right.margin Numeric; right margin space for the at-risk table (default = 18).
#' @param bottom.margin Numeric; bottom margin space for the at-risk table (default = 96).
#' @param left.margin Numeric; left margin space for the at-risk table (default = 96).
#'
#' @examples
#'
#'
#' ## Showing all events
#' cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
#'patid = PatientID,
#' idx_dt = SurgeryDate,
#' evt_dt = TransplantDate,
#' end_dt = LastVisitDate,
#' death_dt = DeathDate,
#' append = TRUE,
#' units = "months",
#' adm_cnr_time = 24)
#'
#' CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)
#'
#'
# #show_cif(CIF,evt_type = c(0,1,2),add_legend = TRUE,x_break = seq(0,24,by=3),
#'          #evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))  %>%
#'           #  grid::grid.draw()
#'
#'## Including a covariate
#'
#' CIF_Sex <- estimate_cif_km(cmp_risk_data , evt = evt,evt_time = evt_time,group = Sex)
#'
#' #show_cif(CIF_Sex,evt_type = c(1),add_legend = FALSE,x_break = seq(0,24,by=3),
#'         #evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death")) %>%
#'        # grid::grid.draw()
#'
#' @return A \code{ggplot} object representing the cumulative incidence function plot.
#' @export
#' @importFrom magrittr %>%
show_cif <- function(surv_obj,
                     evt_type = 1,
                      evt_label = c('0' = "Event free",'1' = "Event",'2'="Competing",'3' = "Second Competing"),
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

                     print_fig = FALSE,

                     top.margin = 18,
                     right.margin = 18,
                     bottom.margin = 96,
                     left.margin = 96

) {


  #---- prepare survfit for plot ----
  cmprisk_mat<- prepare_survfit(surv_obj)
  cmprisk_mat<- cmprisk_mat %>%
    dplyr::filter(state %in% evt_type) %>%
    dplyr::mutate(state_label = dplyr::recode(state, !!!evt_label),
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
                     'brewer' = quote(ggplot2::scale_fill_brewer(palette = "Set1", guide = ggplot2::guide_legend(title= ""))),
                     'grey'   = quote(ggplot2::scale_fill_grey(start= 0, end= 0.65, guide = ggplot2::guide_legend(title= ""))),
                     'viridis'= quote(viridis::scale_fill_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE,guide = ggplot2::guide_legend(title= ""))),
                     'manual' = match.call(do.call, call('do.call', what= 'scale_fill_manual', args= color_list)))
  color_fun<- switch(color_scheme,
                     'brewer' = quote(ggplot2::scale_color_brewer(palette = "Set1", guide = ggplot2::guide_legend(title= ""))),
                     'grey'   = quote(ggplot2::scale_color_grey(start= 0, end= 0.65, guide = ggplot2::guide_legend(title= ""))),
                     'viridis'= quote(viridis::scale_color_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE, guide = ggplot2::guide_legend(title= ""))),
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

