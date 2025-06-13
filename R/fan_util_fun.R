#---- utility functions ----

#' @title decimalplaces
#'
#' @details
#' An internal function that determines the number of digits in the summary of a continuous variable.
#'
#' @param x a continous variable
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
    info<- strsplit(y, ".", fixed=TRUE)
    info<- info[ vapply(info, length, integer(1L) ) == 2]

    n_dec<- nchar(unlist(info))[ 2 * (1:length(y)) ]
    dec<- sort(table(n_dec))

    # return( pmin.int(max_dec, as.integer( names(dec)[length(dec)])) )
    out<- pmin.int(max_dec, as.integer( names(dec)[length(dec)]))

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
recode_missing<- function(x, na.value= NULL) {
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
#' @examples
#' set.seed(0)
#' nn<- 100
#' test<- data.frame(idx_dt= as.Date("1970-01-01"),
#'                   evt_dt= c(sample((-30:70), nn-1, replace= TRUE), 0) + as.Date("1970-01-01"),
#'                   end_dt= sample((0:99), nn, replace= TRUE) + as.Date("1970-01-01")) %>%
#'   mutate(flag= evt_dt > end_dt,
#'          evt_dt= replace(evt_dt, flag, NA),
#'          end_dt= replace(end_dt, !flag, NA),
#'          patid= 1:n())
#' test %>% construct_surv_var(idx_dt, evt_dt, end_dt, patid)
#' test %>% construct_surv_var(idx_dt, evt_dt, end_dt, patid, surv_varname= c("day_dth", "dth"))
#' @export
construct_surv_var<- function(df, patid, idx_dt, evt_dt, end_dt, surv_varname= NULL, append= FALSE) {

  # date of origin in R:   1970-01-01
  # date of origin in SAS: 1960-01-01

  ## Excel is said to use 1900-01-01 as day 1 (Windows default) or
  ## 1904-01-01 as day 0 (Mac default), but this is complicated by Excel
  ## thinking 1900 was a leap year.
  ## So for recent dates from Windows Excel
  #       as.Date(35981, origin="1899-12-30") # 1998-07-05
  ## and Mac Excel
  #       as.Date(34519, origin="1904-01-01") # 1998-07-05

  idx_dt<- enquo(idx_dt)
  evt_dt<- enquo(evt_dt)
  end_dt<- enquo(end_dt)
  patid <- enquo(patid)

  if (quo_is_missing(idx_dt)) stop("No index date (time zero).")
  if (quo_is_missing(evt_dt)) stop("No event date.")
  if (quo_is_missing(end_dt)) stop("No date of the end of follow-up.")
  if (quo_is_missing(patid))  stop("Please provide subject id")

  tmp_df<- df %>%
    mutate(tmp_idx_dt= as.Date(as.character(!!idx_dt), origin= "1970-01-01"),
           tmp_evt_dt= as.Date(as.character(!!evt_dt), origin= "1970-01-01"),
           tmp_end_dt= as.Date(as.character(!!end_dt), origin= "1970-01-01"),
           evt       = ifelse(is.na(tmp_evt_dt), 0L, 1L),
           time2evt  = as.numeric(ifelse(is.na(tmp_evt_dt),
                                         tmp_end_dt - tmp_idx_dt,
                                         tmp_evt_dt - tmp_idx_dt)),
    ) %>%
    dplyr::select(!!patid, time2evt, evt, matches("^tmp_(idx|evt|end)_dt$"))

  flag<- FALSE
  flag_df<- tmp_df %>%
    filter(time2evt<=0) %>%
    mutate(flag_evt_time_zero= (time2evt==0),
           flag_evt_time_neg = (time2evt< 0))

  if (any(tmp_df$time2evt==0)) {
    warning("Event at time zero")
    tmp_df$time2evt<- replace(tmp_df$time2evt, tmp_df$time2evt==0, 0.5)
    flag<- TRUE
  }

  if (any(tmp_df$time2evt<0)) {
    warning("Negative time-to-event!?")
    tmp_df$time2evt<- replace(tmp_df$time2evt, tmp_df$time2evt<0, NA)
    flag<- TRUE
  }

  if (flag) print(as.data.frame(flag_df))

  tmp_df<- if (is.null(surv_varname)) {
    rename(tmp_df,
           evt_time= time2evt)
  } else {
    rename(tmp_df,
           !!surv_varname[1]:= time2evt,
           !!surv_varname[2]:= evt)
  }

  if (append) {
    df %>%
      inner_join(dplyr::select(tmp_df, -matches("^tmp_(idx|evt|end)_dt$")), by= as_name(patid))
  } else {
    tmp_df %>%
      dplyr::select(-matches("^tmp_(idx|evt|end)_dt$"))
  }

}

#' @title construct_cmprisk_var
#'
#' @details
#' The function creates time-to-event variables for competing risk data
#'
#' @export
construct_cmprisk_var<- function(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname= NULL, append= FALSE, ...) {
  patid <- enquo(patid)
  idx_dt<- enquo(idx_dt)
  evt_dt<- enquo(evt_dt)
  end_dt<- enquo(end_dt)
  cmp_evt_dt<- enquos(...)

  if (quo_is_missing(idx_dt)) stop("No index date (time zero).")
  if (quo_is_missing(evt_dt)) stop("No event date.")
  if (quo_is_missing(end_dt)) stop("No date of the end of follow-up.")
  if (quo_is_missing(patid))  stop("Please provide subject id")

  n_cmp_evt<- length(cmp_evt_dt)
  names(cmp_evt_dt)<- sapply(cmp_evt_dt, lazyeval::as_name) # without, dplyr::select(df, !!!cmp_evt_dt) changes the variable name in the output data

  cmp_evt_desc<- paste0('cmp_evt_', seq_len(n_cmp_evt) + 1)
  evt_desc<- c('evt_1', cmp_evt_desc, 'censored_0')

  tmp_df<- df %>%
    dplyr::select(!!patid, !!idx_dt, !!evt_dt, !!!cmp_evt_dt, !!end_dt) %>%
    group_by(!!patid) %>%
    # mutate(first_evt_dt= pmin(!!evt_dt, !!!cmp_evt_dt, !!end_dt, na.rm= TRUE))
    mutate(first_evt= ifelse(is_empty((evt_desc[which.min(c(!!evt_dt, !!!cmp_evt_dt, !!end_dt))])),
                             NA, (evt_desc[which.min(c(!!evt_dt, !!!cmp_evt_dt, !!end_dt))])),
           first_evt_dt= pmin(!!evt_dt, !!!cmp_evt_dt, !!end_dt, na.rm= TRUE),

           time2evt= case_when(
             is.infinite(first_evt_dt) | is.na(first_evt_dt) ~ NA_real_,
             TRUE ~ as.numeric(first_evt_dt - !!idx_dt)),

           # Steve: this is the part of the code that generates warning messages.
           # evt= case_when(
           #   is.na(time2evt) ~ NA_integer_,
           #   first_evt=='censored' ~ 0L,
           #   first_evt=='evt' ~ 1L,
           #   TRUE ~ as.integer(gsub('^cmp_evt_', '', first_evt))),

           evt= case_when(
             is.na(time2evt) ~ NA_integer_,
             TRUE ~ as.integer(gsub('^(cmp_evt|evt|censored)_', '', first_evt))),
    ) %>%
    ungroup()

  # per documentation, evt will be treated as a factor with the 1st level as censoring
  # in the situtation in which no pts are censored, no observations have a value of zero.
  # need to convert evt to a factor forcing 0 as the first level of the factor.
  tmp_df<- tmp_df %>%
    mutate(evt= factor(evt, 0:(n_cmp_evt + 1), labels = 0:(n_cmp_evt + 1)))


  flag<- FALSE
  flag_df<- tmp_df %>%
    filter(time2evt<=0) %>%
    mutate(flag_evt_time_zero= (time2evt==0),
           flag_evt_time_neg = (time2evt< 0))

  if (any(tmp_df$time2evt==0)) {
    warning("Event at time zero")
    tmp_df$time2evt<- replace(tmp_df$time2evt, tmp_df$time2evt==0, 0.5)
    flag<- TRUE
  }

  if (any(tmp_df$time2evt<0)) {
    warning("Negative time-to-event!?")
    tmp_df$time2evt<- replace(tmp_df$time2evt, tmp_df$time2evt<0, NA)
    flag<- TRUE
  }

  if (flag) print(as.data.frame(flag_df))

  tmp_df<- if (is.null(cmprisk_varname)) {
    tmp_df %>%
      select(!!patid, time2evt, evt) %>%
      rename(evt_time= time2evt)
  } else {
    tmp_df %>%
      select(!!patid, time2evt, evt) %>%
      rename(!!cmprisk_varname[1]:= time2evt,
             !!cmprisk_varname[2]:= evt)
  }

  # tmp_df<- dplyr::select(tmp_df, !!patid, one_of(c(cmprisk_varname, 'evt_time', 'evt')))

  if (!append) tmp_df else {
    df %>%
      inner_join(tmp_df, by= as_name(patid))
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
#' @examples
#' aml %>% admin_censor_surv(evt_time= time, evt= status) # No admin censoring
#' aml %>% admin_censor_surv(evt_time= time, evt= status, adm_cnr_time= 30)
#' aml %>% admin_censor_surv(evt_time= time, evt= status, adm_cnr_time= 30, overwrite_var= TRUE)
#' @export
admin_censor_surv<- function(df, evt_time, evt, adm_cnr_time= NULL, overwrite_var= FALSE) {
  ######################################################################################
  ## the function creates administrately censored version of event time and indicator ##
  ## for survival (binary) process                                                    ##
  ##   df - input dataframe                                                           ##
  ##   evt_time - continuous time to event                                            ##
  ##   evt - event indicator (1= event; 0= non-event)                                 ##
  ##   adm_cnr_time - time at which admin censoring is applied                        ##
  ######################################################################################

  evt_time<- enquo(evt_time)
  evt     <- enquo(evt)

  if (!is.null(adm_cnr_time)) {

    if (overwrite_var) {
      cnr_evt_time_name<- lazyeval::as_name(evt_time)
      cnr_evt_name     <- lazyeval::as_name(evt)
    } else {
      cnr_evt_time_name<- paste0(lazyeval::as_name(evt_time), "_adm")
      cnr_evt_name     <- paste0(lazyeval::as_name(evt), "_adm")
    }

    df<- df %>%
      mutate(!!cnr_evt_name      := replace(!!evt, !!evt_time> adm_cnr_time & !!evt!=0, 0),
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
#' @examples
#' cmprisk_df<- read.csv2("http://www.stat.unipg.it/luca/misc/bmt.csv")
#' admin_censor_cmprisk(cmprisk_df, ftime, status, evt_label = c("0"= "Event free", "1"= "Event", "2"= "Competing event"), adm_cnr_time= 10)
#' @export
admin_censor_cmprisk<- function(df, evt_time, evt, adm_cnr_time= NULL, evt_label= NULL, overwrite_var= FALSE) {

  evt_time<- enquo(evt_time)
  evt<- enquo(evt)

  if (is.null(adm_cnr_time)) {

    stop("No administrative censor time is given.")
    # if (!is.null(evt_label)) {
    #   # df<- df %>%
    #   #   mutate(!!lazyeval::as_name(evt):= factor(!!evt, as.integer(names(evt_label)), labels = evt_label))
    #   # df[[as_name(evt)]]<- evt_label[levels(df[[as_name(evt)]])]
    #   df$as_name(evt)<- evt_label[levels(df$as_name(evt))]
    # }

  } else {

    if (overwrite_var) {
      cnr_evt_time_name<- lazyeval::as_name(evt_time)
      cnr_evt_name     <- lazyeval::as_name(evt)
    } else {
      cnr_evt_time_name<- paste0(lazyeval::as_name(evt_time), "_adm")
      cnr_evt_name     <- paste0(lazyeval::as_name(evt), "_adm")
    }

    df<- if (is.null(evt_label)) {
      df %>%
        mutate(!!cnr_evt_name      := replace(!!evt, !!evt_time> adm_cnr_time & !!evt!= "0", "0"),
               !!cnr_evt_time_name := replace(!!evt_time, !!evt_time>adm_cnr_time, adm_cnr_time))
    } else {
      df %>%
        mutate(!!cnr_evt_name      := factor(replace(!!evt, !!evt_time> adm_cnr_time & !!evt!= "0", "0"),
                                             # as.integer(names(evt_label)),
                                             names(evt_label),
                                             labels = evt_label),
               !!cnr_evt_time_name := replace(!!evt_time, !!evt_time>adm_cnr_time, adm_cnr_time))
    }
  }

  # df<- if (overwrite_var | is.null(evt_label)) df else mutate(df,
  #                                                             !!quo_name(evt):= factor(!!evt, as.integer(names(evt_label)), labels = evt_label))
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
      map2(.x= c('surv', 'conf_low', 'conf_high'),
           .y= list(surv= surv, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               mutate(strata= strata,
                      times = time) %>%
               melt(id.vars= c('strata', 'times'),
                    value.name = var) %>%
               dplyr::select(-variable)
           }) %>%
      reduce(full_join, by = c('strata', 'times')) %>%
      mutate_at(vars(one_of('surv', 'conf_low', 'conf_high')),
                function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      mutate(stat= paste0(surv, " [", conf_low, ", ", conf_high, "]")) %>%
      dcast(times ~ strata, value.var = 'stat')

  } else {

    ss %$%
      map2(.x= c('surv', 'conf_low', 'conf_high'),
           .y= list(surv= surv, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               mutate(times = time) %>%
               melt(id.vars= c('times'),
                    value.name = var) %>%
               dplyr::select(-variable)
           }) %>%
      reduce(full_join, by = c('times')) %>%
      mutate_at(vars(one_of('surv', 'conf_low', 'conf_high')),
                function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      mutate(stat= paste0(surv, " [", conf_low, ", ", conf_high, "]")) %>%
      dcast(times ~ 'Overall', value.var = 'stat')

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
      map2(.x= c('pstate', 'conf_low', 'conf_high'),
           .y= list(pstate= pstate, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               mutate(strata= strata,
                      times = time) %>%
               melt(id.vars= c('strata', 'times'),
                    value.name = var,
                    variable.name = 'states')
           }) %>%
      reduce(full_join, by = c('strata', 'times', 'states')) %>%
      mutate_at(vars(one_of('pstate', 'conf_low', 'conf_high')),
                function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      mutate(stat= paste0(pstate, " [", conf_low, ", ", conf_high, "]")) %>%
      dcast(times ~ states + strata, value.var = 'stat')

  } else {

    ss %$%
      map2(.x= c('pstate', 'conf_low', 'conf_high'),
           .y= list(pstate= pstate, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               mutate(times = time) %>%
               melt(id.vars= c('times'),
                    value.name = var,
                    variable.name = 'states')
           }) %>%
      reduce(full_join, by = c('times', 'states')) %>%
      mutate_at(vars(one_of('pstate', 'conf_low', 'conf_high')),
                function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      mutate(stat= paste0(pstate, " [", conf_low, ", ", conf_high, "]")) %>%
      dcast(times ~ states, value.var = 'stat')
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
    rownames_to_column("term")

  if (any(class(mdl)== "coxph.penal")) {
    out<- rename(out, se= 'se(coef)')
  } else if (all(class(mdl)== "coxph")) {
    out<- rename(out, se= 'se(coef)', p= 'Pr(>|z|)')
    # names(out)[grep("^p", names(out), ignore.case = TRUE)]<- "p"
  }

  out<- out %>%
    mutate(conf_low = coef - qnorm(1-alpha/2) * se,
           conf_high= coef + qnorm(1-alpha/2) * se,
           coef     = if (exponentiate) exp(coef) else coef,
           conf_low = if (exponentiate) exp(conf_low) else conf_low,
           conf_high= if (exponentiate) exp(conf_high) else conf_high,
           stat= ifelse(is.na(coef), NA_character_,
                        paste0(formatC(coef, format= "f", digits= 3, flag= "#"), " [",
                               formatC(conf_low, format= "f", digits= 3, flag= "#"), ", ",
                               formatC(conf_high, format= "f", digits= 3, flag= "#"), "]")),
           pval= format_pvalue(p)) %>%
    dplyr::select(one_of(c("term", "stat", "pval")))

  type3_coxph<- function(mdl, beta_var= vcov(mdl)) {
    x<- model.matrix(mdl)
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
                   pval<- pchisq(wald_stat,
                                 df= if (any(class(mdl)=="coxph.penal") && !is.na(mdl$df[i])) mdl$df[i] else df,
                                 lower.tail = FALSE)

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
    filter(df> 1) %>%
    mutate(pval= format_pvalue(chisq_p)) %>%
    dplyr::select(variable, pval) %>%
    rename(term= variable) %>%
    bind_rows(out) %>%
    arrange(term) %>%
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
  x<- model.matrix(tmp<- getfit(mira_obj, 1L))
  varseq<- attr(x, "assign")
  df<- sapply(split(varseq, varseq), length)
  m <- length(mira_obj$analyses)

  # coef estimate and its vcov for each MI model
  betas<- MIextract(mira_obj$analyses, fun= coef)
  vars <- MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)

  # average betas and vcov cross MI mdls
  mean_betas<- purrr::reduce(betas, .f= `+`)/m
  with_var<- purrr::reduce(vars, .f= `+`)/m # with MI
  # between-MI vcov
  btwn_var<- lapply(betas, function(cc) (cc - mean_betas) %*% t(cc - mean_betas)) %>%
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

                 pval<- pf(wald_stat, df1= df, df2= df_denominator, lower.tail= FALSE)

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
  # betas<- MIextract(mira_obj$analyses, fun= coef)
  # vars <- MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)
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
  #                pval<- pf(wald_stat, df1= df, df2= df_denominator, lower.tail= FALSE)
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
    bind_rows()

  type3_out<- out %>%
    bind_rows() %>%
    mutate(pval= format_pvalue(chisq_p))

  glm_out<- MIcombine(MIextract(mira_obj$analyses, fun= coef),
                      MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)) %$%
    data.frame(est= coefficients,
               se = sqrt(diag(variance))) %>%
    rownames_to_column(var= "term") %>%
    right_join(out_tmp, by= "term") %>%
    mutate(conf_low = est - qnorm(1-alpha/2) * se,
           conf_high= est + qnorm(1-alpha/2) * se,
           est      = if (exponentiate) exp(est) else est,
           conf_low = if (exponentiate) exp(conf_low) else conf_low,
           conf_high= if (exponentiate) exp(conf_high) else conf_high,
           stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
           pval= type3_out$pval[charmatch(gsub("TRUE$", "", term), type3_out$var)]) %>%
    select(term, stat, pval, rid, everything())  %>%
    rename(var= term)

    # mutate(var= as.character(term),
    #        # est      = if (exponentiate) exp(est) else est,
    #        # conf_low = if (exponentiate) exp(conf_low) else conf_low,
    #        # conf_high= if (exponentiate) exp(conf_high) else conf_high,
    #        stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
    #        pval= format_pvalue(pval)) %>%
    # dplyr::select(var, stat, pval, est, conf_low, conf_high) %>%
    # full_join(out_tmp, by= c("var" = "term"))


  type3_out<- type3_out %>%
    filter(df>1) %>%
    dplyr::select(var, pval, rid)

  glm_out %>%
    bind_rows(type3_out) %>%
    arrange(rid, var) %>%
    select(var, stat, pval, everything())

  # type3_out<- out %>%
  #   bind_rows(.id= "var") %>%
  #   mutate(pval= format_pvalue(chisq_p)) %>%
  #   filter(df>1) %>%
  #   dplyr::select(var, pval)
  #
  # type3_out$rid<- sapply(type3_out$var,
  #                        function(x) min(grep(x, glm_out$var, ignore.case = TRUE)))
  #
  # glm_out %>%
  #   mutate(rid= 1:n()) %>%
  #   bind_rows(type3_out) %>%
  #   arrange(rid, var) %>%
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
  # betas<- MIextract(cox_mira$analyses, fun= coef)
  # vars <- MIextract(cox_mira$analyses, fun= vcov)
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
  #                pval<- pf(wald_stat, df1= df, df2= df_denominator, lower.tail= FALSE)
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
    bind_rows()

  type3_out<- out %>%
    bind_rows() %>%
    mutate(pval= format_pvalue(chisq_p)) %>%
    filter(df>1) %>%
    dplyr::select(var, pval, rid)

  cox_out<- cox_mira %>%
    # pool() %>%
    # see https://github.com/amices/mice/issues/246#
    pool(dfcom = getfit(., 1L)$nevent - length(coef(getfit(., 1L)))) %>%
    summary(conf.int = TRUE,
            conf.level = 1-alpha,
            exponentiate= exponentiate) %>%
    # as.data.frame() %>%
    # rownames_to_column("var") %>%
    rename(est      = estimate,
           pval     = p.value,
           conf_low = `2.5 %`,
           conf_high= `97.5 %`) %>%
    mutate(var= as.character(term),
           stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
           pval= format_pvalue(pval)) %>%
    dplyr::select(var, stat, pval, est, conf_low, conf_high) %>%
    full_join(out_tmp, by= c("var" = "term"))

  cox_out %>%
    bind_rows(type3_out) %>%
    arrange(rid, var) %>%
    select(var, stat, pval, everything())
  # bind_rows(cox_out, type3_out) %>% arrange(rid, var)
}

#' @export
generate_mi_glm_termplot_df<- function(mira_obj,
                                       terms= NULL,
                                       center_at= NULL,
                                       vcov_fun= NULL, ...) {
  require(mitools)
  dummy_mdl<- getfit(mira_obj, 1L)
  tt<- stats::terms(dummy_mdl)
  terms<- if (is.null(terms)) 1:length(labels(tt)) else terms
  cn<- attr(tt, "term.labels")[terms]
  varseq<- attr(mm_orig<- model.matrix(dummy_mdl), "assign")

  carrier.name <- function(term) {
    if (length(term) > 1L)
      carrier.name(term[[2L]])
    else as.character(term)
  }

  betas <- MIextract(mira_obj$analyses,
                     fun = coef)

  vars  <- MIextract(mira_obj$analyses,
                     fun = if (is.null(vcov_fun)) vcov else vcov_fun)

  mi_res<- MIcombine(betas, vars)

  dummy_mdl$coefficients<- mi_res$coefficients

  plot_d<- termplot(dummy_mdl, terms= terms, plot= FALSE)

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
      mutate(df, y = y - y[which_x])
    } else df

    # now calculate the standard error
    tmp<- df
    names(tmp)<- gsub("x", sapply(str2expression(var), carrier.name), names(tmp))
    mm[, tt == varseq]<- (model.matrix(as.formula(paste("~ ", var)), data= tmp)[,-1])
    df$se<- sqrt(diag(mm %*% mi_res$variance %*% t(mm)))

    df %>%
      mutate(conf_low= y - qnorm(0.975) * se,
             conf_high= y + qnorm(0.975) * se)
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
