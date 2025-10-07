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

  flag <- FALSE

  flag_df <- tmp_df %>%
    dplyr::filter(time2evt <= 0) %>%
    dplyr::mutate(flag_evt_time_zero = (time2evt == 0),
                  flag_evt_time_neg = (time2evt< 0))

  if (any(tmp_df$time2evt == 0)) {
    warning("Event at time zero")
    tmp_df$time2evt <- replace(tmp_df$time2evt, tmp_df$time2evt==0, 0.5)
    flag <- TRUE
  }

  if (any(tmp_df$time2evt < 0)) {
    warning("Negative time-to-event!?")
    tmp_df$time2evt <- replace(tmp_df$time2evt, tmp_df$time2evt<0, NA)
    flag <- TRUE
  }

  if (flag) print(as.data.frame(flag_df))

  tmp_df <- if (is.null(surv_varname)) {
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

  n_cmp_evt <- length(cmp_evt_dt)
  names(cmp_evt_dt)<- sapply(cmp_evt_dt, rlang::as_name) # without, dplyr::select(df, !!!cmp_evt_dt) changes the variable name in the output data

  cmp_evt_desc<- paste0('cmp_evt_', seq_len(n_cmp_evt) + 1)
  evt_desc<- c('evt_1', cmp_evt_desc, 'censored_0')

  tmp_df <- df %>%
    dplyr::select(!!patid, !!idx_dt, !!evt_dt, !!!cmp_evt_dt, !!end_dt) %>%
    dplyr::group_by(!!patid) %>%
    # dplyr::mutate(first_evt_dt= pmin(!!evt_dt, !!!cmp_evt_dt, !!end_dt, na.rm= TRUE))
    dplyr::mutate(first_evt = ifelse(rlang::is_empty((evt_desc[which.min(c(!!evt_dt, !!!cmp_evt_dt, !!end_dt))])),
                                    NA, (evt_desc[which.min(c(!!evt_dt, !!!cmp_evt_dt, !!end_dt))])),
                  first_evt_dt = pmin(!!evt_dt, !!!cmp_evt_dt, !!end_dt, na.rm= TRUE),

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
  tmp_df <- tmp_df %>%
    dplyr::mutate(evt= factor(evt, 0:(n_cmp_evt + 1), labels = 0:(n_cmp_evt + 1)))


  flag <- FALSE
  flag_df <- tmp_df %>%
    dplyr::filter(time2evt<=0) %>%
    dplyr::mutate(flag_evt_time_zero= (time2evt==0),
                  flag_evt_time_neg = (time2evt< 0))

  if (any(tmp_df$time2evt==0)) {
    warning("Event at time zero")
    tmp_df$time2evt <- replace(tmp_df$time2evt, tmp_df$time2evt==0, 0.5)
    flag <- TRUE
  }

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

  # tmp_df<- dplyr::select(tmp_df, !!patid, dplyr::one_of(c(cmprisk_varname, 'evt_time', 'evt')))

  if (!append) tmp_df else {
    df %>%
      dplyr::inner_join(tmp_df, by = rlang::as_name(patid))
  }
}
