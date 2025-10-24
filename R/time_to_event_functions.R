


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

