
# dplyr::desc table helpers  -------------------------------------------------------------------------
#' @keywords internal

table_one_overall <- function(df,total = TRUE,round_to_100 = FALSE,drop.unused.levels = FALSE,overall_label = "Overall"){

  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select_if(Negate(is.character)) %>%
    dplyr::select_if(Negate(lubridate::is.Date)) %>%
    as.data.frame() %>% tibble::as_tibble()

  if(drop.unused.levels) df <- df %>% dplyr::mutate_if(is.factor, droplevels)



  num_out_lst <- if (any(sapply(df, class) %in% c("numeric", "integer"))) {
    numeric_desp(df) %>%
      tibble::rownames_to_column("row_id") %>%
      dplyr::mutate(row_id= paste(variable, type, sep= "_")) %>%
      split(., .$variable)
  } else NULL

  fct_out_lst <- if (any(sapply(df, class)=="factor")) {
    factor_desp(df,round_to_100 = round_to_100,drop.unused.levels=drop.unused.levels) %>%
      tibble::rownames_to_column("row_id") %>%
      dplyr::rename(type= level) %>%
      dplyr::mutate(row_id= ifelse(type!= "." & !is.na(type),
                                   paste(variable, gsub("\\ ", "_", trimws(type)), sep= "_"),
                                   variable)) %>%
      split(., .$variable)
  } else NULL

  logic_out_lst <- if (any(sapply(df, class)=="logical")) {
    logical_desp(df) %>%
      tibble::rownames_to_column("row_id") %>%
      dplyr::mutate(row_id= paste0(variable, "TRUE")) %>%
      split(., .$variable)
  } else NULL

  Total_N <- if (total) {

    N <- df %>%
      dplyr::count() %>% unlist()

    list(N = data.frame(row_id = "Total_N",variable  = "Total N",type = as.character(NA), n = as.character(N),stat = as.character(N)))

  } else NULL

  out_lst <-
    Total_N %>%
    append(num_out_lst) %>%
    append(fct_out_lst) %>%
    append(logic_out_lst)

  Output <- out_lst[c("N",names(df))] %>% dplyr::bind_rows() %>% dplyr::rename(!!paste0(overall_label,"_n") := n, !!paste0(overall_label,"_stat") := stat)



  return(Output)

}

#' @keywords internal
table_one_stratify <- function(df,group,total = TRUE,round_to_100 = FALSE,drop.unused.levels = FALSE){

  group <- rlang::enquo(group)

  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select_if(Negate(is.character)) %>%
    dplyr::select_if(Negate(lubridate::is.Date)) %>%
    dplyr::filter(!is.na(!!group)) %>%
    dplyr::group_by(!!group)

  if(drop.unused.levels) df <- df %>% dplyr::mutate_if(is.factor, droplevels)


  group_var_idx <- match(dplyr::group_vars(df), names(df))


  num_out_lst <- if (any(sapply(df[-group_var_idx], class) %in% c("numeric", "integer"))) {
    numeric_desp(df, !!group) %>%
      tibble::rownames_to_column("row_id") %>%
      dplyr::mutate(row_id= paste(variable, type, sep= "_")) %>%
      split(., .$variable)
  } else NULL

  fct_out_lst <- if (any(sapply(df[-group_var_idx], class)=="factor")) {
    factor_desp(df,!!group,round_to_100 = round_to_100,drop.unused.levels=drop.unused.levels) %>%
      tibble::rownames_to_column("row_id") %>%
      dplyr::rename(type= level) %>%
      dplyr::mutate(row_id= ifelse(type!= "." & !is.na(type),
                                   paste(variable, gsub("\\ ", "_", trimws(type)), sep= "_"),
                                   variable)) %>%
      split(., .$variable)
  } else NULL

  logic_out_lst <- if (any(sapply(df[-group_var_idx], class)=="logical")) {
    logical_desp(df, !!group) %>%
      tibble::rownames_to_column("row_id") %>%
      dplyr::mutate(row_id= paste0(variable, "TRUE")) %>%
      split(., .$variable)
  } else NULL

  Total_N <- if (total) {

    n_var <- df %>%
      dplyr::count(!!group) %>%
      tidyr::pivot_wider(names_from = !!group, values_from = n, values_fill = 0) %>%
      dplyr::mutate(variable = "Total N") %>%
      select(variable, dplyr::everything()) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.integer), as.character))

    list(N = dplyr::left_join(n_var, n_var, by= "variable", suffix= c("_n", "_stat")) %>%
           dplyr::mutate(row_id = "Total_N", type = as.character(NA), pval = as.character(NA), test = as.character(NA)) %>%
           dplyr::select(row_id,variable, type, dplyr::everything()) %>%
           dplyr::mutate(type= NA_character_))

  } else NULL

  out_lst <-
    Total_N %>%
    append(num_out_lst) %>%
    append(fct_out_lst) %>%
    append(logic_out_lst)

  Output <- out_lst[c("N",names(df))] %>% dplyr::bind_rows()



  return(Output)

}

kable_table_one <- function(out,pval,include_Missing,total,print_test,caption){
  indent <-  out %>% dplyr::filter(row_id != "Total_N") %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    select(dplyr::matches("_n$"),row_number)  %>%
    dplyr::filter(rowSums(is.na(.)) == (ncol(.)-1)) %>%
    dplyr::pull(row_number)


  first_row <- out %>% utils::head(1)  %>%
    select(dplyr::ends_with("_n"))

  variable_names <- gsub("_n", "", names(first_row))
  n_columns <- paste0(variable_names, "_n")
  stat_columns <- paste0(variable_names, "_stat")

  headers <- if(total){
    paste0(variable_names," (N = ",first_row,")")} else{
      variable_names
    }

  out <- out %>%
    dplyr::filter(!(dplyr::row_number() == 1 & total == TRUE)) %>%
    select(
      dplyr::all_of(c("var_desp", c(rbind(n_columns, stat_columns)))),
      dplyr::any_of(if (pval) c("pval", "pval.No.Missing", "pval.Missing") else NULL),
      dplyr::any_of(if (print_test) "test" else NULL)
    ) %>%

    kableExtra::kbl(caption = caption,
                    booktabs=TRUE,
                    escape = FALSE,
                    align= c('l', rep(c('c', 'c'), length(headers)), 'r'),
                    col.names = c('Variables', rep(c('N', 'Stat'), length(headers)),
                                  if (pval & !include_Missing) '*P*-value' else character(0) ,
                                  if (pval & include_Missing) 'Without missing' else character(0) ,
                                  if (pval & include_Missing) 'With missing' else character(0) ,
                                  if (print_test) 'Statistical test' else character(0))) %>%
    kableExtra::row_spec(row = 0, align = "c") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = FALSE) %>%
    kableExtra::add_header_above(c("", stats::setNames (rep(2, length(headers)), headers), if (pval & !include_Missing) '' else character(0),if(pval & include_Missing) stats::setNames (rep(2, 1), "*P*-value") else character(0), if (print_test ) '' else character(0)))%>%
    kableExtra::add_indent(indent)
}

#' @title titles_non_missing
#' @description Add a title to a dataframe
#' @export
#'
titles_non_missing <- function(df, columns, new_col_name = "Title") {
  df %>%
    dplyr::mutate(
      {{ new_col_name }} := dplyr::if_else(
        rowSums(!is.na(dplyr::across(dplyr::all_of(columns)))) > 0,
        TRUE,
        NA
      )
    ) %>%
    dplyr::relocate({{ new_col_name }}, .before = dplyr::all_of(columns[1]))
}

#' @title check_box_convert
#' @description To add checkbox questions to a table 1 in R
#' @export

check_box_convert <- function(df, check_box_cols, title = NULL) {
  out <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(check_box_cols), ~ as.logical(.x))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(check_box_cols), ~ {
      if (all(c_dplyr::across(dplyr::all_of(check_box_cols)) == FALSE)) NA else .x
    })) %>%
    dplyr::ungroup()

  if (!is.null(title)) {
    out <- titles_non_missing(out, columns = check_box_cols, new_col_name = !!title)
  }

  return(out)
}



# dplyr::desc table -------------------------------------------------------------------------


#---- this is the main function ----
#' @title table_one
#' @description Generates a table of summary statistics for descriptive analysis.
#' @details
#' The `table_one` function computes summary statistics for continuous, logical, and factor variables,
#' following the statistical reporting guidelines of the *Annals of Medicine*. If a grouping variable is provided,
#' the function can also evaluates between-group differences. The input data frame should consist only of numeric,
#' logical, and factor variables. Factor variables with only two levels should be converted to logical variables.
#' Date and datetime variables should be excluded.

#'
#' @param df A data frame consisting of numeric, logical, and factor variables with or without a grouping variable
#' @param group Name of the grouping variable (optional).
#' @param datadic datadic A data frame serving as a data dictionary, containing variable names and their descriptions.
#' @param var_name the column name of `data_dict` that
#'   contains the variable names. Only required if the column name is not "var_name"
#' @param var_desp the column name of `data_dict` that
#'   contains the variable descriptions Only required if the column name is not "var_desp"
#' @param seed An optional seed value for reproducibility of p-values
#' @param include_overall Character string specifying whether and how to include an overall summary.
#'   Must be one of:
#'   \itemize{
#'     \item `"none"`: Do not include an overall summary (default).
#'     \item `"group"`: Include an overall summary only for observations with non-missing values in the grouping variable.
#'     \item `"all"`: Include an overall summary for all observations, regardless of missingness in the grouping variable.
#'   }
#'   Default is `"none"`.
#' @param total Logical; whether to report the total N. Default is `TRUE`.
#' @param  pval Logical; whether to report p-values for between-group comparisons. Default is `TRUE`.
#' @param print_test Logical. If TRUE, the output will include the type of statistical test applied to each variable. Default is `FALSE`.
#' @param  continuous Character string specifying the summary statistics for continuous variables.
#'  Must be one of:
#'   \itemize{
#'     \item `"mediqr"`: Median and interquartile range.
#'     \item `"meansd"`: Mean and standard deviation.
#'     \item `"c("mediqr","meansd")"`: Both median/IQR and mean/SD.
#'   }
#' @param round_to_100 force rounded total to add up to 100 calculated using the largest remainder method for factor variables
#' @param drop.unused.levels Removes factor variables with 0. Levels with zero are not included in statistical tests
#' @param  kable_output Logical; if `TRUE`, outputs a formatted `kable` table including variable descriptions, N, statistics, and p-values.
#' @return A data frame containing summary statistics by variable type, optionally stratified by group and formatted for reporting, or a formatted kable table if `kable_output = TRUE`..
#' @examples
#' table_one(df = cardio_data, group = Sex)
#' @export
#' @importFrom rlang enquo quo_is_missing
#' @importFrom dplyr select
#'
table_one <- function(df, group, datadic = NULL, var_name, var_desp, seed = 123, include_overall  = c("none","group","all"),
                      total = TRUE,pval=TRUE,print_test  = FALSE,continuous = "mediqr",round_to_100 = FALSE,
                      drop.unused.levels = FALSE,
                      kable_output =TRUE,caption = NULL,overall_label = "Overall",include_Missing = FALSE,
                      Check_box = NULL,Check_box_title = NULL) {

  set.seed(seed)




  # Setting up  -------------------------------------------------------------------------


  include_overall <- match.arg(include_overall)
  op <- options(warn = -1)
  on.exit(options(op))

  group <- rlang::enquo(group)
  var_name <- rlang::enquo(var_name)
  var_desp <- rlang::enquo(var_desp)

  if (rlang::quo_is_missing(var_name)) var_name <- rlang::quo(var_name)
  if (rlang::quo_is_missing(var_desp)) var_desp <- rlang::quo(var_desp)

  if(rlang::quo_is_missing(group)) pval <- FALSE #No p-values without a grouping variable
  if(!pval) print_test <- FALSE #Can't print the test is there is no pvalue

  # Errors -------------------------------------------------------------------------

  ## Only allows mediqr & meansd for continuous variables

  invalid_continuous <- continuous[!continuous %in% c("mediqr", "meansd")]
  if (length(invalid_continuous) > 0) {
    stop(paste0('Invalid value(s) in "continuous": ', paste(unique(invalid_continuous), collapse = ", "),
                '. Allowed values are "mediqr" and "meansd".'))
  }

  #Overall name can not match a group name
  if((!rlang::quo_is_missing(group) & include_overall %in% c("group","all")) & overall_label %in% (df %>% dplyr::pull(!!group) %>% unique() %>% stats::na.omit())) {
    stop(paste0("`overall_label` ('", overall_label, "') cannot match an existing level in the grouping variable `"))
  }


  #Grouped summary table -------------------------------------------------------------------------

  if(!rlang::quo_is_missing(group)){
    summary_group <- table_one_stratify(df,group = !!group,total = total,round_to_100 = round_to_100,drop.unused.levels = drop.unused.levels)


    # Including a missing column and p-value -------------------------------------------------------------------------


    if(include_Missing){
      summary_group <-   df %>% dplyr::mutate(
        {{ group }} := {{ group }} %>%
          forcats::fct_na_value_to_level(level = "Missing")) %>%
        table_one_stratify(group = !!group,total = total,round_to_100 = round_to_100,drop.unused.levels = drop.unused.levels) %>%
        dplyr::left_join((summary_group %>% select(row_id,pval)),by = c('row_id'),suffix = c(".Missing",".No.Missing"))

    }

  }


  # Overall summary table -------------------------------------------------------------------------

  ##If include overall = "group" remove rows where the grouping variable is missing
  if(!rlang::quo_is_missing(group) & include_overall == "group"){
    df <- df %>% dplyr::filter(!is.na(!!group))
  }


  ##Remove the grouping variable from the overall table
  if(!rlang::quo_is_missing(group)){
    df <- df %>% select(-!!group)
  }

  if(rlang::quo_is_missing(group) |include_overall == "all" | include_overall == "group" ){
    summary_overall <- table_one_overall(df,total = total,round_to_100 = round_to_100,overall_label = overall_label,drop.unused.levels = drop.unused.levels)
  }


  # Combining summary tables together -------------------------------------------------------------------------

  if(!rlang::quo_is_missing(group) & (include_overall == "all" | include_overall == "group" )){
    summary <- summary_overall %>% dplyr::left_join(summary_group,by = c('row_id','variable','type'))
  }else if(rlang::quo_is_missing(group)){
    summary <- summary_overall
  }else {
    summary <- summary_group
  }

  ##Run the grouped table (If include_overall is include_overall = all or include_overall = group)


  # Removing pre-defined columns (pval, continuous) -------------------------



  if(!pval) summary$pval = summary$pval.Missing = summary$pval.No.Missing <- NULL
  if(!print_test ) summary$print_test  <- NULL

  #Optionally removing the continuous variables
  if(!"meansd" %in% (continuous)) summary <- summary %>% dplyr::filter(!grepl("_meansd$", row_id))
  if(!"mediqr" %in% (continuous)) summary <- summary %>% dplyr::filter(!grepl("_mediqr$", row_id))


  # Adding the datadic and cleaning the table -------------------------------------------------------------------------


  if (is.null(datadic)) {
    out <- summary %>%
      dplyr::select(row_id, variable, type,
                    dplyr::ends_with("n"), dplyr::ends_with("stat"), dplyr::everything()) %>%
      # dplyr::select(-!!var_desp) %>%
      dplyr::mutate(type = ifelse(is.na(type) & row_id==variable,
                           gsub("(^[[:lower:]])", "\\U\\1", variable, perl=TRUE), type),
             type = ifelse(type %in% c("meansd", "mediqr"),
                           gsub("(^[[:lower:]])", "\\U\\1", variable, perl=TRUE), type),
             type = ifelse(row_id==paste0(variable, "TRUE"),
                           gsub("(^[[:lower:]])", "\\U\\1", variable, perl=TRUE), type)) %>%
      dplyr::rename(`var_desp`= type)
  } else {
    out <- summary %>%
      dplyr::left_join(dplyr::select(datadic, !!var_name, !!var_desp),
                by = c("variable"= rlang::quo_name(var_name))) %>%
      dplyr::mutate(type = ifelse(is.na(type) & row_id==variable, !!var_desp, type), # factor
             type = ifelse(type %in% c("meansd", "mediqr"), !!var_desp, type), # continuous
             type = ifelse(row_id==paste0(variable, "TRUE"), !!var_desp, type), # logical
      ) %>%
      dplyr::select(row_id, variable, type,
                    dplyr::ends_with("n"), dplyr::ends_with("stat"), dplyr::everything()) %>%
      dplyr::select(-!!var_desp) %>%
      dplyr::rename(`var_desp`= type)
  }

  ##Formatting check box rows to indent
  out <- out   %>%
    dplyr::mutate(dplyr::across(
      dplyr::ends_with("_n"),
      ~ dplyr::if_else(variable %in% Check_box, NA, .)
    ))

  ##Formatting title rows to
  out <- out   %>%
    dplyr::mutate(dplyr::across(
      dplyr::ends_with("_stat"),
      ~ dplyr::if_else(variable %in% Check_box_title, NA, .)
    ))



  # out <- out %>%
  #   dplyr::mutate(dplyr::across(
  #     c(test, pval),
  #     ~ dplyr::if_else(variable %in% Check_box_title, NA, .)
  #   ))



  # Creating a kable table -------------------------------------------------------------------------



  if(kable_output){

    out <-  kable_table_one(out,pval = pval,include_Missing = include_Missing,print_test = print_test,total=total,caption=caption)

  }

  out

}






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







# Factor desp -------------------------------------------------------------------------

#---- summarize factor variables ----
#' @title factor_desp
#'
#' @details An internal function that calculate frequencies and proportion of levels in factor/categorical variables.
#' If there are more than 1 group, then fisher exact tests are applied to assess the between-group differences.
#'
#' @param df Dataframe
#' @return a dataframe consisting of columns of character variables indicating the frequency and proportion of logical variables.
#' @export
factor_desp <- function(df, group, includeNA = FALSE,round_to_100 = FALSE,drop.unused.levels = FALSE) {

  ##
  make_univariate_fml <- function(x, y= NULL) {
    sapply(x, function(x){
      if (is.null(y)) stats::formula(paste0("~", x)) else stats::formula(paste0(y, "~", x))
    })
  }

  make_bivariate_fml <- function(x, z, y= NULL) {
    sapply(x, function(x){
      if (is.null(y)) stats::formula(paste0("~", x, " + ", z)) else stats::formula(paste0(y, "~", x, " + ", z))
    })
  }

  output_one_way_tbl <- function(freq, pct_digits = 1,round_to_100 = FALSE) {
    pct  <- prop.table(freq)
    var_name <- names(dimnames(freq))
    freq <- freq %>%
      stats::addmargins() %>%
      as.data.frame(row.names= names(dimnames(.)),
                    responseName = "freq",
                    stringsAsFactors = FALSE) %>%
      tibble::rownames_to_column("level") %>%
      dplyr::mutate(n   = ifelse(level=="Sum", freq, NA),
             freq= ifelse(level=="Sum", NA, freq),
             n   = ifelse(!is.na(n), formatC(n, format= "d", big.mark = ","), NA_character_),
             freq= ifelse(!is.na(freq), formatC(freq, format= "d", big.mark = ","), NA_character_)
      ) %>%
      dplyr::select(level, n, freq)

    pct <- pct %>%
      as.data.frame(row.names= names(dimnames(.)),
                    responseName = "pct",
                    stringsAsFactors = FALSE) %>%
      tibble::rownames_to_column("level")  %>%
      dplyr::mutate(pct = if (round_to_100) formatC(exact_round_100(pct * 100,digits = pct_digits), digits = pct_digits, format = "f") else formatC(pct * 100, digits = pct_digits, format = "f"))%>%
      dplyr::select(level, pct)

    freq <- freq %>%
      dplyr::mutate_all(as.character)

    pct <- pct %>%
      dplyr::mutate_all(as.character)

    out <- dplyr::full_join(freq, pct, by = c("level")) %>%
      dplyr::mutate(stat= paste0(freq, " (", pct, "%)"),
             stat= ifelse(level=="Sum", NA_character_, stat),
             level= ifelse(level=="Sum", ".", level)) %>%
      dplyr::select(level, n, stat) %>%
      dplyr::mutate_all(as.character)

    out[match(c('.', levels(df[[var_name]])), out$level),]
  }

  output_two_way_tbl <- function(freq, pct_digits = 1,round_to_100 = FALSE) {
    tbl_var_name <- names(dimnames(freq))
    pct  <- prop.table(freq, margin = 2)

    # fisher exact test
    test <- try(stats::fisher.test(freq[rowSums(freq) != 0, ], hybrid = TRUE, conf.int = FALSE, simulate.p.value= TRUE, B= 9999), silent = TRUE)
    test <- if (class(test)=="try-error") NA else test$p.value

    # total
    total <- margin.table(freq, 2) %>%
      as.data.frame(responseName = "n", stringsAsFactors = FALSE) %>%
      dplyr::mutate(n= ifelse(!is.na(n), formatC(n, format= "d", big.mark = ","), NA_character_)) %>%
      reshape2::dcast(stats::as.formula( paste0(". ~ ", tbl_var_name[2])), value.var = "n") %>%
      dplyr::bind_cols(pval= format_pvalue(test))  %>%  dplyr::mutate(test = "Fisher")

    freq <- freq %>%
      as.data.frame(responseName = "freq", stringsAsFactors = FALSE) %>%
      dplyr::mutate(freq= ifelse(!is.na(freq), formatC(freq, format= "d", big.mark = ","), NA_character_))

    if(round_to_100){
      pct <- pct %>%
        as.data.frame(responseName = "pct", stringsAsFactors = FALSE) %>%
        dplyr::mutate(pct = ifelse(is.nan(pct),0,pct)) %>%
        dplyr::group_by(!!rlang::sym(tbl_var_name[2])) %>%
        dplyr::mutate(pct=  formatC(exact_round_100(pct * 100,digits = pct_digits), digits = pct_digits, format = "f"))


    }else {
      pct <- pct %>%
        as.data.frame(responseName = "pct", stringsAsFactors = FALSE) %>%
        dplyr::mutate(pct = ifelse(is.nan(pct),0,pct)) %>%
        dplyr::mutate(pct= formatC(pct*100, digits= pct_digits, format= "f"))
    }



    freq <- freq %>%
      dplyr::mutate_all(as.character)

    pct <- pct %>%
      dplyr::mutate_all(as.character)

    out <- dplyr::full_join(freq, pct, by= tbl_var_name) %>%
      dplyr::mutate(stat= paste0(freq, " (", pct, "%)")) %>%
      # dplyr::bind_rows(total) %>%
      dplyr::select(-freq, -pct) %>%
      reshape2::dcast(stats::as.formula(paste0(tbl_var_name, collapse= " ~ ")),
            value.var = "stat") %>%
      dplyr::mutate_all(as.character)

    names(out)[1]<- names(total)[1]<- "level"
    out <- dplyr::full_join(total, out, by= "level", suffix= c("_n", "_stat"))
    out[match(c('.', levels(df[[tbl_var_name[1]]])), out$level),]
    # out
  }
  ##


  group <- rlang::enquo(group)

  # 1 - select variables of factor class
  if (rlang::quo_is_missing(group)) {
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::select_if(is.factor)

    fml <- make_univariate_fml(names(df))
    # 2 - create table object for ALL selected factor variables (not sure if it is a good idea but ...)
    # here we are going to drop unused levels (drop.unused.levels = TRUE)
    tbl_list <- lapply(fml, xtabs, data= df, addNA= includeNA, drop.unused.levels = drop.unused.levels)
    out     <- lapply(tbl_list, output_one_way_tbl, pct_digits = if (nrow(df)< 200) 0 else 1,round_to_100 = round_to_100)

  } else {
    df <- df %>%
      dplyr::group_by(!!group) %>%
      dplyr::select_if(is.factor)

    fml <- make_bivariate_fml(grep(paste0("^", rlang::quo_name(group), "$"), names(df), value= TRUE, invert = TRUE),
                              z= rlang::quo_name(group))

    # 2 - create table object for ALL selected factor variables (not sure if it is a good idea but ...)
    # here we are going to drop unused levels (drop.unused.levels = TRUE)
    tbl_list <- lapply(fml, xtabs, data= df, addNA= includeNA, drop.unused.levels = drop.unused.levels)
    out     <- lapply(tbl_list, output_two_way_tbl, pct_digits = if (nrow(df)< 200) 0 else 1,round_to_100 = round_to_100)
  }

  out <- out %>%
    dplyr::bind_rows(.id= "variable") %>%
    dplyr::mutate(level= ifelse(level==".", NA_character_, level))
  out

}



#' @title factor_dist
#'
#' @details
#' An internal function that takes a table object (i.e. a cross table for all factor variables in the data) and outputs
#' marginal table. If there are more than 1 group, then fisher exact tests are applied to assess the between-group differences.
#'
#' @param df Dataframe
#' @return a dataframe consisting of columns of character variables indicating the frequency and proportion of logical variables.
factor_dist<- function(table_obj, col_var, pct_digits= 1, removeNA= TRUE) {
  ## This function takes a table object (i.e. a cross table for all factor variables in the data) and outputs
  ## marginal table.
  ##   Row variables are the variables of factor class in input data
  ##   At most one column variable is allowed.

  col_var<- rlang::enquo(col_var)
  fct_var_name<- names(dimnames(table_obj))

  # no column variable
  if (rlang::quo_is_missing(col_var)) {

    out<- lapply(fct_var_name,
                 function(x) {
                   # for each row variable do the following:

                   # get the dimension index for each of the fct_var_name
                   m_idx<- grep(x, fct_var_name)

                   freq <- margin.table(table_obj, margin= m_idx)
                   # if (removeNA) {
                   #   freq <- freq[!is.na(names(freq))]
                   # }
                   pct  <- prop.table(freq)

                   freq<- freq %>%
                     stats::addmargins() %>%
                     as.data.frame(row.names= names(dimnames(.)),
                                   responseName = "freq",
                                   stringsAsFactors = FALSE) %>%
                     tibble::rownames_to_column("level") %>%
                     dplyr::mutate(n   = ifelse(level=="Sum", freq, NA_integer_),
                            n   = ifelse(!is.na(n), formatC(n, format= "d", big.mark = ","), NA_character_),
                            freq= ifelse(level=="Sum", NA_integer_, freq),
                            freq= ifelse(!is.na(freq), formatC(freq, format= "d", big.mark = ","), NA_character_),
                     ) %>%
                     dplyr::select(level, n, freq)

                   pct<- pct %>%
                     as.data.frame(row.names= names(dimnames(.)),

                                   responseName = "pct",
                                   stringsAsFactors = FALSE) %>%
                     tibble::rownames_to_column("level") %>%
                     dplyr::mutate(pct= formatC(pct*100, digits= pct_digits, format= "f")) %>%
                     dplyr::select(level, pct)

                   dplyr::full_join(freq, pct, by = c("level")) %>%
                     dplyr::mutate(stat= paste0(freq, " (", pct, "%)"),
                            stat= ifelse(level=="Sum", NA_character_, stat),
                            level= ifelse(level=="Sum", ".", level)) %>%
                     dplyr::select(level, n, stat) %>%
                     dplyr::arrange(level)

                 }) %>%
      stats::setNames (fct_var_name)

  } else {
    # column is the grouping variable
    col_idx <- grep(rlang::quo_name(col_var), fct_var_name)
    row_vars<- grep(rlang::quo_name(col_var), fct_var_name, invert = TRUE, value= TRUE)

    out<- lapply(row_vars,
                 function(x) {

                   # get the dimension index for each of the fct_var_name
                   m_idx<- grep( paste0("^", x, "$"), fct_var_name)

                   freq <- margin.table(table_obj, margin= c(m_idx, col_idx))
                   # if (removeNA) {
                   #   freq <- freq[!is.na(rownames(freq)), !is.na(colnames(freq))]
                   # }
                   pct  <- prop.table(freq, margin = 2)

                   # fisher exact test
                   test<- try(stats::fisher.test(freq, hybrid = TRUE, conf.int = FALSE), silent = TRUE)
                   test<- if (class(test)=="try-error") NA else test$p.value

                   # total
                   total<- margin.table(freq, 2) %>%
                     as.data.frame(responseName = "n", stringsAsFactors = FALSE) %>%
                     dplyr::mutate(n= formatC(n, format= "d", big.mark = ",")) %>%
                     reshape2::dcast(stats::as.formula( paste0(". ~ ", rlang::quo_name(col_var))), value.var = "n") %>%
                     dplyr::bind_cols(pval= format_pvalue(test))


                   freq<- freq %>%
                     as.data.frame(responseName = "freq", stringsAsFactors = FALSE) %>%
                     dplyr::mutate(freq= formatC(freq, format= "d", big.mark = ","))

                   pct<- pct %>%
                     as.data.frame(responseName = "pct", stringsAsFactors = FALSE) %>%
                     dplyr::mutate(pct= formatC(pct*100, digits= pct_digits, format= "f"))

                   out<- dplyr::full_join(freq, pct, by= c(x, rlang::quo_name(col_var))) %>%
                     dplyr::mutate(stat= paste0(freq, " (", pct, "%)")) %>%
                     # dplyr::bind_rows(total) %>%
                     dplyr::select(-freq, -pct) %>%
                     reshape2::dcast(stats::as.formula(paste(x, rlang::quo_name(col_var), sep= " ~ ")), value.var = "stat")

                   names(out)[1]<- names(total)[1]<- "level"
                   out<- dplyr::full_join(total, out, by= "level", suffix= c("_n", "_stat"))
                   out
                 }) %>%
      stats::setNames (row_vars)
  }
  out %>%
    dplyr::bind_rows(.id= "variable") %>%
    dplyr::mutate(level= ifelse(level==".", NA_character_, level))
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
admin_censor_surv<- function(df, evt_time, evt, adm_cnr_time= NULL, overwrite_var= FALSE) {
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






# logical_descp -------------------------------------------------------------------------



#---- summarize logical variables ----
#' @title logical_desp
#'
#' @details
#' An internal function that calculate frequencies and proportion of TRUE in logical variables. If there are more
#' than 1 group, then fisher exact tests are applied to assess the between-group differences.
#'
#' @param df Dataframe
#' @return a dataframe consisting of columns of character variables indicating the frequency and proportion of logical variables.
#' @export
logical_desp <- function(df, group) {

  binary_desp <- function(x, pct_digits= 1) {
    fun <- c(sum, mean)
    out <- sapply(fun,
                  function(f) {
                    res <- try(f(x, na.rm= TRUE), silent = TRUE)
                    res <- if (class(res)== "try-error") NA else res
                    return(res)
                  })

    freq <- formatC(out[1], format= "d", big.mark = ",")
    pct <- formatC(out[2]*100, digits= pct_digits, format= "f")
    pct <- paste0(pct, "%")
    out <- paste0(freq, " (", pct, ")")
    out
  }

  group <- rlang::enquo(group)
  df <- df %>%
    dplyr::ungroup()

  # fisher exact test
  # test<- try(stats::fisher.test(freq, hybrid = TRUE, conf.int = FALSE), silent = TRUE)
  # test<- if (class(test)=="try-error") NA else test$p.value
  test_fun <- fisher_test

  if (rlang::quo_is_missing(group)) {

    sum_stat <- df %>%
      dplyr::summarise_if(is.logical, dplyr::funs(binary_desp),
                   pct_digits= if (nrow(.)>= 200) 1 else 0) %>%
      tibble::rownames_to_column() %>%
      reshape2::melt(id.vars= "rowname", value.name = "stat") %>%
      dplyr::mutate(type= "freq") %>%
      dplyr::select(variable, type, stat)

    n_var <- df %>%
      dplyr::summarise_if(is.logical, dplyr::funs(n_avail)) %>%
      tibble::rownames_to_column() %>%
      reshape2::melt(id.vars= "rowname", value.name = "n") %>%
      dplyr::select(-rowname)

  } else {

    df <- df %>%
      # dplyr::ungroup() %>%
      dplyr::group_by(!!group)

    sum_stat <- df %>%
      dplyr::summarise_if(is.logical, dplyr::funs(binary_desp),
                   pct_digits= if (nrow(.)>= 200) 1 else 0) %>%
      reshape2::melt(id.vars= rlang::quo_name(group), factorsAsStrings= TRUE) %>%
      reshape2::dcast(stats::as.formula(paste("variable", rlang::quo_name(group), sep= " ~ "))) %>%
      dplyr::mutate(type= "freq") %>%
      # dplyr::left_join(test_fun(df, rlang::UQ(group)), by= c("variable", "type"))
      dplyr::left_join(test_fun(df, !!group), by= c("variable", "type"))

    n_var <- df %>%
      dplyr::summarise_if(is.logical, dplyr::funs(n_avail)) %>%
      reshape2::melt(id.vars= rlang::quo_name(group), factorsAsStrings= TRUE) %>%
      reshape2::dcast(stats::as.formula(paste("variable", rlang::quo_name(group), sep= " ~ ")))
  }

  n_var <- n_var %>%
    dplyr::mutate_all(as.character)

  sum_stat <- sum_stat %>%
    dplyr::mutate_all(as.character)

  dplyr::left_join(n_var, sum_stat, by= "variable", suffix= c("_n", "_stat")) %>%
    dplyr::select(variable, type, dplyr::everything()) %>%
    dplyr::mutate(type= NA_character_) %>%
    dplyr::arrange(variable, type)
}


#' @title fisher_test
#'
#' @details
#' An internal function that applies fisher exact tests to assess the between-group differences in logical variables
#'
#' @param df Dataframe
#' @return a dataframe of a single column of character variables indicating p-values.
fisher_test <- function(df, group) {

  group <- enquo(group)
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!group)

  # fisher_test<- function(...) try(stats::fisher.test(..., hybrid = TRUE, conf.int= FALSE), silent = TRUE)
  fisher_test <- if (dplyr::n_groups(df)==2) {
    function(...) try(stats::fisher.test(..., conf.int= FALSE), silent = TRUE)
  } else {
    function(...) try(stats::fisher.test(..., hybrid = TRUE, conf.int= FALSE), silent = TRUE)
  }

  df %>%
    dplyr::select_if(is.logical) %>%
    reshape2::melt(id.vars= rlang::quo_name(group),
         factorsAsStrings= TRUE,
         na.rm= TRUE) %>%
    dplyr::group_by(variable) %>%
    tidyr::nest() %>%
    dplyr::mutate(freq= purrr::map(data,
                     function(df) {

                       res<- df %>%
                         table() %>%
                         fisher_test()

                       # res<- if (class(res)=="try-error") NA else res$p.value
                       res<- if (class(res)=="try-error") {
                         fisher_test<- if (dplyr::n_groups(df)==2) {
                           function(...) try(stats::fisher.test(..., conf.int= FALSE, simulate.p.value = TRUE, B=5000), silent = TRUE)
                         } else {
                           function(...) try(stats::fisher.test(..., hybrid = TRUE, conf.int= FALSE, simulate.p.value = TRUE, B=5000), silent = TRUE)
                         }
                         res_try<- df %>%
                           table() %>%
                           fisher_test()
                         if (class(res_try)=="try-error") NA else res_try$p.value
                       } else res$p.value
                       res
                     })) %>%
    dplyr::select(-data) %>%
    tidyr::unnest() %>%
    reshape2::melt(id.vars= "variable", variable.name= "type", value.name = "pval") %>%
    dplyr::mutate(pval= format_pvalue(pval)) %>%
    dplyr::mutate_all(as.character) %>% dplyr::mutate(test = "Fisher")

  #   variable  type       pval
  # 1       dm  freq 0.04608804
  # 2       af  freq 0.19948767
}




# numeric_desp -------------------------------------------------------------------------



#---- summarize numeric variables ----
#' @title numeric_desp
#'
#' @details
#' An internal function that report mean, standard deviation, median and interquartile range by group.
#'
#' @param df Dataframe
#' @return a dataframe consisting of columns of character variables.
#' @export
numeric_desp<- function(df, group) {
  group<- rlang::enquo(group)
  df<- df %>%
    dplyr::ungroup()

  if (rlang::quo_is_missing(group)) {

    df<- df %>%
      dplyr::select_if(is.numeric)

    n_var <- df %>%
      dplyr::summarise_if(is.numeric, list(~ n_avail(.))) %>%
      tibble::rownames_to_column() %>%
      reshape2::melt(id.vars= "rowname", value.name = "n") %>%
      dplyr::select(-rowname)

    sum_stat <- if (ncol(df)==1) {
      # the naming rule changes when there is only one numeric variable
      df %>%
        dplyr::summarise_if(is.numeric, list(~ mean_sd(.), ~ med_iqr(.))) %>%
        dplyr::rename_at(dplyr::vars(mean_sd, med_iqr),
                  function(x) paste(names(df), x, sep= "_")) %>%
        tibble::rownames_to_column() %>%
        reshape2::melt(id.vars = "rowname", value.name = "stat") %>%
        dplyr::mutate(type = ifelse(grepl("mean_sd$", variable), "meansd", "mediqr"),
               variable = gsub("(_mean_sd|_med_iqr)$", "", variable)) %>%
        dplyr::select(variable, type, stat)

    } else {
      df %>%
        dplyr::summarise_if(is.numeric, list(~ mean_sd(.), ~ med_iqr(.))) %>%
        tibble::rownames_to_column() %>%
        reshape2::melt(id.vars = "rowname", value.name = "stat") %>%
        dplyr::mutate(type = ifelse(grepl("mean_sd$", variable), "meansd", "mediqr"),
               variable = gsub("(_mean_sd|_med_iqr)$", "", variable)) %>%
        dplyr::select(variable, type, stat)

    }

  } else {

    df<- df %>%
      dplyr::group_by(!!group) %>%
      dplyr::select_if(is.numeric)

    n_var<- df %>%
      dplyr::summarise_if(is.numeric, list(~ n_avail(.))) %>%
      reshape2::melt(id.vars= rlang::quo_name(group), factorsAsStrings= TRUE) %>%
      reshape2::dcast(stats::as.formula(paste("variable", rlang::quo_name(group), sep= " ~ ")))

    sum_stat <- if (length(grep(dplyr::group_vars(df), names(df), invert = TRUE))==1) {
      df %>%
        dplyr::summarise_if(is.numeric, list(~ mean_sd(.), ~ med_iqr(.))) %>%
        dplyr::rename_at(dplyr::vars(mean_sd, med_iqr),
                  function(x) paste(grep(dplyr::group_vars(df), names(df), invert = TRUE, value= TRUE), x, sep= "_")) %>%
        reshape2::melt(id.vars= rlang::quo_name(group), factorsAsStrings= TRUE) %>%
        reshape2::dcast(stats::as.formula(paste("variable", rlang::quo_name(group), sep= " ~ "))) %>%
        dplyr::mutate(type= ifelse(grepl("mean_sd$", variable), "meansd", "mediqr"),
               variable= gsub("(_mean_sd|_med_iqr)$", "", variable))
    } else {
      df %>%
        dplyr::summarise_if(is.numeric, list(~ mean_sd(.), ~ med_iqr(.))) %>%
        reshape2::melt(id.vars= rlang::quo_name(group), factorsAsStrings= TRUE) %>%
        reshape2::dcast(stats::as.formula(paste("variable", rlang::quo_name(group), sep= " ~ "))) %>%
        dplyr::mutate(type= ifelse(grepl("mean_sd$", variable), "meansd", "mediqr"),
               variable= gsub("(_mean_sd|_med_iqr)$", "", variable))
    }

    # adding the p-values
    test_fun<- if (dplyr::n_groups(df)==2) two_sample_test else if (dplyr::n_groups(df)>2) k_sample_test

    sum_stat<- sum_stat %>%
      # dplyr::left_join(test_fun(df, rlang::UQ(group)), by= c("variable", "type"))
      dplyr::left_join(test_fun(df, !!group), by= c("variable", "type"))

  }

  n_var<- n_var %>%
    dplyr::mutate_all(as.character)

  sum_stat<- sum_stat %>%
    dplyr::mutate_all(as.character)

  dplyr::left_join(n_var, sum_stat, by= "variable", suffix= c("_n", "_stat")) %>%
    dplyr::select(variable, type, dplyr::everything()) %>%
    dplyr::arrange(variable, type)
}



#' @title n_avail
#'
#' @details
#' An internal function that format the number of non-missing observations in a variable
#'
#' @param x Numeric variable
#' @return a character reporting the number of non-missing observations
#' @export
n_avail<- function(x) formatC( sum( !is.na(x) ), digits= 0, format= "d", big.mark = ",")

#' @title mean_sd
#'
#' @details An internal function that format mean and standard deviation of a continuous variable
#'
#' @param x Numeric variable
#' @return a character reporting the mean plus/minus standard deviation
#' @export
mean_sd <- function(x) {
  n_dec <- decimalplaces(x)

  funs<- c(mean, stats::sd)
  out<- sapply(funs,
               function(f) {
                 res<- try(f(x, na.rm= TRUE), silent = TRUE)
                 res<- if (class(res)== "try-error") NA else res
                 return(res)
               })

  if (length(x[!is.na(x)])==0) {
    out<- "---"
  } else if (length(x[!is.na(x)])==1) {
    out<- formatC( out[1], digits= n_dec, format= "f", big.mark = ",", flag= "#")
  } else {
    out<- formatC( out, digits= n_dec, format= "f", big.mark = ",", flag= "#")
    out<- paste0(out, collapse = " \u00B1 ") # plusminus sign
  }

  out<- c(stat= out)
  out
}



#' @title med_iqr
#'
#' @details An internal function that format median and interquartile range of a continuous variable
#'
#' @param x Numeric variable
#' @return a character reporting the median (Q1 - Q3)
#' @export
med_iqr <- function(x) {
  q1 <- function(x, ...) stats::quantile(x, probs = .25, ...)
  q3 <- function(x, ...) stats::quantile(x, probs = .75, ...)

  n_dec<- decimalplaces(x)
  funs<- c(stats::median, q1, q3)
  out<- sapply(funs,
               function(f) {
                 res <- try(f(x, na.rm= TRUE), silent = TRUE)
                 res <- if (class(res)== "try-error") NA else res
                 return(res)
               })
  if (length(x[!is.na(x)])==0) {
    out<- "---"
  } else if (length(x[!is.na(x)])==1) {
    out<- formatC( out[1], digits= n_dec, format= "f", big.mark = ",", flag= "#")
  } else {
    out<- formatC( out, digits= n_dec, format= "f", big.mark = ",", flag= "#")
    out<- paste0(out[1], " (",
                 paste0(out[-1], collapse= " \u2013 "), ")") # long dash
  }

  out<- c(stat= out)
  out
}



#---- test ----
#' @title two_sample_test
#'
#' @details
#' An internal function that tests equality of location parameter using two sample t-tests with unequal
#' variance when mean is reported and nonparametric rank-sum tests otherwise in comparing of two groups.
#'
#' @param x Numeric variable
#' @param grp Factor variable
#' @return a 2-tuple vectors reporting p-values
#' @export
two_sample_test<- function(df, group) {

  group<- enquo(group)

  df %>%
    dplyr::ungroup() %>%
    # dplyr::group_by(rlang::UQ(group)) %>%
    dplyr::group_by(!!group) %>%
    dplyr::select_if(is.numeric) %>%
    reshape2::melt(id.vars= rlang::quo_name(group), factorsAsStrings= TRUE, na.rm= TRUE) %>%
    dplyr::group_by(variable) %>%
    tidyr::nest() %>%
    dplyr::mutate(ttest= purrr::map_dbl(data,
                          function(df) {
                            fml<- stats::as.formula(paste0("value ~ factor(", rlang::quo_name(group), ")"))
                            res<- try(stats::t.test(fml, data= df, var.equal = FALSE), silent = FALSE)
                            if (class(res)=="try-error") NA_real_ else res$p.value
                          }),
           wilcox= purrr::map_dbl(data,
                           function(df) {
                             # if (!is.factor(df[rlang::quo_name(group)])) df[rlang::quo_name(group)]<- factor(df[rlang::quo_name(group)])
                             fml<- stats::as.formula(paste0("value ~ factor(", rlang::quo_name(group), ")"))
                             res<- try(stats::wilcox.test(fml, data= df), silent = FALSE)
                             if (class(res)=="try-error") NA_real_ else res$p.value
                           })) %>%
    dplyr::select(-data) %>%
    # tidyr::unnest(cols = c(ttest, wilcox)) %>%
    reshape2::melt(id.vars= "variable", variable.name= "test", value.name = "pval") %>%
    dplyr::mutate(type= ifelse(grepl("^ttest", test), "meansd", "mediqr"),
           pval= format_pvalue(pval)
    ) %>%
    dplyr::mutate_all(as.character)
  #   variable   type   pval
  # 1   income meansd   0.52
  # 2   weight meansd <0.001
  # 3   income mediqr   0.47
  # 4   weight mediqr <0.001
}



#' @title k_sample_test
#'
#' @details An internal function that tests equality of location parameter using one way ANOVA with unequal
#' variance when mean is reported and nonparametric Kruskal-Wallis tests otherwise in comparing >2 groups.
#'
#' @param x Numeric variable
#' @param grp Factor variable
#' @return a 2-tuple vectors reporting p-values
#' @export
k_sample_test<- function(df, group) {

  group<- enquo(group)

  df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!group) %>%
    dplyr::select_if(is.numeric) %>%
    reshape2::melt(id.vars= rlang::quo_name(group), factorsAsStrings= TRUE, na.rm= TRUE) %>%
    dplyr::group_by(variable) %>%
    tidyr::nest() %>%
    dplyr::mutate(oneway= purrr::map_dbl(data,
                           function(df) {
                             fml<- stats::as.formula(paste0("value ~ factor(", rlang::quo_name(group), ")"))
                             res<- try(stats::oneway.test(fml, data= df, var.equal = FALSE), silent = FALSE)
                             if (class(res)=="try-error") NA_real_ else res$p.value
                           }),
           kruskal= purrr::map_dbl(data,
                            function(df) {
                              fml<- stats::as.formula(paste0("value ~ factor(", rlang::quo_name(group), ")"))
                              res<- try(stats::kruskal.test(fml, data= df), silent = FALSE)
                              if (class(res)=="try-error") NA_real_ else res$p.value
                            })) %>%
    dplyr::select(-data) %>%
    # tidyr::unnest(cols = c(ttest, wilcox)) %>%
    reshape2::melt(id.vars= "variable", variable.name= "test", value.name = "pval") %>%
    dplyr::mutate(type= ifelse(grepl("^oneway", test), "meansd", "mediqr"),
           pval= format_pvalue(pval)
    ) %>%
    dplyr::mutate_all(as.character)
}


