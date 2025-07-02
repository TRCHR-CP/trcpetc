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
#' @param  kable_output Logical; if `TRUE`, outputs a formatted `kable` table including variable descriptions, N, statistics, and p-values.
#' @return A data frame containing summary statistics by variable type, optionally stratified by group and formatted for reporting, or a formatted kable table if `kable_output = TRUE`..
#' @examples
#' set.seed(0)
#' df<- data_frame(sex   = factor(c(rep("F", 90), rep("M", 900))),
#'                 grade = factor(sample(c("A", "B", "C"), 990, replace= TRUE), c("A", "B", "C", "D")),
#'                 income=  100 * (rnorm(990) + 5),
#'                 dm= rbernoulli(990, p= .5),
#'                 af= rbernoulli(990, p= .95)) %>%
#'   mutate(weight= if_else( sex=="F" & income>500, 3, 1),
#'          income= ifelse(income<456, NA, income),
#'          sex   = ifelse(runif(990)<.2, NA, sex),
#'          sex   = factor(sex, 1:2, labels = c("Female", "Male")),
#'          grade = ifelse(runif(990)<.25, NA, grade),
#'          grade   = factor(grade, 1:4, labels = c("A", "B", "C", "D")))
#'
#'  datadic<- data.frame(var_name= c("sex", "grade", "income", "dm", "af"),
#'                       var_desp= c("Sex", "Grade", "Household income",
#'                                   "Presence of diabetes mellitus", "African American"),
#'                      stringsAsFactors = FALSE)
#'
#' table_one(df, sex)
#' table_one(df, sex, datadic= datadic)
#' @export
#' @importFrom rlang enquo quo_is_missing
#' @importFrom dplyr select
#'
table_one <- function(df, group, datadic = NULL, var_name, var_desp, seed = 123, include_overall  = c("none","group","all"),
                      total = TRUE,pval=TRUE,print_test  = FALSE,continuous = "mediqr",kable_output=TRUE,caption = NULL) {



  invalid_continuous <- continuous[!continuous %in% c("mediqr", "meansd")]

  if (length(invalid_continuous) > 0) {
    stop(paste0('Invalid value(s) in "continuous": ', paste(unique(invalid_continuous), collapse = ", "),
                '. Allowed values are "mediqr" and "meansd".'))
  }


  if(!pval) print_test <- FALSE #Can't print the test is there is no pvalue

  set.seed(seed)
  include_overall <- match.arg(include_overall)
  op <- options(warn = -1)
  on.exit(options(op))

  group <- rlang::enquo(group)
  var_name <- rlang::enquo(var_name)
  var_desp <- rlang::enquo(var_desp)

  if (rlang::quo_is_missing(var_name)) var_name <- quo(var_name)
  if (rlang::quo_is_missing(var_desp)) var_desp <- quo(var_desp)

  if (rlang::quo_is_missing(group)) {  #If there is a grouping variable

    summary <- table_one_overall(df,total = total)
    pval <- FALSE
    print_test  <- FALSE

  }else{ #If there is not a grouping variable

  if(include_overall == "none") { #When only reporting the grouping variable

    summary <- table_one_stratify(df,group = !!group,total = total)

  } else if(include_overall == "group") { #Including the overall total but only when the group is not missing

      df_group <- df %>% filter(!is.na(!!group)) %>% select(-!!group)
      summary_full <- table_one_overall(df_group,total = total)
      summary_group <- table_one_stratify(df,group = !!group,total = total)
      summary <- summary_full %>% left_join(summary_group,by = c('row_id','variable','type'))

  } else if(include_overall == "all") { #Including the overall total for all patients


    summary_full <- table_one_overall(df %>% select(-!!group),total = total)
    summary_group <- table_one_stratify(df,group = !!group,total = total)
    summary <- summary_full %>% left_join(summary_group,by = c('row_id','variable','type'))

  }
if(!pval) summary$pval <- NULL
if(!print_test ) summary$print_test  <- NULL
}
  #Optionally removing the continuous variables
  if(!"meansd" %in% (continuous)) summary <- summary %>% filter(!grepl("_meansd$", row_id))
  if(!"mediqr" %in% (continuous)) summary <- summary %>% filter(!grepl("_mediqr$", row_id))


  if (is.null(datadic)) {
    out <- summary %>%
      dplyr::select(row_id, variable, type,
                    ends_with("n"), ends_with("stat"), everything()) %>%
      # dplyr::select(-!!var_desp) %>%
      mutate(type = ifelse(is.na(type) & row_id==variable,
                          gsub("(^[[:lower:]])", "\\U\\1", variable, perl=TRUE), type),
             type = ifelse(type %in% c("meansd", "mediqr"),
                          gsub("(^[[:lower:]])", "\\U\\1", variable, perl=TRUE), type),
             type = ifelse(row_id==paste0(variable, "TRUE"),
                          gsub("(^[[:lower:]])", "\\U\\1", variable, perl=TRUE), type)) %>%
      rename(`var_desp`= type)
  } else {
    out <- summary %>%
      left_join(dplyr::select(datadic, !!var_name, !!var_desp),
                by = c("variable"= quo_name(var_name))) %>%
      mutate(type = ifelse(is.na(type) & row_id==variable, !!var_desp, type), # factor
             type = ifelse(type %in% c("meansd", "mediqr"), !!var_desp, type), # continuous
             type = ifelse(row_id==paste0(variable, "TRUE"), !!var_desp, type), # logical
             ) %>%
      dplyr::select(row_id, variable, type,
                    ends_with("n"), ends_with("stat"), everything()) %>%
      dplyr::select(-!!var_desp) %>%
      rename(`var_desp`= type)
  }

  if(kable_output){

    #Getting the rows to indent
    indent <-  out %>% filter(row_id != "Total_N") %>%
      mutate(row_number = row_number()) %>%
      select(matches("_n$"),row_number)  %>%
      filter(rowSums(is.na(.)) == (ncol(.)-1)) %>%
      pull(row_number)


    first_row <- out %>% head(1)  %>%
      select(ends_with("_n"))

    variable_names <- gsub("_n", "", names(first_row))
    n_columns <- paste0(variable_names, "_n")
    stat_columns <- paste0(variable_names, "_stat")

    headers <- if(total){
      paste0(variable_names," (N = ",first_row,")")} else{
        variable_names
      }

    out <- out %>%
      filter(!(row_number() == 1 & total == TRUE)) %>%
      select(all_of(c("var_desp", c(rbind(n_columns, stat_columns)), if (pval) "pval" else NULL,if (print_test ) "test" else NULL))) %>%
      kableExtra::kbl(caption = caption,
                   booktabs=TRUE,
                   escape = FALSE,
                   align= c('l', rep(c('c', 'c'), length(headers)), 'r'),
                   col.names = c('Variables', rep(c('N', 'Stat'), length(headers)), if (pval) '*P*-value' else character(0) , if (print_test ) 'Statistical test' else character(0))) %>%
      kableExtra::row_spec(row = 0, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                full_width = FALSE)%>%
      kableExtra::add_header_above(c("", setNames(rep(2, length(headers)), headers), if (pval) '' else character(0), if (print_test ) '' else character(0)))%>%
      kableExtra::add_indent(indent)


  }
 out

}

