#---- this is the main function ----
#' @title table_one
#' @description Creates a table of summary statistics
#' @details
#' \code{table_one} calculate the selected summary statistics for continuous, logical,
#' and factor variables per statistical guidelines of the Annals of medicine. If a group variable is provided, then
#' it will also assess the between-group difference.The input data frame should only consists of numeric, logical
#' and factor variables. Factor variables with
#' only two levels should be converted to logical variables. Date and datetime variables should be removed.
#'
#' @param df A data frame consisting of numeric, logical, and factor variables with or without a grouping variable
#' @param group Name of the grouping variable.
#' @param datadic A data frame containing a data dictionary of variable names and their descriptions.
#' @param var_name the column name of `data_dict` that
#'   contains the variable names. Only required if the column name is not "var_name"
#' @param var_desp the column name of `data_dict` that
#'   contains the variable descriptions Only required if the column name is not "var_desp"
#' @param seed Sets a seed
#' @param include_overall Character string specifying whether and how to include an overall summary.
#' @param total Logical variable to report total N. Default is TRUE
#'   Must be one of:
#'   \itemize{
#'     \item `"none"`: Do not include an overall summary.
#'     \item `"group"`: Include an overall summary only for observations with non-missing values in the grouping variable.
#'     \item `"all"`: Include an overall summary for all observations, regardless of missingness in the grouping variable.
#'   }
#'   Default is `"none"`.
#' @param  pval Option to report p-value
#' @param  continuous Select which continuous descriptors to include using "mediqr" for the median with interquartile range and "meansd" for the mean and standard deviation. Can also put c("mediqr","meansd") for both
#' @param  kable_output Outputs table as a formatted kable table and includes the column var_desp, N, Stat and p-value
#' @return The function returns a dataframe, rows of which are summary statistics depending on the variable types.
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
                      total = TRUE,pval=TRUE,continuous = "mediqr",kable_output=TRUE,caption = NULL) {


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

  }else{ #If there is not a grouping variable

  if(include_overall == "none") { #When only reporting the grouping variable

    summary <- table_one_stratify(df,group = !!group,total = total)

  } else if(include_overall == "group") { #Including the overall total but only when the group is not missing

      df_group <- df %>% filter(!is.na(!!group)) %>% select(-!!group)
      summary_full <- table_one_overall(df_group,total = total)
      summary_group <- table_one_stratify(df,group = !!group,total = total)
      summary <- summary_full %>% left_join(summary_group)

  } else if(include_overall == "all") { #Including the overall total for all patients


    summary_full <- table_one_overall(df %>% select(-!!group),total = total)
    summary_group <- table_one_stratify(df,group = !!group,total = total)
    summary <- summary_full %>% left_join(summary_group)

  }
if(!pval) summary$pval <- NULL

}
  #Optionally removing the continuous variables
  if(!"meansd" %in% (continuous)) summary <- summary %>% filter(!grepl("_meansd$", row_id))
  if(!"mediqr" %in% (continuous)) summary <- summary %>% filter(!grepl("_meansd$", row_id))


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

    indent <-

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
      select(all_of(c("var_desp", c(rbind(n_columns, stat_columns)), if (pval) "pval" else NULL))) %>%
      knitr::kable(caption = caption,
                   booktabs=TRUE,
                   escape = FALSE,
                   align= c('l', rep(c('c', 'c'), length(headers)), 'r'),
                   col.names = c('Variables', rep(c('N', 'Stat'), length(headers)), if (pval) '*P*-value' else character(0))) %>%
      kableExtra::row_spec(row = 0, align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                full_width = FALSE)%>%
      kableExtra::add_header_above(c("", setNames(rep(2, length(headers)), headers), if (pval) '' else character(0)))


  }
 out

}



table_one(df,sex,include_overall = "all",pval = FALSE,total = TRUE,caption = "Baseline demographics",
            continuous = c("mediqr","meansd"))
