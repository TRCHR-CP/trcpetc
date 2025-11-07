



#' @title table_one
#'
#' @description
#' Generates a table of summary statistics for descriptive analysis.
#'
#' @details
#' The \code{table_one} function computes summary statistics for continuous, logical, and factor variables,
#' following the statistical reporting guidelines of the \emph{Annals of Medicine}. If a grouping variable is provided,
#' the function can also evaluate between-group differences. The input data frame should consist only of numeric,
#' logical, and factor variables. Factor variables with only two levels should be converted to logical variables.
#' Date and datetime variables should be excluded.
#'
#' @param df A data frame consisting of numeric, logical, and factor variables with or without a grouping variable.
#' @param group Name of the grouping variable (optional).
#' @param datadic A data frame serving as a data dictionary, containing variable names and their descriptions.
#' @param var_name The column name of \code{datadic} that contains the variable names. Only required if the column name is not \code{"var_name"}.
#' @param var_desp The column name of \code{datadic} that contains the variable descriptions. Only required if the column name is not \code{"var_desp"}.
#' @param seed An optional seed value for reproducibility of p-values.
#' @param include_overall Character string specifying whether and how to include an overall summary.
#'   Must be one of:
#'   \itemize{
#'     \item \code{"none"}: Do not include an overall summary (default).
#'     \item \code{"group"}: Include an overall summary only for observations with non-missing values in the grouping variable.
#'     \item \code{"all"}: Include an overall summary for all observations, regardless of missingness in the grouping variable.
#'   }
#'   Default is \code{"none"}.
#' @param total Logical; whether to report the total N. Default is \code{TRUE}.
#' @param pval Logical; whether to report p-values for between-group comparisons. Default is \code{TRUE}.
#' @param print_test Logical. If \code{TRUE}, the output will include the type of statistical test applied to each variable. Default is \code{FALSE}.
#' @param continuous Character string specifying the summary statistics for continuous variables.
#'   Must be one of:
#'   \itemize{
#'     \item \code{"mediqr"}: Median and interquartile range.
#'     \item \code{"meansd"}: Mean and standard deviation.
#'     \item \code{c("mediqr","meansd")}: Both median/IQR and mean/SD.
#'   }
#' @param round_to_100 Logical; force rounded total to add up to 100 using the largest remainder method for factor variables.
#' @param drop.unused.levels Logical; removes factor levels with zero counts. Levels with zero are not included in statistical tests.
#' @param kable_output Logical; if \code{TRUE}, outputs a formatted \code{kable} table including variable descriptions, N, statistics, and p-values.
#' @param caption Optional character string for the table caption.
#' @param overall_label Character string to label the overall summary column. Default is \code{"Overall"}.
#' @param include_Missing Logical; whether to include missing value counts in the summary. Default is \code{FALSE}.
#' @param Check_box Optional character vector of variable names from a checkbox-style question. In the output table, these variables will be displayed together as levels of a single item, but each level will be analyzed independently with its own statistical test.
#' @param Check_box_title Optional character string to identify the checkbox column titles.
#' @param print_unused Logical; whether to print variables that were excluded due to unsupported types. Default is \code{FALSE}.
#' @param bold_variables Logical; whether to bold variable names in the output table. Default is \code{TRUE}.
#' @param full_width Logical; passed to \code{kableExtra::kable_styling()} to control table width.
#'
#' @return A data frame containing summary statistics by variable type, optionally stratified by group and formatted for reporting, or a formatted \code{kable} table if \code{kable_output = TRUE}.
#'
#' @examples


#' library(dplyr)
#'
#' ## Pulling out the checkbox questions (More than one may be applicable for the same header)
#' Comorbidities  <- cardio_data %>% select(Diabetes:CAD) %>% names()
#'
#'## Cleaning the data by putting SurgeryType in descending order
#'## Converting the Comorbidities columns to checkbox and including a title.
#'
#'work_d <- cardio_data %>%
#'  mutate(SurgeryType = factor_order(SurgeryType)) %>%
#'  check_box_convert(check_box_cols = Comorbidities,title = "Comorbidities¹")
#'
#'
#'table_one(df = work_d  ,
#'          group = Sex,
#'          caption =  "Summary table overall and stratified by sex",
#'          include_overall = "all",
#'          overall_label = "Overall",
#'          drop.unused.levels = FALSE,
#'          round_to_100 = TRUE,
#'          kable_output = TRUE,
#'          include_Missing = FALSE,
#'          print_unused = TRUE,
#'          Check_box = Comorbidities,
#'          Check_box_title = "Comorbidities¹")%>%
#'  kableExtra::footnote(
#'    general = "¹Patients could present with more than one comorbidity, totals may not sum to 100%.",
#'    general_title = "",
#'    footnote_as_chunk = TRUE
#'  )
#' @export
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#'
table_one <- function(df, group, datadic = NULL, var_name, var_desp, seed = 123, include_overall  = c("none","group","all"),
                      total = TRUE,pval=TRUE,print_test  = FALSE,continuous = "mediqr",round_to_100 = FALSE,
                      drop.unused.levels = FALSE,
                      kable_output =TRUE,caption = NULL,overall_label = "Overall",include_Missing = FALSE,
                      Check_box = NULL,Check_box_title = NULL,print_unused = FALSE, bold_variables = TRUE, full_width = NULL) {

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
  if((!rlang::quo_is_missing(group) & include_overall %in% c("group","all"))){
    if(overall_label %in% (df %>% dplyr::pull(!!group) %>% unique() %>% stats::na.omit())){
      stop(paste0("`overall_label` ('", overall_label, "') cannot match an existing level in the grouping variable `"))
    }

  }

  if(print_unused){

    print(paste(
      "Unused columns include:",
      paste0(
        df %>%
          dplyr::ungroup() %>%
          dplyr::select(where(~ is.character(.) || lubridate::is.Date(.))) %>%
          colnames(),
        collapse = ", "
      )))

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
        dplyr::left_join((summary_group %>% dplyr::select(row_id,pval)),by = c('row_id'),suffix = c(".Missing",".No.Missing"))

    }

  }


  # Overall summary table -------------------------------------------------------------------------

  ##If include overall = "group" remove rows where the grouping variable is missing
  if(!rlang::quo_is_missing(group) & include_overall == "group"){
    df <- df %>% dplyr::filter(!is.na(!!group))
  }


  ##Remove the grouping variable from the overall table
  if(!rlang::quo_is_missing(group)){
    df <- df %>% dplyr::select(-!!group)
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
      ~ dplyr::if_else(variable %in% Check_box, NA_character_, .)
    ))

  ##Formatting title rows to
  out <- out   %>%
    dplyr::mutate(dplyr::across(
      dplyr::matches("(_stat$|^pval$)"),
      ~ dplyr::if_else(variable %in% Check_box_title, NA_character_, .)
    ))




  # Creating a kable table -------------------------------------------------------------------------



  if(kable_output){

    out <-  kable_table_one(out,pval = pval,include_Missing = include_Missing,print_test = print_test,total=total,caption=caption,bold_variables=bold_variables,full_width=full_width)

  }

  out

}

