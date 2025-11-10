
#' @title titles_non_missing
#' @description Adds a grouping title column to indicate whether any non-missing values are present across a set of specified columns.
#' @details
#' This function creates a new logical column that flags rows where at least one of the specified columns contains a non-missing value. The new column is placed immediately before the first column in the specified set. This is useful for visually grouping related variables (e.g., checkbox-style questions) in summary tables.

#' @param df A data frame containing the variables to be grouped.
#' @param columns Character vector of column names to check for non-missing values.
#' @param new_col_name Optional name for the new grouping column. Default is `"Title"`.
#'
#' @return A data frame with an added logical column indicating presence of non-missing values across the specified columns, relocated before the first column in the group.
#' @importFrom data.table :=

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


#' @title check box convert
#' @description Converts checkbox-style variables into logical format for inclusion in a Table 1. Ensures that checkbox responses are treated as binary indicators and handles missingness appropriately. To be used with \code{\link{table_one}}
#' @details
#' This function prepares checkbox-style questions (i.e., multiple binary columns representing selections) for descriptive analysis. It converts specified columns to logical type and sets all values to `NA` for rows where none of the checkbox options were selected. Optionally, it adds a title column to group these variables visually in the output table.
#' @param df A data frame containing the checkbox-style variables.
#' @param check_box_cols Character vector of column names representing the checkbox-style question. The columns must be either logical or 0/1
#' @param title Optional character string used to label the checkbox group in the output table.
#'
#' @return A modified data frame with checkbox columns converted to logical type and missingness handled. If `title` is provided, an additional column is added to group the checkbox variables.
#' @export

#' @examples
#'
#' library(dplyr)
#'
#' Comorbidities  <- cardio_data %>% select(Diabetes:NoComorbidities) %>% names()
#'
#' ## Without checkbox question (Assuming missing all questions is FALSE)
#' cardio_data %>% select(Comorbidities) %>%
#'  table_one()

#' # Example with one checkbox question (Assumes missing all questions is missing)
#'
#' cardio_data %>% select(Comorbidities) %>%
#'  check_box_convert(check_box_cols = Comorbidities,title = "Comorbidities¹")  %>%
#'  table_one(Check_box = Comorbidities,
#'            Check_box_title = "Comorbidities¹")%>%
#'  kableExtra::footnote(
#'    general = "¹Patients could present with more than one comorbidity, totals may not sum to 100%.",
#'    general_title = "",
#'    footnote_as_chunk = TRUE
#'  )
#'
#'
#' ## Example with two different checkbox questions
#'
#' Comorbidities1  <- cardio_data %>% select(Diabetes:COPD) %>% names()
#' Comorbidities2  <- cardio_data %>% select(CKD:CAD) %>% names()
#'
#'
#'
#' cardio_data %>% select(Comorbidities1,Comorbidities2) %>%
#'  check_box_convert(check_box_cols = Comorbidities1,title = "Comorbidities1¹")  %>%
#'  check_box_convert(check_box_cols = Comorbidities2,title = "Comorbidities2¹")  %>%
#'  table_one(Check_box = Comorbidities,
#'            Check_box_title = c("Comorbidities¹","Comorbidities2¹"))%>%
#'  kableExtra::footnote(
#'    general = "¹Patients could present with more than one comorbidity, totals may not sum to 100%.",
#'    general_title = "",
#'    footnote_as_chunk = TRUE
#'  )
#'
#'
#' @export

check_box_convert <- function(df, check_box_cols, title = NULL) {


  out <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(check_box_cols), ~ as.logical(.x))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(check_box_cols), ~ {
      if (all(c_across(dplyr::all_of(check_box_cols)) == FALSE, na.rm = TRUE)) NA else .x
    })) %>%
    dplyr::ungroup()

  if (!is.null(title)) {
    out <- titles_non_missing(out, columns = check_box_cols, new_col_name = !!title)
  }



  return(out)
}



# Factor order -------------------------------------------------------------------------

#---- Order factor variables ----
#' @title Order a factor variable by descending frequency.
#' @description  Order a factor variable by descending frequency. To be used with \code{\link{table_one}}
#' @details Automatically orders the levels of a factor variable by descending frequency, improving clarity in summary tables
#'
#' @param var A factor or character vector to be reordered.
#' @return A factor vector with levels sorted in descending order of frequency.
#' @examples
#'
#'
#' library(dplyr)
#'
# Without factor_order
#'cardio_data %>% select(SurgeryType) %>% table_one()
#'
# With factor_order
#'cardio_data %>%
#'  mutate(SurgeryType = factor_order(SurgeryType)) %>% select(SurgeryType) %>% table_one()
#'
#' @export
factor_order <- function(var){
  factor(var, levels = names(sort(table(var), decreasing = TRUE)))
}
