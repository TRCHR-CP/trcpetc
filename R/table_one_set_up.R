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
