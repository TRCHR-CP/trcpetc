#' @keywords internal
table_one_overall <- function(df,total = TRUE,round_to_100 = FALSE,drop.unused.levels = FALSE,overall_label = "Overall"){

  df <- df %>%
    dplyr::ungroup() %>%
    select_if(Negate(is.character)) %>%
    select_if(Negate(is.Date)) %>%
    as.data.frame() %>% as_tibble()

    if(drop.unused.levels) df <- df %>% dplyr::mutate_if(is.factor, droplevels)



  num_out_lst <- if (any(sapply(df, class) %in% c("numeric", "integer"))) {
    numeric_desp(df) %>%
      rownames_to_column("row_id") %>%
      dplyr::mutate(row_id= paste(variable, type, sep= "_")) %>%
      split(., .$variable)
  } else NULL

  fct_out_lst <- if (any(sapply(df, class)=="factor")) {
    factor_desp(df,round_to_100 = round_to_100,drop.unused.levels=drop.unused.levels) %>%
      rownames_to_column("row_id") %>%
      rename(type= level) %>%
      dplyr::mutate(row_id= ifelse(type!= "." & !is.na(type),
                            paste(variable, gsub("\\ ", "_", trimws(type)), sep= "_"),
                            variable)) %>%
      split(., .$variable)
  } else NULL

  logic_out_lst <- if (any(sapply(df, class)=="logical")) {
    logical_desp(df) %>%
      rownames_to_column("row_id") %>%
      dplyr::mutate(row_id= paste0(variable, "TRUE")) %>%
      split(., .$variable)
  } else NULL

  Total_N <- if (total) {

    N <- df %>%
      count() %>% unlist()

    list(N = data.frame(row_id = "Total_N",variable  = "Total N",type = as.character(NA), n = as.character(N),stat = as.character(N)))

  } else NULL

  out_lst <-
    Total_N %>%
    append(num_out_lst) %>%
    append(fct_out_lst) %>%
    append(logic_out_lst)

  Output <- out_lst[c("N",names(df))] %>% bind_rows() %>% rename(!!paste0(overall_label,"_n") := n, !!paste0(overall_label,"_stat") := stat)



  return(Output)

}

#' @keywords internal
table_one_stratify <- function(df,group,total = TRUE,round_to_100 = FALSE,drop.unused.levels = FALSE){

  group <- rlang::enquo(group)

  df <- df %>%
    dplyr::ungroup() %>%
    select_if(Negate(is.character)) %>%
    select_if(Negate(is.Date)) %>%
    filter(!is.na(!!group)) %>%
    group_by(!!group)

  if(drop.unused.levels) df <- df %>% dplyr::mutate_if(is.factor, droplevels)


  group_var_idx <- match(group_vars(df), names(df))


  num_out_lst <- if (any(sapply(df[-group_var_idx], class) %in% c("numeric", "integer"))) {
    numeric_desp(df, !!group) %>%
      rownames_to_column("row_id") %>%
      dplyr::mutate(row_id= paste(variable, type, sep= "_")) %>%
      split(., .$variable)
  } else NULL

  fct_out_lst <- if (any(sapply(df[-group_var_idx], class)=="factor")) {
    factor_desp(df,!!group,round_to_100 = round_to_100,drop.unused.levels=drop.unused.levels) %>%
      rownames_to_column("row_id") %>%
      rename(type= level) %>%
      dplyr::mutate(row_id= ifelse(type!= "." & !is.na(type),
                            paste(variable, gsub("\\ ", "_", trimws(type)), sep= "_"),
                            variable)) %>%
      split(., .$variable)
  } else NULL

  logic_out_lst <- if (any(sapply(df[-group_var_idx], class)=="logical")) {
    logical_desp(df, !!group) %>%
      rownames_to_column("row_id") %>%
      dplyr::mutate(row_id= paste0(variable, "TRUE")) %>%
      split(., .$variable)
  } else NULL

  Total_N <- if (total) {

    n_var <- df %>%
      count(!!group) %>%
      tidyr::pivot_wider(names_from = !!group, values_from = n, values_fill = 0) %>%
      dplyr::mutate(variable = "Total N") %>%
      select(variable, everything()) %>%
      dplyr::mutate(dplyr::across(where(is.integer), as.character))

    list(N = left_join(n_var, n_var, by= "variable", suffix= c("_n", "_stat")) %>%
      dplyr::mutate(row_id = "Total_N", type = as.character(NA), pval = as.character(NA), test = as.character(NA)) %>%
      dplyr::select(row_id,variable, type, everything()) %>%
      dplyr::mutate(type= NA_character_))

  } else NULL

  out_lst <-
    Total_N %>%
    append(num_out_lst) %>%
    append(fct_out_lst) %>%
    append(logic_out_lst)

  Output <- out_lst[c("N",names(df))] %>% bind_rows()



  return(Output)

}

kable_table_one <- function(out,pval,include_Missing,total,print_test,caption){
  indent <-  out %>% filter(row_id != "Total_N") %>%
  dplyr::mutate(row_number = row_number()) %>%
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
  kableExtra::add_header_above(c("", setNames(rep(2, length(headers)), headers), if (pval & !include_Missing) '' else character(0),if(pval & include_Missing) setNames(rep(2, 1), "*P*-value") else character(0), if (print_test ) '' else character(0)))%>%
  kableExtra::add_indent(indent)
}

#' @title titles_non_missing
#' @description Add a title to a dataframe
#' @export
#'
titles_non_missing <- function(df, columns, new_col_name = "Title") {
  df %>%
    dplyr::mutate(
      {{ new_col_name }} := if_else(
        rowSums(!is.na(dplyr::across(dplyr::all_of(columns)))) > 0,
        TRUE,
        NA
      )
    ) %>%
    relocate({{ new_col_name }}, .before = dplyr::all_of(columns[1]))
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


