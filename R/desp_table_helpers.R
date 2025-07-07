#' @keywords internal

table_one_overall <- function(df,total = TRUE,round_to_100 = FALSE){

  df <- df %>%
    ungroup() %>%
    select_if(Negate(is.character)) %>%
    select_if(Negate(is.Date)) %>%
    as.data.frame() %>%
    mutate_if(is.factor, droplevels) %>%
    as_tibble()


  num_out_lst <- if (any(sapply(df, class) %in% c("numeric", "integer"))) {
    numeric_desp(df) %>%
      rownames_to_column("row_id") %>%
      mutate(row_id= paste(variable, type, sep= "_")) %>%
      split(., .$variable)
  } else NULL

  fct_out_lst <- if (any(sapply(df, class)=="factor")) {
    factor_desp(df,round_to_100 = round_to_100) %>%
      rownames_to_column("row_id") %>%
      rename(type= level) %>%
      mutate(row_id= ifelse(type!= "." & !is.na(type),
                            paste(variable, gsub("\\ ", "_", trimws(type)), sep= "_"),
                            variable)) %>%
      split(., .$variable)
  } else NULL

  logic_out_lst <- if (any(sapply(df, class)=="logical")) {
    logical_desp(df) %>%
      rownames_to_column("row_id") %>%
      mutate(row_id= paste0(variable, "TRUE")) %>%
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

  Output <- out_lst[c("N",names(df))] %>% bind_rows() %>% rename(Overall_n = n,Overall_stat = stat)



  return(Output)

}

#' @keywords internal

table_one_stratify <- function(df,group,total = TRUE,round_to_100 = FALSE){

  group <- rlang::enquo(group)

  df <- df %>%
    ungroup() %>%
    select_if(Negate(is.character)) %>%
    select_if(Negate(is.Date)) %>%
    mutate_if(is.factor, droplevels) %>%
    filter(!is.na(!!group)) %>%
    group_by(!!group)

  group_var_idx <- match(group_vars(df), names(df))


  num_out_lst <- if (any(sapply(df[-group_var_idx], class) %in% c("numeric", "integer"))) {
    numeric_desp(df, !!group) %>%
      rownames_to_column("row_id") %>%
      mutate(row_id= paste(variable, type, sep= "_")) %>%
      split(., .$variable)
  } else NULL

  fct_out_lst <- if (any(sapply(df[-group_var_idx], class)=="factor")) {
    factor_desp(df, !!group,round_to_100 = round_to_100) %>%
      rownames_to_column("row_id") %>%
      rename(type= level) %>%
      mutate(row_id= ifelse(type!= "." & !is.na(type),
                            paste(variable, gsub("\\ ", "_", trimws(type)), sep= "_"),
                            variable)) %>%
      split(., .$variable)
  } else NULL

  logic_out_lst <- if (any(sapply(df[-group_var_idx], class)=="logical")) {
    logical_desp(df, !!group) %>%
      rownames_to_column("row_id") %>%
      mutate(row_id= paste0(variable, "TRUE")) %>%
      split(., .$variable)
  } else NULL

  Total_N <- if (total) {

    n_var <- df %>%
      count(!!group) %>%
      tidyr::pivot_wider(names_from = !!group, values_from = n, values_fill = 0) %>%
      mutate(variable = "Total N") %>%
      select(variable, everything()) %>%
      mutate(across(where(is.integer), as.character))

    list(N = left_join(n_var, n_var, by= "variable", suffix= c("_n", "_stat")) %>%
      mutate(row_id = "Total_N", type = as.character(NA), pval = as.character(NA), test = as.character(NA)) %>%
      dplyr::select(row_id,variable, type, everything()) %>%
      mutate(type= NA_character_))

  } else NULL

  out_lst <-
    Total_N %>%
    append(num_out_lst) %>%
    append(fct_out_lst) %>%
    append(logic_out_lst)

  Output <- out_lst[c("N",names(df))] %>% bind_rows()



  return(Output)

}


