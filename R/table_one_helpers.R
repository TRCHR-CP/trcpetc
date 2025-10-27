#' @title table_one_overall
#' @description table one with no stratifying variable
#' @keywords internal
#' @export
#'

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

#' @title table_one_stratify
#' @description table one with a stratifying variable
#' @keywords internal
#' @export
#'
table_one_stratify <- function(df,group,total = TRUE,round_to_100 = FALSE,drop.unused.levels = FALSE){

  group <- rlang::enquo(group)

  if(drop.unused.levels) df <- df  %>%  dplyr::ungroup() %>% dplyr::mutate_if(is.factor, droplevels)


  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select_if(Negate(is.character)) %>%
    dplyr::select_if(Negate(lubridate::is.Date)) %>%
    dplyr::filter(!is.na(!!group)) %>%
    dplyr::group_by(!!group)



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
      dplyr::select(variable, dplyr::everything()) %>%
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


#' @title kable_table_one
#' @description creates a kable table one for rmarkdown reports
#' @keywords internal
#' @export
#'

kable_table_one <- function(out,pval,include_Missing,total,print_test,caption){
  indent <-  out %>% dplyr::filter(row_id != "Total_N") %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::select(dplyr::matches("_n$"),row_number)  %>%
    dplyr::filter(rowSums(is.na(.)) == (ncol(.)-1)) %>%
    dplyr::pull(row_number)


  first_row <- out %>% utils::head(1)  %>%
    dplyr::select(dplyr::ends_with("_n"))

  variable_names <- gsub("_n", "", names(first_row))
  n_columns <- paste0(variable_names, "_n")
  stat_columns <- paste0(variable_names, "_stat")

  headers <- if(total){
    paste0(variable_names," (N = ",first_row,")")} else{
      variable_names
    }

  out <- out %>%
    dplyr::filter(!(dplyr::row_number() == 1 & total == TRUE)) %>%
    dplyr::select(
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
factor_dist <- function(table_obj, col_var, pct_digits= 1, removeNA= TRUE) {
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

  group <- rlang::enquo(group)
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

  group <- rlang::enquo(group)

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

  group<- rlang::enquo(group)

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
