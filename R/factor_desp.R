#---- summarize factor variables ----
#' @title factor_desp
#'
#' @details An internal function that calculate frequencies and proportion of levels in factor/categorical variables.
#' If there are more than 1 group, then fisher exact tests are applied to assess the between-group differences.
#'
#' @param df Dataframe
#' @return a dataframe consisting of columns of character variables indicating the frequency and proportion of logical variables.
#' @export
factor_desp<- function(df, group, includeNA= FALSE) {

  ##
  make_univariate_fml<- function(x, y= NULL) {
    sapply(x, function(x){
      if (is.null(y)) formula(paste0("~", x)) else formula(paste0(y, "~", x))
    })
  }

  make_bivariate_fml<- function(x, z, y= NULL) {
    sapply(x, function(x){
      if (is.null(y)) formula(paste0("~", x, " + ", z)) else formula(paste0(y, "~", x, " + ", z))
    })
  }

  output_one_way_tbl<- function(freq, pct_digits= 1) {
    pct  <- prop.table(freq)
    var_name<- names(dimnames(freq))
    freq<- freq %>%
      addmargins() %>%
      as.data.frame(row.names= names(dimnames(.)),
                    responseName = "freq",
                    stringsAsFactors = FALSE) %>%
      rownames_to_column("level") %>%
      mutate(n   = ifelse(level=="Sum", freq, NA),
             freq= ifelse(level=="Sum", NA, freq),
             n   = ifelse(!is.na(n), formatC(n, format= "d", big.mark = ","), NA_character_),
             freq= ifelse(!is.na(freq), formatC(freq, format= "d", big.mark = ","), NA_character_)
      ) %>%
      dplyr::select(level, n, freq)

    pct<- pct %>%
      as.data.frame(row.names= names(dimnames(.)),
                    responseName = "pct",
                    stringsAsFactors = FALSE) %>%
      rownames_to_column("level") %>%
      mutate(pct= formatC(pct*100, digits= pct_digits, format= "f")) %>%
      dplyr::select(level, pct)

    freq<- freq %>%
      mutate_all(as.character)

    pct<- pct %>%
      mutate_all(as.character)

    out<- full_join(freq, pct, by = c("level")) %>%
      mutate(stat= paste0(freq, " (", pct, "%)"),
             stat= ifelse(level=="Sum", NA_character_, stat),
             level= ifelse(level=="Sum", ".", level)) %>%
      dplyr::select(level, n, stat) %>%
      mutate_all(as.character)

    out[match(c('.', levels(df[[var_name]])), out$level),]
  }

  output_two_way_tbl<- function(freq, pct_digits= 1) {
    tbl_var_name<- names(dimnames(freq))
    pct  <- prop.table(freq, margin = 2)

    # fisher exact test
    test<- try(fisher.test(freq, hybrid = TRUE, conf.int = FALSE, simulate.p.value= TRUE, B= 9999), silent = TRUE)
    test<- if (class(test)=="try-error") NA else test$p.value

    # total
    total<- margin.table(freq, 2) %>%
      as.data.frame(responseName = "n", stringsAsFactors = FALSE) %>%
      mutate(n= ifelse(!is.na(n), formatC(n, format= "d", big.mark = ","), NA_character_)) %>%
      dcast(as.formula( paste0(". ~ ", tbl_var_name[2])), value.var = "n") %>%
      bind_cols(pval= format_pvalue(test))

    freq<- freq %>%
      as.data.frame(responseName = "freq", stringsAsFactors = FALSE) %>%
      mutate(freq= ifelse(!is.na(freq), formatC(freq, format= "d", big.mark = ","), NA_character_))

    pct<- pct %>%
      as.data.frame(responseName = "pct", stringsAsFactors = FALSE) %>%
      mutate(pct= formatC(pct*100, digits= pct_digits, format= "f"))

    freq<- freq %>%
      mutate_all(as.character)

    pct<- pct %>%
      mutate_all(as.character)

    out<- full_join(freq, pct, by= tbl_var_name) %>%
      mutate(stat= paste0(freq, " (", pct, "%)")) %>%
      # bind_rows(total) %>%
      dplyr::select(-freq, -pct) %>%
      dcast(as.formula(paste0(tbl_var_name, collapse= " ~ ")),
            value.var = "stat") %>%
      mutate_all(as.character)

    names(out)[1]<- names(total)[1]<- "level"
    out<- full_join(total, out, by= "level", suffix= c("_n", "_stat"))
    out[match(c('.', levels(df[[tbl_var_name[1]]])), out$level),]
    # out
  }
  ##


  group<- rlang::enquo(group)

  # 1 - select variables of factor class
  if (rlang::quo_is_missing(group)) {
    df<- df %>%
      ungroup() %>%
      select_if(is.factor)

    fml<- make_univariate_fml(names(df))
    # 2 - create table object for ALL selected factor variables (not sure if it is a good idea but ...)
    # here we are going to drop unused levels (drop.unused.levels = TRUE)
    tbl_list<- lapply(fml, xtabs, data= df, addNA= includeNA, drop.unused.levels = TRUE)
    out     <- lapply(tbl_list, output_one_way_tbl, pct_digits = if (nrow(df)< 200) 0 else 1)

  } else {
    df<- df %>%
      group_by(!!group) %>%
      select_if(is.factor)

    fml<- make_bivariate_fml(grep(paste0("^", quo_name(group), "$"), names(df), value= TRUE, invert = TRUE),
                             z= quo_name(group))

    # 2 - create table object for ALL selected factor variables (not sure if it is a good idea but ...)
    # here we are going to drop unused levels (drop.unused.levels = TRUE)
    tbl_list<- lapply(fml, xtabs, data= df, addNA= includeNA, drop.unused.levels = TRUE)
    out     <- lapply(tbl_list, output_two_way_tbl, pct_digits = if (nrow(df)< 200) 0 else 1)
  }

  out<- out %>%
    bind_rows(.id= "variable") %>%
    mutate(level= ifelse(level==".", NA_character_, level))
  out
  # 2 - create table object for ALL selected factor variables (not sure if it is a good idea but ...)
  # here we are going to drop unused levels (drop.unused.levels = TRUE)
  # table_obj<- xtabs( ~ ., data= df, addNA= TRUE, drop.unused.levels = TRUE)

  # if (rlang::quo_is_missing(group)) {
  #   factor_dist(table_obj = table_obj,
  #               pct_digits = if (nrow(df)< 200) 0 else 1)
  # } else {
  #   factor_dist(table_obj = table_obj,
  #               col_var = rlang::UQ(group),
  #               pct_digits = if (nrow(df)< 200) 0 else 1)
  # }
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
                     addmargins() %>%
                     as.data.frame(row.names= names(dimnames(.)),
                                   responseName = "freq",
                                   stringsAsFactors = FALSE) %>%
                     rownames_to_column("level") %>%
                     mutate(n   = ifelse(level=="Sum", freq, NA_integer_),
                            n   = ifelse(!is.na(n), formatC(n, format= "d", big.mark = ","), NA_character_),
                            freq= ifelse(level=="Sum", NA_integer_, freq),
                            freq= ifelse(!is.na(freq), formatC(freq, format= "d", big.mark = ","), NA_character_),
                     ) %>%
                     dplyr::select(level, n, freq)

                   pct<- pct %>%
                     as.data.frame(row.names= names(dimnames(.)),

                                   responseName = "pct",
                                   stringsAsFactors = FALSE) %>%
                     rownames_to_column("level") %>%
                     mutate(pct= formatC(pct*100, digits= pct_digits, format= "f")) %>%
                     dplyr::select(level, pct)

                   full_join(freq, pct, by = c("level")) %>%
                     mutate(stat= paste0(freq, " (", pct, "%)"),
                            stat= ifelse(level=="Sum", NA_character_, stat),
                            level= ifelse(level=="Sum", ".", level)) %>%
                     dplyr::select(level, n, stat) %>%
                     arrange(level)

                 }) %>%
      setNames(fct_var_name)

  } else {
    # column is the grouping variable
    col_idx <- grep(quo_name(col_var), fct_var_name)
    row_vars<- grep(quo_name(col_var), fct_var_name, invert = TRUE, value= TRUE)

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
                   test<- try(fisher.test(freq, hybrid = TRUE, conf.int = FALSE), silent = TRUE)
                   test<- if (class(test)=="try-error") NA else test$p.value

                   # total
                   total<- margin.table(freq, 2) %>%
                     as.data.frame(responseName = "n", stringsAsFactors = FALSE) %>%
                     mutate(n= formatC(n, format= "d", big.mark = ",")) %>%
                     dcast(as.formula( paste0(". ~ ", quo_name(col_var))), value.var = "n") %>%
                     bind_cols(pval= format_pvalue(test))


                   freq<- freq %>%
                     as.data.frame(responseName = "freq", stringsAsFactors = FALSE) %>%
                     mutate(freq= formatC(freq, format= "d", big.mark = ","))

                   pct<- pct %>%
                     as.data.frame(responseName = "pct", stringsAsFactors = FALSE) %>%
                     mutate(pct= formatC(pct*100, digits= pct_digits, format= "f"))

                   out<- full_join(freq, pct, by= c(x, quo_name(col_var))) %>%
                     mutate(stat= paste0(freq, " (", pct, "%)")) %>%
                     # bind_rows(total) %>%
                     dplyr::select(-freq, -pct) %>%
                     dcast(as.formula(paste(x, quo_name(col_var), sep= " ~ ")), value.var = "stat")

                   names(out)[1]<- names(total)[1]<- "level"
                   out<- full_join(total, out, by= "level", suffix= c("_n", "_stat"))
                   out
                 }) %>%
      setNames(row_vars)
  }
  out %>%
    bind_rows(.id= "variable") %>%
    mutate(level= ifelse(level==".", NA_character_, level))
}


