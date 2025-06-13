

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
logical_desp<- function(df, group) {

  binary_desp<- function(x, pct_digits= 1) {
    fun<- c(sum, mean)
    out<- sapply(fun,
                 function(f) {
                   res<- try(f(x, na.rm= TRUE), silent = TRUE)
                   res<- if (class(res)== "try-error") NA else res
                   return(res)
                 })

    freq<- formatC(out[1], format= "d", big.mark = ",")
    pct <- formatC(out[2]*100, digits= pct_digits, format= "f")
    pct <- paste0(pct, "%")
    out <- paste0(freq, " (", pct, ")")
    out
  }

  group<- rlang::enquo(group)
  df<- df %>%
    ungroup()

  # fisher exact test
  # test<- try(fisher.test(freq, hybrid = TRUE, conf.int = FALSE), silent = TRUE)
  # test<- if (class(test)=="try-error") NA else test$p.value
  test_fun<- fisher_test

  if (rlang::quo_is_missing(group)) {

    sum_stat<- df %>%
      summarise_if(is.logical, funs(binary_desp),
                   pct_digits= if (nrow(.)>= 200) 1 else 0) %>%
      rownames_to_column() %>%
      melt(id.vars= "rowname", value.name = "stat") %>%
      mutate(type= "freq") %>%
      dplyr::select(variable, type, stat)

    n_var<- df %>%
      summarise_if(is.logical, funs(n_avail)) %>%
      rownames_to_column() %>%
      melt(id.vars= "rowname", value.name = "n") %>%
      dplyr::select(-rowname)

  } else {

    df<- df %>%
      # ungroup() %>%
      group_by(!!group)

    sum_stat<- df %>%
      summarise_if(is.logical, funs(binary_desp),
                   pct_digits= if (nrow(.)>= 200) 1 else 0) %>%
      melt(id.vars= quo_name(group), factorsAsStrings= TRUE) %>%
      dcast(as.formula(paste("variable", quo_name(group), sep= " ~ "))) %>%
      mutate(type= "freq") %>%
      # left_join(test_fun(df, rlang::UQ(group)), by= c("variable", "type"))
      left_join(test_fun(df, !!group), by= c("variable", "type"))

    n_var<- df %>%
      summarise_if(is.logical, funs(n_avail)) %>%
      melt(id.vars= quo_name(group), factorsAsStrings= TRUE) %>%
      dcast(as.formula(paste("variable", quo_name(group), sep= " ~ ")))
  }

  n_var<- n_var %>%
    mutate_all(as.character)

  sum_stat<- sum_stat %>%
    mutate_all(as.character)

  left_join(n_var, sum_stat, by= "variable", suffix= c("_n", "_stat")) %>%
    dplyr::select(variable, type, everything()) %>%
    mutate(type= NA_character_) %>%
    arrange(variable, type)
}


#' @title fisher_test
#'
#' @details
#' An internal function that applies fisher exact tests to assess the between-group differences in logical variables
#'
#' @param df Dataframe
#' @return a dataframe of a single column of character variables indicating p-values.
fisher_test<- function(df, group) {

  group<- enquo(group)
  df<- df %>%
    ungroup() %>%
    group_by(!!group)

  # fisher_test<- function(...) try(fisher.test(..., hybrid = TRUE, conf.int= FALSE), silent = TRUE)
  fisher_test<- if (n_groups(df)==2) {
    function(...) try(fisher.test(..., conf.int= FALSE), silent = TRUE)
  } else {
    function(...) try(fisher.test(..., hybrid = TRUE, conf.int= FALSE), silent = TRUE)
  }

  df %>%
    select_if(is.logical) %>%
    melt(id.vars= quo_name(group),
         factorsAsStrings= TRUE,
         na.rm= TRUE) %>%
    group_by(variable) %>%
    nest() %>%
    mutate(freq= map(data,
                     function(df) {

                       res<- df %>%
                         table() %>%
                         fisher_test()

                       # res<- if (class(res)=="try-error") NA else res$p.value
                       res<- if (class(res)=="try-error") {
                         fisher_test<- if (n_groups(df)==2) {
                           function(...) try(fisher.test(..., conf.int= FALSE, simulate.p.value = TRUE, B=5000), silent = TRUE)
                         } else {
                           function(...) try(fisher.test(..., hybrid = TRUE, conf.int= FALSE, simulate.p.value = TRUE, B=5000), silent = TRUE)
                         }
                         res_try<- df %>%
                           table() %>%
                           fisher_test()
                         if (class(res_try)=="try-error") NA else res_try$p.value
                       } else res$p.value
                       res
                     })) %>%
    dplyr::select(-data) %>%
    unnest() %>%
    melt(id.vars= "variable", variable.name= "type", value.name = "pval") %>%
    mutate(pval= format_pvalue(pval)) %>%
    mutate_all(as.character)

  #   variable  type       pval
  # 1       dm  freq 0.04608804
  # 2       af  freq 0.19948767
}


