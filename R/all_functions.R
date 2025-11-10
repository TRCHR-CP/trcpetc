
#' @title summarize_coxph
#'
#' @details
#' The function summarizes the fitted cox model with the type 3 error based on Wald's statistics.
#'
#' @export
summarize_coxph <- function(mdl, exponentiate= TRUE, maxlabel= 100, alpha= 0.05) {

  if (!any(class(mdl) %in% c("coxph", "coxph.penal"))) stop("Not a coxph or coxph.penal object.")

  out<- summary(mdl, maxlabel= maxlabel)$coefficient %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term")

  if (any(class(mdl)== "coxph.penal")) {
    out<- dplyr::rename(out, se= 'se(coef)')
  } else if (all(class(mdl)== "coxph")) {
    out<- dplyr::rename(out, se= 'se(coef)', p= 'Pr(>|z|)')
    # names(out)[grep("^p", names(out), ignore.case = TRUE)]<- "p"
  }

  out<- out %>%
    dplyr::mutate(conf_low = coef - stats::qnorm(1-alpha/2) * se,
                  conf_high= coef + stats::qnorm(1-alpha/2) * se,
                  coef     = if (exponentiate) exp(coef) else coef,
                  conf_low = if (exponentiate) exp(conf_low) else conf_low,
                  conf_high= if (exponentiate) exp(conf_high) else conf_high,
                  stat= ifelse(is.na(coef), NA_character_,
                               paste0(formatC(coef, format= "f", digits= 3, flag= "#"), " [",
                                      formatC(conf_low, format= "f", digits= 3, flag= "#"), ", ",
                                      formatC(conf_high, format= "f", digits= 3, flag= "#"), "]")),
                  pval= format_pvalue(p)) %>%
    dplyr::select(dplyr::one_of(c("term", "stat", "pval")))

  type3_coxph<- function(mdl, beta_var= vcov(mdl)) {
    x<- stats::model.matrix(mdl)
    varseq <- attr(x, "assign")
    out<- lapply(unique(varseq),
                 function(i){
                   df<- sum(varseq==i)
                   # set out the contrast matrix
                   L<- matrix(0, nrow= df, ncol= ncol(x))
                   L[, varseq==i]<- diag(df)

                   #
                   vv<- L %*% beta_var %*% t(L)
                   cc<- L %*% coef(mdl)

                   # calculate Wald's test statistics and p-value
                   wald_stat<- as.numeric( t(cc) %*% solve(vv) %*% cc )
                   pval<- stats::pchisq(wald_stat,
                                 df= if (any(class(mdl)=="coxph.penal") && !is.na(mdl$df[i])) mdl$df[i] else df,
                                 lower.tail  = FALSE)

                   data.frame(df= round(df, 0), stat= wald_stat, chisq_p= pval)
                 })
    out<- do.call(rbind, out)
    # out<- cbind(variable= attr(mdl$terms, "term.labels"), out)

    # term_excld<- attr(mdl$terms, "response")
    # term_excld<- if (!is.null(attr(mdl$terms, "specials"))) c(term_excld, unlist(attr(mdl$terms, "specials")))
    # term_excld<- if (!is.null(attr(mdl$terms, "specials"))) c(term_excld, unlist(attr(mdl$terms, "specials")[c("strata", "cluster")]))
    # out<- cbind(variable= names(attr(mdl$terms, "dataClasses"))[-term_excld],
    #             out, stringsAsFactors= FALSE)
    # var_label<- attr(mdl$terms, "term.labels")[
    #   match(names(attr(mdl$terms, "dataClasses"))[-term_excld],
    #                   attr(mdl$terms, "term.labels"))
    #   ]
    var_label<- grep("(strata|cluster|tt|ridge|pspline|frailty)\\(.*\\)", attr(mdl$terms, "term.labels"), value = T, invert = T)
    out<- cbind(variable= var_label, out, stringsAsFactors= FALSE)
    out
  }

  type3_out<- type3_coxph(mdl)

  out<- type3_out %>%
     dplyr::filter(df> 1) %>%
    dplyr::mutate(pval= format_pvalue(chisq_p)) %>%
    dplyr::select(variable, pval) %>%
    dplyr::rename(term= variable) %>%
    dplyr::bind_rows(out) %>%
    dplyr::arrange(term) %>%
    dplyr::select(term, stat, pval)

  out
}

#' @title calculate_type3_mi
#'
#' @details
#' The function calculates the  3 p-values based on Wald's statistics.
#'
#' @export
calculate_type3_mi <- function(mira_obj, vcov_fun= NULL) {

  # to calulate the type 3 error
  # Li, Meng, Raghunathan and Rubin. Significance levels from repeated p-values with multiply-imputed data. Statistica Sinica (1991)
  # x<- model.matrix(mira_obj$analyses[[1]])
  x<- stats::model.matrix(tmp <- mice::getfit(mira_obj, 1L))
  varseq<- attr(x, "assign")
  df<- sapply(split(varseq, varseq), length)
  m <- length(mira_obj$analyses)

  # coef estimate and its vcov for each MI model
  betas <- mitools::MIextract(mira_obj$analyses, fun= coef)
  vars <- mitools::MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)

  # average betas and vcov cross MI mdls
  mean_betas <- purrr::reduce(betas, .f= `+`)/m
  with_var <- purrr::reduce(vars, .f= `+`)/m # with MI
  # between-MI vcov
  btwn_var <- lapply(betas, function(cc) (cc - mean_betas) %*% t(cc - mean_betas)) %>%
    purrr::reduce(.f= `+`)/(m-1)

  out<- lapply(unique(varseq),
               function(i){
                 df<- sum(varseq==i)
                 # set out the contrast matrix
                 L<- matrix(0, nrow= df, ncol= ncol(x))
                 L[, varseq==i]<- diag(df)

                 cc<- L %*% mean_betas
                 vv<- L %*% with_var %*% t(L) # with-mi vcov for beta
                 v2<- L %*% btwn_var %*% t(L) # btwn-mi vcov for beta

                 # calcualte Wald's test statistics and p-value
                 rm<- (1 + 1/m) * sum(diag(v2 %*% solve(vv))) # eqn (1.18) without dividing by k
                 wald_stat<- as.numeric( t(cc) %*% solve(vv) %*% cc )/(df + rm) # eqn (1.17)
                 # expr (1.19)
                 nu<- df * (m-1)
                 df_denominator<- if (nu> 4) {
                   4 + (nu-4)*(1 + (1-2/nu)/(rm/df))^2
                 } else {
                   0.5*(m-1)*(df+1)*(1+1/(rm/df))^2
                 }

                 pval<- stats::pf(wald_stat, df1= df, df2= df_denominator, lower.tail = FALSE)

                 out<- data.frame(var= if (i==0) '(Intercept)' else attr(tmp$terms, "term.labels")[i],
                                  rid = i,
                                  df= round(df, 0),
                                  stat= wald_stat,
                                  chisq_p= pval)
                 attr(out, "col_in_X")<- data.frame(term= colnames(x)[i==varseq],
                                                    rid= i,
                                                    stringsAsFactors = FALSE)
                 out
               })
  out
}

#' @export
summarize_mi_glm <- function(mira_obj, exponentiate= FALSE, alpha= .05, vcov_fun= NULL) {

  out<- calculate_type3_mi(mira_obj, vcov_fun= vcov_fun)

    # the order of the variables in the output
  out_tmp<- out %>%
    lapply(function(x) attr(x, "col_in_X")) %>%
    dplyr::bind_rows()

  type3_out<- out %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(pval= format_pvalue(chisq_p))

  glm_out<- mitools::MIcombine(mitools::MIextract(mira_obj$analyses, fun= coef),
                      mitools::MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)) %$%
    data.frame(est= coefficients,
               se = sqrt(diag(variance))) %>%
    tibble::rownames_to_column(var= "term") %>%
    dplyr::right_join(out_tmp, by= "term") %>%
    dplyr::mutate(conf_low = est - stats::qnorm(1-alpha/2) * se,
                  conf_high= est + stats::qnorm(1-alpha/2) * se,
                  est      = if (exponentiate) exp(est) else est,
                  conf_low = if (exponentiate) exp(conf_low) else conf_low,
                  conf_high= if (exponentiate) exp(conf_high) else conf_high,
                  stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
                  pval= type3_out$pval[charmatch(gsub("TRUE$", "", term), type3_out$var)]) %>%
    dplyr::select(term, stat, pval, rid, dplyr::everything())  %>%
    dplyr::rename(var= term)




  type3_out <- type3_out %>%
     dplyr::filter(df>1) %>%
    dplyr::select(var, pval, rid)

  glm_out %>%
    dplyr::bind_rows(type3_out) %>%
    dplyr::arrange(rid, var) %>%
    dplyr::select(var, stat, pval, dplyr::everything())


}

#' @export
summarize_mi_coxph <- function(cox_mira, exponentiate= TRUE, alpha= .05) {

   out<- calculate_type3_mi(cox_mira)
  # names(out)<- attr(tmp$terms, "term.labels")[unique(varseq)]
  # the order of the variables in the output
  out_tmp<- out %>%
    lapply(function(x) attr(x, "col_in_X")) %>%
    dplyr::bind_rows()

  type3_out<- out %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(pval= format_pvalue(chisq_p)) %>%
     dplyr::filter(df>1) %>%
    dplyr::select(var, pval, rid)

  cox_out<- cox_mira %>%
    # mice::pool() %>%
    # see https://github.com/amices/mice/issues/246#
    mice::pool(dfcom = mice::getfit(., 1L)$nevent - length(coef(mice::getfit(., 1L)))) %>%
    summary(conf.int = TRUE,
            conf.level = 1-alpha,
            exponentiate= exponentiate) %>%
    # as.data.frame() %>%
    # tibble::rownames_to_column("var") %>%
    dplyr::rename(est      = estimate,
                 pval     = p.value,
                 conf_low = `2.5 %`,
                 conf_high= `97.5 %`) %>%
    dplyr::mutate(var= as.character(term),
                  stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
                  pval= format_pvalue(pval)) %>%
    dplyr::select(var, stat, pval, est, conf_low, conf_high) %>%
    dplyr::full_join(out_tmp, by= c("var" = "term"))

  cox_out %>%
    dplyr::bind_rows(type3_out) %>%
    dplyr::arrange(rid, var) %>%
    dplyr::select(var, stat, pval, dplyr::everything())
  # dplyr::bind_rows(cox_out, type3_out) %>% dplyr::arrange(rid, var)
}

#' @export
generate_mi_glm_termplot_df <- function(mira_obj,
                                       terms= NULL,
                                       center_at= NULL,
                                       vcov_fun= NULL, ...) {
  require(mitools)
  dummy_mdl<- mice::getfit(mira_obj, 1L)
  tt<- stats::terms(dummy_mdl)
  terms<- if (is.null(terms)) 1:length(labels(tt)) else terms
  cn<- attr(tt, "term.labels")[terms]
  varseq<- attr(mm_orig<- stats::model.matrix(dummy_mdl), "assign")

  carrier.name <- function(term) {
    if (length(term) > 1L)
      carrier.name(term[[2L]])
    else as.character(term)
  }

  betas <- mitools::MIextract(mira_obj$analyses,
                     fun = coef)

  vars  <- mitools::MIextract(mira_obj$analyses,
                     fun = if (is.null(vcov_fun)) vcov else vcov_fun)

  mi_res<- mitools::MIcombine(betas, vars)

  dummy_mdl$coefficients<- mi_res$coefficients

  plot_d<- stats::termplot(dummy_mdl, terms= terms, plot= FALSE)

  plot_d<- mapply(function(df, cc, var, tt) {

    mm<- matrix(apply(mm_orig, 2, mean),
                nrow = nrow(df),
                ncol = length(varseq),
                byrow = T)
    colnames(mm)<- colnames(mm_orig)
    rownames(mm)<- df$x

    # calculate fitted value
    df<- if (!is.null(cc)) {
      which_x<- if (is.factor(df$x)) {
        which(df$x==cc)
      } else if (is.logical(df$x)) {
        which(!df$x)
      } else {
        min(which(df$x>=cc))
      }
      dplyr::mutate(df, y = y - y[which_x])
    } else df

    # now calculate the standard error
    tmp<- df
    names(tmp)<- gsub("x", sapply(str2expression(var), carrier.name), names(tmp))
    mm[, tt == varseq]<- (stats::model.matrix(stats::as.formula(paste("~ ", var)), data= tmp)[,-1])
    df$se<- sqrt(diag(mm %*% mi_res$variance %*% t(mm)))

    df %>%
      dplyr::mutate(conf_low= y - stats::qnorm(0.975) * se,
                    conf_high= y + stats::qnorm(0.975) * se)
  },
  df= plot_d,
  cc= if (is.null(center_at)) vector("list", length(terms)) else center_at,
  var= cn,
  tt= terms,
  SIMPLIFY = FALSE)

  # the next two (commented out) lines to check if I constructed the design matrix correctly
  # they should equal plot_d$y
  # xx<- as.numeric(mm %*% mi_res$coefficients)
  # xx<- xx - xx[which_x]

  plot_d
}




