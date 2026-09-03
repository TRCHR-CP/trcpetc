remove.packages("trcpetc")

install.packages(c("devtools"))
install.packages(c("testthat"))

devtools::install(
  "/home/rstudio/data/Staff/Brigitte Mueller/R-Packages/TRCPETC",
  upgrade = "never",
  build_vignettes = TRUE
)

library(trcpetc)

devtools::test(
  "/home/rstudio/data/Staff/Brigitte Mueller/R-Packages/TRCPETC"
)

library(dplyr)
library(trcpetc)

data(cardio_data)

Comorbidities <- cardio_data %>%
  select(Diabetes:NoComorbidities) %>%
  names()

work_d <- cardio_data %>%
  mutate(SurgeryType = factor_order(SurgeryType)) %>%
  check_box_convert(
    check_box_cols = Comorbidities,
    title = "Comorbidities¹"
  )

t1 <- table_one(df = work_d ,
                group = Sex,
                datadic = cardio_data_dictionary %>%
                  rbind(data.frame("VariableName" = "Comorbidities¹", "Label" ="Comorbidities¹", "Description"= "All Comorbidities")),
                var_name = VariableName,
                var_desp = Label,
                # caption =  "Summary table overall and stratified by sex",
                include_overall = "all",
                Check_box = Comorbidities,
                Check_box_title = "Comorbidities¹")

kable_table_one(t1, caption = "Summary table overall and stratified by sex") %>%
  kableExtra::footnote(
    general = "¹Patients could present with more than one comorbidity, totals may not sum to 100%.",
    general_title = "",
    footnote_as_chunk = TRUE)


# TODO Compare SMD to the one in CreatTaableOne package
# TODO Compare p-values to the ones from fanetc::table_one

#Rscript -e 'install.packages("tableone", repos="https://cloud.r-project.org")'

Rscript -e 'library(trcpetc); library(tableone); data(cardio_data); vars <- names(cardio_data)[vapply(cardio_data, function(x) is.numeric(x) || is.integer(x) || is.logical(x) || is.factor(x), logical(1))]; vars <- setdiff(vars, "Sex"); ref <- CreateTableOne(vars=vars, strata="Sex", data=cardio_data, test=FALSE, smd=TRUE); print(ExtractSmd(ref)); trc <- table_one(cardio_data, group=Sex)$tab; print(trc[, c("variable", "row_id", "smd")]);'
