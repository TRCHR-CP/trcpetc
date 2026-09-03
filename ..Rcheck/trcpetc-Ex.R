pkgname <- "trcpetc"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('trcpetc')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check_box_convert")
### * check_box_convert

flush(stderr()); flush(stdout())

### Name: check_box_convert
### Title: check box convert
### Aliases: check_box_convert

### ** Examples


library(dplyr)

Comorbidities  <- cardio_data %>% select(Diabetes:NoComorbidities) %>% names()

## Without checkbox question (Assuming missing all questions is FALSE)
cardio_data %>% select(Comorbidities) %>%
 table_one()
# Example with one checkbox question (Assumes missing all questions is missing)

cardio_data %>% select(Comorbidities) %>%
 check_box_convert(check_box_cols = Comorbidities,title = "Comorbidities¹")  %>%
 table_one(Check_box = Comorbidities,
           Check_box_title = "Comorbidities¹")%>%
 kableExtra::footnote(
   general = "¹Patients could present with more than one comorbidity, totals may not sum to 100%.",
   general_title = "",
   footnote_as_chunk = TRUE
 )


## Example with two different checkbox questions

Comorbidities1  <- cardio_data %>% select(Diabetes:COPD) %>% names()
Comorbidities2  <- cardio_data %>% select(CKD:CAD) %>% names()



cardio_data %>% select(Comorbidities1,Comorbidities2) %>%
 check_box_convert(check_box_cols = Comorbidities1,title = "Comorbidities1¹")  %>%
 check_box_convert(check_box_cols = Comorbidities2,title = "Comorbidities2¹")  %>%
 table_one(Check_box = Comorbidities,
           Check_box_title = c("Comorbidities¹","Comorbidities2¹"))%>%
 kableExtra::footnote(
   general = "¹Patients could present with more than one comorbidity, totals may not sum to 100%.",
   general_title = "",
   footnote_as_chunk = TRUE
 )





cleanEx()
nameEx("construct_surv_cmprisk_var")
### * construct_surv_cmprisk_var

flush(stderr()); flush(stdout())

### Name: construct_surv_cmprisk_var
### Title: construct survival or competing risks process
### Aliases: construct_surv_cmprisk_var

### ** Examples



## For survival

survival_data <-  construct_surv_cmprisk_var(cardio_data,
                                            patid = PatientID,
                                            idx_dt = SurgeryDate,
                                            evt_dt = DeathDate,
                                            end_dt = LastVisitDate,
                                            append = TRUE,
                                            units = "months",
                                            adm_cnr_time = 24)

KM <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time)
KM_Sex <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time,group = Sex)


# For competing risks
cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
                                           patid = PatientID,
                                           idx_dt = SurgeryDate,
                                           evt_dt = TransplantDate,
                                           end_dt = LastVisitDate,
                                           death_dt = DeathDate,
                                           append = TRUE,
                                           units = "months",
                                           adm_cnr_time = 24)

CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)
CIF_Sex <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time,group = Sex)




cleanEx()
nameEx("estimate_cif_km")
### * estimate_cif_km

flush(stderr()); flush(stdout())

### Name: estimate_cif_km
### Title: Kaplan-Meier survival and cumulative incidence (CIF) estimates
### Aliases: estimate_cif_km

### ** Examples



## For survival

survival_data <-  construct_surv_cmprisk_var(cardio_data,
                                            patid = PatientID,
                                            idx_dt = SurgeryDate,
                                            evt_dt = DeathDate,
                                            end_dt = LastVisitDate,
                                            append = TRUE,
                                            units = "months",
                                            adm_cnr_time = 24)

KM <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time)
KM_Sex <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time,group = Sex)


# For competing risks
cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
                                           patid = PatientID,
                                           idx_dt = SurgeryDate,
                                           evt_dt = TransplantDate,
                                           end_dt = LastVisitDate,
                                           death_dt = DeathDate,
                                           append = TRUE,
                                           units = "months",
                                           adm_cnr_time = 24)

CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)
CIF_Sex <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time,group = Sex)




cleanEx()
nameEx("factor_order")
### * factor_order

flush(stderr()); flush(stdout())

### Name: factor_order
### Title: Order a factor variable by descending frequency.
### Aliases: factor_order

### ** Examples



library(dplyr)

cardio_data %>% select(SurgeryType) %>% table_one()

cardio_data %>%
 mutate(SurgeryType = factor_order(SurgeryType)) %>% select(SurgeryType) %>% table_one()




cleanEx()
nameEx("kable_table_one")
### * kable_table_one

flush(stderr()); flush(stdout())

### Name: kable_table_one
### Title: kable_table_one
### Aliases: kable_table_one

### ** Examples

library(dplyr)
Comorbidities  <- cardio_data %>% select(Diabetes:NoComorbidities) %>% names()

work_d <- cardio_data %>%
 mutate(SurgeryType = factor_order(SurgeryType)) %>%
 check_box_convert(check_box_cols = Comorbidities,title = "Comorbidities¹")



demo_table <-  table_one(df = work_d ,
         group = Sex,
        datadic = cardio_data_dictionary %>%
           rbind(data.frame("VariableName" = "Comorbidities¹",
                            "Label" ="Comorbidities¹", "Description"= "All Comorbidities")),
        var_name = VariableName,
         var_desp = Label,
         include_overall = "all",
         Check_box = Comorbidities,
         Check_box_title = "Comorbidities¹")


 # Creating a report ready kable output
 options(knitr.kable.NA = '')
 kable_table_one(demo_table,caption =  "Summary table overall and stratified by sex") %>%
 kableExtra::footnote(
   general = "¹Patients could present with more than one comorbidity, totals may not sum to 100%.",
   general_title = "",
   footnote_as_chunk = TRUE)



cleanEx()
nameEx("median_time_to_event")
### * median_time_to_event

flush(stderr()); flush(stdout())

### Name: median_time_to_event
### Title: Calculate Median Time to Event for a Specific State
### Aliases: median_time_to_event

### ** Examples



cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
patid = PatientID,
idx_dt = SurgeryDate,
evt_dt = TransplantDate,
end_dt = LastVisitDate,
death_dt = DeathDate,
append = TRUE,
units = "months",
adm_cnr_time = 24)


CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)

median_time_to_event(survfitms_obj = CIF, target_prob= 0.2,evt_type = 1, interpolate = TRUE)

CIF_Sex <- estimate_cif_km(cmp_risk_data , evt = evt,evt_time = evt_time,group = Sex)
median_time_to_event(survfitms_obj = CIF_Sex,subgroup = "Sex=Male",
target_prob= 0.2,evt_type = 1, interpolate = TRUE)
median_time_to_event(survfitms_obj = CIF_Sex,subgroup = "Sex=Female",
target_prob= 0.2,evt_type = 1, interpolate = TRUE)




cleanEx()
nameEx("show_cif")
### * show_cif

flush(stderr()); flush(stdout())

### Name: show_cif
### Title: Plot Cumulative Incidence Function for Competing Risks
### Aliases: show_cif

### ** Examples



windowsFonts(Arial = windowsFont("Arial"))

## Showing all events
cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
patid = PatientID,
idx_dt = SurgeryDate,
evt_dt = TransplantDate,
end_dt = LastVisitDate,
death_dt = DeathDate,
append = TRUE,
units = "months",
adm_cnr_time = 24)

CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)


    show_cif(CIF,evt_type = c(0,1,2),add_legend = TRUE,x_break = seq(0,24,by=3),
         evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"),
x_lab = "Time since surgery (months)")


## Including a covariate

CIF_Sex <- estimate_cif_km(cmp_risk_data , evt = evt,evt_time = evt_time,group = Sex)

show_cif(CIF_Sex,evt_type = c(1),add_legend = FALSE,x_break = seq(0,24,by=3),
        evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"),
x_lab = "Time since surgery (months)")




cleanEx()
nameEx("show_surv")
### * show_surv

flush(stderr()); flush(stdout())

### Name: show_surv
### Title: Plot Survival or Cumulative Death Function
### Aliases: show_surv

### ** Examples


## All patients

survival_data <-  construct_surv_cmprisk_var(cardio_data,
                                            patid = PatientID,
                                            idx_dt = SurgeryDate,
                                            evt_dt = DeathDate,
                                            end_dt = LastVisitDate,
                                            append = TRUE,
                                            units = "months",
                                            adm_cnr_time = 24)

KM <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time)
show_surv(KM, pvalue_pos = "bottomleft",
add_legend = TRUE,x_break = seq(0,24,by=3),
x_lab = "Time since surgery (months)")
## Including a covariate
KM_Sex <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time,group = Sex)
show_surv(KM_Sex,pvalue_pos = "bottomleft",
add_legend = TRUE,x_break = seq(0,24,by=3),
x_lab = "Time since surgery (months)")





cleanEx()
nameEx("summarize_cif")
### * summarize_cif

flush(stderr()); flush(stdout())

### Name: summarize_cif
### Title: Summarize Cumulative Incidence Function (CIF)
### Aliases: summarize_cif

### ** Examples



cmp_risk_data <- construct_surv_cmprisk_var(cardio_data,
                                           patid = PatientID,
                                           idx_dt = SurgeryDate,
                                           evt_dt = TransplantDate,
                                           end_dt = LastVisitDate,
                                           death_dt = DeathDate,
                                           append = TRUE,
                                           units = "months",
                                           adm_cnr_time = 24)

CIF <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time)

## Presenting one event for all patients
summarize_cif(CIF,time_lab = "Time since surgery (months)",evt_type = 1,
caption = "Time to transplant for all patients",
              evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))

## Presenting all events for all patients
summarize_cif(CIF,time_lab = "Time since surgery (months)",
caption = "Time to transplant for all patients",
              evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))

CIF_Sex <- estimate_cif_km(cmp_risk_data, evt = evt,evt_time = evt_time,group = Sex)
## Presenting only the events by a covariate

summarize_cif(CIF_Sex,time_lab = "Time since surgery (months)",evt_type = 1,
caption = "Time to transplant by sex",
evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))


## Presenting all events by a covariate


summarize_cif(CIF_Sex, time_lab = "Time since surgery (months)",
caption = "Time to transplant by sex",
             evt_label = c('0' = "Event free", '1' = "Transplant", '2'= "Death"))




cleanEx()
nameEx("summarize_km")
### * summarize_km

flush(stderr()); flush(stdout())

### Name: summarize_km
### Title: Summarize Kaplan-Meier Estimates
### Aliases: summarize_km

### ** Examples


survival_data <-  construct_surv_cmprisk_var(cardio_data,
                                            patid = PatientID,
                                            idx_dt = SurgeryDate,
                                            evt_dt = DeathDate,
                                            end_dt = LastVisitDate,
                                            append = TRUE,
                                            units = "months",
                                            adm_cnr_time = 24)

KM <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time)
summarize_km(KM,overall_label = "All patients",
time_lab = "Time since surgery (months)",caption = "Overall Survival by sex")

KM_Sex <- estimate_cif_km(survival_data, evt = evt,evt_time = evt_time,group = Sex)
summarize_km(KM_Sex,overall_label = "All patients",
time_lab = "Time since surgery (months)",caption = "Overall Survival for all patients")




cleanEx()
nameEx("table_one")
### * table_one

flush(stderr()); flush(stdout())

### Name: table_one
### Title: table_one
### Aliases: table_one

### ** Examples

library(dplyr)
Comorbidities  <- cardio_data %>% select(Diabetes:NoComorbidities) %>% names()

work_d <- cardio_data %>%
 mutate(SurgeryType = factor_order(SurgeryType)) %>%
 check_box_convert(check_box_cols = Comorbidities,title = "Comorbidities¹")



demo_table <-  table_one(df = work_d ,
         group = Sex,
        datadic = cardio_data_dictionary %>%
           rbind(data.frame("VariableName" = "Comorbidities¹",
                            "Label" ="Comorbidities¹", "Description"= "All Comorbidities")),
        var_name = VariableName,
         var_desp = Label,
         include_overall = "all",
         Check_box = Comorbidities,
         Check_box_title = "Comorbidities¹")


 # Creating a report ready kable output
 options(knitr.kable.NA = '')
 kable_table_one(demo_table,caption =  "Summary table overall and stratified by sex") %>%
 kableExtra::footnote(
   general = "¹Patients could present with more than one comorbidity, totals may not sum to 100%.",
   general_title = "",
   footnote_as_chunk = TRUE)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
