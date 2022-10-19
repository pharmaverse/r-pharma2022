library(Tplyr)
library(haven)
library(xportr)
library(dplyr)
library(nlme)
library(emmeans)
library(broom)
library(stringr)
library(tidyr)

adsl <- read_xpt("datasets/ADAM/adsl.xpt")
adae <- read_xpt("datasets/ADAM/adae.xpt") 
adpft <- read_xpt("datasets/ADAM/adpft.xpt") %>% 
  mutate(VISIT = factor(VISIT, levels = c("BASELINE", "WEEK 4", "WEEK 8", "WEEK 12")))

# Demog -------------------------------------------------------------------
demog <- demog <- adsl %>% 
  tplyr_table(TRT01A, where= SAFFL =="Y") %>%
  add_total_group() %>% 
  add_layer(
    group_count(target_var = SEX, by = vars("Sex n (%)"))
  ) %>% 
  add_layer(
    group_desc(target_var = AGE, by = "Age (years)")
  )  %>%
  add_layer(
    group_count(target_var = AGEGR1, by = vars("Age (years)" ))
  ) %>%
  add_layer(
    group_count(target_var = RACE, by = vars("Race n (%)"))
  ) %>%
  add_layer(
    group_desc(target_var = HEIGHTBL, by = "Height (cm)")
  ) %>%
  add_layer(
    group_desc(target_var = WEIGHTBL, by = "Weight (kg)")
  ) %>%
  get_numeric_data()  %>% 
  mutate(value = if_else(param == "pct", value*100, value),
         ord1 = case_when(row_label2 == "Sex n (%)" ~ 1, 
                          row_label2 == "Age (years)" ~ 2, 
                          row_label2 == "Race n (%)" ~ 3, 
                          row_label2 == "Ethnicity n (%)" ~ 4, 
                          row_label2 == "Height (cm)" ~ 5, 
                          row_label2 == "Weight (kg)" ~ 6, 
                          row_label2 == "BMI (kg/m^2)" ~ 7, 
                          ), 
         ord2 =  case_when(row_label1 == "n" ~ 1, 
                           row_label1 == "Mean (SD)" ~ 2, 
                           row_label1 == "Median" ~ 3, 
                           row_label1 == "Q1, Q3" ~ 4, 
                           row_label1 == "Min, Max" ~ 5, 
                           row_label1 == "Missing" ~ 6, 
                           TRUE ~ 7
         )) 

# AE table ----------------------------------------------------------------
ae <- tplyr_table(adae, TRT01A) %>%
  set_pop_data(adsl) %>% 
  add_total_group() %>% 
  add_layer(
    group_count("Any Body System") %>% 
      set_distinct_by(USUBJID) 
  ) %>%
  add_layer(
    group_count(target_var = vars(AEBODSYS, AETERM)) %>% 
      set_distinct_by(USUBJID) 
  )  %>% 
  get_numeric_data() %>% 
  mutate(
    row_label2 = if_else(is.na(row_label2), row_label1, row_label2) %>%
      str_to_title(),
    row_label1 = str_remove(row_label1, "^\\s*") %>%
      str_to_title(),
    value = if_else(param == "distinct_pct", value*100, value),
    ord = row_number()
  ) %>% 
  group_by(row_label2) %>% 
  mutate(ord1 = min(ord)) %>% 
  group_by(row_label1) %>% 
  mutate(ord2 = min(ord)) %>% 
  ungroup() %>% 
  select(-ord)


# Stats -------------------------------------------------------------------
adpft_mod <- adpft %>% 
  filter(MITTFL == "Y",PARAMCD == "FEV1", VISITNUM > 2, TRTA != "Total") 

# MMRM model
mod <- gls(CHG ~ BASE + TRTA + VISIT + BASE:VISIT + TRTA:VISIT,
           data = adpft_mod,
           # unstructured covariance matrix
           correlation = corSymm(form = ~as.numeric(VISIT)|USUBJID), # unstructured corr matrix
           weights = varIdent(form = ~1|VISIT), # allow distinct variance for each visit
           na.action = "na.omit")

# Extract lsmeans and contrasts
emm <- mod %>% emmeans(~TRTA|VISIT)  
emm_diff <- emm %>% contrast(method = "pairwise", adjust = "none")


### Broom to tidy up
emm_tidy <- tidy(emm)
emm_diff_tidy <- tidy(emm_diff, conf.int = TRUE)

result_tidy <- bind_rows(emm_tidy, emm_diff_tidy) %>% 
  mutate(pfts = "FEV1 (L)",
         span_col = coalesce(TRTA, contrast),
         contrast_yn = ifelse(!is.na(contrast), "y", "n")) %>% 
  pivot_longer(cols = where(is.numeric), names_to = "param", values_to = "value") %>% 
  filter((contrast_yn=="y" & param %in% c("estimate", "conf.low", "conf.high", "p.value")) |
           (contrast_yn=="n" & param %in% c("estimate", "std.error"))) %>% 
  mutate(lower_col = case_when(
    str_detect(param, "conf") ~ "[95% CI]",
    str_detect(span_col, " - ") & param == "estimate" ~ "Difference",
    param == "estimate" ~ "Adjusted Mean",
    param == "std.error" ~ "(SE)",
    param == "p.value" ~ "p-value"),
    ord1 = case_when(
      VISIT == "WEEK 4" ~ 1,
      VISIT == "WEEK 8" ~ 2,
      VISIT == "WEEK 12" ~ 3
    )) %>% 
  select(pfts, VISIT, span_col, lower_col, param, value, ord1)




# Write data  -------------------------------------------------------------

write_xpt(demog, "datasets/ARD/demog.xpt")
write_xpt(pft, "datasets/ARD/pft.xpt")
write_xpt(ae, "datasets/ARD/ae.xpt")
write_xpt(result_tidy, "datasets/ARD/model.xpt")



