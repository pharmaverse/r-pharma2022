library(metacore)
library(tidyverse)
library(admiral)
library(haven)
library(metatools)
library(lubridate)
library(xportr)

metacore <- spec_to_metacore("specs/specs.xlsx", where_sep_sheet = FALSE)

# Read the data in
dm <- read_xpt("datasets/SDTM/dm.xpt")
vs <- read_xpt("datasets/SDTM/vs.xpt")
ex <- read_xpt("datasets/SDTM/ex.xpt")
sv <- read_xpt("datasets/SDTM/sv.xpt")
ae <- read_xpt("datasets/SDTM/ae.xpt")
suppae <- read_xpt("datasets/SDTM/suppae.xpt")
re <- read_xpt("datasets/SDTM/re.xpt")

# ADSL --------------------------------------------------------------------
adsl_spec <- metacore %>%
  select_dataset("ADSL")

adsl_pred <- build_from_derived(adsl_spec, ds_list = list("dm" = dm), keep = TRUE) %>%
  filter(ARMCD %in% c("A", "P"))

adsl_decode <- adsl_pred %>%
  create_cat_var(adsl_spec, AGE, AGEGR1, AGEGR1N) %>%
  create_var_from_codelist(adsl_spec, COUNTRY, ACOUNTRY) %>%
  create_var_from_codelist(adsl_spec, SEX, SEXN) %>%
  create_var_from_codelist(adsl_spec, ETHNIC, ETHNICN) %>%
  create_var_from_codelist(adsl_spec, RACE, RACEN) %>%
  create_var_from_codelist(adsl_spec, ARMCD, TRT01PN) %>%
  create_var_from_codelist(adsl_spec, ACTARMCD, TRT01AN) %>%
  create_var_from_codelist(adsl_spec, ARMCD, TRT01P) %>%
  create_var_from_codelist(adsl_spec, ACTARMCD, TRT01A)

adsl_bl <- vs %>%
  filter(VSTESTCD %in% c("HEIGHT", "WEIGHT") & VSBLFL == "Y") %>%
  select(USUBJID, VSTESTCD, VSSTRESN) %>%
  mutate(VSTESTCD = paste0(VSTESTCD, "BL")) %>%
  pivot_wider(names_from = VSTESTCD, values_from = VSSTRESN) %>%
  mutate(BMIBL = compute_bmi(HEIGHTBL, WEIGHTBL)) %>%
  right_join(adsl_decode, by = "USUBJID")

adsl_ex <- adsl_bl %>%
  derive_vars_merged_dtm(
    dataset_add = ex,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & nchar(EXSTDTC) >= 10,
    new_vars_prefix = "TRTS",
    dtc = EXSTDTC,
    time_imputation = "FIRST",
    order = vars(TRTSDTM, EXSEQ),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID),
    flag_imputation = "none"
  ) %>%
  derive_vars_merged_dtm(
    dataset_add = ex,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & nchar(EXENDTC) >= 10,
    new_vars_prefix = "TRTE",
    dtc = EXENDTC,
    time_imputation = "last",
    order = vars(EXSEQ),
    mode = "last",
    by_vars = vars(STUDYID, USUBJID),
    flag_imputation = "none"
  )

# Safety Flag
adsl <- adsl_ex %>%
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = vars(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "Placebo")))
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_var = MITTFL,
    condition = (VISITDY >= 56)
  ) %>%
  mutate(RANDFL = if_else(ACTARMCD %in% c("P", "A"), "Y", NA_character_))

adsl <- adsl %>%
  order_cols(adsl_spec) %>%
  set_variable_labels(adsl_spec) %>%
  check_ct_data(adsl_spec) %>%
  check_variables(adsl_spec)

adsl %>%
  xportr_label(adsl_spec) %>%
  xportr_df_label(adsl_spec) %>%
  xportr_write("datasets/ADAM/adsl.xpt")

# ADAE --------------------------------------------------------------------
adae_spec <- metacore %>%
  select_dataset("ADAE")

ae_tot <- combine_supp(ae, suppae)
adae <- build_from_derived(
  adae_spec,
  ds_list = list("adsl" = adsl, "ae" = ae_tot), keep = TRUE
) %>%
  filter(AEBODSYS != "") %>%
  mutate(
    ASTDT = floor_date(AESTDTC, "day"),
    AENDT = floor_date(AEENDTC, "day")
  )

adae <- adae %>%
  order_cols(adae_spec) %>%
  set_variable_labels(adae_spec) %>%
  check_ct_data(adae_spec) %>%
  check_variables(adae_spec)

adae %>%
  xportr_label(adae_spec) %>%
  xportr_df_label(adae_spec) %>%
  xportr_write("datasets/ADAM/adae.xpt")

# ADPFT -------------------------------------------------------------------
adpft_spec <- metacore %>%
  select_dataset("ADPFT")

adpft_decode <- build_from_derived(
  adpft_spec,
  ds_list = list("adsl" = adsl, "re" = re), keep = FALSE
) %>%
  create_var_from_codelist(adpft_spec, PARAM, PARAMN) %>%
  mutate(ABLFL = if_else(VISIT == "BASELINE", "Y", ""))

adpft <- adpft_decode %>%
  filter(VISIT == "BASELINE") %>%
  select(USUBJID, PARAMCD, BASE = AVAL) %>%
  left_join(adpft_decode, ., by = c("USUBJID", "PARAMCD")) %>%
  mutate(CHG = case_when(VISIT != "SCREENING" ~ AVAL - BASE))

adpft <- adpft %>%
  filter(VISIT != "SCREENING") %>%
  order_cols(adpft_spec) %>%
  set_variable_labels(adpft_spec) %>%
  check_ct_data(adpft_spec) %>%
  check_variables(adpft_spec)

adpft %>%
  xportr_label(adpft_spec) %>%
  xportr_df_label(adpft_spec) %>%
  xportr_write("datasets/ADAM/adpft.xpt")
