# Loading the packages we will need
library(metacore)
library(tidyverse)
library(admiral)
library(haven)
library(metatools)
library(xportr)

source("code/01-metacore-metatools.R")

# Read in the metadata ----
metacore <- spec_to_metacore("specs/specs.xlsx", where_sep_sheet = FALSE)

# Get the specifications for the dataset we are currently building
adsl_spec <- metacore %>%
  select_dataset("ADSL")

# Read the data in ----
dm <- read_xpt("datasets/SDTM/dm.xpt")
vs <- read_xpt("datasets/SDTM/vs.xpt")
ex <- read_xpt("datasets/SDTM/ex.xpt")
sv <- read_xpt("datasets/SDTM/sv.xpt")
ae <- read_xpt("datasets/SDTM/ae.xpt")

# Baseline Characteristics ----
adsl_bl <- adsl_decode %>%
  derive_vars_transposed(
    select(vs, USUBJID, VSTESTCD, VSSTRESN, VSBLFL),
    by_vars = vars(USUBJID),
    key = VSTESTCD,
    value = VSSTRESN,
    filter = VSTESTCD %in% c("HEIGHT", "WEIGHT") & VSBLFL == "Y"
  ) %>%
  rename(HEIGHTBL = HEIGHT, WEIGHTBL = WEIGHT) %>%
  select(-VSBLFL) %>%
  mutate(BMIBL = compute_bmi(HEIGHTBL, WEIGHTBL))


# Treatment Start and End Date ----
ex_dt <- ex %>%
  derive_vars_dt(
    new_vars_prefix = "EXST",
    dtc = EXSTDTC
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "EXEN",
    dtc = EXENDTC
  )

adsl_ex <- adsl_bl %>%
  derive_vars_merged(
    dataset_add = ex_dt,
    by_vars = vars(STUDYID, USUBJID),
    order = vars(EXSTDT, EXSEQ),
    new_vars = vars(TRTSDT = EXSTDT),
    mode = "first",
    filter_add = EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))
  ) %>%
  derive_vars_merged(
    dataset_add = ex_dt,
    by_vars = vars(STUDYID, USUBJID),
    order = vars(EXENDT, EXSEQ),
    new_vars = vars(TRTEDT = EXENDT),
    mode = "last",
    filter_add = EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))
  )


# Safety Flags ----
adsl_saff <- adsl_ex %>%
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


# Last Date Known Alive ----
ae_dt <- ae %>%
  derive_vars_dt("AEST", AESTDTC) %>%
  derive_vars_dt("AEEN", AEENDTC)

ae_start_src <- date_source(
  dataset_name = "ae",
  date = AESTDT,
  traceability_vars = vars(
    LALVDOM = "AE",
    LALVSEQ = AESEQ,
    LALVVAR = "AESTDTC"
  )
)
ae_end_src <- date_source(
  dataset_name = "ae",
  date = AEENDT,
  traceability_vars = vars(
    LALVDOM = "AE",
    LALVSEQ = AESEQ,
    LALVVAR = "AENTDTC"
  )
)

ex_start_src <- date_source(
  dataset_name = "ex",
  date = EXSTDT,
  traceability_vars = vars(
    LALVDOM = "EX",
    LALVSEQ = EXSEQ,
    LALVVAR = "EXSTDTC"
  )
)
ex_end_src <- date_source(
  dataset_name = "ex",
  date = EXENDT,
  traceability_vars = vars(
    LALVDOM = "EX",
    LALVSEQ = EXSEQ,
    LALVVAR = "EXENDTC"
  )
)

adsl_dt_src <- date_source(
  dataset_name = "adsl",
  date = TRTEDT,
  traceability_vars = vars(
    LALVDOM = "ADSL",
    LALVSEQ = NA,
    LALVVAR = "TRTEDT"
  )
)

adsl_lstalv <- adsl_saff %>%
  derive_var_extreme_dt(
    new_var = LSTALVDT,
    ae_start_src, ae_end_src, ex_start_src, ex_end_src, adsl_dt_src,
    source_datasets = list(ae = ae_dt, ex = ex_dt, adsl = adsl_saff),
    mode = "last"
  )


# Apply Metadata ----
adsl <- adsl_lstalv %>%
  order_cols(adsl_spec) %>%
  set_variable_labels(adsl_spec) %>%
  check_ct_data(adsl_spec) %>%
  check_variables(adsl_spec)


# Export Dataset ----
adsl %>%
  xportr_label(adsl_spec) %>%
  xportr_df_label(adsl_spec) %>%
  xportr_write("datasets/ADAM/adsl.xpt")
