library(admiral)
library(metacore)
library(dplyr)
library(haven)

metacore <- spec_to_metacore("specs/specs.xlsx", quiet = TRUE, where_sep_sheet = FALSE)

adaette_spec <- metacore %>%
  select_dataset("ADAETTE")

adsl <- read_xpt("datasets/ADAM/adsl.xpt")
adae <- read_xpt("datasets/ADAM/adae.xpt")

adsl_pred <- build_from_derived(adaette_spec, list(adsl = adsl))

lstalv_censor <- censor_source(
  "adsl",
  date = LSTALVDT,
  set_values_to = vars(
    EVNTDESC = "Last Known Alive Date",
    SRCDOM = "ADSL",
    SRCVAR = "LSTALVDT"
  )
)

any_ae_src <- event_source(
  dataset_name = "adae",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "Any Adverse Event",
    SRCDOM = "ADAE",
    SRCVAR = "AEDECOD"
  )
)

cardiac_src <- event_source(
  dataset_name = "adae",
  filter = AEBODSYS == "CARDIAC DISORDERS",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = AEBODSYS,
    SRCDOM = "ADAE",
    SRCVAR = "AEBODSYS"
  )
)

adaette_any_ae <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adae = adae, adsl = adsl),
  start_date = TRTSDT,
  event_conditions = list(any_ae_src),
  censor_conditions = list(lstalv_censor),
  set_values_to = vars(
    PARAMCD = "ANYAETTE",
    PARAM = "Time to any first adverse event"
  )
)

adaette_cardiac <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adae = adae, adsl = adsl),
  start_date = TRTSDT,
  event_conditions = list(cardiac_src),
  censor_conditions = list(lstalv_censor),
  set_values_to = vars(
    PARAMCD = "CARDAETTE",
    PARAM = "Time to first cardiac adverse event"
  )
)

adaette_aval <- bind_rows(adaette_any_ae, adaette_nausea) %>%
  derive_vars_duration(
    new_var = AVAL,
    new_var_unit = AVALU,
    start_date = STARTDT,
    end_date = ADT,
    out_unit = "days"
  ) %>%
  derive_vars_dy(reference_date = STARTDT, source_vars = vars(ADT)) %>%
  derive_vars_merged(adsl_pred, by_vars = vars(STUDYID, USUBJID))

adaette <- adaette_aval %>%
  order_cols(adaette_spec) %>%
  set_variable_labels(adaette_spec) %>%
  check_ct_data(adaette_spec) %>%
  check_variables(adaette_spec)

adaette %>%
  xportr_label(adaette_spec) %>%
  xportr_df_label(adaette_spec) %>%
  xportr_write("datasets/ADAM/adaette.xpt")
