library(admiral)
library(metacore)
library(dplyr)

metacore <- spec_to_metacore("specs/specs.xlsx", where_sep_sheet = FALSE)

adsl <- haven::read_xpt("datasets/ADAM/adsl.xpt")
adae <- haven::read_xpt("datasets/ADAM/adae.xpt")

adaette_spec <- metacore %>%
  select_dataset("ADAETTE")

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
    SRCVAR = "AETERM"
  )
)

nausea_src <- event_source(
  dataset_name = "adae",
  filter = AETERM == "NAUSEA",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = AETERM,
    SRCDOM = "ADAE",
    SRCVAR = "AETERM"
  )
)

protocol_event_src <- event_source(
  dataset_name = "adae",
  filter = AESPROT == "Y",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = AETERM,
    SRCDOM = "ADAE",
    SRCVAR = "AETERM"
  )
)

adaette_any_ae <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adae = adae, adsl = adsl),
  start_date = TRTSDT,
  event_conditions = list(any_ae_src),
  censor_conditions = list(lstalv_censor),
  set_values_to = NULL
)

adaette_nausea <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adae = adae, adsl = adsl),
  start_date = TRTSDT,
  event_conditions = list(nausea_src),
  censor_conditions = list(lstalv_censor),
  set_values_to = NULL
)

adaette_aval <- bind_rows(adaette_any_ae, adaette_nausea) %>%
  derive_vars_duration(
    new_var = AVAL,
    new_var_unit = AVALU,
    start_date = STARTDT,
    end_date = ADT,
    out_unit = "days"
  )

adaette <- adaette_aval %>%
  order_cols(adaette_spec) %>%
  set_variable_labels(adaette_spec) %>%
  check_ct_data(adaette_spec) %>%
  check_variables(adaette_spec)

adaette %>%
  xportr_label(adaette_spec) %>%
  xportr_df_label(adaette_spec) %>%
  xportr_write("datasets/ADAM/adaette.xpt")
