library(admiral)
library(haven)
library(xportr)

adsl <- read_xpt("datasets/ADAM/adsl.xpt")
adae <- read_xpt("datasets/ADAM/adae.xpt")
adaette <- read_xpt("datasets/ADAM/adaette.xpt")

# A)
# Derive the time to first protocol specified event (ADAE.AESPORT)
lstalv_censor <- censor_source(
  dataset_name = "adsl",
  date = LSTALVDT,
  set_values_to = vars(
    EVNTDESC = "Last Known Alive Date",
    SRCDOM = "ADSL",
    SRCVAR = "LSTALVDT"
  )
)

prot_src <- event_source(
  dataset_name = "adae",
  date = ASTDT,
  filter = AESPROT == "Y",
  set_values_to = vars(
    EVNTDESC = AEDECOD,
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

adaette_prot <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adae = adae, adsl = adsl),
  start_date = TRTSDT,
  event_conditions = list(prot_src),
  censor_conditions = list(lstalv_censor),
  set_values_to = vars(
    PARAMCD = "PROTAETTE",
    PARAM = "Time to first protocol specified event"
  )
)

# B)
# Derive the time to first overall *serious* adverse event (ADAE.AESER)
ser_ae_src <- event_source(
  dataset_name = "adae",
  date = ASTDT,
  filter = AESER == "Y",
  set_values_to = vars(
    EVNTDESC = AEDECOD,
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

adaette_ser_ae <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adae = adae, adsl = adsl),
  start_date = TRTSDT,
  event_conditions = list(ser_ae_src),
  censor_conditions = list(lstalv_censor),
  set_values_to = vars(
    PARAMCD = "SERAETTE",
    PARAM = "Time to first serious adverse event"
  )
)

# C)
# Derive the time to first moderate or severe adverse event (ASAE.AESEV) in the
# 'RENAL AND URINARY DISORDERS' system organ class
sev_renal_src <- event_source(
  dataset_name = "adae",
  date = ASTDT,
  filter = AESEV %in% c("Moderate", "Severe") & AEBODSYS == "RENAL AND URINARY DISORDERS",
  set_values_to = vars(
    EVNTDESC = AEDECOD,
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

adaette_sev_renal <- derive_param_tte(
  dataset_adsl = adsl,
  source_datasets = list(adae = adae, adsl = adsl),
  start_date = TRTSDT,
  event_conditions = list(sev_renal_src),
  censor_conditions = list(lstalv_censor),
  set_values_to = vars(
    PARAMCD = "SERAETTE",
    PARAM = "Time to first moderate/severe renal and urinary disorder"
  )
)

# D)
# Combine all of the datasets created above into a single datasets and save as
# .xpt file
adaette <- bind_rows(adaette_prot, adaette_ser_ae, adaette_sev_renal)
