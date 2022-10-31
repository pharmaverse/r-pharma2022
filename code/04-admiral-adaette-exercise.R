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
  dataset_name =
)

adaette_prot <- derive_param_tte()

# B)
# Derive the time to first overall *serious* adverse event (ADAE.AESER)


# C)
# Derive the time to first moderate or severe adverse event (ASAE.AESEV) in the
# 'RENAL AND URINARY DISORDERS' system organ class

# D)
# Combine all of the datasets created above into a single datasets and save as
# .xpt file
