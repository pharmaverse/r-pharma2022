library(Tplyr)
library(dplyr)

# Load in SDTM ------------------------------------------------------------
adsl <- read_xpt("datasets/ADAM/adsl.xpt")

adsl %>% 
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
  get_numeric_data()
