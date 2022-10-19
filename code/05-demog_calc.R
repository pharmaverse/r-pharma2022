library(Tplyr)
library(dplyr)
library(haven)

# Load in SDTM ------------------------------------------------------------
adsl <- read_xpt("datasets/ADAM/adsl.xpt")


# ADSL Analysis -----------------------------------------------------------

# Setup the table. Selecting a treatment column and population flag. Also adding
# a total group
tbl_setup <- adsl %>% 
  tplyr_table(TRT01A, where= SAFFL =="Y") %>%
  add_total_group() 
tbl_setup

# Now we can add our first layer. Let's start with SEX. Here we want to
# calculate the n and the percent so we are going to use 'group_count'
demog_sex <- tbl_setup %>% 
  add_layer(
    group_count(target_var = SEX, by = vars("Sex n (%)"))
  )

demog_sex %>% 
  get_numeric_data()

# Our next layer is AGE and here we want to get the mean, median, min, max, and
# quartiles.  To get that we are going to use 'group_desc' rather than
# 'group_count'

demog_age_sex <- demog_sex %>% 
  add_layer(
    group_desc(target_var = AGE, by = "Age (years)")
  ) 
demog_age_sex %>% 
  get_numeric_data()  %>% 
  tail(18)

# Now let's string everything together! 
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
