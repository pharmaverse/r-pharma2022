# Exercise 2 - Calculate n(%) for AE's 
library(Tplyr)
library(haven)
library(dplyr)

# Read in datasets 
adsl <- read_xpt("datasets/ADAM/adsl.xpt")
adae <- read_xpt("datasets/ADAM/adae.xpt") 


# A) ----------------------------------------------------------------------
# Calculate the number and percentage of *unique* subjects with at least one AE
# by AEBODSYS, AETERM, and treatment (hint: you will need to use multiple target
# variables in `group_count`)
?set_distinct_by

tplyr_table(adae, TRT01A) %>%
  set_pop_data(adsl) %>% 
  #ANSWER
  add_layer(
    group_count(target_var = vars(AEBODSYS, AETERM)) %>% 
      set_distinct_by(USUBJID) 
  )  %>% 
  get_numeric_data()
  

# B) ----------------------------------------------------------------------
# Calculate the number and percentage of *unique* subjects with any AE
# by adding an additional count layer to the code from 5B. Also add a total
# treatment group. 
tplyr_table(adae, treat_var = TRT01A) %>%
  set_pop_data(adsl) %>% 
  add_layer(
    group_count("Any Body System") %>% 
      set_distinct_by(USUBJID) 
  ) %>%
  #ANSWER
  add_total_group() %>%
  add_layer(
    group_count(target_var = vars(AEBODSYS, AETERM)) %>% 
      set_distinct_by(USUBJID) 
  )  %>% 
  get_numeric_data()