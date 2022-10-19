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
  set_pop_data(adsl)

# B) ----------------------------------------------------------------------
# Calculate the number and percentage of *unique* subjects with any AE
# by adding an additional count layer to the code from 5B. Also add a total
# treatment group. 