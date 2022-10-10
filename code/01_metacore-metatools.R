
# Loading the packages we will need 
library(metacore)
library(tidyverse)
library(admiral)
library(haven)
library(metatools)

# Read in the metadata 
metacore<- spec_to_metacore("specs/specs.xlsx", where_sep_sheet = FALSE)

# Read the data in 
dm <- read_xpt("datasets/SDTM/dm.xpt")

# Get the specifications for the dataset we are currently building
adsl_spec <- metacore %>% 
  select_dataset("ADSL")

# Pull together all the predecessor variables 
adsl_pred <- build_from_derived(adsl_spec, 
                                ds_list = list("dm" = dm), 
                                keep = TRUE) %>% 
  filter(ARMCD %in% c("A", "P"))

# Now we have some variables lets check what is missing
check_variables(adsl_pred, adsl_spec)

# Once all the predecessors combined lets make some code/decode variables 
get_control_term(adsl_spec, SEXN)

adsl_pred %>%  
  create_var_from_codelist(adsl_spec, SEX, SEXN)

adsl_decode <- adsl_pred %>%  
  create_var_from_codelist(adsl_spec, SEX, SEXN) %>% 
  create_var_from_codelist(adsl_spec, ETHNIC, ETHNICN) %>% 
  create_var_from_codelist(adsl_spec, ARMCD, TRT01PN) %>% 
  create_var_from_codelist(adsl_spec, ACTARMCD, TRT01AN) %>%
  create_var_from_codelist(adsl_spec, ARMCD, TRT01P) %>% 
  create_var_from_codelist(adsl_spec, ACTARMCD, TRT01A)

# With the code/decode 
check_variables(adsl_decode, adsl_spec)
# Now we want to get categorize age into subgroups
adsl_decode %>% 
  create_cat_var(adsl_spec, AGE, AGEGR1) %>% 
  select(USUBJID, AGE, AGEGR1)

adsl <- adsl_decode %>% 
  create_cat_var(adsl_spec, AGE, AGEGR1, AGEGR1N)

check_variables(adsl, adsl_spec)
