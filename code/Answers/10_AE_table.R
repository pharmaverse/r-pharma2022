# Exercise 3 - Making AE Table
#
# Loading the packages we will need 
library(tfrmt)
library(haven)

# Load in ARDs ------------------------------------------------------------
ae <- read_xpt("datasets/ARD/ae.xpt")

# Before we get started lets look at the dataset quickly 
ae %>% 
  head(20)

# A) ----------------------------------------------------------------------
# Using the `tfrmt_n_pct()` template included in {tfrmt}, create the foundation
# of a tfrmt for an AE table and print it. Our goal is to have treatment
# across the top with AEs going vertically.  
# (Hint: You will need to supply values for 'n' and 'pct')

ae_tfrmt_base <- tfrmt_n_pct(n = "distinct_n", pct = "distinct_pct")
print_to_gt(ae_tfrmt_base, ae)



# B) ----------------------------------------------------------------------
# Modify the tfrmt from (A) so that 'row_label2' is the group and 'row_label1'
# is the label. Also add in ord1 and ord 2 as sorting columns, 'sorting_col'.
# Print the updated tfrmt to preview the new table.

ordered_ae <- ae_tfrmt_base %>%
  tfrmt(group = row_label2,
        label = row_label1,
        sorting_cols = c("ord1", "ord2")) 

print_to_gt(ordered_ae, ae)

# C) ----------------------------------------------------------------------
# Using the tfrmt from (B) add a col_plan to remove the order columns. Also add
# a title to the table. Print the updtated tfrmt. 

ordered_ae %>% 
  tfrmt(col_plan = col_plan(-starts_with("ord")),
        title = "Adverse Event Table") %>% 
  print_to_gt(ae)
