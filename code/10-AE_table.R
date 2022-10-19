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




# B) ----------------------------------------------------------------------
# Modify the tfrmt from (A) so that 'row_label2' is the group and 'row_label1'
# is the label. Also add in ord1 and ord 2 as sorting columns, 'sorting_col'.
# Print the updated tfrmt to preview the new table.


# C) ----------------------------------------------------------------------
# Using the tfrmt from (B) add a col_plan to remove the order columns. Also add
# a title to the table. Print the updtated tfrmt. 


