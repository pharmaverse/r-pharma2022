library(tfrmt)
library(haven)

# Load in ARDs ------------------------------------------------------------
demog <- read_xpt("datasets/ARD/demog.xpt")


# Make demog tfrmt --------------------------------------------------------
# We are going to start with a template, that way some of the work is done for us
demog_tfrmt <- tfrmt_n_pct() %>% 
  # Now I am just going to add columns from the demog dataset we are going to use
  tfrmt(group = row_label2,
        label = row_label1,
        column = col1, 
        param = param,
        value = value,
        sorting_cols = c(ord1, ord2))

print_to_gt(demog_tfrmt, demog)


# Let's finish rounding all the numbers before we get too carried away 
# NOTE: in the console are suggested formatting for unrounded numbers
rounded_demog <- demog_tfrmt %>% 
  tfrmt(
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val ="Median",
                     frmt("xx.x")),
      frmt_structure(group_val = ".default", label_val = "Mean (SD)", 
                     frmt_combine("{mean}, {sd}",
                                  mean = frmt("xx.x"), 
                                  sd = frmt("x.xx"))),
      frmt_structure(group_val = ".default", label_val = c("Min, Max"), 
                     frmt_combine("{min}, {max}",
                                  min = frmt("xx"), 
                                  max = frmt("xx"))),
      frmt_structure(group_val = ".default", label_val = c("Q1, Q3"), 
                     frmt_combine("{q1}, {q3}",
                                  q1 = frmt("xx.x"), 
                                  q3 = frmt("xx.x")))
    )
  ) 
  
print_to_gt(rounded_demog, demog)

# Let's quickly change the indentation. So it is easier to see the groups
indented_demog <- rounded_demog %>% 
  tfrmt(row_grp_plan = row_grp_plan(
    label_loc = element_row_grp_loc("indented")
  )) 

print_to_gt(indented_demog, demog)


# Finally we are going to set the remove the order columns and add a title
indented_demog %>% 
  tfrmt(col_plan = col_plan(-starts_with("ord")),
        title = "Demography Table") %>% 
  print_to_gt(demog)



# Version using multiple templates ----------------------------------------
significance <- tibble::tribble(
  ~row_label1, ~row_label2, ~sigdig,
  ".default", ".default", 0
)

tfrmt_sigdig(significance, 
             group = row_label2,
             label = row_label1,
             param_defaults = param_set("{q1}, {q3}" = 1,
                                   "{min}, {max}" = 0)
             ) %>% 
  tfrmt_n_pct(tfrmt = .) %>% 
  tfrmt(
    column = col1, 
    param = param,
    value = value,
    sorting_cols = c(ord1, ord2),
    row_grp_plan = row_grp_plan(
      row_grp_structure(group_val = ".default", element_block = element_block(post_space = "  ")),
      label_loc = element_row_grp_loc(location = "indent")
    ), 
    col_plan = col_plan(-starts_with("ord"))
    ) %>%
  print_to_gt(demog)
  


