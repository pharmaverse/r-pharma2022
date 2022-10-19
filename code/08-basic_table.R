library(tibble)
library(tfrmt)
library(dplyr)

# Here we will make our first table using tfrmt 

# Create the dataset that matches the slides
data <- tibble(Group = rep(c("Age (y)", "Sex", "Age (y)", "Sex"), c(3, 3, 6,12)),
               Label = rep(c("n", "Mean (SD)", "Male","Female"), c(6, 6,6,6)),
               Column = rep(c("Placebo", "Treatment", "Total"), times = 8),
               Param = rep(c("n", "mean", "sd", "n", "pct", "n", "pct"),  c(6, 3, 3, 3,3,3,3)),
               Value = c(15,13,28,14,13,27,73.56, 74.231,71.84,9.347,7.234,8.293,8,7,15,8/14,7/13,15/27,6,6,12,6/14,6/13,12/27
               )
) %>% 
  # Note because tfrmt only does rounding we will need to have the percents multiplied by 100 
  mutate(Value = case_when(Param == "pct" ~ Value * 100,
                           TRUE ~ Value), 
         ord1 = if_else(Group == "Age (y)", 1, 2),
         ord2 = if_else(Label == "n", 1, 2)) 
data


# set columns in tfrmt ----------------------------------------------------

first_tfrmt <- tfrmt(
  group = Group,
  label = Label, 
  column = Column, 
  param = Param, 
  value = Value
)

# now print this out ------------------------------------------------------
first_tfrmt %>% 
  print_to_gt(data)



# add body_plan ------------------------------------------------------------

tfrmt(
  group = Group,
  label = Label, 
  column = Column, 
  param = Param, 
  value = Value,
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", 
                   frmt_combine("{n} ({pct}%)",
                                n = frmt("XX"),
                                pct = frmt("x.x"))
    )
  )
) %>% 
  print_to_gt(data)

# Notice that some of the ns don't have percents with them so those show up as
# missing. Also, the means and sds don't have any formatting applied to them.
# So, lets add some more formatting.

rounded_tfrmt <- tfrmt(
  group = Group,
  label = Label, 
  column = Column, 
  param = Param, 
  value = Value,
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", 
                   frmt_combine("{n} ({pct}%)",
                                n = frmt("XX"),
                                pct = frmt("xx.x"))),
    frmt_structure(group_val = ".default", label_val = "n",
                   frmt("XX")),
    frmt_structure(group_val = "Age (y)", label_val = "Mean (SD)",
                   frmt_combine("{mean} ({sd})",
                                mean = frmt("xx.x"),
                                sd = frmt("x.xx")))
  )
) 

rounded_tfrmt %>% 
  print_to_gt(data)



# add col_plan ------------------------------------------------------------

# It looks better, but now the rows are out of order and we would like to get
# rid of those order columns. By this point, we have done lots of typing. I
# don't really want to write all that again. So we are going to layer.

# this orders the rows 
rounded_tfrmt %>% 
  tfrmt(
    sorting_cols = c(ord1, ord2),
  ) %>% 
  print_to_gt(data)

# add in the col plan 
ordered_tfrmt <- rounded_tfrmt %>% 
  tfrmt(
    sorting_cols = c(ord1, ord2),
    col_plan = col_plan(-starts_with("ord"))
  ) 

ordered_tfrmt %>% 
  print_to_gt(data)

# add row_grp_plan -------------------------------------------------------


# Looking even better!! The last thing we will do is add some formatting of the
# row groups and their labels.

# Indent nested row labels and add a space after groups of rows
added_space <- ordered_tfrmt %>% 
  tfrmt(row_grp_plan = 
          row_grp_plan(
            row_grp_structure(group_val = ".default", element_block(post_space = "  "))
          )
  ) 

added_space %>% 
  print_to_gt(data)


# Here is the full tfrmt  -------------------------------------------------

tfrmt(
  group = Group,
  label = Label,
  column = Column,
  value = Value,
  param = Param, 
  sorting_cols = c(ord1, ord2),
  body_plan = body_plan(
    frmt_structure(group_val = ".default", 
                   label_val = ".default",
                   frmt_combine("{n} ({pct})",
                                n = frmt("X"),
                                pct = frmt("xx.x")
                   )
    ),
    frmt_structure(group_val = ".default", label_val = "n", frmt("XX")),
    frmt_structure(group_val = "Age (y)", label_val = "Mean (SD)", 
                   frmt_combine("{mean} ({sd})", 
                                mean = frmt("XX.X"),
                                sd = frmt("x.xx")
                   )
    )
  ),
  col_plan = col_plan(-starts_with("ord")),
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block(post_space = " "))
  )
) %>% 
  print_to_gt(data)


# templates ---------------------------------------------------------------

# One of the great things about layering is that you can make templates to be
# reused for lots of tables. 
tfrmt_n_pct() %>%
  tfrmt(group = Group,
        label = Label,
        column = Column,
        value = Value,
        param = Param, 
        sorting_cols = c(ord1, ord2),
        body_plan = body_plan(
          frmt_structure(group_val = ".default", label_val = "n", frmt("X")),
          frmt_structure(group_val = "Age (y)", label_val = "Mean (SD)", 
                         frmt_combine("{mean} ({sd})",   
                                      mean = frmt("XX.X"),
                                      sd = frmt("x.xx")
                         )
          )
        ),
        col_plan = col_plan(-starts_with("ord")),
        row_grp_plan = row_grp_plan(
          row_grp_structure(group_val = ".default", element_block(post_space = " "))
        )
  ) %>%
  print_to_gt(data)


