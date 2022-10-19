# Art of the Possible

library(tidyverse)
library(haven)
library(tfrmt)
library(gt)
library(gtExtras)

# ARD Created
primary_tbl <- read_xpt("datasets/ARD/model.xpt") %>%
  mutate(across(everything(),function(x){x[x==""]<-NA;x}))

# Build a Primary Results Table ------------------------------------------------

## Define ARD columns of importance -------------------------------------------

### Sort out which columns exist, and what they contain

head(primary_tbl)

primary_results_tfrmt <- tfrmt(
  group = model_results_category,
  label = measure,
  column = c(visit, trt),
  param = param,
  value = value,
  sorting_cols = ord1,
)

# NOTE: Since we do not know which columns are actually in the data, if you
# have an object defined in your environment, when you create a tfrmt that
# requires a column name that is the same as the object, use quotes. otherwise
# it will use the objects values:

fake_obj <- "132456"

tfrmt(column = fake_obj)$column
tfrmt(column = "fake_obj")$column

## Define Body Plan - Basics  -----------

# - frmt_structure defines:
#    - Group, Row, and format to apply
# - format types:
#     Covered Previously
#      - frmt()
#      - frmt_combine()

primary_tbl %>%
  distinct(param)

primary_tbl %>%
  dplyr::filter(param != "big_n") %>%
  dplyr::group_by(trt, measure) %>%
  dplyr::summarise(
    param_grp = paste(unique(param), collapse = ", ")
  )

# Single param into a single cell
frmt_structure(
  group_val = ".default",
  label_val = ".default",
  estimate = frmt("x.xxxx")
)

frmt_structure(
  group_val = ".default",
  label_val = "Week 4", ## special rounding for week 4 row
  estimate = frmt("x.xxxxx")
)

# Combining params into a single cell
frmt_structure(
  group_val = ".default",
  label_val = ".default",
  frmt_combine("[{conf.low}, {conf.high}]",
               conf.low = frmt("x.xxxx"),
               conf.high = frmt("x.xxxx")
  ))

primary_results_tfrmt <- primary_results_tfrmt %>%
  tfrmt(
    body_plan = body_plan(

      ## by default round all values to 2
      frmt_structure(
        group_val = ".default",
        label_val = ".default",
        frmt("x.xx")
      ),

      ## For all group "Model Estimates", and labels Adjusted
      ## Mean/SE apply rounding to 4 decimals and 5 decimals respectively
      frmt_structure(
        group_val = "Model Estimates",
        label_val = "Adjusted Mean",
        estimate = frmt("x.xxxx")
      ),
      frmt_structure(
        group_val = "Model Estimates",
        label_val = "SE",
        std.error = frmt("x.xxxxx")
      ),

      ## For group value of "Contrast", and label value of
      ## "Difference", round to 4 decimals
      frmt_structure(
        group_val = "Contrast",
        label_val = "Difference",
        estimate = frmt("x.xxxx")
      ),

      ## For group value of "Contrast", and label value of
      ## "95% CI [high, low]", combine `conf.low` and `conf.high` together,
      ## rounding to 4 decimals
      frmt_structure(
        group_val = "Contrast",
        label_val = "95% CI [high, low]",
        frmt_combine("[{conf.low}, {conf.high}]", frmt("x.xxxx"))
      )
    )
  )


## Define Body Plan - Conditional Formatting  ----------

#  What about the p.value param?
#
#  Formatting may depend on the value. More than just rounding, but
#  commonly pvalues are limited to when values are less than 0.05 to
#  just displaying "<0.05", or adding asterisks to significant values, etc
#
#  Conditional Formatting with frmt's with frmt_when!
#  ?frmt_when

# Left side evaluates comparing against _input_ value to format.
# Right side is the frmt or output to be applied to the input value

conditional_frmt <- frmt_when(
  ">=10" ~ frmt("xx.x"),
  ">=1" ~ frmt("x.x"),
  "<1" ~ frmt("x.xx **"),
  "TRUE" ~ "MISSING VALUE"
)

# Preview how the frmt_when may impact the content
apply_frmt(
  frmt_def = conditional_frmt,
  .data = tibble::tibble(x = c(11,9,2,.005,NA)),
  value = rlang::quo(x)
)


primary_results_tfrmt <- primary_results_tfrmt %>%
  tfrmt(
    body_plan = body_plan(
      ## For all groups and labels, conditionally format p.value such that
      ## when the value is less than .001, display "<0.001", when the
      ## value is greater than .99, display ">0.99", and otherwise round to
      ## 3 decimals
      frmt_structure(
        group_val = "Contrast",
        label_val = "p-value",
        p.value = frmt_when(
          "<0.001" ~ "<0.001",
          ">0.99" ~ ">0.99",
          TRUE ~ frmt("x.xxx")
          )
      )
    )
  )


## Define "Big N's" ----------------------------------------------------------

# we know they are included in the ARD, so what are they?

primary_tbl %>%
  dplyr::filter(param == "big_n")


primary_results_tfrmt <- primary_results_tfrmt %>%
  tfrmt(
    ## define "big N" dressings. Values from s
    big_n = big_n_structure(
      param_val = "big_n",
      n_frmt = frmt("\n(N=XX)")
    )
  )

## Define col_plan -----------------------------------------------------------

# We need to define the column order for which we want things to appear.
# by default all columns (between column columns and actual columns in ARD)
# are preserved. To drop non-defined columns, set ".drop" in in col_plan to TRUE.
#
# The column plan takes unquoted columns (can also optionally pass) as quoted.
#
# Behavior is _similar_ to dplyr::select, but goes with "last identified" model
# as opposed to "first identified" that tidyselect does. Renaming works similarly.
#
# If you want to define column orders for spanning header content, use the `span_structure()`
# function. This expects the argname to be the original column name then the
# values are a vector. Renaming uses named vectors.s
#

primary_tbl %>% filter(param != "big_n") %>% distinct(visit, trt)
primary_tbl %>% colnames


primary_results_tfrmt <- primary_results_tfrmt %>%
  tfrmt(
    ## Define order of columns
    col_plan = col_plan(
      model_results_category,
      measure,
      span_structure(
        visit = c(`Week 4`, `Week 8`, `Week 12`),
        trt = c(`Placebo`,`GSK123456 100 mg`)
      ),
      -starts_with("ord")
    )
  )


## Define Row Group Plan -----------------------------------------------------

# Some formatting is related to spacing around groups and row label placement
#
# `row_grp_plan()` is a collection of defining how rows will be displayed
#
#  -`row_grp_structure()` is passed to define how we may style groups and
#  display them. Multiple may be passed to a plan.
#  - `label_loc` argument allows user to define how groups and labels get combined

# set up a structure for blocking based on the groups
# collapse the groups with label and indent

primary_results_tfrmt <- primary_results_tfrmt %>%
  tfrmt(
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "indented")
    )
  )


## Define footnote plan -----------------------------------------------------

#  -`footnote_plan()` defines the set of footnotes to be added, containing 1 or more
# `footnote_structure()`, and the mark type to use.
#
#  -`footnote_structure()` is passed to define
#     - The footnote text
#     - Location of the footnote based on group, label, and columns
#         - specifying one of group, label,  column puts it in the row/column labels
#         - specifying multiple puts it into the table cell
# set up a structure for blocking based on the groups
# collapse the groups with label and indent

primary_results_tfrmt <- primary_results_tfrmt %>%
  tfrmt(
    footnote_plan = footnote_plan(
      footnote_structure(
        "Estimates based on MMRM using an unstructured correlation matrix and allowing distinct variance for each visit",
        group_val = list(model_results_category = c("Model Estimates","Contrast"))
      ),
      footnote_structure(
        "Contrasts based on pairwise contrast method with no adjustment",
        group_val = list(model_results_category = "Contrast"),
        label_val = list(measure = "p-value")
      ),
      footnote_structure(
        "Special footnote to demo calling out a value",
        column_val = list(visit = "Week 12")
      )
    )
  )



# Generate Table ------------------

primary_gt <- print_to_gt(primary_results_tfrmt, primary_tbl)

## New Body Plan Components-------------------------------------------

### Apply additional styling as needed, say for using scientific notation for
### small p-values

primary_results_tfrmt_alt <- primary_results_tfrmt %>%
  tfrmt(
  # new formatting for p-values
  body_plan = body_plan(
    frmt_structure(
      group_val = "Contrast",
      label_val = "p-value",
      p.value = frmt_when(
        ## styling
        "<0.001" ~ frmt("x.xxx", scientific = "x10^xx"),
        ">0.99" ~ ">0.99",
        TRUE ~ frmt("x.xxx")
        )
    )
  )
)

primary_gt_alt <- print_to_gt(primary_results_tfrmt_alt, primary_tbl)



## Add styling as preferred
primary_gt_alt_styled <- primary_gt_alt %>%
  gtExtras::gt_theme_guardian() %>%
  gt::tab_style(
    style = cell_text(
      color = "red",
      style = "italic"
    ),
    locations = cells_body(
      columns = contains('Week 12'),
      rows = grepl("p-value", x = measure)
    )
  )

primary_gt_alt_styled



## Save gt as an html:
# Many other supported outputs (?gtsave)

primary_gt_alt_styled %>%
  gtsave(
    "Primary_Results.html"
  )

primary_gt_alt_styled %>%
  gtsave(
    "Primary_Results.docx"
  )

# MOCKS! ----------------------------------------------------------------------

# Not only does tfrmt allow you to define your tables ahead of time, you can
# define your mocks as well

## provide scaffolding for the data, you can start with
## "Real" data
mock_primary_gt <- print_mock_gt(
  primary_results_tfrmt,
  primary_tbl %>% select(-value)
)

## Generate data that in general should contain rows/column values ----

# what are the expected columns
columns <- crossing(
  trt = c("Placebo","GSK123456 100 mg"),
  visit = c("Week 4", "Week 8", "Week 12")
)

# Construct model esimates section of table
model_ests <- tibble(
  model_results_category = c("Model Estimates"),
  measure = c("Adjusted Mean","(SE)"),
  param = c("estimate","std.error"),
  ord1 = c(1,2)
  ) %>%
  crossing(
    columns
  )

# Construct model contrasts section of table
model_contrasts <- tibble(
    model_results_category = c("Contrast"),
    measure = c("Difference","95% CI [high, low]","95% CI [high, low]","p-value"),
    param = c("estimate","conf.low","conf.high","p.value"),
    ord1 = c(3,4,4,5)
  ) %>%
  crossing(
    columns %>%
      filter(
        trt == "GSK123456 100 mg"
      )
  )

# Construct "big n" param section to indicate where big n will be
# recorded (at the column label level)
big_n <-  tibble(
  param = c("big_n")
  ) %>%
  crossing(
    columns
  )

mock_dat <- bind_rows(
  model_ests,
  model_contrasts,
  big_n
)

mock_primary_gt <- print_mock_gt(
  primary_results_tfrmt,
  mock_dat
)


## let tfrmt guess ----

# tfrmt can take a guess based on provided tfrmt
# this is likely off, but can give you a quick view
# for basic tables.

print_mock_gt(primary_results_tfrmt)


