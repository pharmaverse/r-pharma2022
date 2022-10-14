# Art of the Possible

library(tidyverse)
library(haven)

pft_tbl <- read_xpt("datasets/ARD/pft.xpt")
primary_tbl <- read_xpt("datasets/ARD/model.xpt")

## From an ARD, create a plot --------------------------------------------------
plot_dat <- pft_tbl %>%
  filter(
    row_label2 == "FEV1 (L)",
    param %in% c("mean", "sd", "n"),
    col1 %in% c("GSK123456 100 mg", "Placebo")
  ) %>%
  mutate(
    row_label3  = as.numeric(factor(row_label3 , levels = c("Baseline", "Week 4", "Week 8", "Week 12")))
  ) %>%
  select(
    Visit = row_label3,
    treatment = col1,
    param,
    value
  ) %>%
  pivot_wider(
    names_from = param,
    values_from = value
  ) %>%
  mutate(
    se = sd/sqrt(n)
  )

ggplot(
  data = plot_dat,
  aes(x = Visit,
      y = mean,
      color = treatment,
      group = treatment)
  ) +
  geom_point(,
    size = 3
  ) +
  geom_line(
    size = 1
  ) +
  geom_errorbar(
    aes(
      ymin = mean + se,
      ymax = mean - se
    ),
    width = 0.1,
    size = 1
  ) +
  geom_label(
    data = filter(plot_dat, Visit == 4),
    aes(
      label = treatment,
      x = Visit + 0.2,
      y = mean + 0.08
    ),
    hjust = 1
  ) +

  # ggplot styling
  scale_color_manual(values = c("#4DAF4A", "#377EB8")) +
  theme_bw() +
  labs(y = "FEV1 (L)\nMean ± SE") +
  theme_bw() +
  theme(legend.position = "none",panel.grid.minor.x = element_blank()) +
  scale_x_continuous(
    limits = c(.5, 4.5),
    expand = c(0, 0),
    breaks = c(1, 2, 3, 4),
    labels = c("Baseline","Week 4", "Week 8", "Week 12")
  )


## Build a Primary Results Table
library(tfrmt)
library(gt)
library(gtExtras)

### Define ARD columns of importance ------------------------------------------

## Sort out which columns exist, and what they contain

primary_results_tfrmt <- tfrmt(
  group = "pfts",
  column = c(span_col, lower_col),
  param = param,
  label = VISIT,
  value = "value",
  sorting_cols = ord1,
)

## NOTE: Since we do not know which columns are actually in the data, if you
## have an object defined in your environment, when you create a tfrmt that
## requires a column name that is the same as the object, use quotes. otherwise
## it will use the objects values:

fake_obj <- "132456"

tfrmt(column = fake_obj)$column
tfrmt(column = "fake_obj")$column

### Define Body Plan -----------

# - frmt_structure defines:
#    - Group, Row, and format to apply
# - format types:
#     Covered Previously
#      - frmt()
#      - frmt_combine()


primary_tbl %>%
  dplyr::group_by(VISIT) %>%
  dplyr::summarise(
    param_grp = paste(unique(param), collapse = ", ")
  ) %>%
  dplyr::group_by(param_grp) %>%
  dplyr::summarise(
    VISIT = paste(unique(VISIT), collapse = ", ")
  )


primary_results_tfrmt <- primary_results_tfrmt %>%
  tfrmt(
  body_plan = body_plan(

    ## For all groups and labels, combine `conf.low` and `conf.high` together, rounding to 4 decimals
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("[{conf.low}, {conf.high}]",
                                frmt("x.xxxx"))),

    ## For all groups and labels, apply rounding to 4 decimals to estimate, and 5 decimals to std.error
    frmt_structure(group_val = ".default", label_val = ".default", estimate = frmt("x.xxxx")),
    frmt_structure(group_val = ".default", label_val = ".default", std.error = frmt("x.xxxxx")),

    ## For all groups and labels, conditionally format p.value such that
    ## when the value is less than .001, display "<0.001", when the
    ## value is greater than .99, display ">0.99", and otherwise round to
    ## 3 decimals
    frmt_structure(group_val = ".default", label_val = ".default",
                   p.value = frmt_when(
                     "<0.001" ~ "<0.001",
                     ">0.99" ~ ">0.99",
                     TRUE ~ frmt("x.xxx")
                   )
    )
  )
)


### Define Row Group Plan -----------------------------------------------------

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


### Define col_plan -----------------------------------------------------------

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

primary_tbl %>% distinct(span_col, lower_col)


primary_results_tfrmt <- primary_results_tfrmt %>%
  tfrmt(
    ## Define order of columns
    col_plan = col_plan(
      pfts,
      VISIT,
      span_structure(
        span_col = c(`GSK123456 100 mg`, Placebo),
        lower_col = c(`Adjusted Mean`, `(SE)`)
      ),
      span_structure(
        span_col = `GSK123456 100 mg - Placebo`,
        lower_col = c(Difference, `[95% CI]`, pValue = `p-value`)
      ),
      -starts_with("ord")
    )
  )


primary_gt <- print_to_gt(primary_results_tfrmt, primary_tbl)

### Layering new Body Plan Components-------------------------------------------

### Apply additional styling as needed, say for using scientific notation for
### small p-values


primary_results_tfrmt_alt <- primary_results_tfrmt %>%

  tfrmt(
  # new formatting for p-values
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
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
      columns = contains('pValue'),
      rows = grepl("WEEK 12", x = VISIT)
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

