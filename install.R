# Install script

# General Packages
gen_pakages <- c(
  "tidyverse",
  "haven",
  "gt",
  "gtExtras",
  "devtools",
  "nlme",
  "emmeans",
  "broom"
)

#Check which packages you need
needed_pkgs <- gen_pakages[which(!gen_pakages %in% installed.packages())]
install.packages(needed_pkgs)

library(devtools)

# Pharmaverse packages
install_github("GSK-Biostatistics/tfrmt")
install_github("atorus-research/Tplyr@gh_issue_53_desc")
install.packages("metacore")
install.packages("metatools")
install.packages("admiral")
install.packages("xportr")
