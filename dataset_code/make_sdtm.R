# Make the SDTM datasets
library(tidyverse)
library(lubridate)
library(xportr)
library(safetyData)
library(mvtnorm)
set.seed(1234)

# Start DM ----

country_count <- rpois(8, 39)
tot <- sum(country_count)
countries <- c(
  "CAN",
  "CZE",
  "ESP",
  "FRA",
  "ITA",
  "JPN",
  "POL",
  "USA"
)

per_arm <- 100
arms <- c(rep(c("A", "P"), per_arm), rep("SCRNFAIL", tot - 200))

race <- c(
  "AMERICAN INDIAN OR ALASKA NATIVE",
  "ASIAN",
  "BLACK OR AFRICAN AMERICAN",
  "MULTIPLE",
  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
  "WHITE"
)

temp_dm <- tibble(
  STUDYID = "GSK123456",
  DOMAIN = "DM",
  COUNTRY = rep(countries, country_count),
  SUBJID = 1000 + as.numeric(as.factor(COUNTRY)) * 100,
  SITEID = as.numeric(as.factor(COUNTRY)) * 100 + 1,
  ARMCD = sample(arms, tot, replace = FALSE),
  ACTARMCD = ARMCD,
  ARM = case_when(
    ARMCD == "A" ~ "GSK123456 100 mg",
    ARMCD == "P" ~ "Placebo",
    ARMCD == "SCRNFAIL" ~ "Screen Failure"
  ),
  ACTARM = ARM,
  # RFSTDTC = if_else(ACTARMCD == "SCRNFAIL", as_date(NA), start_date),
  # RFXSTDTC = RFSTDTC,
  AGE = round(rnorm(tot, 55, 10)),
  AGEU = "YEARS",
  SEX = sample(c("F", "M"), tot, replace = TRUE)
) %>%
  group_by(COUNTRY) %>%
  mutate(
    SUBJID = SUBJID + row_number(),
    AGE = if_else(AGE < 18, 20, AGE),
    USUBJID = paste0("GSK123456-", SUBJID)
  ) %>%
  rowwise() %>%
  mutate(
    RACE = case_when(
      COUNTRY == "JPN" ~ "ASIAN",
      TRUE ~ sample(race, 1, replace = TRUE)
    ),
    ETHNIC = if_else(COUNTRY == "USA",
      sample(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"),
        1,
        prob = c(0.2, 0.8)
      ),
      "NOT HISPANIC OR LATINO"
    )
  ) %>%
  ungroup()

# Make TV ----
total_days <- 84
tv <- tibble(
  STUDYID = "GSK123456",
  DOMAIN = "TV",
  VISIT = c("SCREENING", "BASELINE", "WEEK 4", "WEEK 8", "WEEK 12"),
  VISITDY = c(-7, 1, 28, 56, total_days)
) %>%
  mutate(VISITNUM = row_number())


# Make SV ----
start_org <- dmy("1/1/2020")
start_date <- start_org + days(round(runif(tot, 1, 350)))

sv <- temp_dm %>%
  select(STUDYID, DOMAIN, USUBJID, ACTARMCD) %>%
  mutate(
    DOMAIN = "SV",
    org_date = start_date
  ) %>%
  left_join(select(tv, -DOMAIN), by = "STUDYID") %>%
  mutate(
    SVSTDTC = if_else(ACTARMCD != "SCRNFAIL" | VISITDY < 0,
      org_date + days(VISITDY),
      as_date(NA)
    ),
    SVENDTC = SVSTDTC
  ) %>%
  group_by(USUBJID) %>%
  mutate(SVSEQ = row_number()) %>%
  ungroup() %>%
  select(-org_date, -ACTARMCD) %>%
  filter(!is.na(SVSTDTC))




# Finish off DM ----
dm <- sv %>%
  select(USUBJID, VISIT, SVSTDTC) %>%
  filter(VISIT %in% c("BASELINE", "WEEK 12")) %>%
  pivot_wider(names_from = VISIT, values_from = SVSTDTC) %>%
  rename(RFSTDTC = BASELINE, RFENDTC = `WEEK 12`) %>%
  right_join(temp_dm, by = "USUBJID") %>%
  mutate(
    RFXSTDTC = RFSTDTC,
    RFXENDTC = RFENDTC
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, SUBJID, RFSTDTC, RFENDTC,
    RFXSTDTC, RFXENDTC, SITEID, everything()
  )



# Make EX ----
ex <- sv %>%
  select(STUDYID, USUBJID, VISIT, VISITNUM, VISITDY, EXSTDTC = SVSTDTC) %>%
  left_join(select(dm, USUBJID, EXTRT = ACTARM), by = "USUBJID") %>%
  filter(EXTRT != "SCREEN FAILURE", VISIT != "SCREENING") %>%
  mutate(
    EXDOSE = if_else(EXTRT == "Placebo", 0, 100),
    EXDOSEU = "mg",
    DOMAIN = "EX"
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    EXSEQ = row_number(),
    EXENDTC = if_else(VISIT == "WEEK 8",
      lead(EXSTDTC),
      lead(EXSTDTC) - days(1)
    ),
    EXSTDTC = as.character(EXSTDTC),
    EXENDTC = as.character(EXENDTC)
  ) %>%
  ungroup() %>%
  filter(!is.na(EXENDTC)) %>%
  select(
    STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSEU,
    VISITNUM, VISIT, VISITDY, EXSTDTC, EXENDTC
  )


# Make VS ----
vs_tests <- tribble(
  ~VSTEST, ~VSPOS, ~VSTPT, ~VSTPTREF, ~VSORRESU, ~VSTESTCD,
  "Diastolic Blood Pressure", "SUPINE", "AFTER LYING DOWN FOR 5 MINUTES", "PATIENT SUPINE", "mmHg", "DIABP",
  "Diastolic Blood Pressure", "STANDING", "AFTER STANDING FOR 1 MINUTE", "PATIENT STANDING", "mmHg", "DIABP",
  "Diastolic Blood Pressure", "STANDING", "AFTER STANDING FOR 3 MINUTES", "PATIENT STANDING", "mmHg", "DIABP",
  "Height", NA, NA, NA, "cm", "HEIGHT",
  "Pulse Rate", "SUPINE", "AFTER LYING DOWN FOR 5 MINUTES", "PATIENT SUPINE", "BEATS/MIN", "PULSE",
  "Pulse Rate", "STANDING", "AFTER STANDING FOR 1 MINUTE", "PATIENT STANDING", "BEATS/MIN", "PULSE",
  "Pulse Rate", "STANDING", "AFTER STANDING FOR 3 MINUTES", "PATIENT STANDING", "BEATS/MIN", "PULSE",
  "Systolic Blood Pressure", "SUPINE", "AFTER LYING DOWN FOR 5 MINUTES", "PATIENT SUPINE", "mmHg", "SYSBP",
  "Systolic Blood Pressure", "STANDING", "AFTER STANDING FOR 1 MINUTE", "PATIENT STANDING", "mmHg", "SYSBP",
  "Systolic Blood Pressure", "STANDING", "AFTER STANDING FOR 3 MINUTES", "PATIENT STANDING", "mmHg", "SYSBP",
  "Temperature", NA, NA, NA, "C", "TEMP",
  "Weight", NA, NA, NA, "kg", "WEIGHT"
)

dist <- tribble(
  ~VSTESTCD, ~mean, ~sd,
  "DIABP", 80, 7,
  "HEIGHT", 173, 7,
  "PULSE", 72, 10,
  "SYSBP", 130, 10,
  "TEMP", 36.47, 0.25,
  "WEIGHT", 62, 2
)

vs <- sv %>%
  select(STUDYID, USUBJID,
    VSSEQ = SVSEQ, VISIT, VISITNUM, VISITDY,
    EXSTDTC = SVSTDTC
  ) %>%
  left_join(select(dm, USUBJID, SEX), by = "USUBJID") %>%
  crossing(vs_tests) %>%
  group_by(VSTESTCD) %>%
  group_split() %>%
  map_dfr(function(x) {
    vals <- dist %>%
      filter(VSTESTCD == unique(x$VSTESTCD))
    if (unique(x$VSTESTCD) == "HEIGHT") {
      out <- x %>%
        left_join(tibble(
          USUBJID = unique(x$USUBJID),
          VSORRES = rnorm(n_distinct(x$USUBJID), vals$mean, vals$sd)
        ), by = "USUBJID")
    } else {
      out <- x %>%
        mutate(
          VSORRES = rnorm(nrow(x), vals$mean, vals$sd)
        )
    }
    out
  }) %>%
  mutate(
    VSORRES = case_when(
      SEX == "F" & VSTESTCD == "HEIGHT" ~ VSORRES - 10,
      SEX == "F" & VSTESTCD == "WEIGHT" ~ VSORRES - 2,
      TRUE ~ VSORRES
    ),
    VSORRES = sprintf("%.1f", VSORRES),
    VSSTRESC = VSORRES,
    VSSTRESN = as.numeric(VSORRES),
    VSBLFL = if_else(VISIT == "BASELINE", "Y", NA_character_),
    DOMAIN = "VS"
  ) %>%
  select(-SEX)



# Make AE ----
ae_to_sample <- sdtm_ae %>%
  filter(AESDTH == "N") %>%
  distinct(AEBODSYS, AETERM, AELLT) %>%
  mutate(id = row_number()) %>%
  filter()


ae <- dm %>%
  filter(ARMCD %in% c("A", "P")) %>%
  select(STUDYID, USUBJID, RFSTDTC, RFENDTC) %>%
  mutate(
    n_ae = sample(0:5, 200, replace = TRUE, prob = c(0.05, 0.225, 0.225, 0.225, 0.225, 0.05)),
    id = map(n_ae, ~ sample(1:nrow(ae_to_sample), ., replace = TRUE))
  ) %>%
  unnest(id) %>%
  left_join(ae_to_sample, by = "id") %>%
  select(-n_ae, -id) %>%
  mutate(
    AESTDY = sample(0:(total_days - 1), nrow(.), replace = TRUE),
    AESTDTC = as.character(RFSTDTC + days(AESTDY)),
    AEENDY = map_int(AESTDY, ~ sample(.:total_days, 1)),
    AESEV = sample(c("MILD", "MODERATE", "SEVERE"), nrow(.),
      replace = TRUE, c(0.7, 0.27, 0.03)
    ),
    AEOUT = sample(c("NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED"), nrow(.),
      replace = TRUE, prob = c(0.6, 0.4)
    ),
    AEENDY = if_else(AEOUT == "NOT RECOVERED/NOT RESOLVED", NA_integer_, AEENDY),
    AEENDTC = as.character(RFSTDTC + days(AEENDY)),
    AESDTH = "N",
    DOMAIN = "AE"
  ) %>%
  group_by(USUBJID) %>%
  mutate(AESEQ = row_number()) %>%
  ungroup()

# Make SUPPAE ----
suppae <- ae %>%
  select(STUDYID, RDOMAIN = DOMAIN, USUBJID, IDVARVAL = AESEQ, AETERM, AESEV) %>%
  mutate(
    QNAM = "AESPROT",
    QLABEL = "Protocol Specified Event",
    QVAL = if_else(AESEV != "MILD" &
      AETERM %in% c("COUGH", "RALES", "DYSPNOEA"),
    "Y",
    "N"
    ),
    QORIG = "DERIVED",
    QEVAL = "CLINICAL STUDY SPONSOR",
    IDVAR = "AESEQ"
  ) %>%
  select(-AETERM, -AESEV)

# Make RE ----
retest <- tribble(
  ~RETESTCD, ~RETEST, ~REORRESU,
  "FEV1", "FEV1 (L)", "L",
  "FEV1PD", "Predicted Normal FEV1", "L",
  "FEV1PP", "Percent Predicted FEV1 (%)", "%",
  "PEF", "Peak Expiratory Flow", "L/s",
  "PEFPD", "Predicted Normal PEF", "L/s",
  "PEFPP", "Percent Predicted PEF (%)", "%"
)


# Correlation of the perdicted pfts
ptfs_corr <-
  tibble::tribble(
    ~fev1.scrn, ~fev1.baseline, ~fev1.w4, ~fev1.w8, ~fev1.w12, ~PEF.scr, ~PEF.baseline, ~PEF.w4, ~PEF.w8, ~PEF.w12,
    1, 0.8, 0.6, 0.55, 0.5, 0.6, 0.2, 0.2, 0.2, 0.2,
    0.8, 1, 0.7, 0.6, 0.55, 0.2, 0.6, 0.2, 0.2, 0.2,
    0.6, 0.7, 1, 0.75, 0.6, 0.2, 0.2, 0.8, 0.2, 0.2,
    0.55, 0.6, 0.75, 1, 0.8, 0.2, 0.2, 0.2, 0.6, 0.2,
    0.5, 0.55, 0.6, 0.8, 1, 0.2, 0.2, 0.2, 0.2, 0.6,
    0.6, 0.2, 0.2, 0.2, 0.2, 1, 0.8, 0.6, 0.55, 0.5,
    0.2, 0.6, 0.2, 0.2, 0.2, 0.8, 1, 0.7, 0.6, 0.55,
    0.2, 0.2, 0.8, 0.2, 0.2, 0.6, 0.7, 1, 0.75, 0.6,
    0.2, 0.2, 0.2, 0.6, 0.2, 0.55, 0.6, 0.75, 1, 0.8,
    0.2, 0.2, 0.2, 0.2, 0.6, 0.5, 0.55, 0.6, 0.8, 1
  ) %>%
  as.matrix()

ptfs_sd <- c(rep(5, 5), rep(5, 5))

ptfs_cov <- diag(ptfs_sd) * ptfs_corr * diag(ptfs_sd)

# Get the percent predicted values for the treatment arm
trt_mu <- c(
  c(70, 70, 77, 85, 90),
  c(75, 75, 80, 85, 90)
)

trt_pp <- rmvnorm(per_arm, mean = trt_mu, sigma = ptfs_cov)

# Get the percent predicted values for the placebo arm
placebo_mu <- c(
  rep(70, 5),
  rep(75, 5)
)
# Adding all visits for screening will drop later so combining screening and placebo
p_scrn_pp <- rmvnorm(tot - per_arm, mean = placebo_mu, sigma = ptfs_cov)

all_pp <- rbind(trt_pp, p_scrn_pp)

all_pp_fmt <- all_pp %>%
  as_tibble(rownames = "id") %>%
  pivot_longer(-id) %>%
  group_by(id) %>%
  mutate(
    name = if_else(name %in% paste0("V", 1:5), "FEV1PP", "PEFPP"),
    VISIT = rep(tv$VISIT, 2)
  ) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  nest(data = -id)


re <- vs %>%
  filter(VSTESTCD == "HEIGHT") %>%
  select(USUBJID, HEIGHT = VSSTRESN) %>%
  distinct() %>%
  left_join(select(
    dm, USUBJID, ARMCD,
    AGE, SEX
  ),
  by = "USUBJID"
  ) %>%
  mutate(
    PEFPD = case_when(
      SEX == "M" ~ 1.0523 + 0.08272 * AGE - 0.001301 * AGE^2 + 0.00024962 * HEIGHT^2,
      SEX == "F" ~ 0.9267 + 0.06929 * AGE - 0.001031 * AGE^2 + 0.00018623 * HEIGHT^2
    ),
    FEV1PD = case_when(
      SEX == "M" ~ 0.5536 - 0.01303 * AGE - 0.000172 * AGE^2 + 0.00014098 * HEIGHT^2,
      SEX == "F" ~ 0.4333 - 0.00361 * AGE - 0.000194 * AGE^2 + 0.00011496 * HEIGHT^2
    )
  ) %>%
  arrange(ARMCD) %>%
  bind_cols(all_pp_fmt) %>%
  select(-id, -ARMCD, -AGE, -SEX, -HEIGHT) %>%
  unnest(data) %>%
  right_join(distinct(vs, STUDYID, USUBJID, VISIT, VISITNUM), by = c("USUBJID", "VISIT")) %>%
  mutate(
    FEV1 = FEV1PD * FEV1PP / 100,
    PEF = PEFPD * PEFPP / 100
  ) %>%
  pivot_longer(matches("PEF|FEV"), names_to = "RETESTCD", values_to = "REORRES") %>%
  left_join(retest, by = "RETESTCD") %>%
  mutate(
    REBLFL = if_else(VISIT == "BASELINE", "Y", NA_character_),
    RESTRESN = REORRES,
    RECAT = "PULMONARY FUNCTION TEST"
  ) %>%
  filter(RETESTCD != "PEFPD")


# re %>%
#   filter(RETESTCD  == "FEV1",   VISITNUM > 2) %>%
#   left_join(select(dm, USUBJID, ARM), by = "USUBJID") %>%
#   ggplot(aes(x = VISITNUM, y = REORRES, group = USUBJID )) +
#   geom_line() +
#   facet_grid(cols = vars(ARM))

# Writing out datasets
xportr_write(dm, "datasets/SDTM/dm.xpt", label = "Demographics")
xportr_write(sv, "datasets/SDTM/sv.xpt", label = "Subject Visits")
xportr_write(vs, "datasets/SDTM/vs.xpt", label = "Vital Signs")
xportr_write(ex, "datasets/SDTM/ex.xpt", label = "Exposure")
xportr_write(tv, "datasets/SDTM/tv.xpt", label = "Trial Visits")
xportr_write(ae, "datasets/SDTM/ae.xpt", label = "Adverse Events")
xportr_write(suppae, "datasets/SDTM/suppae.xpt", label = "Supplemental Qualifiers for AE")
xportr_write(re, "datasets/SDTM/re.xpt", label = "Respiratory System Findings")
