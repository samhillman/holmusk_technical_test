# libraries ---------------------------------------------------------------

library(tidyverse) # for data munging
library(lubridate) # for date munging
library(here) # for good practice in filepaths
library(glmmTMB) # modelling package
library(DHARMa) # checking model assumptions
library(testthat) # for basic data tests
library(broom) # for model output
library(gt) # for model output
library(gtsummary) # for model output
library(gtforester) # for model output

# data read in ------------------------------------------------------------

bill_amount <- read_csv(here("data/bill_amount.csv"))
bill_id <- read_csv(here("data/bill_id.csv"))
clinical_data <- read_csv("data/clinical_data.csv")
demographics <- read_csv("data/demographics.csv")

# problem set -------------------------------------------------------------

# MDD is typically treated with antidepressants and psychological therapy.
# Individuals who do not respond to an antidepressant therapy may be co-prescribed an adjunctive
# medication (e.g., antipsychotic, anticonvulsant, or lithium) alongside their antidepressant
# to help improve their symptoms

# what could we look at?

# Co-prescription changes over time
# Efficacy of different co-prescription options (changes in symptoms compared to baseline)
# Cost comparisons of different treatment
# Does co-prescription reduce inpatient costs
# Toxicity/symptoms of different co-prescription options
# Length of inpatient stay by co-prescription

# We'll choose to look at inpatient stay length by treatment combinations

# Data tests and quality --------------------------------------------------

# We know there are two tables that we want to use for this  - demographics and clinical
# Need to make sure the data is correct and remove discrepant results. For a full study,
# this would be unit tests and more in-depth code.

# looks like all of these need to be standardised!
demographics |> count(gender)
demographics |> count(race)
demographics |> count(resident_status)

# the dates look reasonable
demographics |> summarise(
  min = min(date_of_birth),
  max = max(date_of_birth)
)

# admission / release dates look reasonable
clinical_data |>
  mutate(across(contains("date"), ~ lubridate::dmy(.))) |>
  summarise(
    min_admit = min(date_of_admission),
    max_admit = max(date_of_admission),
    min_disc = min(date_of_discharge),
    max_disc = max(date_of_discharge)
  )

# some 1/0 / yes/no overlap among the data here
clinical_data |> count(medical_history_hbp)

# heights and weights look reasonable
clinical_data |>
  summarise(
    min_weight = min(weight),
    max_weight = max(weight),
    min_height = min(height),
    max_height = max(height)
  )

# data munging ------------------------------------------------------------

# need to format some data for modelling

# demographics first
# we're also adding factors here for ease of modelling later - could be done
# in a separate step for clarity but we're going for a quick turnaround here!
demographics_formatted <-
  demographics |>
  mutate(
    gender = case_when(
      gender == "f" ~ "Female",
      gender == "m" ~ "Male",
      TRUE ~ gender
    ),
    gender = factor(gender, levels = c("Male", "Female")),
    race = case_when(
      race == "India" ~ "Indian",
      TRUE ~ race
    ),
    race = tolower(race),
    race = tools::toTitleCase(race),
    race = factor(race, levels = c("Chinese", "Malay", "Indian", "Others")),
    resident_status = case_when(
      resident_status == "Singapore citizen" ~ "Singaporean",
      TRUE ~ resident_status
    ),
    resident_status = factor(resident_status, levels = c("Singaporean", "PR", "Foreigner"))
  )

demographics_formatted |> count(gender)
demographics_formatted |> count(race)
demographics_formatted |> count(resident_status)

# clinical data
clinical_data_formatted <-
  clinical_data |>
  mutate(across(contains("date"), ~ lubridate::dmy(.)),
    medical_history_hbp = case_when(
      medical_history_hbp == "No" ~ 0,
      medical_history_hbp == "Yes" ~ 1
    ),
    medical_history_hbp = as.numeric(medical_history_hbp)
  )


# length of stay by treatment categories ----------------------------------

# Now we've cleaned the data we can create our variables for modelling. Need duration of stay,
# treatment categories, and age at inpatient stay

# two approaches here.
# 1) With a smaller sample size - does the addition of another pharmaceutical intervention reduce inpatient stay time?
# 2) with a larger sample - what combinations of pharmaceutical interventions reduce inpatient stay time?

# We'll first create our clinical data -
# We'll calculate the length of stay, keep only patients that have had SSRIs, and then create our
# treatment categories. We'll also add the factor levels here for modelling

clinical_data_ssri_only <-
  clinical_data |>
  mutate(
    across(contains("date"), ~ lubridate::dmy(.)),
    length_of_stay = as.numeric(difftime(date_of_discharge, date_of_admission, units = "days"))
  ) |>
  filter(trt_ssr == 1) |> # Ensure everyone has SSRIs
  mutate(
    # Define treatment_combination with ordered factor levels
    treatment_combination = factor(case_when(
      trt_anx == 0 & trt_con == 0 & trt_oth == 0 ~ "SSRI Only",
      trt_anx == 1 & trt_con == 0 & trt_oth == 0 ~ "SSRI + Anxiety Treatment",
      trt_anx == 0 & trt_con == 1 & trt_oth == 0 ~ "SSRI + Anticonvulsants",
      trt_anx == 0 & trt_con == 0 & trt_oth == 1 ~ "SSRI + Other Psychiatric Medications",
      trt_anx == 1 & trt_con == 1 & trt_oth == 0 ~ "SSRI + Anxiety Treatment + Anticonvulsants",
      trt_anx == 1 & trt_con == 0 & trt_oth == 1 ~ "SSRI + Anxiety Treatment + Other Psychiatric Medications",
      trt_anx == 0 & trt_con == 1 & trt_oth == 1 ~ "SSRI + Anticonvulsants + Other Psychiatric Medications",
      trt_anx == 1 & trt_con == 1 & trt_oth == 1 ~ "SSRI + Anxiety Treatment + Anticonvulsants + Other Psychiatric Medications"
    ), levels = c(
      "SSRI Only",
      "SSRI + Anxiety Treatment",
      "SSRI + Anticonvulsants",
      "SSRI + Other Psychiatric Medications",
      "SSRI + Anxiety Treatment + Anticonvulsants",
      "SSRI + Anxiety Treatment + Other Psychiatric Medications",
      "SSRI + Anticonvulsants + Other Psychiatric Medications",
      "SSRI + Anxiety Treatment + Anticonvulsants + Other Psychiatric Medications"
    )),
    # Define treatment_category with SSRI Only as the first level
    treatment_category = fct_collapse(treatment_combination,
      "SSRI Only" = "SSRI Only",
      "SSRI Plus Any Other Treatment" = c(
        "SSRI + Anxiety Treatment",
        "SSRI + Anticonvulsants",
        "SSRI + Other Psychiatric Medications",
        "SSRI + Anxiety Treatment + Anticonvulsants",
        "SSRI + Anxiety Treatment + Other Psychiatric Medications",
        "SSRI + Anticonvulsants + Other Psychiatric Medications",
        "SSRI + Anxiety Treatment + Anticonvulsants + Other Psychiatric Medications"
      )
    )
  ) |>
  mutate(treatment_category = factor(treatment_category, levels = c("SSRI Only", "SSRI Plus Any Other Treatment")))


# For the demographic data we'll add age at admission. We'll need to join the datasets to do this first
# add in demographic variables
clinical_and_demographic_data <-
  clinical_data_ssri_only |>
  left_join(demographics_formatted, by = c("id" = "patient_id")) |>
  # let's create some more demographic variables
  mutate(
    age_at_admission_days = difftime(date_of_admission, date_of_birth, units = "days"),
    age_at_admission_years = age_at_admission_days / 365.25
  )


# unit tests --------------------------------------------------------------

# Simple unit tests to check our conversions and munging looks OK.
# This would be expanded for real studies

# Test Date Conversion
test_that("Date columns are Date objects", {
  expect_s3_class(clinical_and_demographic_data$date_of_discharge, "Date")
  expect_s3_class(clinical_and_demographic_data$date_of_admission, "Date")
})

# Test Length of Stay Calculation
test_that("Length of stay is calculated correctly", {
  calculated_length <- as.numeric(difftime(clinical_and_demographic_data$date_of_discharge, clinical_and_demographic_data$date_of_admission, units = "days"))
  expect_equal(clinical_and_demographic_data$length_of_stay, calculated_length)
  expect_true(all(clinical_and_demographic_data$length_of_stay >= 0))
})

# Test everyone has SSRIs in our dataset
test_that("Data contains only SSRI treatments", {
  expect_true(all(clinical_data_ssri_only$trt_ssr == 1))
})

# Test Age at Admission Calculation
test_that("Age at admission is calculated correctly", {
  calculated_age_days <- difftime(clinical_and_demographic_data$date_of_admission, clinical_and_demographic_data$date_of_birth, units = "days")
  calculated_age_years <- calculated_age_days / 365.25
  expect_equal(clinical_and_demographic_data$age_at_admission_days, calculated_age_days)
  expect_equal(clinical_and_demographic_data$age_at_admission_years, calculated_age_years)
  expect_true(all(clinical_and_demographic_data$age_at_admission_years >= 0))
})

# Data Completeness and Format
test_that("Data completeness and format checks", {
  expect_true(all(!is.na(clinical_and_demographic_data$length_of_stay)))
  expect_true(all(!is.na(clinical_and_demographic_data$treatment_category)))
  expect_true(all(!is.na(clinical_and_demographic_data$age_at_admission_years)))
  expect_is(clinical_and_demographic_data$treatment_combination, "factor")
  expect_is(clinical_and_demographic_data$treatment_category, "factor")
})

# data modelling ----------------------------------------------------------

# we're interested in modelling the length of inpatient stay as a function of treatment.
# Let's have a look and see what this looks like

# very normally distributed!
clinical_and_demographic_data |>
  ggplot(aes(x = length_of_stay)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Length of inpatient stay (days)") +
  facet_wrap(~treatment_combination)

# nothing too weird here
clinical_and_demographic_data |>
  ggplot(aes(x = floor(age_at_admission_years))) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Age at inpatient admnission (years)")

# changes in co-prescribing per year?
clinical_and_demographic_data |>
  mutate(admission_year = floor_date(date_of_admission, unit = "years")) |>
  ggplot(aes(x = admission_year)) +
  geom_histogram() +
  facet_wrap(~treatment_combination) +
  theme_bw() +
  labs(x = "Year of inpatient admnission (years)")


# Cox model isn't really appropriate for this data - let's use a LM

# something along the lines of this!
glmmTMB(length_of_stay ~ treatment_category,
  data = clinical_and_demographic_data,
  family = gaussian()
)

# Let's consider the variables we want to include in our model.
# I'm not a fan of step-wise selection - let's use our domain knowledge instead.
# Again, in a full study this would include an in-depth discussion and literature search
# rather than just using the model below

names(clinical_and_demographic_data)

treatment_category_model <- glmmTMB(
  length_of_stay ~ treatment_category +
    age_at_admission_years + gender + race + resident_status +
    cgis_adm,
  data = clinical_and_demographic_data,
  family = gaussian()
)

treatment_combination_model <- glmmTMB(
  length_of_stay ~ treatment_combination +
    age_at_admission_years + gender + race + resident_status +
    cgis_adm,
  data = clinical_and_demographic_data,
  family = gaussian()
)


summary(treatment_category_model)
summary(treatment_combination_model)


# model checks ------------------------------------------------------------

# want to make sure we have a reasonable fitting model even with simulated date
# models seem to fit

simulationOutput_category <- simulateResiduals(fittedModel = treatment_category_model)
plot(simulationOutput_category)
plotResiduals(simulationOutput_category) # Basic residual diagnostics
plotQQunif(simulationOutput_category) # QQ plot for uniformity of residuals
testDispersion(simulationOutput_category) # Test for over/underdispersion
testOutliers(simulationOutput_category) # Test for outliers

simulationOutput_combination <- simulateResiduals(fittedModel = treatment_combination_model)
plot(simulationOutput_combination)
plotResiduals(simulationOutput_combination)
plotQQunif(simulationOutput_combination)
testDispersion(simulationOutput_combination)
testOutliers(simulationOutput_combination)


# model outputs -----------------------------------------------------------

# now we can clean up the model outputs for further use / presentation

treatment_category_model |>
  tbl_regression(
    label = list(
      treatment_category = "Treatments",
      age_at_admission_years = "Age at admission (years)",
      gender = "Gender",
      race ~ "Race",
      resident_status ~ "Resident Status",
      cgis_adm ~ "Clinical Global Impression Scale (numeric, 1-7)"
    )
  ) |>
  bold_labels()

treatment_combination_model |>
  tbl_regression(
    label = list(
      treatment_combination = "Treatments",
      age_at_admission_years = "Age at admission (years)",
      gender = "Gender",
      race ~ "Race",
      resident_status ~ "Resident Status",
      cgis_adm ~ "Clinical Global Impression Scale (numeric, 1-7)"
    )
  ) |>
  bold_labels()


# save out output ---------------------------------------------------------

length_of_stay_distributions <-
  clinical_and_demographic_data |>
  ggplot(aes(x = length_of_stay)) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Length of inpatient stay (days)") +
  facet_wrap(~treatment_combination) +
  theme(
    text = element_text(size = 20),
    strip.text = element_text(size = 12)
  )


age_at_admission_distributions <-
  clinical_and_demographic_data |>
  ggplot(aes(x = floor(age_at_admission_years))) +
  geom_histogram() +
  theme_bw() +
  labs(x = "Age at inpatient admission (years)") +
  theme(
    text = element_text(size = 20),
    strip.text = element_text(size = 12)
  )

prescribing_patterns_over_time <-
  clinical_and_demographic_data |>
  mutate(admission_year = floor_date(date_of_admission, unit = "years")) |>
  ggplot(aes(x = admission_year)) +
  geom_histogram() +
  facet_wrap(~treatment_combination) +
  theme_bw() +
  labs(x = "Year of inpatient admnission (years)") +
  theme(
    text = element_text(size = 20),
    strip.text = element_text(size = 12)
  )


treatment_category_model_gt_table <-
  treatment_category_model |>
  tbl_regression(
    label = list(
      treatment_category = "Treatments",
      age_at_admission_years = "Age at admission (years)",
      gender = "Gender",
      race ~ "Race",
      resident_status ~ "Resident Status",
      cgis_adm ~ "Clinical Global Impression Scale (numeric, 1-7)"
    )
  ) |>
  bold_labels()

treatment_combination_model_gt_table <-
  treatment_combination_model |>
  tbl_regression(
    label = list(
      treatment_combination = "Treatments",
      age_at_admission_years = "Age at admission (years)",
      gender = "Gender",
      race ~ "Race",
      resident_status ~ "Resident Status",
      cgis_adm ~ "Clinical Global Impression Scale (numeric, 1-7)"
    )
  ) |>
  bold_labels()

ggsave(here("output/length_of_stay_distributions.png"), length_of_stay_distributions,
  height = 12, width = 20, units = "in"
)

ggsave(here("output/age_at_admission_distributions.png"), age_at_admission_distributions,
  height = 12, width = 20, units = "in"
)

ggsave(here("output/prescribing_patterns_over_time.png"), prescribing_patterns_over_time,
  height = 12, width = 20, units = "in"
)

treatment_category_model_gt_table |>
  as_gt() |>
  gtsave(here("output/treatment_category_model_gt_table_html.html"))
treatment_category_model_gt_table |>
  as_gt() |>
  gtsave(here("output/treatment_category_model_gt_table_png.png"))

treatment_combination_model_gt_table |>
  as_gt() |>
  gtsave(here("output/treatment_combination_model_gt_table_html.html"))
treatment_combination_model_gt_table |>
  as_gt() |>
  gtsave(here("output/treatment_combination_model_gt_table_png.png"))
