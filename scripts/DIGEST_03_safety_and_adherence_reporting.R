############################################################################################
# Name of file: DIGEST_03_safety_and_adherence_reporting.R
#
# Original author: Oluwatobi Oni
# Original date: August, 2025
# Written/run on: RStudio Desktop for macOS version 2025.05.1+513
# Version of R: 4.5.1
#
# Description of content
#       This script computes:
#         - Safety analysis of the trial
#         - Adherence and completion summaries
#
############################################################################################
#-------------------------------------------------------------------------------------------
# Section 1 - Housekeeping
#-------------------------------------------------------------------------------------------
# This section loads the required packages and sets the environment for running the script.

# WORKING DIRECTORY
#   - The local repo "DIGEST" with the project (DIGEST.Rproj) is on the machines's desktop
#     ~/Desktop/DIGEST/
#   - Using a similar directory across machines allows for automation and makes it easy
#     to run the script without any changes being made.

# Important to clean the global environment before beginning analysis
rm(list = ls())

# PACKAGES
# Defining the object 'required_packages' for the required packages
required_packages_03 <- c(
  "tidyverse", # Using the tidyverse packages for data wrangling
  "janitor", # For cleaning column names
  "gtsummary", # For creating publication-ready summary tables
  "lubridate", # To simplify working with dates and times
  "kableExtra", # Enhancing table appearance - making clean readable tables
  "knitr", # For easier reporting
  "flextable", # For creating customisable tables
  "officer", # Makes it easy to export R reports to Microsoft packages
  "survival", # Used for survival analysis
  "here" # For specifying file paths to make finding files easier
)

# Installing any missing packages
# For automation purposes, I am defining the function 'install_if_missing_03' to install
# "required_packages_03" only if not already installed. Helps for reproducility.
install_if_missing_03 <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
invisible(lapply(required_packages_03, install_if_missing_03))

# Loading all packages above
invisible(lapply(required_packages_03, library, character.only = TRUE))

#
#-------------------------------------------------------------------------------------------
# Section 2 - Data Ingestion
#-------------------------------------------------------------------------------------------
# Reading in the cleaned "adverse_events_analysis.csv" dataset as ae_analysis

# Reading in data file

# Defining file paths (using the "here" package)
data_path_03 <- here::here("data", "master")

# Reading in the cleaned and merged dataset
master <- read_csv(file.path(data_path_03, "master_dataset.csv")) %>%
  janitor::clean_names()

ae_analysis <- read_csv(file.path(data_path_03, "adverse_events_analysis.csv")) %>%
  janitor::clean_names()

med_analysis <- read_csv(file.path(data_path_03, "medications_analysis.csv")) %>%
  janitor::clean_names()

# Result of data injestion
# ae_analysis: 210 obs. of 14 variables
# master: 101 obs. of 39 variables
# med_analysis: 420 obs. of 14 variables
#
#-------------------------------------------------------------------------------------------
# Section 3 - Data Preparation
#-------------------------------------------------------------------------------------------
#
ae_data <- ae_analysis %>%
  mutate(
    # Standardising severity levels and causality as factor based on clinical guidelines
    severity = factor(severity, levels = c("mild", "moderate", "severe")),
    causality = factor(causality, levels = c(
      "not related", "unlikely related",
      "possibly related", "probably related", "definitely related"
    )),

    # Creating SAE flag
    sae_flag = ifelse(results_in_death == "y" | life_threatening == "y" |
      requires_hospitalization == "y" | disability == "y" |
      congenital == "y" | medically_significant == "y", "Yes", "No"),

    # Determine if event is treatment-related (possibly/probably/definitely related)
    treatment_related = ifelse(causality %in% c("possibly related", "probably related", "definitely related"),
      "Yes", "No"
    )
  )

#
#-------------------------------------------------------------------------------------------
# Section 4 - MACE and MADE Classification
#-------------------------------------------------------------------------------------------
#
# Define a list of terms that constitute a Major Adverse Cardiovascular Event (MACE)
mace_terms <- c(
  "myocardial infarction", "heart attack", "stroke", "cardiovascular death",
  "coronary revascularization", "unstable angina", "cardiac arrest",
  "atherosclerosis.*myocardial infarction"
)

# Define a list of terms that constitute a Major Adverse Diabetes Event (MADE)
made_terms <- c(
  "diabetic ketoacidosis", "dka", "severe hypoglycemia", "diabetic coma",
  "hyperosmolar hyperglycemic state", "hhs",
  "diabetes-related hospitalization", "diabetic emergency"
)

# Classify events based on the defined MACE/MADE terms and other safety flags
ae_data <- ae_data %>%
  mutate(

    # Create a flag for MACE by searching for any term in `mace_terms` within the event description
    mace_flag = ifelse(str_detect(
      tolower(event_term),
      paste(mace_terms, collapse = "|")
    ), "Yes", "No"),

    # Create a flag for MADE by searching for any term in `made_terms` within the event description
    made_flag = ifelse(str_detect(
      tolower(event_term),
      paste(made_terms, collapse = "|")
    ), "Yes", "No"),

    # Assign a primary event category (MACE, MADE, SAE, or AE) in a hierarchical order
    event_category = case_when(
      mace_flag == "Yes" ~ "MACE",
      made_flag == "Yes" ~ "MADE",
      sae_flag == "Yes" ~ "SAE (Other)",
      TRUE ~ "AE"
    )
  )

#
#-------------------------------------------------------------------------------------------
# Section 5 - Person-Months Calculation
#-------------------------------------------------------------------------------------------
#
# To calculate person month, the enrollment and last followup date will be obtained from
# the master dataset and the data will be merged onto the ae_analysis ae_data

person_time <- master %>%
  # Select key columns for the calculation
  dplyr::select(subject_id, treatment_group, collection_date_t0, collection_date_t1, collection_date_t2, collection_date_t3) %>%
  # Reshape the data to find the last valid date for each subject
  pivot_longer(
    cols = starts_with("collection_date_t"),
    names_to = "time_point",
    values_to = "collection_date",
    values_drop_na = TRUE # This automatically removes rows with NA dates
  ) %>%
  # Group by subject to find their last visit date
  group_by(subject_id) %>%
  mutate(
    baseline_date = first(collection_date[time_point == "collection_date_t0"]),
    last_visit_date = last(collection_date)
  ) %>%
  ungroup() %>%
  # Keep only one row per subject to perform the final calculation
  distinct(subject_id, .keep_all = TRUE) %>%
  # Perform the person-time calculation
  mutate(
    # Convert dates to a 'Date' type to get the difference in days
    follow_up_days = as.numeric(as.Date(last_visit_date) - as.Date(baseline_date)),
    person_months = follow_up_days / 30.44 # Using an average of 30.44 days per month
  ) %>%
  # Select final columns for merging
  dplyr::select(subject_id, treatment_group, follow_up_days, person_months)

# Preview the results
print(person_time)

# Merging the person time with the processed ae_data
safety_analysis <- ae_data %>%
  left_join(person_time, by = "subject_id")

#
#-------------------------------------------------------------------------------------------
# Section 6 - Summary Tables
#-------------------------------------------------------------------------------------------
#
# 6.1 Overall AE/SAE/MACE/MADE Incidence Table
incidence_summary <- safety_analysis %>%
  group_by(treatment_group) %>%
  summarise(
    n_subjects = n_distinct(subject_id),
    total_aes = n(),
    subjects_with_aes = n_distinct(subject_id[!is.na(event_term)]),
    total_saes = sum(sae_flag == "Yes"),
    subjects_with_saes = n_distinct(subject_id[sae_flag == "Yes"]),
    total_mace = sum(mace_flag == "Yes"),
    subjects_with_mace = n_distinct(subject_id[mace_flag == "Yes"]),
    total_made = sum(made_flag == "Yes"),
    subjects_with_made = n_distinct(subject_id[made_flag == "Yes"]),
    total_person_months = sum(unique(person_months), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ae_incidence_rate_per_pm = round((total_aes / total_person_months) * 100, 2),
    sae_incidence_rate_per_pm = round((total_saes / total_person_months) * 100, 2),
    mace_incidence_rate_per_pm = round((total_mace / total_person_months) * 100, 2),
    made_incidence_rate_per_pm = round((total_made / total_person_months) * 100, 2)
  )

print("=== AE/SAE/MACE/MADE Incidence Summary ===")
kable(incidence_summary, caption = "Adverse Event Incidence Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# 6.2 Severity Summary
severity_summary <- safety_analysis %>%
  filter(!is.na(severity)) %>%
  group_by(treatment_group, severity) %>%
  summarise(
    n_events = n(),
    n_subjects = n_distinct(subject_id),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = severity, values_from = c(n_events, n_subjects),
    values_fill = 0
  )

print("=== Severity Summary ===")
kable(severity_summary, caption = "Adverse Event Severity Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# 6.3 Causality/Relatedness Summary
causality_summary <- safety_analysis %>%
  filter(!is.na(causality)) %>%
  group_by(treatment_group, causality) %>%
  summarise(
    n_events = n(),
    n_subjects = n_distinct(subject_id),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = causality, values_from = c(n_events, n_subjects),
    values_fill = 0
  )

print("=== Causality/Relatedness Summary ===")
kable(causality_summary, caption = "Adverse Event Causality Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# 6.4 Detailed Event Category Table
event_category_summary <- safety_analysis %>%
  group_by(treatment_group, event_category) %>%
  summarise(
    n_events = n(),
    n_subjects = n_distinct(subject_id),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = event_category, values_from = c(n_events, n_subjects),
    values_fill = 0
  )

print("=== Event Category Summary ===")
kable(event_category_summary, caption = "Event Category Summary (AE/SAE/MACE/MADE)") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#
#-------------------------------------------------------------------------------------------
# Section 7 - Incidence Rates Per Person-Month
#-------------------------------------------------------------------------------------------
#
# Calculate incidence rates by event type
incidence_rates <- safety_analysis %>%
  group_by(treatment_group) %>%
  summarise(
    total_person_months = sum(unique(person_months), na.rm = TRUE),
    ae_count = n(),
    sae_count = sum(sae_flag == "Yes"),
    mace_count = sum(mace_flag == "Yes"),
    made_count = sum(made_flag == "Yes"),
    treatment_related_count = sum(treatment_related == "Yes"),
    .groups = "drop"
  ) %>%
  mutate(
    ae_rate_per_100pm = round((ae_count / total_person_months) * 100, 3),
    sae_rate_per_100pm = round((sae_count / total_person_months) * 100, 3),
    mace_rate_per_100pm = round((mace_count / total_person_months) * 100, 3),
    made_rate_per_100pm = round((made_count / total_person_months) * 100, 3),
    tr_ae_rate_per_100pm = round((treatment_related_count / total_person_months) * 100, 3)
  )

print("=== Incidence Rates per 100 Person-Months ===")
kable(incidence_rates, caption = "Incidence Rates per 100 Person-Months") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
#
#-------------------------------------------------------------------------------------------
# Section 8 - AE/SAE/MACE/MADE Listings
#-------------------------------------------------------------------------------------------
#
# SAE Listing
sae_listing <- safety_analysis %>%
  filter(sae_flag == "Yes") %>%
  dplyr::select(
    subject_id, treatment_group, event_term, start_date, end_date,
    severity, causality, outcome, event_category
  ) %>%
  arrange(treatment_group, subject_id, start_date)

print("=== SAE Detailed Listing ===")
if (nrow(sae_listing) > 0) {
  kable(sae_listing, caption = "Serious Adverse Events Detailed Listing") %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
} else {
  print("No SAEs reported in the dataset")
}

# MACE/MADE Listing
mace_made_listing <- safety_analysis %>%
  filter(mace_flag == "Yes" | made_flag == "Yes") %>%
  dplyr::select(
    subject_id, treatment_group, event_term, start_date, end_date,
    severity, causality, outcome, event_category
  ) %>%
  arrange(treatment_group, subject_id, start_date)

print("=== MACE/MADE Detailed Listing ===")
if (nrow(mace_made_listing) > 0) {
  kable(mace_made_listing, caption = "MACE/MADE Events Detailed Listing") %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
} else {
  print("No MACE or MADE events identified in the dataset")
}
#
#-------------------------------------------------------------------------------------------
# Section 9 - Saving Safety Analysis Reports
#-------------------------------------------------------------------------------------------
#
# Create a list of the data frames you want to save
safety_reports <- list(
  "AE_Incidence" = incidence_summary,
  "Severity_Summary" = severity_summary,
  "Causality_Summary" = causality_summary,
  "Event_Category_Summary" = event_category_summary
)

# Create the output directory if it doesn't exist
output_dir <- here::here("output", "safety")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Use a loop to write each data frame to a separate CSV file
for (name in names(safety_reports)) {
  file_path <- file.path(output_dir, paste0(name, ".csv"))
  write_csv(safety_reports[[name]], file_path)
}

print("=== All summary tables have been saved as individual CSV files ===")

#
#-------------------------------------------------------------------------------------------
# Section 10 - Adherence/Completion Summaries
#-------------------------------------------------------------------------------------------
#
# Create completion flags based on available data at each timepoint
adherence_data <- master %>%
  mutate(
    # Completion at 3 months (t1) - check if key measurements are available
    completed_3m = !is.na(collection_date_t1) |
      (!is.na(sysbp_t1) | !is.na(weight_t1) | !is.na(hba1c_mmol_mol_t1)),

    # Completion at 6 months (t2) - check if key measurements are available
    completed_6m = !is.na(collection_date_t2) |
      (!is.na(sysbp_t2) | !is.na(weight_t2) | !is.na(hba1c_mmol_mol_t2))
  )

#
#-------------------------------------------------------------------------------------------
# Section 11 - Population Counts and Completion Rates at 3M and 6M
#-------------------------------------------------------------------------------------------
#
# Per-Protocol and Safety Population Counts
population_summary <- adherence_data %>%
  group_by(treatment_group) %>%
  summarise(
    # Population counts
    itt_population = sum(population_itt == TRUE, na.rm = TRUE),
    fas_population = sum(population_fas == TRUE, na.rm = TRUE),
    pp_population = sum(!is.na(population_pp) | population_pp == TRUE, na.rm = TRUE),
    safety_population = sum(population_sp == TRUE, na.rm = TRUE),

    # Total subjects per group
    total_subjects = n(),
    .groups = "drop"
  )

print("=== Population Summary ===")
kable(population_summary, caption = "Per-Protocol and Safety Population Counts") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Overall completion rates by treatment group
completion_summary <- adherence_data %>%
  group_by(treatment_group) %>%
  summarise(
    total_subjects = n(),

    # 3-month completion
    completed_3m_n = sum(completed_3m, na.rm = TRUE),
    completed_3m_rate = round((completed_3m_n / total_subjects) * 100, 1),

    # 6-month completion
    completed_6m_n = sum(completed_6m, na.rm = TRUE),
    completed_6m_rate = round((completed_6m_n / total_subjects) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    # Format for display
    completion_3m = paste0(completed_3m_n, "/", total_subjects, " (", completed_3m_rate, "%)"),
    completion_6m = paste0(completed_6m_n, "/", total_subjects, " (", completed_6m_rate, "%)")
  ) %>%
  dplyr::select(treatment_group, total_subjects, completion_3m, completion_6m)

print("=== Completion Rates Summary ===")
kable(completion_summary,
  caption = "Completion Rates at 3 Months and 6 Months",
  col.names = c("Treatment Group", "Total Subjects", "3-Month Completion", "6-Month Completion")
) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#
#-------------------------------------------------------------------------------------------
# Section 12 - Completion Rates for the Intervention Group
#-------------------------------------------------------------------------------------------
#
# Intervention group only analysis as specified in SAP
intervention_completion <- adherence_data %>%
  filter(treatment_group == "intervention") %>%
  summarise(
    total_subjects = n(),

    # 3-month completion
    completed_3m_n = sum(completed_3m, na.rm = TRUE),
    completed_3m_rate = round((completed_3m_n / total_subjects) * 100, 1),

    # 6-month completion
    completed_6m_n = sum(completed_6m, na.rm = TRUE),
    completed_6m_rate = round((completed_6m_n / total_subjects) * 100, 1),

    # Format for display
    completion_3m = paste0(completed_3m_n, "/", total_subjects, " (", completed_3m_rate, "%)"),
    completion_6m = paste0(completed_6m_n, "/", total_subjects, " (", completed_6m_rate, "%)")
  )

print("=== Intervention Group Completion (SAP Specification) ===")
intervention_table <- data.frame(
  Timepoint = c("3 Months (t1)", "6 Months (t2)"),
  Completed = c(intervention_completion$completion_3m, intervention_completion$completion_6m),
  Rate = c(
    paste0(intervention_completion$completed_3m_rate, "%"),
    paste0(intervention_completion$completed_6m_rate, "%")
  )
)

kable(intervention_table,
  caption = "Intervention Group Completion Rates (as per SAP)",
  col.names = c("Timepoint", "Completed (n/N)", "Completion Rate")
) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#-------------------------------------------------------------------------------------------
# Breakdown by Population Type
# Completion rates within each population subset
population_completion <- adherence_data %>%
  summarise(
    # ITT Population
    itt_total = sum(population_itt == TRUE, na.rm = TRUE),
    itt_3m = sum(population_itt == TRUE & completed_3m == TRUE, na.rm = TRUE),
    itt_6m = sum(population_itt == TRUE & completed_6m == TRUE, na.rm = TRUE),
    itt_3m_rate = round((itt_3m / itt_total) * 100, 1),
    itt_6m_rate = round((itt_6m / itt_total) * 100, 1),

    # Safety Population
    sp_total = sum(population_sp == TRUE, na.rm = TRUE),
    sp_3m = sum(population_sp == TRUE & completed_3m == TRUE, na.rm = TRUE),
    sp_6m = sum(population_sp == TRUE & completed_6m == TRUE, na.rm = TRUE),
    sp_3m_rate = round((sp_3m / sp_total) * 100, 1),
    sp_6m_rate = round((sp_6m / sp_total) * 100, 1),

    # Per-Protocol Population
    pp_total = sum(!is.na(population_pp) | population_pp == TRUE, na.rm = TRUE),
    pp_3m = sum((!is.na(population_pp) | population_pp == TRUE) & completed_3m == TRUE, na.rm = TRUE),
    pp_6m = sum((!is.na(population_pp) | population_pp == TRUE) & completed_6m == TRUE, na.rm = TRUE),
    pp_3m_rate = round((pp_3m / pp_total) * 100, 1),
    pp_6m_rate = round((pp_6m / pp_total) * 100, 1)
  )

# Format the population completion table
population_completion_table <- data.frame(
  Population = c("ITT", "Safety", "Per-Protocol"),
  Total_N = c(
    population_completion$itt_total,
    population_completion$sp_total,
    population_completion$pp_total
  ),
  Completion_3M = c(
    paste0(population_completion$itt_3m, "/", population_completion$itt_total, " (", population_completion$itt_3m_rate, "%)"),
    paste0(population_completion$sp_3m, "/", population_completion$sp_total, " (", population_completion$sp_3m_rate, "%)"),
    paste0(population_completion$pp_3m, "/", population_completion$pp_total, " (", population_completion$pp_3m_rate, "%)")
  ),
  Completion_6M = c(
    paste0(population_completion$itt_6m, "/", population_completion$itt_total, " (", population_completion$itt_6m_rate, "%)"),
    paste0(population_completion$sp_6m, "/", population_completion$sp_total, " (", population_completion$sp_6m_rate, "%)"),
    paste0(population_completion$pp_6m, "/", population_completion$pp_total, " (", population_completion$pp_6m_rate, "%)")
  )
)

print("=== Completion Rates by Population Type ===")
kable(population_completion_table,
  caption = "Completion Rates at 3M/6M by Analysis Population",
  col.names = c("Population", "Total N", "3-Month Completion", "6-Month Completion")
) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# Summary Statistics
#-------------------------------------------------------------------------------------------

print("=== SUMMARY ===")
print(paste("Total subjects in dataset:", nrow(master)))
print(paste("ITT population:", sum(master$population_itt == TRUE, na.rm = TRUE)))
print(paste("Safety population:", sum(master$population_sp == TRUE, na.rm = TRUE)))
print("Completion rates calculated based on availability of key measurements at each timepoint")
#
#
#
###################################### END OF SCRIPT #######################################
