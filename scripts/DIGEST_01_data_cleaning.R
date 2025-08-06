############################################################################################
# Name of file: DIGEST_01_data_cleaning.R
#
# Original author: Oluwatobi Oni
# Original date: August, 2025
# Written/run on: RStudio Desktop for macOS version 2025.05.1+513
# Version of R: 4.5.1
#
# Description of content
#       This script prepares and cleans the DIGEST trial datasets for analysis,
#       Specifically, it:
#         - Ingests raw CSV files across 8 datasets (AE, BMI, Medications, etc.)
#         - Cleans and standardises individual datasets
#         - Merges datasets by subject ID and timepoint where needed
#         - Prepares a combined analysis dataset with baseline and follow-up data, and
#         - Generates a missing data report across key endpoints (weight, HbA1c, BP, waist)
#
############################################################################################
#-------------------------------------------------------------------------------------------
# Section 1 - Housekeeping
#-------------------------------------------------------------------------------------------
# This section loads the libraries required for data reading, cleaning, and exploration.

# WORKING DIRECTORY
#   - The local repo "DIGEST" with the project (DIGEST.Rproj) is on the machines's desktop
#     ~/Desktop/DIGEST/
#   - Using a similar directory across machines allows for automation and makes it easy 
#     to run the script without any changes being made.

# PACKAGES
# Defining the function 'required_packages' for the required packages
required_packages <- c(
  "tidyverse",    # Using the tidyverse packages for data wrangling
  "janitor",      # For cleaning column names
  "readr",        # For reading in the .csv files
  "lubridate",    # To simplify working with dates and times
  "here"          # For specifying file paths to make finding files easier
)

# Installing any missing packages
# For automation purposes, I am defining the function 'install_if_missing' to install
# "required_packages" only if not already installed. Helps for reproducility.
  install_if_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }
  invisible(lapply(required_packages, install_if_missing))

# Loading all packages above
  invisible(lapply(required_packages, library, character.only = TRUE))

# EXPECTED INPUT FILES
# Located in: data/raw/
# - Adverse Events-DIGEST-2024-12-20.csv
# - BMI Values-DIGEST-2024-12-20.csv
# - Concomitant Medications-DIGEST-2024-12-20.csv
# - Data Dictionary-DIGEST-2024-12-20.csv
# - Demographics-DIGEST-2024-12-20.csv
# - Lab Values-DIGEST-2024-12-20.csv
# - Survey Responses-DIGEST-2024-12-20.csv
# - Vital Signs-DIGEST-2024-12-20.csv
#
#-------------------------------------------------------------------------------------------
# Section 2 - Data Ingestion
#-------------------------------------------------------------------------------------------
# Reading in raw data files
  
# Defining file paths (using the "here" package)
  data_path <- here::here("data", "raw")
  
# Reading in the .csv data frames using the readr package in the tidyverse
# Defining and renaming (shortening) data frames for ease of use
  ae_raw <- read_csv(file.path(data_path, "Adverse Events-DIGEST-2024-12-20.csv")) %>%
    janitor::clean_names()
  
  bmi_raw <- read_csv(file.path(data_path, "BMI Values-DIGEST-2024-12-20.csv")) %>%
    janitor::clean_names()
  
  meds_raw <- read_csv(file.path(data_path, "Concomitant Medications-DIGEST-2024-12-20.csv")) %>%
    janitor::clean_names()
  
  dict_raw <- read_csv(file.path(data_path, "Data Dictionary-DIGEST-2024-12-20.csv")) %>%
    janitor::clean_names()
  
  demo_raw <- read_csv(file.path(data_path, "Demographics-DIGEST-2024-12-20.csv")) %>%
    janitor::clean_names()
  
  lab_raw <- read_csv(file.path(data_path, "Lab Values-DIGEST-2024-12-20.csv")) %>%
    janitor::clean_names()
  
  survey_raw <- read_csv(file.path(data_path, "Survey Responses-DIGEST-2024-12-20.csv")) %>%
    janitor::clean_names()
  
  vitals_raw <- read_csv(file.path(data_path, "Vital Signs-DIGEST-2024-12-20.csv")) %>%
    janitor::clean_names()

# Result of data injestion
# ae_raw: 211 obs. of 38 variables
# bmi_raw: 102 obs. of 21 variables
# demo_raw: 101 obs. of 3 variables 
# dict_raw: 192 obs. of 23 variables
# lab_raw: 342 obs. of 26 variables
# meds_raw: 421 obs. of 17 variables
# survey_raw: 19053 obs. of 21 variables
# vitals_raw: 3764 obs. of 18 variables
#
#-------------------------------------------------------------------------------------------
# Section 3 - Data Cleaning: Demographics Dataset
#-------------------------------------------------------------------------------------------
# cleaning and standardising patient demographics
  
# Standardising patient demographics
  
  demo_clean <- demo_raw %>%
    mutate(
      # Removing leading/trailing spaces
      subject_id = str_trim(subject_id),
      
# Standardising sex to lowercase ("male", "female")
    sex = str_to_lower(str_trim(sex)),
    sex = case_when(
      sex %in% c("male", "m") ~ "male",
      sex %in% c("female", "f") ~ "female",
      TRUE ~ NA_character_
    ),
      
# Fixing DOB parsing, assuming all dates are in mdy format and 
# using 1900s for 2-digit years
  date_of_birth = mdy(date_of_birth),
  date_of_birth = if_else(year(date_of_birth) > year(Sys.Date()), 
                          date_of_birth %m-% years(100), 
                          date_of_birth),
      
# Calculating age as of December 31, 2023 - Confirm recruitment cutoff with Silja
age_years = floor(interval(date_of_birth, ymd("2023-12-31")) / years(1))
    )
  
# Quick checks
  glimpse(demo_clean)
  summary(demo_clean)
  
# Saving the cleaned demographics data
  readr::write_csv(demo_clean, here::here("data", "clean", "demographics.csv"))
#
#-------------------------------------------------------------------------------------------
# Section 4 - Data Cleaning: BMI Values Dataset
#-------------------------------------------------------------------------------------------
# Removing the SDTM-style second header row and cleaning the BMI dataset
  bmi_clean <- bmi_raw %>%
    slice(-1) %>%  # Remove second SDTM-style header
    
# Removing completely empty columns
  select(where(~ !all(is.na(.)))) %>%
    
# Renaming/keeping relevant variables, including timepoint hint variables
  transmute(
    subject_id = unique_subject_identifier,
    sequence_number = as.integer(sequence_number),
    visit = visit_name,
    visit_number = as.integer(visit_number),
    category = category_of_question,
    bmi = as.numeric(numeric_finding_in_standard_units),
    bmi_finding_date = lubridate::mdy(date_time_of_finding),
    study_day = as.integer(study_day_of_finding)
  )  # Added missing closing parenthesis
  
# Preview cleaned BMI data
  glimpse(bmi_clean)
  
# Save the cleaned BMI dataset with a custom filename
  write_csv(bmi_clean, here::here("data", "clean", "bmi.csv"))
#
#-------------------------------------------------------------------------------------------
# Section 5 - Data Cleaning: Adverse Events Dataset
#-------------------------------------------------------------------------------------------
# Re-reading the file without applying column names since column 1 contains useful metadata
  ae_raw_full <- read_csv(
    file.path(data_path, "Adverse Events-DIGEST-2024-12-20.csv"),
    col_names = FALSE
  )
  
# Get row 2 (real headers) as column names
  new_names <- ae_raw_full[2, ] |> unlist() |> as.character() |> janitor::make_clean_names()
  
# Drop rows 1 and 2, assign proper headers
  ae_data <- ae_raw_full[-c(1, 2), ]
  colnames(ae_data) <- new_names

  
  ae_clean <- ae_data %>%
    select(where(~ !all(is.na(.)))) %>%
    transmute(
      subject_id = usubjid,
      seq = as.integer(aeseq),
      event_term = aeterm,
      # More robust approach that handles parsing failures
      start_date = lubridate::parse_date_time(aestdtc, orders = c("ymd", "mdy", "dmy")),
      end_date = lubridate::parse_date_time(aeendtc, orders = c("ymd", "mdy", "dmy")),
      severity = tolower(aesev),
      causality = tolower(aerel),
      outcome = tolower(aeout),
      results_in_death = tolower(aesdth),
      life_threatening = tolower(aeslife),
      requires_hospitalization = tolower(aeshosp),
      disability = tolower(aesdisab),
      congenital = tolower(aescong),
      medically_significant = tolower(aesmie)  # Removed trailing comma
    )
  
# Preview cleaned AE data
  glimpse(ae_clean)
  
# Saving the cleaned AE dataset (event-level for safety analysis)
  write_csv(ae_clean, here::here("data", "clean", "adverse_events.csv"))
#
#-------------------------------------------------------------------------------------------
# Section 6 - Data Cleaning: Lab Values Dataset
#-------------------------------------------------------------------------------------------
# Previewing the lab_raw data frame to see what if looks like, to guide cleaning
  glimpse(lab_raw)
  names(lab_raw)
  
# Re-reading the file without applying column names since column 1 contains useful metadata
  lab_raw_full <- read_csv(
    file.path(data_path, "Lab Values-DIGEST-2024-12-20.csv"),
    col_names = FALSE
  )
  
# Pushing up row 2 (real headers) as column names
  new_names <- lab_raw_full[2, ] |> unlist() |> as.character() |> janitor::make_clean_names()
  
# Droping rows 1 and 2 and assigning proper headers
  lab_data <- lab_raw_full[-c(1, 2), ]
  colnames(lab_data) <- new_names
  
# Checking the actual column names after proper header assignment
  names(lab_data)
  
# Cleaning the lab values dataset, focusing on HbA1c measurements
  lab_clean <- lab_data %>%

# Removing completely empty columns
  select(where(~ !all(is.na(.)))) %>%
    
# Filtering for HbA1c tests only (primary endpoint requirement)
  filter(lbtestcd == "HBA1C") %>%
    
# Selecting and cleaning relevant columns
    transmute(
      subject_id = usubjid,
      sequence_number = as.integer(lbseq),
# visit and visitnum columns are empty - excluding them
      test_code = lbtestcd,
# Removed test_name, category, and units due to redundancy (all same values)
      
# HbA1c value in mmol/mol (standard units from SAP)
    hba1c_mmol_mol = as.numeric(lbstresn),
      
# Converting HbA1c to percentage format as well (SAP mentions both formats)
    hba1c_percent = round((as.numeric(lbstresn) + 46.7) / 28.7, 1),
      
# Important flags and dates
    baseline_flag = case_when(
      lbblfl == "Y" ~ TRUE,
      lbblfl == "N" ~ FALSE,
      TRUE ~ NA
    ),
      
# Fixed date parsing - handling different datetime formats
      collection_date = case_when(
        !is.na(lbdtc) ~ lubridate::ymd_hms(lbdtc, tz = "UTC"),
        TRUE ~ as.POSIXct(NA)
      ),
      
# Metadata timestamps - handle microseconds in timestamps  
      date_recorded = lubridate::ymd_hms(supplbfr, tz = "UTC"),
      date_updated = lubridate::ymd_hms(supplblu, tz = "UTC")
    ) %>%
    
# Keep all records including those with missing HbA1c (needed for missing data report)
# Arrange by subject and collection date for easier review
    arrange(subject_id, collection_date)
  
# Previewing the cleaned lab data
  glimpse(lab_clean)
  
# Check the HbA1c conversion and baseline flags
  lab_clean %>%
    select(subject_id, hba1c_mmol_mol, hba1c_percent, baseline_flag, collection_date) %>%
    head(10)

# Saving the cleaned lab dataset
  write_csv(lab_clean, here::here("data", "clean", "lab_values.csv"))
#
#-------------------------------------------------------------------------------------------
# Section 7 - Data Cleaning: concomitant Medication Dataset
#-------------------------------------------------------------------------------------------
# Previewing the meds_raw data frame to see what if looks like, to guide cleaning
  glimpse(meds_raw)
  names(meds_raw)

# Re-reading the file without applying column names since column 1 contains useful metadata
  meds_raw_full <- read_csv(
    file.path(data_path, "Concomitant Medications-DIGEST-2024-12-20.csv"),
    col_names = FALSE
  )
  
# Promoting row 2 (real headers) as column names
  new_names <- meds_raw_full[2, ] |> unlist() |> as.character() |> janitor::make_clean_names()
  
# Dropping rows 1 and 2; assigning proper headers
  meds_data <- meds_raw_full[-c(1, 2), ]
  colnames(meds_data) <- new_names
  
# Checking the actual column names after proper header assignment
  names(meds_data)
  
# Cleaning the medications dataset
  meds_clean <- meds_data %>%

# Removing completely empty columns
    select(where(~ !all(is.na(.)))) %>%
    
# Selecting and cleaning relevant columns
    transmute(
      subject_id = usubjid,
      sequence_number = as.integer(cmseq),
      medication_name = cmtrt,
      category = cmcat,
      indication = cmindc,
      
# Dose information
    dose = as.numeric(cmdose),
    dose_units = cmdosu,
    frequency = cmdosfrq,
    route = cmroute,
      
# Medication timing
    start_date = case_when(
      !is.na(cmstdtc) ~ lubridate::ymd_hms(cmstdtc, tz = "UTC"),
      TRUE ~ as.POSIXct(NA)
     ),
    end_date = case_when(
      !is.na(cmendtc) ~ lubridate::ymd_hms(cmendtc, tz = "UTC"),
      TRUE ~ as.POSIXct(NA)
    ),
      
# Ongoing status
   ongoing_status = tolower(suppcmong),
      
# Metadata timestamps
    date_recorded = lubridate::ymd_hms(suppcmfr, tz = "UTC"),
    date_updated = lubridate::ymd_hms(suppcmlu, tz = "UTC")
    # Note: study_day column excluded per analyst recommendation (unreliable computed field)
  ) %>%
    
# Keeping all records including those with missing data (needed for missing data report)
# Arranging by subject and sequence for easier review
    arrange(subject_id, sequence_number)
  
# Previewing cleaned medications data
  glimpse(meds_clean)
  
# Checking medication categories and ongoing status
  meds_clean %>%
    count(category, ongoing_status) %>%
    arrange(category, ongoing_status)
  
# Checking dose information
  meds_clean %>%
    count(dose_units, frequency) %>%
    arrange(dose_units, frequency)
  
# Saving the cleaned medications dataset
  write_csv(meds_clean, here::here("data", "clean", "medications.csv"))
#
#-------------------------------------------------------------------------------------------
# Section 8 - Data Cleaning: Vitals Dataset
#-------------------------------------------------------------------------------------------
# Previewing the vitals_raw data frame to see what if looks like, to guide cleaning
  glimpse(vitals_raw)
  names(vitals_raw)

# A look at the dataset shows that vital signs dataset has multiple variables in which 
# some of them define time points and some will be used to assign the treatment groups
  
# Exploring VSCAT values to understand treatment group identification 
# Aim is to obtain the unique values in the variables in order to better understand the data
  
# Re-reading the file without applying column names since column 1 contains useful metadata
  vitals_raw_full <- read_csv(
    file.path(data_path, "Vital Signs-DIGEST-2024-12-20.csv"),
    col_names = FALSE
  )
  
# Promoting the row 2 (real headers) as column names
  new_names <- vitals_raw_full[2, ] |> unlist() |> as.character() |> janitor::make_clean_names()
  
# Dropping rows 1 and 2, assign proper headers
  vitals_data <- vitals_raw_full[-c(1, 2), ]
  colnames(vitals_data) <- new_names
  
# Confirming the actual column names after proper header assignment
  cat("Column names:\n")
  print(names(vitals_data))
  
# Exploring unique values in VSCAT (Category for Vital Signs)
  cat("\n=== UNIQUE VALUES IN VSCAT (Category for Vital Signs) ===\n")
  unique_vscat <- vitals_data %>%
    count(vscat, sort = TRUE) %>%
    arrange(desc(n))
  
  print(unique_vscat)
  
# Looking specifically at patterns that might indicate blood pressure checks
  cat("\n=== VSCAT VALUES CONTAINING 'BLOOD' OR 'PRESSURE' ===\n")
  bp_related <- vitals_data %>%
    filter(str_detect(vscat, regex("blood|pressure", ignore_case = TRUE))) %>%
    count(vscat, sort = TRUE)
  
  print(bp_related)
  
# Checking the first few rows to see the data structure
  cat("\n=== FIRST FEW ROWS OF DATA ===\n")
  vitals_data %>%
    select(usubjid, vsseq, vscat, vsscat, vstestcd, vstest) %>%
    head(10) %>%
    print()
  
# Checking if there are any subjects with sequence 1 that have different VSCAT values
  cat("\n=== VSCAT VALUES FOR SEQUENCE 1 ONLY ===\n")
  seq1_vscat <- vitals_data %>%
    filter(as.integer(vsseq) == 1) %>%
    count(vscat, sort = TRUE)
  
  print(seq1_vscat)
  
# Looking at a few specific subjects/patients to understand the pattern
  cat("\n=== EXAMPLE: FIRST 3 SUBJECTS AND ALL THEIR VSCAT VALUES ===\n")
  first_subjects <- vitals_data %>%
    arrange(usubjid, as.integer(vsseq)) %>%
    slice_head(n = 1) %>%
    pull(usubjid) %>%
    head(3)
  
  vitals_data %>%
    filter(usubjid %in% first_subjects) %>%
    select(usubjid, vsseq, vscat, vstestcd) %>%
    arrange(usubjid, as.integer(vsseq)) %>%
    print(n = 50)
  
# Checking if there's a pattern by looking at subjects who might have BP checks
  cat("\n=== CHECKING FOR POTENTIAL INTERVENTION SUBJECTS ===\n")
  if(nrow(bp_related) > 0) {
    # If we found BP-related entries, let's see which subjects have them
    bp_subjects <- vitals_data %>%
      filter(str_detect(vscat, regex("blood|pressure", ignore_case = TRUE))) %>%
      distinct(usubjid) %>%
      pull(usubjid)
    
    cat("Number of subjects with BP-related VSCAT entries:", length(bp_subjects), "\n")
    
# Showing examples of these subjects
    vitals_data %>%
      filter(usubjid %in% head(bp_subjects, 3)) %>%
      select(usubjid, vsseq, vscat, vstestcd) %>%
      arrange(usubjid, as.integer(vsseq)) %>%
      print(n = 30)
  } else {
    cat("No obvious BP-related entries found. Let's check other patterns.\n")
    
# Maybe the pattern is different - let's look for any unusual VSCAT values
    cat("\n=== LOOKING FOR UNUSUAL PATTERNS IN VSCAT ===\n")
    unusual_patterns <- vitals_data %>%
      filter(!str_detect(vscat, "Fortnightly")) %>%
      count(vscat, sort = TRUE)
    
    print(unusual_patterns)
  }
  
  
# Now that I have a better understanding of my data, I will re-read in the data frame
# and then clean it up, and then create the variables needed for downstream analysis
  
  
# Re-reading the file without applying column names since column 1 contains useful metadata
  vitals_raw_full <- read_csv(
    file.path(data_path, "Vital Signs-DIGEST-2024-12-20.csv"),
    col_names = FALSE
  )
  
# Get row 2 (real headers) as column names
  new_names <- vitals_raw_full[2, ] |> unlist() |> as.character() |> janitor::make_clean_names()
  
# Drop rows 1 and 2, assign proper headers
  vitals_data <- vitals_raw_full[-c(1, 2), ]
  colnames(vitals_data) <- new_names
  
# FIRST: Identify intervention subjects based on "Blood Pressure Check" entries
# These subjects received the BP check survey that was only sent to intervention arm
  intervention_subjects <- vitals_data %>%
    filter(vscat == "Blood Pressure Check") %>%
    distinct(usubjid) %>%
    pull(usubjid)
  
  cat("Number of intervention group subjects identified:", length(intervention_subjects), "\n")
  
# Verifying that this matches sequence 1 pattern
  seq1_bp_check <- vitals_data %>%
    filter(as.integer(vsseq) == 1, vscat == "Blood Pressure Check") %>%
    distinct(usubjid) %>%
    pull(usubjid)
  
  cat("Number of subjects with 'Blood Pressure Check' in sequence 1:", length(seq1_bp_check), "\n")
  cat("Do these match?", identical(sort(intervention_subjects), sort(seq1_bp_check)), "\n")
  
# Cleaning the vital signs dataset with timepoint mapping described in the SAP
  vitals_clean <- vitals_data %>%
   # Remove completely empty columns
    select(where(~ !all(is.na(.)))) %>%
    
# Selecting and cleaning the relevant columns
    transmute(
      subject_id = usubjid,
      sequence_number = as.integer(vsseq),
      visit_name = visit,
      visit_number = as.integer(visitnum),
      
    # Vital sign information
      test_code = vstestcd,  # WEIGHT, WSTCIR, SYSBP, DIABP
      test_name = vstest,
      
    # Treatment group identification (based on BP check survey receipt)
      treatment_group = if_else(usubjid %in% intervention_subjects, 
                                "intervention", "control"),
      
# Storing original category as an object for reference
      original_category = vscat,
      
      # Timepoint mapping
      survey_category = case_when(
        vscat == "Blood Pressure Check" ~ "bp_check",
        str_detect(vscat, "Fortnightly-036") ~ "fortnightly_special",
        str_detect(vscat, "Fortnightly") ~ "fortnightly_regular",
        vscat == "FollowUp-9" ~ "followup_9",
        TRUE ~ vscat
      ),
      
      # Timepoint mapping from subcategory (VSSCAT)
      timepoint_numeric = as.integer(vsscat),
      
# Creating standardised timepoint labels aligned with SAP (t0, t1, t2, t3)
      timepoint_sap = case_when(
        # For Fortnightly-036: 0=baseline(t0), 1=3months(t1), 2=6months(t2)
        survey_category == "fortnightly_special" & vsscat == "0" ~ "t0",
        survey_category == "fortnightly_special" & vsscat == "1" ~ "t1", 
        survey_category == "fortnightly_special" & vsscat == "2" ~ "t2",
        # For regular Fortnightly surveys - need to map based on actual timing
        survey_category == "fortnightly_regular" & vsscat == "0" ~ "week_2",
        survey_category == "fortnightly_regular" & vsscat == "1" ~ "month_4",
        survey_category == "fortnightly_regular" & vsscat == "2" ~ "month_6",
        # For FollowUp-9 (likely 9-month follow-up for intervention group)
        survey_category == "followup_9" ~ "t3_followup",
        # For BP checks - these are intervention-specific measurements
        survey_category == "bp_check" ~ "bp_measurement",
        TRUE ~ paste0("timepoint_", vsscat)
      ),
      
# Descriptive timepoint labels
      timepoint_label = case_when(
        timepoint_sap == "t0" ~ "baseline",
        timepoint_sap == "t1" ~ "3_months",
        timepoint_sap == "t2" ~ "6_months", 
        timepoint_sap == "t3_followup" ~ "9_months",
        timepoint_sap == "week_2" ~ "week_2",
        timepoint_sap == "month_4" ~ "month_4",
        timepoint_sap == "month_6" ~ "month_6",
        timepoint_sap == "bp_measurement" ~ "bp_check",
        TRUE ~ timepoint_sap
      ),
      
# Vital sign values (raw and standardized)
      value_original = vsorres,  # Original result
      value_numeric = as.numeric(vsstresn),  # Standardized numeric result
      units_original = vsorresu,  # Original units
      units_standard = vsstresu,  # Standard units
      
# Measurement date (excluding study day per analyst recommendation)
      measurement_date = case_when(
        !is.na(vsdtc) ~ lubridate::parse_date_time(vsdtc, orders = c("ymd", "ymd_HMS", "mdy", "dmy")),
        TRUE ~ as.POSIXct(NA)
      ),
      
# Creating variables specifically needed for SAP analyses
# Weight (needed for primary endpoint: ≥15kg loss)
      weight_kg = if_else(test_code == "WEIGHT", value_numeric, NA_real_),
      
# Blood pressure (needed for secondary endpoints)
      systolic_bp = if_else(test_code == "SYSBP", value_numeric, NA_real_),
      diastolic_bp = if_else(test_code == "DIABP", value_numeric, NA_real_),
      
# Waist circumference (needed for secondary endpoints)
      waist_circumference_cm = if_else(test_code == "WSTCIR", value_numeric, NA_real_),
      
# Metadata timestamps  
      date_recorded = lubridate::ymd_hms(suppvsfr, tz = "UTC"),
      date_updated = lubridate::ymd_hms(suppvslu, tz = "UTC")
    ) %>%
    
# Keep all records including those with missing data (needed for missing data report)
# Arrange by subject, timepoint, and test type for easier review
    arrange(subject_id, timepoint_numeric, test_code)
  
# VERIFICATION AND DIAGNOSTICS
  cat("\n=== TREATMENT GROUP VERIFICATION ===\n")
  
# Overall treatment group counts
  treatment_summary <- vitals_clean %>%
    count(treatment_group) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  
  print(treatment_summary)
  
# Subject-level summary (should match 2:1 intervention:control ratio from SAP)
  subject_treatment_summary <- vitals_clean %>%
    group_by(subject_id) %>%
    summarise(treatment_group = first(treatment_group), .groups = "drop") %>%
    count(treatment_group) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  
  cat("\nSubject-level treatment distribution:\n")
  print(subject_treatment_summary)
  
# Checking SAP timepoint mapping
  cat("\n=== SAP TIMEPOINT VERIFICATION ===\n")
  timepoint_summary <- vitals_clean %>%
    count(survey_category, timepoint_sap, timepoint_label, timepoint_numeric) %>%
    arrange(survey_category, timepoint_numeric)
  
  print(timepoint_summary)
  
# Checking vital sign availability for SAP endpoints
  cat("\n=== VITAL SIGNS AVAILABILITY FOR SAP ENDPOINTS ===\n")
  
# Primary endpoint requirements: Weight
  weight_availability <- vitals_clean %>%
    filter(!is.na(weight_kg)) %>%
    count(timepoint_sap, treatment_group) %>%
    arrange(timepoint_sap, treatment_group)
  
  cat("Weight measurements by timepoint and treatment:\n")
  print(weight_availability)
  
# Secondary endpoint requirements: Blood pressure
  bp_availability <- vitals_clean %>%
    filter(!is.na(systolic_bp) | !is.na(diastolic_bp)) %>%
    count(timepoint_sap, treatment_group, test_code) %>%
    arrange(timepoint_sap, treatment_group, test_code)
  
  cat("\nBlood pressure measurements by timepoint and treatment:\n")
  print(bp_availability)
  
# Waist circumference availability
  waist_availability <- vitals_clean %>%
    filter(!is.na(waist_circumference_cm)) %>%
    count(timepoint_sap, treatment_group) %>%
    arrange(timepoint_sap, treatment_group)
  
  cat("\nWaist circumference measurements by timepoint and treatment:\n")
  print(waist_availability)
  
# Check for baseline measurements (critical for change calculations)
  baseline_check <- vitals_clean %>%
    filter(timepoint_sap == "t0") %>%
    count(test_code, treatment_group) %>%
    arrange(test_code, treatment_group)
  
  cat("\nBaseline measurements (t0) by vital sign and treatment:\n")
  print(baseline_check)
  
# Missing data assessment (required by SAP)
  cat("\n=== MISSING DATA ASSESSMENT ===\n")
  missing_summary <- vitals_clean %>%
    group_by(timepoint_sap, test_code, treatment_group) %>%
    summarise(
      total_records = n(),
      missing_values = sum(is.na(value_numeric)),
      missing_percentage = round(missing_values / total_records * 100, 1),
      .groups = "drop"
    ) %>%
    filter(missing_percentage > 0) %>%
    arrange(desc(missing_percentage))
  
  print(missing_summary)
  
# Creating wide format for easier analysis (optional but I find it totally helpful)
  # FIX: Including all relevant SAP timepoints, not just t0, t1, t2.
  # This will ensure blood pressure (bp_measurement), 9-month follow-up (t3_followup),
  # and other readings are retained in the wide format.
  vitals_wide <- vitals_clean %>%
    filter(timepoint_sap %in% c("t0", "t1", "t2", "t3_followup", "bp_measurement")) %>% 
    select(subject_id, treatment_group, timepoint_sap, test_code, value_numeric) %>%
    pivot_wider(
      names_from = c(test_code, timepoint_sap),
      values_from = value_numeric,
      names_sep = "_"
    )
  
  cat("\nWide format dataset created with", nrow(vitals_wide), "subjects\n")
  cat("Columns in wide format:\n")
  print(names(vitals_wide))
  
# Final summary aligned with SAP requirements
  total_subjects <- length(unique(vitals_clean$subject_id))
  intervention_count <- length(unique(vitals_clean$subject_id[vitals_clean$treatment_group == "intervention"]))
  control_count <- length(unique(vitals_clean$subject_id[vitals_clean$treatment_group == "control"]))
  
  cat("\n=== FINAL SUMMARY - SAP ALIGNMENT CHECK ===\n")
  cat("Total subjects:", total_subjects, "\n")
  cat("Intervention subjects:", intervention_count, "\n") 
  cat("Control subjects:", control_count, "\n")
  cat("Intervention:Control ratio:", round(intervention_count/control_count, 2), ":1\n")
  cat("Expected SAP ratio: 2:1\n")

# Next is to save both long and wide format datasets
  
# The LONG format is usefuul for longitudinal analyses (MMRM), data exploration, 
# and ease of merging with other datasets. It is the most flexible format for statistical modeling
  
# The WIDE makes for calculating change scores and quick baseline comparisons easy such as
# things like "weight loss from baseline"
  write_csv(vitals_clean, here::here("data", "clean", "vital_signs_long.csv"))
  write_csv(vitals_wide, here::here("data", "clean", "vital_signs_wide.csv"))
  
  cat("\nDatasets saved:\n")
  cat("- Long format: data/clean/vital_signs_long.csv\n")
  cat("- Wide format: data/clean/vital_signs_wide.csv\n")
  
# Next is to export summary for integration with other datasets. This summary data frame 
# is useful for determining analysis populations (ITT, FAS, PP) and for making merging decisions
  vitals_summary <- vitals_clean %>%
    group_by(subject_id) %>%
    summarise(
      treatment_group = first(treatment_group),
      has_baseline_weight = any(test_code == "WEIGHT" & timepoint_sap == "t0", na.rm = TRUE),
      has_baseline_bp = any(test_code %in% c("SYSBP", "DIABP") & timepoint_sap == "t0", na.rm = TRUE),
      has_baseline_waist = any(test_code == "WSTCIR" & timepoint_sap == "t0", na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv(vitals_summary, here::here("data", "clean", "vitals_subject_summary.csv"))
  cat("- Subject summary: data/clean/vitals_subject_summary.csv\n")
#
#-------------------------------------------------------------------------------------------
# Section 9 - Data Cleaning: Survey Dataset
#-------------------------------------------------------------------------------------------
# Reading in the raw survey data
  survey_raw_full <- read_csv(
    file.path(data_path, "Survey Responses-DIGEST-2024-12-20.csv"),
    col_names = FALSE
  )
  
# Getting proper headers from row 2 and clean them
  new_names <- survey_raw_full[2, ] |> 
    unlist() |> 
    as.character() |> 
    janitor::make_clean_names()
  
# Removing the header rows and assigning proper column names
  survey_data <- survey_raw_full[-c(1, 2), ]
  colnames(survey_data) <- new_names
  
  cat("=== SURVEY DATA CLEANING ===\n")
  cat("Total records:", nrow(survey_data), "\n")
  cat("Total subjects:", length(unique(survey_data$usubjid)), "\n")
  
# Next is to identify the treatment groups as was done in the vital signs dataset
# Identify intervention subjects based on Blood Pressure Check surveys
# (Only intervention group receives BP check surveys)
  intervention_subjects <- survey_data %>%
    filter(str_detect(qscat, "Blood Pressure Check")) %>%
    distinct(usubjid) %>%
    pull(usubjid)
  
  cat("Intervention subjects identified:", length(intervention_subjects), "\n")
  
# Best to check the columns and data available before attempting to transmute
  
# Checking what columns are actually available
  cat("Available columns:\n")
  cat(paste(names(survey_data), collapse = ", "), "\n\n")
  
# Checking for key columns we need
  required_cols <- c("usubjid", "qscat", "qstestcd", "qstest", "qsorres", "qsstresn", "qsstresc")
  missing_cols <- setdiff(required_cols, names(survey_data))
  if(length(missing_cols) > 0) {
    cat("Warning: Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
  }
  
# Now that I am familiar with the data, I can attempt to clean the data
  
# First, let's see which columns will remain after removing empty ones
  cols_after_filter <- survey_data %>%
    select(where(~ !all(is.na(.)))) %>%
    names()
  
  cat("Columns remaining after removing empty columns:\n")
  cat(paste(cols_after_filter, collapse = ", "), "\n\n")
  
  survey_clean <- survey_data %>%
    # Removing completely empty columns
    select(where(~ !all(is.na(.)))) %>%
    
# Cleaning and standardising all variables (using only the available columns)
    transmute(
      # Subject identifiers used across data sets
      subject_id = str_trim(usubjid),
      sequence_number = as.integer(qsseq),
      visit_name = str_trim(visit),
      visit_number = as.integer(visitnum),
      
  # Treatment group assignment
      treatment_group = if_else(usubjid %in% intervention_subjects, 
                                "intervention", "control"),
      
  # Survey identifiers
      survey_category_raw = str_trim(qscat),
      survey_subcategory = if("qsscat" %in% cols_after_filter) str_trim(qsscat) else NA_character_,
      
  # Classifying survey types as in vital signs data set
      survey_type = case_when(
        str_detect(survey_category_raw, "Fortnightly-036") ~ "fortnightly_special",
        str_detect(survey_category_raw, "Fortnightly-[0-9]+$") ~ "fortnightly_regular", 
        str_detect(survey_category_raw, "FollowUp-12") ~ "followup_12",
        str_detect(survey_category_raw, "FollowUp-9") ~ "followup_9",
        str_detect(survey_category_raw, "Blood Pressure Check") ~ "bp_check",
        str_detect(survey_category_raw, "Onboarding-Intervention") ~ "onboarding_intervention",
        str_detect(survey_category_raw, "Onboarding-Control") ~ "onboarding_control",
        str_detect(survey_category_raw, "Experience Survey") ~ "experience_survey",
        TRUE ~ "other"
      ),
      
# Extracting timepoint information
      timepoint_suffix = str_extract(survey_category_raw, "-[0-9]+$") %>% 
        str_remove("-") %>% 
        as.integer(),
      
# Creating standardised timepoints as described in the SAP
      timepoint_sap = case_when(
      # Fortnightly-036: Main study timepoints
        survey_type == "fortnightly_special" & timepoint_suffix == 0 ~ "t0",
        survey_type == "fortnightly_special" & timepoint_suffix == 1 ~ "t1", 
        survey_type == "fortnightly_special" & timepoint_suffix == 2 ~ "t2",
        
      # Regular fortnightly surveys
        survey_type == "fortnightly_regular" & timepoint_suffix == 0 ~ "week_2",
        survey_type == "fortnightly_regular" & timepoint_suffix == 1 ~ "month_4",
        survey_type == "fortnightly_regular" & timepoint_suffix == 2 ~ "month_6",
        survey_type == "fortnightly_regular" & timepoint_suffix >= 3 ~ 
          paste0("month_", timepoint_suffix * 2 + 2),
        
      # Follow-up surveys
        survey_type == "followup_9" ~ "t3_followup",
        survey_type == "followup_12" ~ "t4_followup",
        
      # Baseline surveys
        survey_type %in% c("onboarding_intervention", "onboarding_control") ~ "t0_onboarding",
        survey_type == "experience_survey" ~ "experience",
        survey_type == "bp_check" ~ "bp_measurement",
        
        TRUE ~ paste0("timepoint_", timepoint_suffix)
      ),
      
# Descriptive timepoint labels
      timepoint_label = case_when(
        timepoint_sap == "t0" ~ "baseline",
        timepoint_sap == "t1" ~ "3_months", 
        timepoint_sap == "t2" ~ "6_months",
        timepoint_sap == "t3_followup" ~ "9_months",
        timepoint_sap == "t4_followup" ~ "12_months",
        timepoint_sap == "t0_onboarding" ~ "onboarding",
        timepoint_sap == "experience" ~ "experience_survey",
        timepoint_sap == "bp_measurement" ~ "bp_check",
        TRUE ~ timepoint_sap
      ),
      
# Question information
      question_code = if("qstestcd" %in% cols_after_filter) str_trim(qstestcd) else NA_character_,
      question_text = if("qstest" %in% cols_after_filter) str_trim(qstest) else NA_character_,
      question_method = if("qsmethod" %in% cols_after_filter) str_trim(qsmethod) else NA_character_,
      
# Response data
      response_original = if("qsorres" %in% cols_after_filter) str_trim(qsorres) else NA_character_,
      response_standardized = if("qsstresc" %in% cols_after_filter) str_trim(qsstresc) else NA_character_,
      response_numeric = if("qsstresn" %in% cols_after_filter) as.numeric(qsstresn) else NA_real_,
      
# Response metadata
      response_units_original = if("qsorresu" %in% cols_after_filter) str_trim(qsorresu) else NA_character_,
      response_units_standard = if("qsstresu" %in% cols_after_filter) str_trim(qsstresu) else NA_character_,
      completion_status = if("qsstat" %in% cols_after_filter) str_trim(qsstat) else NA_character_,
      reason_not_done = if("qsreasnd" %in% cols_after_filter) str_trim(qsreasnd) else NA_character_,
      
# Date information
      response_date = case_when(
        "qsdtc" %in% cols_after_filter & !is.na(qsdtc) ~ parse_date_time(qsdtc, orders = c("ymd", "ymd_HMS", "mdy", "dmy")),
        TRUE ~ as.POSIXct(NA)
      ),
      
# Completion flag
      response_completed = case_when(
        completion_status == "NOT DONE" ~ FALSE,
        is.na(completion_status) & !is.na(response_original) ~ TRUE,
        is.na(completion_status) & !is.na(response_numeric) ~ TRUE,
        TRUE ~ !is.na(response_original) | !is.na(response_numeric)
      )
    ) %>%
    
    # Sorting the cleaned data
    arrange(subject_id, sequence_number)
  
# Validating the data before attempting to merge with others
  
  cat("\n=== DATA VALIDATION ===\n")
  
# Checking patient counts
  treatment_counts <- survey_clean %>%
    distinct(subject_id, treatment_group) %>%
    count(treatment_group)
  
  cat("Treatment group distribution:\n")
  print(treatment_counts)
  
# Checking timepoint distribution
  timepoint_summary <- survey_clean %>%
    count(timepoint_sap, timepoint_label, survey_type) %>%
    arrange(timepoint_sap)
  
  cat("\nTimepoint distribution:\n")
  print(timepoint_summary)
  
# Check for missing key variables
  missing_check <- survey_clean %>%
    summarise(
      missing_subject_id = sum(is.na(subject_id)),
      missing_treatment_group = sum(is.na(treatment_group)),
      missing_timepoint = sum(is.na(timepoint_sap)),
      missing_question_code = sum(is.na(question_code)),
      total_records = n()
    )
  
  cat("\nMissing data check:\n")
  print(missing_check)
  
# Writing the output data frames to save the cleaned output
  
# Specifying the output directory as done earlier in the script using here
  dir.create(here("data", "clean"), showWarnings = FALSE, recursive = TRUE)
  
# Main cleaned dataset (long format for modelling)
  write_csv(survey_clean, here("data", "clean", "survey_responses_clean.csv"))
  cat("\n=== OUTPUTS CREATED ===\n")
  cat("✓ Main dataset: data/clean/survey_responses_clean.csv\n")
  
# Subject-level quick summary for merging
  survey_subject_summary <- survey_clean %>%
    group_by(subject_id) %>%
    summarise(
      treatment_group = first(treatment_group),
      has_baseline_survey = any(timepoint_sap == "t0", na.rm = TRUE),
      has_3month_survey = any(timepoint_sap == "t1", na.rm = TRUE), 
      has_6month_survey = any(timepoint_sap == "t2", na.rm = TRUE),
      has_9month_followup = any(timepoint_sap == "t3_followup", na.rm = TRUE),
      has_12month_followup = any(timepoint_sap == "t4_followup", na.rm = TRUE),
      total_survey_responses = sum(response_completed, na.rm = TRUE),
      first_survey_date = min(response_date, na.rm = TRUE),
      last_survey_date = max(response_date, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv(survey_subject_summary, here("data", "clean", "survey_subject_summary.csv"))
  cat("✓ Subject summary: data/clean/survey_subject_summary.csv\n")
  
# SAP timepoints dataset (for main analysis)
  sap_timepoints_data <- survey_clean %>%
    filter(timepoint_sap %in% c("t0", "t1", "t2")) %>%
    select(subject_id, treatment_group, timepoint_sap, timepoint_label,
           question_code, question_text, response_original, response_numeric,
           response_standardized, response_completed, response_date)
  
  write_csv(sap_timepoints_data, here("data", "clean", "survey_sap_timepoints.csv"))
  cat("✓ SAP timepoints: data/clean/survey_sap_timepoints.csv\n")
  
# Quick summary of the final datasets as done with the vital signs data set too
  
  total_subjects <- length(unique(survey_clean$subject_id))
  intervention_count <- sum(treatment_counts$treatment_group == "intervention" & 
                              !is.na(treatment_counts$treatment_group))
  control_count <- sum(treatment_counts$treatment_group == "control" & 
                         !is.na(treatment_counts$treatment_group))
  
  cat("\n=== CLEANING SUMMARY ===\n")
  cat("Total subjects:", total_subjects, "\n")
  cat("Intervention subjects:", intervention_count, "\n") 
  cat("Control subjects:", control_count, "\n")
  cat("Total survey records:", nrow(survey_clean), "\n")
  cat("Completed responses:", sum(survey_clean$response_completed, na.rm = TRUE), "\n")
  
  cat("\n✅ Survey data cleaning complete!\n")
  cat("📁 Clean datasets ready for merging with other trial data.\n")

#
#-------------------------------------------------------------------------------------------
# Section 10 - Creating the Master dataset
#-------------------------------------------------------------------------------------------
# First is to load the cleaned datasets
  
# Defining the path to get data from
  clean_path <- here("data", "clean")
  
# Loading the 7 cleaned datasets
  demo_clean <- read_csv(file.path(clean_path, "demographics.csv"))
  bmi_clean <- read_csv(file.path(clean_path, "bmi.csv"))
  ae_clean <- read_csv(file.path(clean_path, "adverse_events.csv"))
  lab_clean <- read_csv(file.path(clean_path, "lab_values.csv"))
  meds_clean <- read_csv(file.path(clean_path, "medications.csv"))
  vitals_clean <- read_csv(file.path(clean_path, "vital_signs_long.csv"))
  survey_clean <- read_csv(file.path(clean_path, "survey_responses_clean.csv"))

# Checking the data structure to confirm it (headers, formats, and structure) is consistent
# across the different data frames
  glimpse(demo_clean)
  glimpse(bmi_clean)
  glimpse(ae_clean)
  glimpse(lab_clean)
  glimpse(meds_clean)
  glimpse(vitals_clean)
  glimpse(survey_clean)
  
# Installing a few more packages that might come in for creating the merged datasets ane reporting
  install.packages("kableExtra") #enhancing table appearance
  library(kableExtra)
  library(kableExtra)  
  library(knitr) #easier documentation and ease of use of R Markdown/LaTeX, HTML, etc

# Defining the file paths
  clean_path <- here("data", "clean")
  master_path <- here("data", "master")
  output_path <- here("output", "tables") # New path for reports
  
# Creating the master directory if it doesn't exist
  if (!dir.exists(master_path)) {
    dir.create(master_path, recursive = TRUE)
    cat("Created directory:", master_path, "\n")
  }
  
# Creating the output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    cat("Created directory:", output_path, "\n")
  }
  

# Loading all datasets and immediately cleaning their column names with janitor::clean_names()

  cat("=== Loading and Cleaning Datasets ===\n")
  
# Using demographics.csv as the primary, definitive source of our patient list.
  demo_clean <- read_csv(file.path(clean_path, "demographics.csv")) %>% clean_names()
  bmi_clean <- read_csv(file.path(clean_path, "bmi.csv")) %>% clean_names()
  ae_clean <- read_csv(file.path(clean_path, "adverse_events.csv")) %>% clean_names()
  lab_clean <- read_csv(file.path(clean_path, "lab_values.csv")) %>% clean_names()
  meds_clean <- read_csv(file.path(clean_path, "medications.csv")) %>% clean_names()
  vitals_long <- read_csv(file.path(clean_path, "vital_signs_long.csv")) %>% clean_names()
  survey_clean <- read_csv(file.path(clean_path, "survey_responses_clean.csv")) %>% clean_names()
  
# Loading the additional dataset suggested by the user
  survey_subject_summary <- read_csv(file.path(clean_path, "survey_subject_summary.csv")) %>% clean_names()
  
# Checking initial data dimensions
  cat("Initial Data Dimensions:\n")
  cat("Demographics:", nrow(demo_clean), "participants\n")
  cat("Vitals (long format):", nrow(vitals_long), "records\n")
  cat("Lab values:", nrow(lab_clean), "records\n")
  cat("BMI:", nrow(bmi_clean), "records\n")
  cat("Survey Summary:", nrow(survey_subject_summary), "records\n")
  
#  Defining the treatment groups based on if they were sent the Blood Pressure Check survey at baseline
# This is a critical step based on the user's clarification. We will combine
# treatment information from vitals_long and survey_subject_summary, then
# default any remaining NAs to 'control'.
# ==============================================================================
  cat("\n=== Defining Treatment Groups based on multiple sources ===\n")
  
# 1. Start with the treatment group from vitals_long (primary source)
  treatment_from_vitals <- vitals_long %>%
    distinct(subject_id, treatment_group) %>%
    filter(!is.na(treatment_group))
  
# 2. Get treatment group from survey_subject_summary (secondary source)
  treatment_from_survey <- survey_subject_summary %>%
    distinct(subject_id, treatment_group) %>%
    filter(!is.na(treatment_group))
  
# 3. Combine the two, prioritizing vitals_long
  combined_treatment_assignment <- bind_rows(treatment_from_vitals, treatment_from_survey) %>%
    group_by(subject_id) %>%
    slice(1) %>%  # Take the first entry, which is from vitals_long if it exists
    ungroup()
  
# 4. Join with the full demographics list to ensure all 101 participants are included
  # Then, assign 'control' to any remaining NA values
  treatment_assignment <- demo_clean %>%
    select(subject_id) %>%
    left_join(combined_treatment_assignment, by = "subject_id") %>%
    mutate(
      treatment_group = if_else(is.na(treatment_group), "control", treatment_group)
    )
  
  # Check the counts from the new combined treatment assignment source
  cat("Final treatment assignment from combined sources:\n")
  print(table(treatment_assignment$treatment_group, useNA = "always"))
  
# Prepaing the vital_signs data for merge as it contains the formatted 12M followup
  cat("\n=== Preparing Vital Signs Data (corrected for t3) ===\n")
  
# New logic: Use row number within subject/test to assign timepoints,
# which is more robust than relying solely on survey category.
  vitals_wide_full <- vitals_long %>%
# Group by subject and test code to correctly order measurements
  group_by(subject_id, test_code) %>%
# Order by date to ensure chronological assignment of timepoints
# CORRECTED: using measurement_date
  arrange(measurement_date) %>%
# Assign timepoint based on the order of the measurement
    mutate(
      timepoint_sap = case_when(
        row_number() == 1 ~ "t0",
        row_number() == 2 ~ "t1",
        row_number() == 3 ~ "t2",
        row_number() == 4 ~ "t3",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
  # Filter to keep only our defined timepoints
    filter(!is.na(timepoint_sap)) %>%
  # Select only the necessary columns for pivoting
    select(subject_id, timepoint_sap, test_code, value_numeric) %>%
  # Convert to wide format
    pivot_wider(
      names_from = c(test_code, timepoint_sap),
      values_from = value_numeric,
      names_sep = "_"
    )
  
# Deduplicate in case a participant has multiple vitals entries
  vitals_wide_deduplicated <- vitals_wide_full %>%
    group_by(subject_id) %>%
    slice(1) %>%
    ungroup()
  
  cat("Vital signs data prepared. Dimensions:", nrow(vitals_wide_deduplicated), "x", ncol(vitals_wide_deduplicated), "\n")
  
  
# Prepaing the labs values dataset in the same manner
  cat("\n=== Processing Lab Data ===\n")
  
# For lab data, we need to be more careful about timepoint assignment
# Use baseline flag for t0, then try to match with vitals dates for other timepoints
  lab_processed <- lab_clean %>%
    # Assign t0 based on baseline flag
    mutate(
      timepoint_assigned = case_when(
        baseline_flag == TRUE ~ "t0",
        TRUE ~ NA_character_
      )
    ) %>%
# For non-baseline, we'll use collection date and try to match with study progression
# This is a simplified approach - in practice you might need more sophisticated matching
    group_by(subject_id) %>%
    arrange(collection_date) %>%
    mutate(
# If not baseline, assign based on order of collection dates
      timepoint_assigned = case_when(
        !is.na(timepoint_assigned) ~ timepoint_assigned,
        row_number() == 2 ~ "t1",  # Second measurement
        row_number() == 3 ~ "t2",  # Third measurement
        row_number() == 4 ~ "t3",  # Fourth measurement (12 months)
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
# Filter for main timepoints
    filter(timepoint_assigned %in% c("t0", "t1", "t2", "t3")) %>%
# Keep one measurement per timepoint
    group_by(subject_id, timepoint_assigned) %>%
    arrange(desc(collection_date)) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
# Convert to wide format
    select(subject_id, timepoint_assigned, collection_date, hba1c_mmol_mol, hba1c_percent) %>%
    pivot_wider(
      names_from = timepoint_assigned,
      values_from = c(hba1c_mmol_mol, hba1c_percent, collection_date),
      names_sep = "_"
    )
  
  cat("Lab data processed. Dimensions:", nrow(lab_processed), "x", ncol(lab_processed), "\n")
  
# Preparing the BMI data in the same format
  cat("\n=== Processing BMI Data ===\n")
  
# Checking BMI data structure
  cat("BMI data timepoints:\n")
  if ("timepoint_sap" %in% names(bmi_clean)) {
    print(table(bmi_clean$timepoint_sap, useNA = "always"))
  } else {
    cat("No timepoint_sap column found. Using all BMI data as baseline.\n")
  }
  
# Processing the BMI data
  bmi_processed <- bmi_clean %>%
    # If there are multiple BMI measurements, we might want to keep them all
    # For now, let's create both baseline and longitudinal if available
    group_by(subject_id) %>%
    arrange(bmi_finding_date) %>%
    mutate(
      bmi_sequence = row_number(),
      timepoint_bmi = case_when(
        bmi_sequence == 1 ~ "baseline",
        bmi_sequence == 2 ~ "followup1",
        bmi_sequence == 3 ~ "followup2",
        TRUE ~ paste0("followup", bmi_sequence - 1)
      )
    ) %>%
    ungroup()
  
# Creating a simple baseline BMI for merging
  bmi_baseline <- bmi_processed %>%
    group_by(subject_id) %>%
    arrange(bmi_finding_date) %>%  # Take earliest BMI as baseline
    slice_head(n = 1) %>%
    ungroup() %>%
    select(subject_id, bmi_baseline = bmi, bmi_baseline_date = bmi_finding_date)
  
  cat("BMI baseline data processed. Dimensions:", nrow(bmi_baseline), "x", ncol(bmi_baseline), "\n")
  
# Finally conducting the merge to create the master dataset
  cat("\n=== Creating Master Dataset ===\n")
  
# Start with demographics and build up
  master_dataset <- demo_clean %>%
    # Now, join the correctly defined treatment assignment
    left_join(treatment_assignment, by = "subject_id") %>%
    # Add deduplicated vitals data (now including corrected t3 data)
    left_join(vitals_wide_deduplicated, by = "subject_id") %>%
    # Add lab data
    left_join(lab_processed, by = "subject_id") %>%
    # Add baseline BMI
    left_join(bmi_baseline, by = "subject_id") %>%
# Define analysis populations per SAP
    mutate(
      # ITT: All randomized participants (those with treatment assignment)
      population_itt = !is.na(treatment_group),
      # FAS: ITT with baseline data for primary endpoints.
      # Note: Column names are corrected here to match pivot_wider output.
      population_fas = population_itt &
        !is.na(WEIGHT_t0) & !is.na(hba1c_percent_t0) &
        !is.na(WSTCIR_t0) & !is.na(SYSBP_t0),
      # PP will be defined later based on adherence data
      population_pp = NA,
      # SP: Safety population (ITT who received at least one intervention component)
      population_sp = population_itt
    )
  
  cat("Master dataset created with", nrow(master_dataset), "participants\n")
  cat("Treatment groups in master:\n")
  print(table(master_dataset$treatment_group, useNA = "always"))
  
  # Check for t3 data for all variables
  t3_weight_available <- if("WEIGHT_t3" %in% names(master_dataset)) {
    sum(!is.na(master_dataset$WEIGHT_t3), na.rm = TRUE)
  } else {
    0
  }
  t3_hba1c_available <- if("hba1c_percent_t3" %in% names(master_dataset)) {
    sum(!is.na(master_dataset$hba1c_percent_t3), na.rm = TRUE)
  } else {
    0
  }
  t3_sysbp_available <- if("SYSBP_t3" %in% names(master_dataset)) {
    sum(!is.na(master_dataset$SYSBP_t3), na.rm = TRUE)
  } else {
    0
  }
  cat("Participants with t3 weight data:", t3_weight_available, "\n")
  cat("Participants with t3 HbA1c data:", t3_hba1c_available, "\n")
  cat("Participants with t3 Systolic BP data:", t3_sysbp_available, "\n")
  
#-------------------------------------------------------------------------------------------
# Section 11 - Creating the Missing data report
#-------------------------------------------------------------------------------------------
  cat("\n=== Generating Missing Data Report ===\n")
  
# Defining key endpoint variables per SAP, with corrected column names
  endpoint_vars <- c(
    # Weight at all timepoints
    "WEIGHT_t0", "WEIGHT_t1", "WEIGHT_t2", "WEIGHT_t3",
    # HbA1c at all timepoints
    "hba1c_percent_t0", "hba1c_percent_t1", "hba1c_percent_t2", "hba1c_percent_t3",
    # Blood pressure at all timepoints
    "SYSBP_t0", "SYSBP_t1", "SYSBP_t2", "SYSBP_t3",
    "DIABP_t0", "DIABP_t1", "DIABP_t2", "DIABP_t3",
    # Waist circumference at all timepoints
    "WSTCIR_t0", "WSTCIR_t1", "WSTCIR_t2", "WSTCIR_t3"
  )
  
# Filter to only include variables that actually exist in the dataset
  existing_endpoint_vars <- endpoint_vars[endpoint_vars %in% names(master_dataset)]
  
# Create comprehensive missing data report
  missing_data_report <- master_dataset %>%
    select(treatment_group, all_of(existing_endpoint_vars)) %>%
    group_by(treatment_group) %>%
    summarise(
      n_group = n(),
      across(all_of(existing_endpoint_vars), ~ sum(is.na(.x)), .names = "missing_{.col}"),
      .groups = "drop"
    ) %>%
    # Add total row
    bind_rows(
      master_dataset %>%
        select(all_of(existing_endpoint_vars)) %>%
        summarise(
          treatment_group = "Total",
          n_group = nrow(master_dataset),
          across(all_of(existing_endpoint_vars), ~ sum(is.na(.x)), .names = "missing_{.col}")
        )
    ) %>%
    # Convert to long format for better reporting
    pivot_longer(
      cols = starts_with("missing_"),
      names_to = "variable",
      values_to = "missing_n"
    ) %>%
    mutate(
      variable = str_remove(variable, "missing_"),
      missing_percent = round(100 * missing_n / n_group, 1),
      endpoint = case_when(
        str_detect(variable, "WEIGHT") ~ "Weight (kg)",
        str_detect(variable, "hba1c") ~ "HbA1c (%)",
        str_detect(variable, "SYSBP") ~ "Systolic BP (mmHg)",
        str_detect(variable, "DIABP") ~ "Diastolic BP (mmHg)",
        str_detect(variable, "WSTCIR") ~ "Waist Circumference (cm)"
      ),
      timepoint = case_when(
        str_detect(variable, "_t0") ~ "Baseline (t0)",
        str_detect(variable, "_t1") ~ "3 Months (t1)",
        str_detect(variable, "_t2") ~ "6 Months (t2)",
        str_detect(variable, "_t3") ~ "12 Months (t3)"
      )
    ) %>%
    arrange(endpoint, timepoint, treatment_group)
  
  # Display missing data summary
  cat("\nMissing Data Summary:\n")
  print(missing_data_report)
  
#-------------------------------------------------------------------------------------------
# Section 12 - Creating the baseline demographics tables: summary statistics
#-------------------------------------------------------------------------------------------
  cat("\n=== Generating Baseline Demographics and Clinical Characteristics ===\n")
  
# Create comprehensive baseline demographics
  baseline_demographics <- master_dataset %>%
    filter(population_itt) %>%
    group_by(treatment_group) %>%
    summarise(
      n = n(),
      # Demographics
      age_mean = round(mean(age_years, na.rm = TRUE), 1),
      age_sd = round(sd(age_years, na.rm = TRUE), 1),
      age_median = round(median(age_years, na.rm = TRUE), 1),
      age_min = min(age_years, na.rm = TRUE),
      age_max = max(age_years, na.rm = TRUE),
      # Sex distribution
      female_n = sum(sex == "female", na.rm = TRUE),
      female_percent = round(100 * female_n / n, 1),
      male_n = sum(sex == "male", na.rm = TRUE),
      male_percent = round(100 * male_n / n, 1),
      # Clinical measures at baseline
      # Note: The column name is now WEIGHT_t0
      weight_mean = round(mean(WEIGHT_t0, na.rm = TRUE), 1),
      weight_sd = round(sd(WEIGHT_t0, na.rm = TRUE), 1),
      weight_median = round(median(WEIGHT_t0, na.rm = TRUE), 1),
      weight_min = min(WEIGHT_t0, na.rm = TRUE),
      weight_max = max(WEIGHT_t0, na.rm = TRUE),
      weight_n = sum(!is.na(WEIGHT_t0)),
      # HbA1c
      hba1c_mean = round(mean(hba1c_percent_t0, na.rm = TRUE), 1),
      hba1c_sd = round(sd(hba1c_percent_t0, na.rm = TRUE), 1),
      hba1c_median = round(median(hba1c_percent_t0, na.rm = TRUE), 1),
      hba1c_min = min(hba1c_percent_t0, na.rm = TRUE),
      hba1c_max = max(hba1c_percent_t0, na.rm = TRUE),
      hba1c_n = sum(!is.na(hba1c_percent_t0)),
      # Blood pressure
      # Note: The column names are now SYSBP_t0 and DIABP_t0
      systolic_mean = round(mean(SYSBP_t0, na.rm = TRUE), 1),
      systolic_sd = round(sd(SYSBP_t0, na.rm = TRUE), 1),
      systolic_median = round(median(SYSBP_t0, na.rm = TRUE), 1),
      systolic_min = min(SYSBP_t0, na.rm = TRUE),
      systolic_max = max(SYSBP_t0, na.rm = TRUE),
      systolic_n = sum(!is.na(SYSBP_t0)),
      diastolic_mean = round(mean(DIABP_t0, na.rm = TRUE), 1),
      diastolic_sd = round(sd(DIABP_t0, na.rm = TRUE), 1),
      diastolic_median = round(median(DIABP_t0, na.rm = TRUE), 1),
      diastolic_min = min(DIABP_t0, na.rm = TRUE),
      diastolic_max = max(DIABP_t0, na.rm = TRUE),
      diastolic_n = sum(!is.na(DIABP_t0)),
      # Waist circumference
      # Note: The column name is now WSTCIR_t0
      waist_mean = round(mean(WSTCIR_t0, na.rm = TRUE), 1),
      waist_sd = round(sd(WSTCIR_t0, na.rm = TRUE), 1),
      waist_median = round(median(WSTCIR_t0, na.rm = TRUE), 1),
      waist_min = min(WSTCIR_t0, na.rm = TRUE),
      waist_max = max(WSTCIR_t0, na.rm = TRUE),
      waist_n = sum(!is.na(WSTCIR_t0)),
      # BMI
      bmi_mean = round(mean(bmi_baseline, na.rm = TRUE), 1),
      bmi_sd = round(sd(bmi_baseline, na.rm = TRUE), 1),
      bmi_median = round(median(bmi_baseline, na.rm = TRUE), 1),
      bmi_min = min(bmi_baseline, na.rm = TRUE),
      bmi_max = max(bmi_baseline, na.rm = TRUE),
      bmi_n = sum(!is.na(bmi_baseline)),
      .groups = "drop"
    )
  
# Adding total row
  baseline_total <- master_dataset %>%
    filter(population_itt) %>%
    summarise(
      treatment_group = "Total",
      n = n(),
      # Demographics
      age_mean = round(mean(age_years, na.rm = TRUE), 1),
      age_sd = round(sd(age_years, na.rm = TRUE), 1),
      age_median = round(median(age_years, na.rm = TRUE), 1),
      age_min = min(age_years, na.rm = TRUE),
      age_max = max(age_years, na.rm = TRUE),
      # Sex distribution
      female_n = sum(sex == "female", na.rm = TRUE),
      female_percent = round(100 * female_n / n, 1),
      male_n = sum(sex == "male", na.rm = TRUE),
      male_percent = round(100 * male_n / n, 1),
      # Clinical measures at baseline
      weight_mean = round(mean(WEIGHT_t0, na.rm = TRUE), 1),
      weight_sd = round(sd(WEIGHT_t0, na.rm = TRUE), 1),
      weight_median = round(median(WEIGHT_t0, na.rm = TRUE), 1),
      weight_min = min(WEIGHT_t0, na.rm = TRUE),
      weight_max = max(WEIGHT_t0, na.rm = TRUE),
      weight_n = sum(!is.na(WEIGHT_t0)),
      # HbA1c
      hba1c_mean = round(mean(hba1c_percent_t0, na.rm = TRUE), 1),
      hba1c_sd = round(sd(hba1c_percent_t0, na.rm = TRUE), 1),
      hba1c_median = round(median(hba1c_percent_t0, na.rm = TRUE), 1),
      hba1c_min = min(hba1c_percent_t0, na.rm = TRUE),
      hba1c_max = max(hba1c_percent_t0, na.rm = TRUE),
      hba1c_n = sum(!is.na(hba1c_percent_t0)),
      # Blood pressure
      systolic_mean = round(mean(SYSBP_t0, na.rm = TRUE), 1),
      systolic_sd = round(sd(SYSBP_t0, na.rm = TRUE), 1),
      systolic_median = round(median(SYSBP_t0, na.rm = TRUE), 1),
      systolic_min = min(SYSBP_t0, na.rm = TRUE),
      systolic_max = max(SYSBP_t0, na.rm = TRUE),
      systolic_n = sum(!is.na(SYSBP_t0)),
      diastolic_mean = round(mean(DIABP_t0, na.rm = TRUE), 1),
      diastolic_sd = round(sd(DIABP_t0, na.rm = TRUE), 1),
      diastolic_median = round(median(DIABP_t0, na.rm = TRUE), 1),
      diastolic_min = min(DIABP_t0, na.rm = TRUE),
      diastolic_max = max(DIABP_t0, na.rm = TRUE),
      diastolic_n = sum(!is.na(DIABP_t0)),
      # Waist circumference
      waist_mean = round(mean(WSTCIR_t0, na.rm = TRUE), 1),
      waist_sd = round(sd(WSTCIR_t0, na.rm = TRUE), 1),
      waist_median = round(median(WSTCIR_t0, na.rm = TRUE), 1),
      waist_min = min(WSTCIR_t0, na.rm = TRUE),
      waist_max = max(WSTCIR_t0, na.rm = TRUE),
      waist_n = sum(!is.na(WSTCIR_t0)),
      # BMI
      bmi_mean = round(mean(bmi_baseline, na.rm = TRUE), 1),
      bmi_sd = round(sd(bmi_baseline, na.rm = TRUE), 1),
      bmi_median = round(median(bmi_baseline, na.rm = TRUE), 1),
      bmi_min = min(bmi_baseline, na.rm = TRUE),
      bmi_max = max(bmi_baseline, na.rm = TRUE),
      bmi_n = sum(!is.na(bmi_baseline))
    )
  
  baseline_demographics <- bind_rows(baseline_demographics, baseline_total)
  
# Creating the formatted table for clinical reporting
  baseline_table_formatted <- baseline_demographics %>%
    mutate(
      `N` = as.character(n),
      `Age, years` = paste0(age_mean, " ± ", age_sd, " (", age_median, ") [", age_min, "-", age_max, "]"),
      `Female sex, n (%)` = paste0(female_n, " (", female_percent, "%)"),
      `Male sex, n (%)` = paste0(male_n, " (", male_percent, "%)"),
      `Weight, kg` = paste0(weight_mean, " ± ", weight_sd, " (", weight_median, ") [", weight_min, "-", weight_max, "] (n=", weight_n, ")"),
      `HbA1c, %` = paste0(hba1c_mean, " ± ", hba1c_sd, " (", hba1c_median, ") [", hba1c_min, "-", hba1c_max, "] (n=", hba1c_n, ")"),
      `Systolic BP, mmHg` = paste0(systolic_mean, " ± ", systolic_sd, " (", systolic_median, ") [", systolic_min, "-", systolic_max, "] (n=", systolic_n, ")"),
      `Diastolic BP, mmHg` = paste0(diastolic_mean, " ± ", diastolic_sd, " (", diastolic_median, ") [", diastolic_min, "-", diastolic_max, "] (n=", diastolic_n, ")"),
      `Waist circumference, cm` = paste0(waist_mean, " ± ", waist_sd, " (", waist_median, ") [", waist_min, "-", waist_max, "] (n=", waist_n, ")"),
      `BMI, kg/m²` = paste0(bmi_mean, " ± ", bmi_sd, " (", bmi_median, ") [", bmi_min, "-", bmi_max, "] (n=", bmi_n, ")")
    ) %>%
    select(treatment_group, N, `Age, years`, `Female sex, n (%)`, `Male sex, n (%)`,
           `Weight, kg`, `HbA1c, %`, `Systolic BP, mmHg`, `Diastolic BP, mmHg`,
           `Waist circumference, cm`, `BMI, kg/m²`)
  
# Display results
  cat("\nBaseline Demographics and Clinical Characteristics Table:\n")
  print(baseline_table_formatted)
  
#-------------------------------------------------------------------------------------------
# Section 13 - Saving the outpus and datasets
#-------------------------------------------------------------------------------------------
  cat("\n=== Saving Outputs ===\n")
  
# Save datasets to master folder
  write_csv(master_dataset, file.path(master_path, "master_dataset.csv"))
  write_csv(ae_clean, file.path(master_path, "adverse_events_analysis.csv"))
  write_csv(meds_clean, file.path(master_path, "medications_analysis.csv"))
  write_csv(baseline_demographics, file.path(master_path, "baseline_demographics_detailed.csv"))
  
# Save reports to the new output/tables folder
  write_csv(missing_data_report, file.path(output_path, "missing_data_report.csv"))
  write_csv(baseline_table_formatted, file.path(output_path, "baseline_demographics_formatted.csv"))
  
  cat("\n✓ Files saved to:", master_path, "\n")
  cat("  - master_dataset.csv\n")
  cat("  - adverse_events_analysis.csv\n")
  cat("  - medications_analysis.csv\n")
  cat("  - baseline_demographics_detailed.csv\n")
  cat("\n✓ Reports saved to:", output_path, "\n")
  cat("  - missing_data_report.csv\n")
  cat("  - baseline_demographics_formatted.csv\n")
  
# Final Summary Report
  cat("\n=== PHASE 1 COMPLETION SUMMARY ===\n")
  
  # Population summary
  population_summary <- master_dataset %>%
    summarise(
      total_participants = n(),
      unique_participants = length(unique(subject_id)),
      itt_population = sum(population_itt, na.rm = TRUE),
      fas_population = sum(population_fas, na.rm = TRUE),
      intervention_group = sum(treatment_group == "intervention", na.rm = TRUE),
      control_group = sum(treatment_group == "control", na.rm = TRUE),
      missing_treatment_group = sum(is.na(treatment_group))
    )
  
  cat("✓ Master dataset created:", population_summary$total_participants, "participants\n")
  cat("✓ Unique participants identified:", population_summary$unique_participants, "\n")
  cat("✓ Treatment groups identified:\n")
  cat("  - Intervention:", population_summary$intervention_group, "participants\n")
  cat("  - Control:", population_summary$control_group, "participants\n")
  if (population_summary$missing_treatment_group > 0) {
    cat("  - Missing treatment assignment:", population_summary$missing_treatment_group, "participants\n")
  }
  
  cat("✓ Analysis populations defined:\n")
  cat("  - ITT:", population_summary$itt_population, "participants\n")
  cat("  - FAS:", population_summary$fas_population, "participants\n")
  
  # Corrected section: Check data availability by timepoint
  summary_vars_to_check <- c("WEIGHT", "hba1c_percent", "SYSBP", "DIABP", "WSTCIR")
  
  cat("✓ Data availability by timepoint:\n")
  for (var in summary_vars_to_check) {
    var_name <- case_when(
      var == "WEIGHT" ~ "Weight",
      var == "hba1c_percent" ~ "HbA1c",
      var == "SYSBP" ~ "Systolic BP",
      var == "DIABP" ~ "Diastolic BP",
      var == "WSTCIR" ~ "Waist Circumference",
      TRUE ~ var
    )
    
    available_data_counts <- c()
    for (t in c("t0", "t1", "t2", "t3")) {
      col_name <- if (var == "hba1c_percent") {
        paste0(var, "_", t)
      } else {
        paste0(str_to_upper(var), "_", t)
      }
      
      if (col_name %in% names(master_dataset)) {
        n_available <- sum(!is.na(master_dataset[[col_name]]))
        available_data_counts <- c(available_data_counts, paste0(t, " = ", n_available))
      } else {
        available_data_counts <- c(available_data_counts, paste0(t, " = 0"))
      }
    }
    
    if (length(available_data_counts) > 0) {
      cat(" ", var_name, ":", paste(available_data_counts, collapse = ", "), "\n")
    }
  }
  
  cat("\n=== READY FOR PHASE 1 REVIEW BY SILJA ===\n")
#
#
#
###################################### END OF SCRIPT #######################################
  