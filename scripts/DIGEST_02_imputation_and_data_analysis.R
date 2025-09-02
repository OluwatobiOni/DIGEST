############################################################################################
# Name of file: DIGEST_02_imputation_and_efficacy_analysis.R
#
# Original author: Oluwatobi Oni
# Original date: August, 2025
# Written/run on: RStudio Desktop for macOS version 2025.05.1+513
# Version of R: 4.5.1
#
# Description of content
#       This script performs the primary and secondary statistical analyses.
#       Specifically, it:
#         - Imputes missing data using Multiple Imputation by Chained Equations (MICE)
#         - Computes within-subject change scores
#         - Performs the primary endpoint analysis
#         - Performs the secondary endpoint analyses
#         - Generates tables and figures to summarise the findings
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
required_packages_02 <- c(
  "tidyverse", # Using the tidyverse packages for data wrangling
  "janitor", # For cleaning column names
  "readr", # For reading in the .csv files
  "lubridate", # To simplify working with dates and times
  "kableExtra", # Enhancing table appearance
  "knitr", # For easier reporting
  "mice", # For multiple imputation
  "VIM", # For visualising the missing data
  "pool", # Pooling together the result from multiple imputation
  "lme4", # Mixed effects models
  "nlme", # Alternative to lme4
  "geepack", # Generalised estimating equations
  "ggplot2", # For making plots
  "tableone", # For creation of tables
  "broom", # For tidy model outputs
  "broom.mixed", # Tidy mixed model outputs
  "car", # For Q-Q plts and normality tests
  "effectsize", # Effect size calculator
  "emmeans", # Estimated marginal means
  "multcomp", # Multiple comparisons
  "nortest", # Tests for normality
  "logistf", # Load required package for Firth logistic regression
  "here" # For specifying file paths to make finding files easier
)

# Installing any missing packages
# For automation purposes, I am defining the function 'install_if_missing_02' to install
# "required_packages_02" only if not already installed. Helps for reproducility.
install_if_missing_02 <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
invisible(lapply(required_packages_02, install_if_missing_02))

# Loading all packages above
invisible(lapply(required_packages_02, library, character.only = TRUE))

# Given that the analysis contains modelling and simulations, I am setting a random seed
# for reproducibility. Though, this number is arbitrary, it is however fixed for the
# purpose of this project and ensures reproducibility in this project.
set.seed(12345)
#
#-------------------------------------------------------------------------------------------
# Section 2 - Data Ingestion
#-------------------------------------------------------------------------------------------
# Reading in data file

# Defining file paths (using the "here" package)
data_path_02 <- here::here("data", "master")

# Reading in the cleaned and merged dataset
master <- read_csv(file.path(data_path_02, "master_dataset.csv")) %>%
  janitor::clean_names()

ae_analysis <- read_csv(file.path(data_path_02, "adverse_events_analysis.csv")) %>%
  janitor::clean_names()

med_analysis <- read_csv(file.path(data_path_02, "medications_analysis.csv")) %>%
  janitor::clean_names()

# Result of data injestion
# ae_analysis: 210 obs. of 14 variables
# master: 101 obs. of 39 variables
# med_analysis: 420 obs. of 14 variables

#-------------------------------------------------------------------------------------------
# Section 3 - Data Preparation and Study Populations
#-------------------------------------------------------------------------------------------

# Defining the study populations based on the SAP (Section 2.1)
define_study_populations <- function(master) {
  # This function uses the existing population columns in the master dataset to find
  # the indices of participants in each group.

  # ITT: All randomised patients
  itt_pop <- master$population_itt == TRUE

  # FAS: ITT with baseline and follow-up data for key variables
  # Using the correct column names from your dataset
  fas_criteria <- !is.na(master$weight_t0) & !is.na(master$hba1c_mmol_mol_t0) &
    !is.na(master$waist_circumference_t0) & !is.na(master$systolic_bp_t0) &
    !is.na(master$diastolic_bp_t0) & !is.na(master$weight_t1) &
    !is.na(master$hba1c_mmol_mol_t1) & !is.na(master$weight_t2) &
    !is.na(master$hba1c_mmol_mol_t2)

  # This line checks if the FAS flag matches the criteria from the SAP.
  fas_pop <- itt_pop & fas_criteria

  # PP: FAS with treatment adherence at 3 months
  pp_pop <- master$population_pp == TRUE

  # Returning a list of indices for each population (removed trailing comma)
  list(
    ITT = which(itt_pop),
    FAS = which(fas_pop),
    PP = which(pp_pop)
  )
}

# Applying the function to the master dataframe
populations <- define_study_populations(master)

# Providing a population summary based on the analysis
cat("=== STUDY POPULATION SUMMARY ===\n")
cat("ITT population:", length(populations$ITT), " participants\n")
cat("FAS population:", length(populations$FAS), " participants\n")
cat("PP population:", length(populations$PP), " participants\n")

# STUDY POPULATION SUMMARY
# ITT population: 101  participants - all patients enrolled and randomised
# FAS population: 0  participants - suggests that every participant has at least one missing values from t0 - t3
# PP population: 0  participants - Because we haven't checked for adherence at t1 in the data so returns false for all participants
# SP population: participants in the ITT who received at least one TDR meal - intervention group in this context

# We would go further to conduct imputation to handle the missingness in the data
#
#-------------------------------------------------------------------------------------------
# Section 4 - Missing data assessment and MICE setup
#-------------------------------------------------------------------------------------------
# Conducted as described in section 2.3 of the SAP

cat("\n=== MISSING DATA PATTERNS ===\n")

# Assessing missing data in ITT population in preparation for MICE
# The SAP requires that missing data for the entire study be assessed using
# the Intention-to-Treat (ITT) population, essentially a copy of the master because the
# study is randomised and all patients are randomly assigned.
# We use the 'populations$ITT' object above to continue with the analysis
itt_data <- master[populations$ITT, ]

# Defining the variable names
# Baseline variables (t0)
correct_baseline_vars <- c("age_years", "sex", "diabp_t0", "sysbp_t0", "weight_t0", "wstcir_t0", "hba1c_mmol_mol_t0", "bmi_baseline")

# Outcome variables at each time point
correct_outcome_vars_t1 <- c("sysbp_t1", "diabp_t1", "weight_t1", "wstcir_t1", "hba1c_mmol_mol_t1")
correct_outcome_vars_t2 <- c("sysbp_t2", "diabp_t2", "weight_t2", "wstcir_t2", "hba1c_mmol_mol_t2")
correct_outcome_vars_t3 <- c("sysbp_t3", "diabp_t3", "weight_t3", "wstcir_t3", "hba1c_mmol_mol_t3")

# Checking that all these variables exist in the dataset and none was dropped
all_vars_to_analyze <- c(correct_baseline_vars, correct_outcome_vars_t1, correct_outcome_vars_t2, correct_outcome_vars_t3)
missing_vars <- all_vars_to_analyze[!all_vars_to_analyze %in% names(itt_data)]
if (length(missing_vars) > 0) {
  cat("WARNING: Some variables still not found:", paste(missing_vars, collapse = ", "), "\n")
} else {
  cat("All variables found in dataset!\n")
}

# Reloading dplyr and tidyr due to package not recognising "c()"
library(dplyr)
library(tidyr)

# Reporting missing values per SAP requirements. This had been done in earlier script
# but is being done again to decide if there is need for sensitivity analysis. The SAP (2.4.4)
# specifies that sensitivity analysis should be done if data > 10% is missing at t2.

# This code creates a tidy summary of the number and percentage of missing values
# for all baseline and outcome variables.

# Select only the variables we need for analysis
selected_data <- itt_data[, all_vars_to_analyze, drop = FALSE]

# Creating missing data summary
missing_summary <- selected_data %>%
  # Use across() to apply the 'is.na' and 'sum' functions to all selected columns.
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  # Use 'pivot_longer()' to reshape the data into a tidy format.
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = round(missing_count / nrow(itt_data) * 100, 1)) %>%
  arrange(desc(missing_percent))

print(missing_summary)

# Visualising missing data patterns
# The VIM::aggr() function is crucial because it plots the *patterns* of missingness.
# This helps to understand if missing data is random (MAR) or missing completely
# at random (MCAR) or if missingness is related.
# other variables, which is a key assumption for the MICE imputation model.
par(mar = c(5.1, 10.1, 4.1, 2.1))
VIM::aggr(itt_data[all_vars_to_analyze],
  col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE
)
# Can always change colours to Habitual brand colours if needed.

# Checking if >10% missing for primary endpoint variables (triggers sensitivity analysis)
primary_missing_weight <- sum(is.na(itt_data$weight_t2)) / nrow(itt_data) * 100
primary_missing_hba1c <- sum(is.na(itt_data$hba1c_mmol_mol_t2)) / nrow(itt_data) * 100

cat("Missing data for primary endpoint:\n")
cat("Weight at t2:", primary_missing_weight, "%\n")
cat("HbA1c at t2:", primary_missing_hba1c, "%\n")

needs_imputation_sensitivity <- (primary_missing_weight > 10 | primary_missing_hba1c > 10)
cat("Imputation sensitivity analysis needed:", needs_imputation_sensitivity, "\n")

# Visualising missing data patterns and saving the plots
# We use the `png()` function to open a file for plotting and `dev.off()` to close it.
# The `here::here()` function creates a file path to save to DIGEST>output>plots.
png(here::here("output", "plots", "missing_data_plots.png"), width = 800, height = 600)
VIM::aggr(itt_data[all_vars_to_analyze],
  col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE
)
dev.off()
#
#-------------------------------------------------------------------------------------------
# Section 5 - Multiple Imputation by Chained Equations (MICE)
#-------------------------------------------------------------------------------------------
# Conducting MICE

cat("\n=== MULTIPLE IMPUTATION (MICE) ===\n")

# The MICE model needs all variables that will be used in the final analysis
# Filter the ITT population data to only include the variables needed for imputation
imputation_vars <- c(
  "subject_id", "treatment_group",
  correct_baseline_vars, correct_outcome_vars_t1,
  correct_outcome_vars_t2, correct_outcome_vars_t3
)

# ITT already includes only essential variables but this is done as a precaution
# FIX: The `select(all_of(...))` function was causing an `unused argument` error.
# This has been replaced with a more robust base R approach that works in all versions.
mice_data <- itt_data[, imputation_vars] %>%
  mutate(
    # Convert categorical variables to factors, which is essential for MICE.
    sex = as.factor(sex),
    treatment_group = as.factor(treatment_group)
  )

# MICE needs a predictor matrix and method vector to know which variables
# to use for imputation and what method to use for each variable.
pred_matrix <- make.predictorMatrix(mice_data)

# Set the 'subject_id' variable to 0 in the predictor matrix. This tells MICE
# not to use 'subject_id' to predict other variables, as it is a unique identifier.
pred_matrix[, "subject_id"] <- 0

# The method vector tells MICE which imputation method to use for each variable.
method_vector <- make.method(mice_data)
# We set the imputation method for 'subject_id' to an empty string,
# which tells MICE not to impute this variable.
method_vector["subject_id"] <- ""

# The SAP requires 5 imputed datasets. This command runs the MICE algorithm
# with 5 imputations over 20 iterations.
cat("Running MICE imputation (5 datasets per SAP)...\n")
mice_result <- mice(mice_data,
  m = 5, maxit = 20, method = method_vector,
  predictorMatrix = pred_matrix, printFlag = TRUE, seed = 12345
)

# Saving the imputed data outputs. First creating the folder to store them
output_folder <- here("data", "mice_datasets")
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}
# Then, we saving the 'mice' object to the new folder.
saveRDS(mice_result, file = here(output_folder, "mice_result.rds"))
cat("MICE results saved to:", here(output_folder, "mice_result.rds"), "\n")

# MICE diagnostics
cat("\n=== IMPUTATION DIAGNOSTICS ===\n")
# Plotting the diagnostics to check for convergence.
# The 'plot()' function shows trace plots, which should be 'fuzzy caterpillars',
# where the lines for each imputation dataset overlap and show no clear upward or downward trend.
# This confirms the model has converged and has reached a stable imputation solution.
plot(mice_result)
# The 'densityplot()' shows the distribution of imputed values against that of the original
# data. The distributions should be similar. This suggests that the imputed data is plausible
# and consistent with the original data's distribution
densityplot(mice_result, ~ weight_t1 + weight_t2 + hba1c_mmol_mol_t1 + hba1c_mmol_mol_t2)

# Save the diagnostic plots
png(here("output", "plots", "mice_diagnostics.png"), width = 1200, height = 800)
plot(mice_result)
dev.off()

png(here("output", "plots", "mice_density_plots.png"), width = 1200, height = 800)
densityplot(mice_result, ~ weight_t1 + weight_t2 + hba1c_mmol_mol_t1 + hba1c_mmol_mol_t2)
dev.off()
#
#-------------------------------------------------------------------------------------------
# Section 6 - Deriving Change Scores and Endpoint Variables
#-------------------------------------------------------------------------------------------
#

cat("\n=== DERIVING CHANGE SCORES (Δ) AND ENDPOINT VARIABLES ===\n")

# This function will compute the change scores and primary/secondary endpoints variables.
# The 'completed_data' argument is one of the imputed datasets.
compute_change_scores <- function(completed_data) {
  completed_data %>%
    mutate(
      delta_weight_t1 = weight_t1 - weight_t0,
      delta_weight_t2 = weight_t2 - weight_t0,
      delta_weight_t3 = weight_t3 - weight_t0,
      delta_hba1c_t1 = hba1c_mmol_mol_t1 - hba1c_mmol_mol_t0,
      delta_hba1c_t2 = hba1c_mmol_mol_t2 - hba1c_mmol_mol_t0,
      delta_hba1c_t3 = hba1c_mmol_mol_t3 - hba1c_mmol_mol_t0,
      delta_sysbp_t1 = sysbp_t1 - sysbp_t0,
      delta_sysbp_t2 = sysbp_t2 - sysbp_t0,
      delta_sysbp_t3 = sysbp_t3 - sysbp_t0,
      delta_diabp_t1 = diabp_t1 - diabp_t0,
      delta_diabp_t2 = diabp_t2 - diabp_t0,
      delta_diabp_t3 = diabp_t3 - diabp_t0,
      delta_wstcir_t1 = wstcir_t1 - wstcir_t0,
      delta_wstcir_t2 = wstcir_t2 - wstcir_t0,
      delta_wstcir_t3 = wstcir_t3 - wstcir_t0,

      # PRIMARY ENDPOINT
      # The endpoint variables are created here so they can be used as the
      # outcome variables in the regression and mixed-effects models.

      # Component 1: Weight loss ≥15kg at 6 months (t2) computed. e.g. If a participant's
      # weight loss is 15 kg or more, weight_loss_15kg_t2 is set to 1, otherwise 0.
      weight_loss_15kg_t2 = ifelse(delta_weight_t2 <= -15, 1, 0),

      # Component 2: HbA1c <6.5% (<48 mmol/mol) at 6 months after ≥2 months off medication
      # Assumption is that in the data collection, patients have been off meds for ≥2 months
      hba1c_remission_t2 = ifelse(hba1c_mmol_mol_t2 < 48, 1, 0),

      # Primary endpoint: Either component
      primary_endpoint = ifelse(weight_loss_15kg_t2 == 1 | hba1c_remission_t2 == 1, 1, 0),

      # SECONDARY ENDPOINTS
      # SEP7: Weight loss categories at 6 months
      weight_loss_5pct_t2 = ifelse(abs(delta_weight_t2) / weight_t0 >= 0.05 & delta_weight_t2 < 0, 1, 0),
      weight_loss_10pct_t2 = ifelse(abs(delta_weight_t2) / weight_t0 >= 0.10 & delta_weight_t2 < 0, 1, 0),
      weight_loss_15pct_t2 = ifelse(abs(delta_weight_t2) / weight_t0 >= 0.15 & delta_weight_t2 < 0, 1, 0),

      # SEP8: HbA1c remission at 6 months
      hba1c_remission_sep8 = hba1c_remission_t2,

      # SEP11: Weight loss categories at 12 months
      weight_loss_5pct_t3 = ifelse(abs(delta_weight_t3) / weight_t0 >= 0.05 & delta_weight_t3 < 0, 1, 0),
      weight_loss_10pct_t3 = ifelse(abs(delta_weight_t3) / weight_t0 >= 0.10 & delta_weight_t3 < 0, 1, 0),
      weight_loss_15pct_t3 = ifelse(abs(delta_weight_t3) / weight_t0 >= 0.15 & delta_weight_t3 < 0, 1, 0),

      # SEP12: HbA1c remission at 12 months
      hba1c_remission_t3 = ifelse(hba1c_mmol_mol_t3 < 48, 1, 0)
    )
}

# The 'imputed_datasets' object contains all 5 imputed datasets as a list.
# We apply the 'compute_change_scores' function to each one.
# This code handles the computation for all datasets automatically using lapply.
imputed_datasets <- complete(mice_result, action = "all")
imputed_datasets_with_scores <- lapply(imputed_datasets, compute_change_scores)

# To check the results, you can look at the first imputed dataset.
cat("\nDerived change scores and endpoints for the first imputed dataset:\n")
glimpse(imputed_datasets_with_scores[[1]])

# Saving the processed datasets with new scores and endpoints
saveRDS(imputed_datasets_with_scores, file = here("data", "mice_datasets", "imputed_datasets_with_scores.rds"))
cat("Processed datasets saved to:", here("data", "mice_datasets", "imputed_datasets_with_scores.rds"), "\n")
#
#-------------------------------------------------------------------------------------------
# Section 7 - Primary Endpoint Analysis by ITT
#-------------------------------------------------------------------------------------------
# This section performs the primary endpoint analysis using logistic regression,
# which is appropriate for the binary outcome variables (e.g., weight loss ≥15kg: 1,0).

cat("\n=== PRIMARY ENDPOINT ANALYSIS ===\n")

# This function fits the logistic regression models for a single dataset; which would later be applied to all 5
# The `glm()` function is used for this, with `family = binomial()` to specify
# a logistic regression model for binary outcomes.
fit_primary_endpoint <- function(data, population_ids = NULL) {
  # This part just filters the data if a specific population is required. For the ITT analysis, we use all participants.
  if (!is.null(population_ids)) {
    data <- data[data$subject_id %in% population_ids, ]
  }

  # Model 1: Weight loss ≥15kg
  # The outcome is the binary variable 'weight_loss_15kg_t2'.
  # The predictors are the treatment group, baseline weight, and sex (as defined in the SAP).
  model1 <- glm(weight_loss_15kg_t2 ~ treatment_group + weight_t0 + sex,
    data = data, family = binomial()
  )

  # Model 2: HbA1c remission
  # The outcome is the binary variable 'hba1c_remission_t2'.
  # The predictors are the treatment group, baseline weight, and sex (as defined in the SAP).
  model2 <- glm(hba1c_remission_t2 ~ treatment_group + hba1c_mmol_mol_t0 + sex,
    data = data, family = binomial()
  )

  list(weight_model = model1, hba1c_model = model2)
}

# Now, we use the imputed datasets with the new change scores and endpoints for analysis
# The object name is `imputed_datasets_with_scores`, as created in the previous step.
# We use lapply() to apply the model fitting function defined above to each of the 5 datasets.
primary_models_itt <- lapply(imputed_datasets_with_scores, function(x) fit_primary_endpoint(x))

# Pool results using Rubin's rules:
# The `pool()` function from the `mice` package combines the results of the
# five separate models into a single, overall result.
pool_weight_itt <- pool(lapply(primary_models_itt, function(x) x$weight_model))
pool_hba1c_itt <- pool(lapply(primary_models_itt, function(x) x$hba1c_model))

# Extracting the results:
# The `summary()` function is used here to extract the key statistical details
# from the pooled results, including the coefficients, standard errors, and p-values.
weight_results_itt <- summary(pool_weight_itt, conf.int = TRUE)
hba1c_results_itt <- summary(pool_hba1c_itt, conf.int = TRUE)

# Calculate effect sizes (Odds Ratios)
# This function calculates the Odds Ratio (OR) and its confidence intervals from the pooled model summary.
# The OR is the effect size for a logistic model. It is the exponentiated coefficient estimate.
calculate_effect_sizes <- function(model_results) {
  treatment_row <- which(grepl("treatment_group", model_results$term))
  if (length(treatment_row) > 0) {
    or <- exp(model_results$estimate[treatment_row])
    or_ci_lower <- exp(model_results$`2.5 %`[treatment_row])
    or_ci_upper <- exp(model_results$`97.5 %`[treatment_row])

    return(data.frame(
      OR = or,
      OR_CI_lower = or_ci_lower,
      OR_CI_upper = or_ci_upper,
      p_value = model_results$p.value[treatment_row]
    ))
  }
}

weight_effect_itt <- calculate_effect_sizes(weight_results_itt)
hba1c_effect_itt <- calculate_effect_sizes(hba1c_results_itt)

# Bonferroni-Holm correction per SAP Section 2.4.2
# This adjusts the p-values to control for the family-wise error rate when
# performing multiple tests. The Holm method is a standard, robust way to do this.
p_values_primary <- c(weight_effect_itt$p_value, hba1c_effect_itt$p_value)
p_adjusted_holm <- p.adjust(p_values_primary, method = "holm")

# Print the final results to the console.
cat("PRIMARY ENDPOINT RESULTS (ITT):\n")
cat("Weight loss ≥15kg:\n")
cat(
  "  OR =", round(weight_effect_itt$OR, 3),
  " (95% CI:", round(weight_effect_itt$OR_CI_lower, 3), "-", round(weight_effect_itt$OR_CI_upper, 3), ")\n"
)
cat("  p-value (adjusted) =", round(p_adjusted_holm[1], 4), "\n")
cat("HbA1c remission:\n")
cat(
  "  OR =", round(hba1c_effect_itt$OR, 3),
  " (95% CI:", round(hba1c_effect_itt$OR_CI_lower, 3), "-", round(hba1c_effect_itt$OR_CI_upper, 3), ")\n"
)
cat("  p-value (adjusted) =", round(p_adjusted_holm[2], 4), "\n")



#-------------------------------------------------------------------------------------------
# LOGISTIC REGRESSION DIAGNOSTICS TO FIGURE OUT THE CAUSE OF NON-SIGNIFICANCE AT t2

# Note: Using the first imputed dataset from the `imputed_datasets_with_scores` list for visualization.
data_to_plot <- imputed_datasets_with_scores[[1]]

# --- Visualisation for Weight Loss ≥15kg ---
# First, let's check the distribution of the outcome variable
cat("\nCounts for Weight Loss >= 15kg by Treatment Group:\n")
weight_counts <- data_to_plot %>%
  count(treatment_group, weight_loss_15kg_t2)
print(weight_counts)

# Creating a bar plot to visualise the proportions
# This plot will help us see if there is "complete separation" in the data.
# Complete separation occurs when one group has all '1's (successes) and another has all '0's (failures).
plot_weight <- ggplot(data_to_plot, aes(x = treatment_group, fill = as.factor(weight_loss_15kg_t2))) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Patients with Weight Loss ≥15kg by Treatment Group",
    x = "Treatment Group",
    y = "Proportion",
    fill = "Weight Loss ≥15kg"
  ) +
  scale_fill_manual(values = c("lightblue", "darkblue"), labels = c("No", "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(plot_weight)

# --- Visualisation for HbA1c Remission ---
# Check the counts for the HbA1c remission endpoint
cat("\nCounts for HbA1c Remission by Treatment Group:\n")
hba1c_counts <- data_to_plot %>%
  count(treatment_group, hba1c_remission_t2)
print(hba1c_counts)

# Creating a bar plot to visualise the proportions
plot_hba1c <- ggplot(data_to_plot, aes(x = treatment_group, fill = as.factor(hba1c_remission_t2))) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Patients with HbA1c Remission by Treatment Group",
    x = "Treatment Group",
    y = "Proportion",
    fill = "HbA1c Remission"
  ) +
  scale_fill_manual(values = c("lightgreen", "darkgreen"), labels = c("No", "Yes")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(plot_hba1c)

# --- Save Plots to File ---
# Define the path for the plots directory
plots_path <- here("output", "plots")

# Create the directory if it doesn't exist
if (!dir.exists(plots_path)) {
  dir.create(plots_path, recursive = TRUE)
  cat("Created plots directory at:", plots_path, "\n")
}

# Save the plots as high-resolution PNG files
cat("\nSaving plots to:", plots_path, "\n")

ggsave(
  filename = file.path(plots_path, "weight_loss_proportion.png"),
  plot = plot_weight,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(plots_path, "hba1c_remission_proportion.png"),
  plot = plot_hba1c,
  width = 8,
  height = 6,
  dpi = 300
)

cat("✓ Plots saved successfully.\n")

#-------------------------------------------------------------------------------------------
# Conducting Firth Logistic Regression due to model not converging for Weight loss at t2
# Here, I tell the model to run glm and switch to Firth if there is rare event

cat("\n=== PRIMARY ENDPOINT ANALYSIS (with rare-event handling) ===\n")

fit_primary_endpoint <- function(data, population_ids = NULL) {
  # Optional population filtering (ITT uses all participants)
  if (!is.null(population_ids)) {
    data <- data[data$subject_id %in% population_ids, ]
  }

  # Helper function to decide between glm() and Firth
  safe_logistic <- function(formula, data) {
    outcome <- all.vars(formula)[1]
    counts <- table(data[[outcome]])

    # Use Firth if fewer than 5 events OR fewer than 5 non-events
    if (min(counts) < 5) {
      message("Rare-event detected for ", outcome, " → using Firth logistic regression")
      logistf::logistf(formula, data = data)
    } else {
      glm(formula, data = data, family = binomial())
    }
  }

  # Model 1: Weight loss ≥15kg
  model1 <- safe_logistic(weight_loss_15kg_t2 ~ treatment_group + weight_t0 + sex, data)

  # Model 2: HbA1c remission
  model2 <- safe_logistic(hba1c_remission_t2 ~ treatment_group + hba1c_mmol_mol_t0 + sex, data)

  list(weight_model = model1, hba1c_model = model2)
}

# Apply to all imputed datasets
primary_models_itt <- lapply(imputed_datasets_with_scores, function(x) fit_primary_endpoint(x))

# Pool results:
# For pooling, Firth models don't directly work with mice::pool().
# We'll handle pooling separately for glm and Firth outputs.
extract_glm <- function(model_list, name) {
  glm_models <- lapply(model_list, function(x) x[[name]])
  glm_models[sapply(glm_models, inherits, "glm")]
}

# Pool only glm models (Firth results will be reported separately)
pool_weight_itt <- if (length(extract_glm(primary_models_itt, "weight_model")) > 0) {
  pool(extract_glm(primary_models_itt, "weight_model"))
} else {
  NULL
}

pool_hba1c_itt <- if (length(extract_glm(primary_models_itt, "hba1c_model")) > 0) {
  pool(extract_glm(primary_models_itt, "hba1c_model"))
} else {
  NULL
}

# Summaries for glm-based results
if (!is.null(pool_weight_itt)) {
  weight_results_itt <- summary(pool_weight_itt, conf.int = TRUE)
}
if (!is.null(pool_hba1c_itt)) {
  hba1c_results_itt <- summary(pool_hba1c_itt, conf.int = TRUE)
}

# Extract effect sizes (glm only)
calculate_effect_sizes <- function(model_results) {
  treatment_row <- which(grepl("treatment_group", model_results$term))
  if (length(treatment_row) > 0) {
    or <- exp(model_results$estimate[treatment_row])
    or_ci_lower <- exp(model_results$`2.5 %`[treatment_row])
    or_ci_upper <- exp(model_results$`97.5 %`[treatment_row])

    return(data.frame(
      OR = or,
      OR_CI_lower = or_ci_lower,
      OR_CI_upper = or_ci_upper,
      p_value = model_results$p.value[treatment_row]
    ))
  }
}

if (!is.null(pool_weight_itt)) {
  weight_effect_itt <- calculate_effect_sizes(weight_results_itt)
} else {
  weight_effect_itt <- NULL
}

if (!is.null(pool_hba1c_itt)) {
  hba1c_effect_itt <- calculate_effect_sizes(hba1c_results_itt)
} else {
  hba1c_effect_itt <- NULL
}

# Bonferroni-Holm correction
p_values_primary <- c(
  if (!is.null(weight_effect_itt)) weight_effect_itt$p_value else NA,
  if (!is.null(hba1c_effect_itt)) hba1c_effect_itt$p_value else NA
)
p_adjusted_holm <- p.adjust(p_values_primary, method = "holm")

# Final reporting
cat("\nPRIMARY ENDPOINT RESULTS (ITT):\n")

if (!is.null(weight_effect_itt)) {
  cat("Weight loss ≥15kg:\n")
  cat(
    "  OR =", round(weight_effect_itt$OR, 3),
    " (95% CI:", round(weight_effect_itt$OR_CI_lower, 3), "-", round(weight_effect_itt$OR_CI_upper, 3), ")\n"
  )
  cat("  p-value (adjusted) =", round(p_adjusted_holm[1], 4), "\n")
} else {
  cat("Weight loss ≥15kg model used Firth regression; see model output for details.\n")
}

if (!is.null(hba1c_effect_itt)) {
  cat("HbA1c remission:\n")
  cat(
    "  OR =", round(hba1c_effect_itt$OR, 3),
    " (95% CI:", round(hba1c_effect_itt$OR_CI_lower, 3), "-", round(hba1c_effect_itt$OR_CI_upper, 3), ")\n"
  )
  cat("  p-value (adjusted) =", round(p_adjusted_holm[2], 4), "\n")
} else {
  cat("HbA1c model used Firth regression; see model output for details.\n")
}

# --- Extract and print Firth regression results if used ---
if (exists("firth_weight_model")) {
  # Extract coefficients for treatment group
  firth_coef <- summary(firth_weight_model)$coef

  # Identify the treatment group row
  treat_row <- grep("treatment_group", rownames(firth_coef))

  if (length(treat_row) > 0) {
    est <- firth_coef[treat_row, "coef"]
    se <- firth_coef[treat_row, "se(coef)"]
    z_val <- firth_coef[treat_row, "z"]
    p_val <- firth_coef[treat_row, "Prob(z)"]

    # Calculate OR and CI
    or <- exp(est)
    ci_lower <- exp(est - 1.96 * se)
    ci_upper <- exp(est + 1.96 * se)

    cat("Weight loss ≥15kg (Firth logistic regression):\n")
    cat(
      "  OR =", round(or, 3),
      " (95% CI:", round(ci_lower, 3), "-", round(ci_upper, 3), ")\n"
    )
    cat("  p-value =", round(p_val, 4), "\n")
  }
}
#
#-------------------------------------------------------------------------------------------
# Regenerating the result above in a formatted table
# Load required package for Firth logistic regression

cat("\n=== PRIMARY ENDPOINT ANALYSIS (with rare-event handling) ===\n")

fit_primary_endpoint <- function(data, population_ids = NULL) {
  # Optional filtering
  if (!is.null(population_ids)) {
    data <- data[data$subject_id %in% population_ids, ]
  }

  # Helper: choose glm or Firth
  safe_logistic <- function(formula, data) {
    outcome <- all.vars(formula)[1]
    counts <- table(data[[outcome]])

    # Firth if rare events
    if (min(counts) < 5) {
      message("Rare-event detected for ", outcome, " → using Firth logistic regression")
      return(logistf::logistf(formula, data = data))
    } else {
      return(glm(formula, data = data, family = binomial()))
    }
  }

  # Primary endpoint models
  model1 <- safe_logistic(weight_loss_15kg_t2 ~ treatment_group + weight_t0 + sex, data)
  model2 <- safe_logistic(hba1c_remission_t2 ~ treatment_group + hba1c_mmol_mol_t0 + sex, data)

  list(weight_model = model1, hba1c_model = model2)
}

# Apply to all imputed datasets
primary_models_itt <- lapply(imputed_datasets_with_scores, fit_primary_endpoint)

# Pool glm models (Firth won't pool)
extract_glm <- function(model_list, name) {
  models <- lapply(model_list, function(x) x[[name]])
  models[sapply(models, inherits, "glm")]
}

pool_weight_itt <- if (length(extract_glm(primary_models_itt, "weight_model")) > 0) {
  pool(extract_glm(primary_models_itt, "weight_model"))
} else {
  NULL
}

pool_hba1c_itt <- if (length(extract_glm(primary_models_itt, "hba1c_model")) > 0) {
  pool(extract_glm(primary_models_itt, "hba1c_model"))
} else {
  NULL
}

# Extract glm effect sizes
calculate_effect_sizes <- function(model_results) {
  treatment_row <- which(grepl("treatment_group", model_results$term))
  if (length(treatment_row) > 0) {
    or <- exp(model_results$estimate[treatment_row])
    or_ci_lower <- exp(model_results$`2.5 %`[treatment_row])
    or_ci_upper <- exp(model_results$`97.5 %`[treatment_row])

    return(data.frame(
      OR = or,
      OR_CI_lower = or_ci_lower,
      OR_CI_upper = or_ci_upper,
      p_value = model_results$p.value[treatment_row]
    ))
  }
}

weight_effect_itt <- if (!is.null(pool_weight_itt)) {
  calculate_effect_sizes(summary(pool_weight_itt, conf.int = TRUE))
} else {
  NULL
}

hba1c_effect_itt <- if (!is.null(pool_hba1c_itt)) {
  calculate_effect_sizes(summary(pool_hba1c_itt, conf.int = TRUE))
} else {
  NULL
}

# Adjust p-values
p_values_primary <- c(
  if (!is.null(weight_effect_itt)) weight_effect_itt$p_value else NA,
  if (!is.null(hba1c_effect_itt)) hba1c_effect_itt$p_value else NA
)
p_adjusted_holm <- p.adjust(p_values_primary, method = "holm")

# --- Final reporting ---
cat("\nPRIMARY ENDPOINT RESULTS (ITT):\n")

# Weight loss ≥15kg
if (!is.null(weight_effect_itt)) {
  cat("Weight loss ≥15kg:\n")
  cat(
    "  OR =", round(weight_effect_itt$OR, 3),
    " (95% CI:", round(weight_effect_itt$OR_CI_lower, 3), "-", round(weight_effect_itt$OR_CI_upper, 3), ")\n"
  )
  cat("  p-value (adjusted) =", round(p_adjusted_holm[1], 4), "\n")
} else {
  firth_models <- lapply(primary_models_itt, function(x) x$weight_model)
  firth_models <- firth_models[sapply(firth_models, inherits, "logistf")]
  if (length(firth_models) > 0) {
    fm <- firth_models[[1]]

    # Correct way to access logistf coefficients
    coef_row <- grep("treatment_group", names(fm$coefficients))
    est <- fm$coefficients[coef_row]

    # Get confidence intervals from the ci.lower and ci.upper slots
    ci_lower_est <- fm$ci.lower[coef_row]
    ci_upper_est <- fm$ci.upper[coef_row]

    # Get p-value from the prob slot
    p_val <- fm$prob[coef_row]

    # Calculate OR and CI
    or <- exp(est)
    ci_lower <- exp(ci_lower_est)
    ci_upper <- exp(ci_upper_est)

    cat("Weight loss ≥15kg (Firth logistic regression):\n")
    cat(
      "  OR =", round(or, 3),
      " (95% CI:", round(ci_lower, 3), "-", round(ci_upper, 3), ")\n"
    )
    cat("  p-value =", round(p_val, 4), "\n")
  }
}

# HbA1c remission
if (!is.null(hba1c_effect_itt)) {
  cat("HbA1c remission:\n")
  cat(
    "  OR =", round(hba1c_effect_itt$OR, 3),
    " (95% CI:", round(hba1c_effect_itt$OR_CI_lower, 3), "-", round(hba1c_effect_itt$OR_CI_upper, 3), ")\n"
  )
  cat("  p-value (adjusted) =", round(p_adjusted_holm[2], 4), "\n")
} else {
  firth_models <- lapply(primary_models_itt, function(x) x$hba1c_model)
  firth_models <- firth_models[sapply(firth_models, inherits, "logistf")]
  if (length(firth_models) > 0) {
    fm <- firth_models[[1]]
    coef_row <- grep("treatment_group", rownames(summary(fm)$coef))
    est <- summary(fm)$coef[coef_row, "coef"]
    se <- summary(fm)$coef[coef_row, "se(coef)"]
    p_val <- summary(fm)$coef[coef_row, "Prob(z)"]
    or <- exp(est)
    ci_lower <- exp(est - 1.96 * se)
    ci_upper <- exp(est + 1.96 * se)
    cat("HbA1c remission (Firth logistic regression):\n")
    cat(
      "  OR =", round(or, 3),
      " (95% CI:", round(ci_lower, 3), "-", round(ci_upper, 3), ")\n"
    )
    cat("  p-value =", round(p_val, 4), "\n")
  }
}
#

#-------------------------------------------------------------------------------------------
# Section 8 - Secondary Endpoint Analysis
#-------------------------------------------------------------------------------------------
# Q-Q Plots and normality testing

cat("\n=== NORMALITY TESTING FOR SECONDARY ENDPOINTS ===\n")
# This script performs normality testing on continuous secondary outcome
# variables using a combination of visual plots and statistical tests.
# The code has been updated to automatically save the generated Q-Q plots
# to the 'output/plots' directory.

# Define the normality testing function.
# The function takes data and a variable name as input,
# creates a Q-Q plot, and performs a statistical normality test.
test_normality <- function(data, variable) {
  # Q-Q plot: Visually check if the data points lie on the line.
  p <- ggplot(data, aes(sample = !!sym(variable))) +
    stat_qq() +
    stat_qq_line(color = "red") +
    ggtitle(paste("Q-Q Plot:", variable)) +
    theme_minimal()

  # Get the non-NA values for the test.
  test_data <- na.omit(data[[variable]])
  n <- length(test_data)

  # Shapiro-Wilk test for sample sizes less than 5000.
  if (n < 5000) {
    if (n > 3) { # Shapiro-Wilk requires at least 3 non-NA values.
      shapiro_test <- shapiro.test(test_data)
      is_normal <- shapiro_test$p.value > 0.05
      test_name <- "Shapiro-Wilk"
    } else {
      is_normal <- NA # Not enough data to test.
      test_name <- "Not enough data"
    }
  } else {
    # Anderson-Darling test for larger samples (n >= 5000).
    ad_test <- nortest::ad.test(test_data)
    is_normal <- ad_test$p.value > 0.05
    test_name <- "Anderson-Darling"
  }

  # Return a list of the results, including the plot object.
  return(list(
    is_normal = is_normal,
    test_name = test_name,
    plot = p
  ))
}

# Define the list of continuous outcome variables to test.
# NOTE: The variable names are now in lowercase to match the data.
continuous_outcomes <- c(
  "weight_t1", "weight_t2",
  "hba1c_mmol_mol_t1", "hba1c_mmol_mol_t2",
  "sysbp_t1", "sysbp_t2",
  "diabp_t1", "diabp_t2",
  "wstcir_t1", "wstcir_t2"
)

# Loop through the variables and perform the normality test.
# Ensure the output directory exists before saving plots.
output_dir <- "output/plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created directory:", output_dir, "\n")
}

cat("\n=== NORMALITY TESTING FOR SECONDARY ENDPOINTS ===\n")
normality_results <- list()

# Use the first imputed dataset for the normality check.
first_imputed_dataset <- imputed_datasets_with_scores[[1]]

for (var in continuous_outcomes) {
  cat("\nTesting normality for:", var, "\n")
  result <- tryCatch(
    {
      test_normality(first_imputed_dataset, var)
    },
    error = function(e) {
      cat("  An error occurred for", var, ":", e$message, "\n")
      return(list(is_normal = NA, test_name = "Error", plot = NULL))
    }
  )

  normality_results[[var]] <- result

  if (!is.na(result$is_normal)) {
    cat("  Normal distribution:", result$is_normal, " (p > 0.05 from", result$test_name, "test)\n")

    # Save the plot only if the function returned a plot object.
    if (!is.null(result$plot)) {
      ggsave(
        filename = paste0(output_dir, "/", var, "_qqplot.png"),
        plot = result$plot,
        width = 6,
        height = 6,
        dpi = 300
      )
      cat("  Saved Q-Q plot to:", paste0(output_dir, "/", var, "_qqplot.png"), "\n")
    }
  } else {
    cat("  Normality test could not be performed.\n")
  }
}
#
#-------------------------------------------------------------------------------------------
names(imputed_datasets[[1]])

cat("\n=== SECONDARY ENDPOINT ANALYSIS ===\n")

# --- Continuous outcomes (SEP1–SEP5) ---
fit_secondary_continuous <- function(data, outcome_prefix, baseline_var, is_normal = TRUE, population_ids = NULL) {
  # Filter to specific population if provided
  if (!is.null(population_ids)) {
    data <- data[data$subject_id %in% population_ids, ]
  }

  # Select outcome columns (excluding baseline) and keep baseline separately
  outcome_cols <- names(data)[grepl(paste0("^", outcome_prefix, "_t"), names(data)) & names(data) != baseline_var]
  all_cols <- c("subject_id", "treatment_group", "sex", baseline_var, outcome_cols)

  long_data <- data[, all_cols] %>%
    tidyr::pivot_longer(
      cols = all_of(outcome_cols),
      names_to = "time_factor",
      values_to = "value"
    ) %>%
    mutate(time_factor = factor(time_factor))

  if (!"value" %in% names(long_data)) {
    stop(paste("Outcome variable", outcome_prefix, "not found in long format data."))
  }

  model_formula <- reformulate(
    termlabels = c("treatment_group * time_factor", "sex", baseline_var),
    response = "value"
  )

  if (is_normal) {
    model <- nlme::lme(
      model_formula,
      data = long_data,
      random = ~ 1 | subject_id,
      na.action = na.omit
    )
  } else {
    model <- geepack::geeglm(
      model_formula,
      data = long_data,
      id = long_data$subject_id,
      corstr = "exchangeable",
      na.action = na.omit
    )
  }

  return(model)
}

# List of continuous outcomes and their baseline variables
continuous_endpoints <- list(
  hba1c = list(outcome_prefix = "hba1c_mmol_mol", baseline = "hba1c_mmol_mol_t0"),
  weight = list(outcome_prefix = "weight", baseline = "weight_t0"),
  wstcir = list(outcome_prefix = "wstcir", baseline = "wstcir_t0"),
  sysbp = list(outcome_prefix = "sysbp", baseline = "sysbp_t0"),
  diabp = list(outcome_prefix = "diabp", baseline = "diabp_t0")
)

# Loop over continuous endpoints
continuous_results <- list()

for (sep_name in names(continuous_endpoints)) {
  cat("\nAnalyzing", sep_name, "change\n")
  ep <- continuous_endpoints[[sep_name]]

  # Check normality at t1 and t2
  is_normal <- normality_results[[paste0(ep$outcome_prefix, "_t1")]]$is_normal &&
    normality_results[[paste0(ep$outcome_prefix, "_t2")]]$is_normal

  # Fit models across all imputed datasets with change scores
  models_itt <- lapply(imputed_datasets_with_scores, function(x) {
    fit_secondary_continuous(x, ep$outcome_prefix, ep$baseline, is_normal = is_normal)
  })

  # Pool and summarise if normal
  if (is_normal) {
    pooled <- pool(models_itt)
    continuous_results[[sep_name]] <- summary(pooled, conf.int = TRUE)
    print(continuous_results[[sep_name]][, c("term", "estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high")])
  } else {
    cat("Using GEE for non-normal", sep_name, "data. Manual pooling of GEE results is required.\n")
  }
}

# --- Binary responder outcomes (SEP7) ---
fit_responder_analysis <- function(data, outcome_var, population_ids = NULL) {
  if (!is.null(population_ids)) {
    data <- data[data$subject_id %in% population_ids, ]
  }

  if (!outcome_var %in% names(data)) {
    stop(paste("Outcome variable", outcome_var, "not found in dataset"))
  }

  formula_str <- paste(outcome_var, "~ treatment_group + weight_t0 + sex")
  glm(as.formula(formula_str), data = data, family = binomial())
}

responder_outcomes <- c("weight_loss_5pct_t2", "weight_loss_10pct_t2", "weight_loss_15pct_t2")
responder_models_itt <- list()

for (outcome in responder_outcomes) {
  cat("\nAnalyzing responder rate:", outcome, "\n")

  # Fit model for each imputed dataset with change scores
  responder_models_itt[[outcome]] <- lapply(imputed_datasets_with_scores, function(x) {
    fit_responder_analysis(x, outcome)
  })

  # Pool results
  pooled_result <- pool(responder_models_itt[[outcome]])
  summary_result <- summary(pooled_result, conf.int = TRUE)

  # Print readable table
  print(summary_result[, c("term", "estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high")])
}
#
#-------------------------------------------------------------------------------------------
# hba1c change, weight change, and wstcir was analysed using GEE and would need manual pooling across all data sets
# Processing one outcome
process_outcome <- function(imputed_datasets, outcome_prefix, outcome_label) {
  # Step 1: Convert to long format
  imputed_datasets_long <- map(imputed_datasets, ~
    .x %>%
      pivot_longer(
        cols = starts_with(outcome_prefix),
        names_to = "time",
        values_to = "value"
      ) %>%
      mutate(
        time = case_when(
          str_detect(time, "_t0") ~ 0,
          str_detect(time, "_t1") ~ 1,
          str_detect(time, "_t2") ~ 2,
          str_detect(time, "_t3") ~ 3
        )
      ))

  # Step 2: Fit LMM for each imputed dataset
  models <- map(imputed_datasets_long, ~
    lmer(value ~ time + treatment_group + (1 | subject_id), data = .x))

  # Step 3: Extract coefficients and SEs
  report_df <- map_df(models, ~ tidy(.x), .id = "model_id") %>%
    rename(
      Estimate = estimate,
      Std.error = std.error
    ) %>%
    filter(!str_detect(term, "sd__")) %>% # remove random effect SD rows
    dplyr::select(model_id, term, Estimate, Std.error)

  # Step 4: Manual pooling (mean & sd)
  pooled_report <- report_df %>%
    group_by(term) %>%
    summarise(
      Estimate_pooled = mean(Estimate),
      SE_pooled = sd(Estimate), # between-imputation variability only
      .groups = "drop"
    )

  # Step 5: Print results
  cat("\n==============================\n")
  cat("Outcome:", outcome_label, "\n")
  cat("==============================\n")
  print(report_df)
  cat("\nPooled Results (Manual):\n")
  print(pooled_report)
}

#-------------------------------------------------------------------------------------------
# Running on all four outcomes
# hba1c
process_outcome(imputed_datasets, "hba1c_mmol_mol_", "HbA1c (mmol/mol)")

# weight change
process_outcome(imputed_datasets, "weight_", "Weight (kg)")

# waist circumference
process_outcome(imputed_datasets, "wstcir_", "Waist Circumference (cm)")

# diastolic blood pressure
process_outcome(imputed_datasets, "diabp_", "Diastolic BP (mmHg)")
#
#-------------------------------------------------------------------------------------------
# Section 9 - Sensitivity Analysis
#-------------------------------------------------------------------------------------------
#
cat("\n=== SENSITIVITY ANALYSES (SAP Table 2) ===\n")


# --------------------------------------------------------------------------
# This function fits logistic regression models for a single dataset.
fit_primary_endpoint <- function(data, population_ids = NULL) {
  # Population filtering (ITT uses all participants)
  if (!is.null(population_ids)) {
    data <- data[data$subject_id %in% population_ids, ]
  }

  # Helper function to decide between glm() and Firth
  safe_logistic <- function(formula, data) {
    outcome <- all.vars(formula)[1]
    # Check if the outcome variable has any data
    if (nrow(data) == 0 || all(is.na(data[[outcome]]))) {
      stop("Outcome data is missing or empty for this population.")
    }
    counts <- table(data[[outcome]])

    # Use Firth if fewer than 5 events OR fewer than 5 non-events
    if (min(counts) < 5) {
      message("Rare-event detected for ", outcome, " -> using Firth logistic regression")
      logistf::logistf(formula, data = data)
    } else {
      glm(formula, data = data, family = binomial())
    }
  }

  # Model 1: Weight loss ≥15kg
  model1 <- tryCatch(
    {
      safe_logistic(weight_loss_15kg_t2 ~ treatment_group + weight_t0 + sex, data)
    },
    error = function(e) {
      warning("Error fitting weight loss model: ", e$message)
      return(NULL)
    }
  )

  # Model 2: HbA1c remission
  model2 <- tryCatch(
    {
      safe_logistic(hba1c_remission_t2 ~ treatment_group + hba1c_mmol_mol_t0 + sex, data)
    },
    error = function(e) {
      warning("Error fitting HbA1c remission model: ", e$message)
      return(NULL)
    }
  )

  list(weight_model = model1, hba1c_model = model2)
}

# This function fits either a MMRM or GEE model for a single dataset.
fit_secondary_continuous <- function(data, outcome_prefix, baseline_var, is_normal = TRUE, population_ids = NULL) {
  # Filter to specific population if provided
  if (!is.null(population_ids)) {
    data <- data[data$subject_id %in% population_ids, ]
  }

  # Select outcome columns (excluding baseline) and keep baseline separately
  outcome_cols <- names(data)[grepl(paste0("^", outcome_prefix, "_t"), names(data)) & names(data) != baseline_var]
  all_cols <- c("subject_id", "treatment_group", "sex", baseline_var, outcome_cols)

  long_data <- data[, all_cols] %>%
    tidyr::pivot_longer(
      cols = all_of(outcome_cols),
      names_to = "time_factor",
      values_to = "value"
    ) %>%
    mutate(time_factor = factor(time_factor))

  if (!"value" %in% names(long_data)) {
    stop(paste("Outcome variable", outcome_prefix, "not found in long format data."))
  }

  model_formula <- reformulate(
    termlabels = c("treatment_group * time_factor", "sex", baseline_var),
    response = "value"
  )

  if (is_normal) {
    model <- nlme::lme(
      model_formula,
      data = long_data,
      random = ~ 1 | subject_id,
      na.action = na.omit
    )
  } else {
    model <- geepack::geeglm(
      model_formula,
      data = long_data,
      id = long_data$subject_id,
      corstr = "exchangeable",
      na.action = na.omit
    )
  }

  return(model)
}

# This function fits a glm for binary responder outcomes.
fit_responder_analysis <- function(data, outcome_var, population_ids = NULL) {
  if (!is.null(population_ids)) {
    data <- data[data$subject_id %in% population_ids, ]
  }

  if (!outcome_var %in% names(data)) {
    stop(paste("Outcome variable", outcome_var, "not found in dataset"))
  }

  formula_str <- paste(outcome_var, "~ treatment_group + weight_t0 + sex")
  glm(as.formula(formula_str), data = data, family = binomial())
}

# Running the sensitivity analysis
run_sensitivity_analysis <- function(analysis_number, endpoint_type, outcome_name, population_name) {
  cat("\nRunning Sensitivity Analysis", analysis_number, ":", outcome_name, "- Population:", population_name, "\n")

  # Get population IDs
  pop_ids <- switch(population_name,
    "FAS" = populations$FAS,
    "PP" = populations$PP,
    populations$ITT
  )

  # Check if the population is valid and has at least two treatment groups
  if (length(pop_ids) == 0) {
    warning(paste("Population", population_name, "is empty. Skipping analysis."))
    return(NULL)
  }

  # A quick check to make sure the data contains multiple treatment groups
  temp_data <- imputed_datasets_with_scores[[1]][imputed_datasets_with_scores[[1]]$subject_id %in% pop_ids, ]
  if (length(unique(temp_data$treatment_group)) < 2) {
    warning(paste("Population", population_name, "does not have multiple treatment groups. Skipping analysis."))
    return(NULL)
  }


  results <- NULL

  if (endpoint_type == "PEP") {
    # Primary endpoint analysis
    models <- lapply(imputed_datasets_with_scores, function(x) {
      fit_primary_endpoint(x, pop_ids)
    })

    # Pool the results for the two separate logistic models.
    # Note: We can only pool glm models, so we filter out any Firth models.
    pool_weight <- pool(lapply(models, function(x) x$weight_model[sapply(x$weight_model, inherits, "glm")]))
    pool_hba1c <- pool(lapply(models, function(x) x$hba1c_model[sapply(x$hba1c_model, inherits, "glm")]))

    results <- list(
      weight_results = if (!is.null(pool_weight)) summary(pool_weight, conf.int = TRUE) else "Firth model used, no pooling.",
      hba1c_results = if (!is.null(pool_hba1c)) summary(pool_hba1c, conf.int = TRUE) else "Firth model used, no pooling."
    )
  } else if (grepl("SEP", endpoint_type)) {
    # Secondary endpoint analysis
    if (outcome_name %in% c(
      "HbA1c reduction", "Weight change", "Waist circumference change",
      "Systolic blood pressure change", "Diastolic blood pressure change"
    )) {
      # Continuous outcomes
      outcome_prefix <- switch(outcome_name,
        "HbA1c reduction" = "hba1c_mmol_mol",
        "Weight change" = "weight",
        "Waist circumference change" = "wstcir",
        "Systolic blood pressure change" = "sysbp",
        "Diastolic blood pressure change" = "diabp"
      )

      baseline_var <- paste0(outcome_prefix, "_t0")
      is_normal_flag <- normality_results[[paste0(outcome_prefix, "_t1")]]$is_normal &&
        normality_results[[paste0(outcome_prefix, "_t2")]]$is_normal

      models <- lapply(imputed_datasets_with_scores, function(x) {
        fit_secondary_continuous(x, outcome_prefix, baseline_var, is_normal = is_normal_flag, population_ids = pop_ids)
      })

      if (is_normal_flag) {
        pooled <- pool(models)
        results <- summary(pooled, conf.int = TRUE)
      } else {
        results <- "GEE models used for non-normal data. Manual pooling is required."
      }
    } else {
      # Binary outcomes (responder rates, remission)
      outcome_var <- switch(outcome_name,
        "Weight reduction of at least 5% (10%, 15%) from t0 to t2" = "weight_loss_5pct_t2",
        "HbA1c of less than 6.5% at t2" = "hba1c_remission_t2"
      )

      models <- lapply(imputed_datasets_with_scores, function(x) {
        fit_responder_analysis(x, outcome_var, population_ids = pop_ids)
      })

      pooled <- pool(models)
      results <- summary(pooled, conf.int = TRUE)
    }
  }

  return(results)
}

# Executing all secondary analysis as described by the SAP
sensitivity_analyses <- list(
  "SA1" = run_sensitivity_analysis(1, "PEP", "Primary endpoint (weight and HbA1c)", "FAS"),
  "SA2" = run_sensitivity_analysis(2, "PEP", "Primary endpoint (weight and HbA1c)", "PP"),
  "SA3" = run_sensitivity_analysis(3, "SEP1", "HbA1c reduction", "PP"),
  "SA4" = run_sensitivity_analysis(4, "SEP2", "Weight change", "PP"),
  "SA5" = run_sensitivity_analysis(5, "SEP3", "Waist circumference change", "PP"),
  "SA6" = run_sensitivity_analysis(6, "SEP4", "Systolic blood pressure change", "PP"),
  "SA7" = run_sensitivity_analysis(7, "SEP5", "Diastolic blood pressure change", "PP"),
  "SA8" = "SEP6 analysis for change in medication is not implemented here. It's a binary outcome.",
  "SA9" = run_sensitivity_analysis(9, "SEP7", "Weight reduction of at least 5% (10%, 15%) from t0 to t2", "PP"),
  "SA10" = run_sensitivity_analysis(10, "SEP8", "HbA1c of less than 6.5% at t2", "PP")
)

# One can print the results for each analysis, for example:print(sensitivity_analyses$SA1)
#
#-------------------------------------------------------------------------------------------
# Section 10 - Subgroup Analysis - By Sex as defined in the SAP
#-------------------------------------------------------------------------------------------
#
# This function calculates the responder outcomes for a given dataset,
# defining a responder as a participant who achieved a specific percentage
# weight loss at the final timepoint (t2).

create_responder_variables <- function(dataset) {
  # Calculate weight change percentage from baseline (t0) to t2
  dataset$weight_change_pct_t2 <- (dataset$weight_t0 - dataset$weight_t2) / dataset$weight_t0 * 100

  # Create binary responder variables for each threshold
  dataset$weight_loss_5pct_t2 <- as.numeric(dataset$weight_change_pct_t2 >= 5)
  dataset$weight_loss_10pct_t2 <- as.numeric(dataset$weight_change_pct_t2 >= 10)
  dataset$weight_loss_15pct_t2 <- as.numeric(dataset$weight_change_pct_t2 >= 15)

  return(dataset)
}

# Apply the new function to each dataset in the imputed_datasets list.
# This ensures all subsequent analyses have access to the responder variables.
imputed_datasets <- lapply(imputed_datasets, create_responder_variables)

cat("\n=== SUBGROUP ANALYSES BY SEX ===\n")

fit_subgroup_continuous <- function(data, outcome_prefix, baseline_var, is_normal = TRUE) {
  # Select outcome columns (excluding baseline) and keep baseline separately
  outcome_cols <- names(data)[grepl(paste0("^", outcome_prefix, "_t"), names(data)) & names(data) != baseline_var]
  all_cols <- c("subject_id", "treatment_group", baseline_var, outcome_cols)

  long_data <- data[, all_cols] %>%
    tidyr::pivot_longer(
      cols = all_of(outcome_cols),
      names_to = "time_factor",
      values_to = "value"
    ) %>%
    mutate(time_factor = factor(time_factor))

  if (!"value" %in% names(long_data)) {
    stop(paste("Outcome variable", outcome_prefix, "not found in long format data."))
  }

  # The model formula now EXCLUDES the 'sex' variable.
  model_formula <- reformulate(
    termlabels = c("treatment_group * time_factor", baseline_var),
    response = "value"
  )

  if (is_normal) {
    model <- nlme::lme(
      model_formula,
      data = long_data,
      random = ~ 1 | subject_id,
      na.action = na.omit
    )
  } else {
    model <- geepack::geeglm(
      model_formula,
      data = long_data,
      id = long_data$subject_id,
      corstr = "exchangeable",
      na.action = na.omit
    )
  }

  return(model)
}

# This function is a modified version of `fit_responder_analysis` that
# excludes 'sex' from the model formula.
fit_subgroup_responder <- function(data, outcome_var) {
  if (!outcome_var %in% names(data)) {
    stop(paste("Outcome variable", outcome_var, "not found in dataset"))
  }

  # The formula now EXCLUDES the 'sex' variable.
  formula_str <- paste(outcome_var, "~ treatment_group + weight_t0")
  glm(as.formula(formula_str), data = data, family = binomial())
}

# This function takes a dataset and a sex filter, then applies the
# appropriate subgroup-specific models for all endpoints.
run_subgroup_analysis <- function(dataset, sex_filter) {
  filtered_data <- dataset[dataset$sex == sex_filter, ]

  # Return NULL to avoid errors if the subgroup has no data.
  if (nrow(filtered_data) == 0) {
    cat("\nWarning: No data for subgroup ", sex_filter, ". Skipping analysis.\n", sep = "")
    return(NULL)
  }

  # Define the continuous outcomes and their baseline variables from your previous script.
  continuous_endpoints <- list(
    hba1c = list(outcome_prefix = "hba1c_mmol_mol", baseline = "hba1c_mmol_mol_t0"),
    weight = list(outcome_prefix = "weight", baseline = "weight_t0"),
    wstcir = list(outcome_prefix = "wstcir", baseline = "wstcir_t0"),
    sysbp = list(outcome_prefix = "sysbp", baseline = "sysbp_t0"),
    diabp = list(outcome_prefix = "diabp", baseline = "diabp_t0")
  )

  # A list to store the fitted models for this dataset.
  subgroup_models <- list()

  # Run analysis for each continuous endpoint using the new subgroup function
  for (ep_name in names(continuous_endpoints)) {
    ep <- continuous_endpoints[[ep_name]]
    is_normal <- TRUE
    if (ep$outcome_prefix %in% names(normality_results)) {
      is_normal <- normality_results[[ep$outcome_prefix]]$is_normal
    }
    subgroup_models[[ep_name]] <- fit_subgroup_continuous(
      filtered_data,
      ep$outcome_prefix,
      ep$baseline,
      is_normal = is_normal
    )
  }

  # Run analysis for each binary responder outcome using the new subgroup function
  responder_outcomes <- c("weight_loss_5pct_t2", "weight_loss_10pct_t2", "weight_loss_15pct_t2")
  for (outcome in responder_outcomes) {
    subgroup_models[[outcome]] <- fit_subgroup_responder(filtered_data, outcome)
  }

  return(subgroup_models)
}

# --- Step 1: Run the analyses on each imputed dataset for each sex ---
male_analyses <- lapply(imputed_datasets, function(dataset) {
  run_subgroup_analysis(dataset, "male")
})

female_analyses <- lapply(imputed_datasets, function(dataset) {
  run_subgroup_analysis(dataset, "female")
})

# --- Step 2: Define a function to pool and print results ---
# This function automates the pooling and printing for a given subgroup and endpoint.
pool_and_print_results <- function(analyses_list, endpoint_name, subgroup_name) {
  models_to_pool <- lapply(analyses_list, `[[`, endpoint_name)
  models_to_pool <- models_to_pool[!sapply(models_to_pool, is.null)]

  if (length(models_to_pool) > 0) {
    pooled_result <- pool(models_to_pool)
    summary_result <- summary(pooled_result, conf.int = TRUE)
    cat("\n", subgroup_name, " Subgroup - ", endpoint_name, " Results:\n", sep = "")
    print(summary_result[, c("term", "estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high")])
  } else {
    cat("\nNo valid models to pool for ", endpoint_name, " in the ", subgroup_name, " subgroup.\n", sep = "")
  }
}

# --- Step 3: Print the results for all endpoints and subgroups ---
all_endpoints <- c(
  "hba1c", "weight", "wstcir", "sysbp", "diabp",
  "weight_loss_5pct_t2", "weight_loss_10pct_t2", "weight_loss_15pct_t2"
)

cat("\n--- SUBGROUP ANALYSIS RESULTS ---\n")

for (ep_name in all_endpoints) {
  pool_and_print_results(male_analyses, ep_name, "Male")
}

for (ep_name in all_endpoints) {
  pool_and_print_results(female_analyses, ep_name, "Female")
}
#
#
#
###################################### END OF SCRIPT #######################################
