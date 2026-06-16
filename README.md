# ISARanalysis

Generic functions and other tools to support cleaning and analysis of ISAR standardised research datasets.

## Download and install package

Download the library from Github:

```{r, eval=FALSE}
pak::pak("williameoswald/ISARanalysis")
```

Package install may require latest Rtools (4.5) <https://cran.r-project.org/bin/windows/Rtools/>

If unable to install with pak, alternatively try:

```{r, eval=FALSE}
devtools::install_github("williameoswald/ISARanalysis")
```

Similarly check that latest versions of tidyverse and other dependent packages are installed. A message may prompt for this.

## Setting up workspace and importing data

The following script is for setting up workspace (using RStudio) and importing the ISAR standardised research datasets.

Includes use of `import_datasets` function for importing list of ISAR standardised datasets and creating new "\_labelled" dataframe objects with variable labels assigned, categorical variables converted to factors with assigned levels, and "YYYY-MM-DD" dates checked and converted to ymd() dates using lubridate package. Any date parsing errors will be reported and summarised in an additional object with "\_date_parse_failures". All these cleaning steps use the "ISAR SRD Data Dictionary v1.0.xlsx" that must be imported as a "dictionary" object before use. Specify filename to import and whether you wish to keep a "\_raw" imported version of dataset without formatting.

```{r isar_master, eval=FALSE}
# ISAR assembly and analysis ----

# Modifications/Decisions:

# To do:

# SET UP ----
rm(list = ls())

options(scipen = 999)

# Change directory to source folder, if not in project
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required packages
if (!require("pacman")) {
  install.packages("pacman")
}
pkgs <- c(
  "tidyverse",
  "pak",
  "haven",
  "labelled",
   "readxl",
  "writexl",
  "here",
  "conflicted",
  "flextable",
  "janitor",
) |>
  pacman::p_load(pkgs, character.only = T)
rm(pkgs)
conflicted::conflict_prefer("filter", "dplyr")

pak::pak("williameoswald/ISARanalysis")
library(ISARanalysis)

# SPECIFICATIONS ----
# Location of data folder on Sharepoint
datapath <- "C:/path/to/folder/Dataset production"

# The most recent folder named with a date is the same as "latest".
# Use dated folder for a useful version tag inside dataset instead of "latest".
versiondate <- "20260603"

# ASSEMBLE DATA ----

# Import ISAR data dictionary
dictionary <- readxl::read_xlsx(
  here::here(datapath, "Documentation", "ISAR SRD Data Dictionary v1.0.xlsx")
)

# Create list of dataset filenames (without full path)
filenames <- list.files(
  here::here(datapath, "datasets", versiondate),
  pattern = "\\.csv$"
)

# Map over all listed files or select specific datasets
# filenames <- filenames[filenames != "7. HCRU.csv"] or filenames[c(1, 4)]
# Will create new objects in global environment for each dataset, tagged with 
# versiondate. Specify keep_raw = T to keep an unlabelled version of dataset.
filenames |>
  purrr::set_names() |>
  purrr::map(\(x) import_datasets(x, keep_raw = F), .progress = T)

```

## Cleaning datasets

Additional cleaning functions can be used after import function to conduct range checks and create additional (e.g. categorical) variables for analysis using imported and labelled datasets.

***clean_demographics***

Derives asthma onset categories (\<12, 12-40, \>40 years), asthma duration (in years), and asthma duration categories (\<10, 10-\<25, ≥25).

***clean_lifestyle***

Applies range checks and plausibility checks height and weight measures and cleans BMI measure accordingly. Derives categorical classifications for obese (BMI \<30 vs. ≥30 kg/m2).

***clean_comorbidities***

Updates labels and derives new variable for history of anxiety and/or depression.

***clean_asthma_control***

Recodes GINA asthma control responses to numeric values to calculate score and uses to derive GINA asthma control classification (Well controlled vs Partly controlled vs Uncontrolled). Derives categorical classifications for asthma control based on available ACQ and ACT scores.

***clean_spirometry***

Applies range checks and plausibility checks (FEV1 \>= FVC) to pre- and post-bronchodilator spirometry measures. Where post-bronchodilator measures are available at every timepoint for a patient, they are prioritised over pre-bronchodilator measures. Derives single composite FEV1, FVC, FEV1/FVC ratio, and percent predicted FEV1 measures, along with categorical classifications for FEV1 percent predicted (\<80% vs ≥80%) and FEV1/FVC ratio (\<0.70 vs ≥0.70).

*NB* - for longitudinal comparisons of FEV1, FVC, FEV1 percent predicted over time use pre- or post-bronchodilator measures consistently.

***clean_biomarkers***

Reshapes labelled biomarkers dataset to long format with multiple rows per test. Applies range checks (0 ≤ BEOS ≤ 5000; 0 ≤ FeNO ≤ 300; 0 ≤ IgE ≤ 100000) to recorded biomarker measures. Flags cases where multiple results are recorded for a test on a single date, and allows for three approaches for reducing these duplicates, either keeping minimum, mean, or maximum value. Further cleaning steps to drop these duplicates and exclude measures failing range checks can be specified.

***clean_biologics***

Creates additional short biologic name variable that excludes product names. Creates biologic type variable that distinguishes between IL5 and IL5R based on product. Renames variables to shorter prefix "bx\_" in place of "bio\_" and expands start and end date variable names.

### Example workflow

```{r, eval=FALSE}
# Cleaning function already refers to spirometry_labelled dataframe 
spirometry_clean <- clean_spirometry() |>    
  remove_empty_imputation_fields()

```

## Saving datasets

Use `output_data` function to save object as .Rdata or Stata .dta (version 14) and export data dictionary in Microsoft Word or Excel.

## Contributors

Please contact me with any questions or suggestions.

## License

Available for use under a CC BY-NC-SA 4.0 license (<https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode>).
