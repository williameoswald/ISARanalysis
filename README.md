# ISARanalysis

Package contains tools to support cleaning and analysis of ISAR standardised research datasets.

## Download and install package

Download the library from Github:

```{r}
devtools::install_github("williameoswald/ISARanalysis")
```

## import_datasets

Function for importing list of ISAR standardised datasets and creating new "\_labelled" dataframe objects with variable labels assigned, categorical variables converted to factors with assigned levels, and "YYYY-MM-DD" dates checked and converted to ymd() dates using lubridate package. Any date parsing errors will be reported and summarised in an additional object with "\_date_parse_failures". All these cleaning steps use the "ISAR SRD Data Dictionary v1.0.xlsx" that must be imported as a "dictionary" object before use.

### Your inputs

Specify filename to import and whether you wish to keep a "\_raw" imported version of dataset without formatting.

### Example workflow

```{r}
# Location of data folder on Sharepoint
datapath <- "C:/path/to/folder/Dataset production"
# Current release date of datasets
versiondate <- "20260309"

# Import ISAR standardised research dataset dictionary as Microsoft Excel workbook using readxl package. NB - make sure file is closed before trying to import.
dictionary <- readxl::read_xlsx(
  "C:/path/to/folder/Dataset production/Data dictionary/ISAR SRD Data Dictionary v1.0.xlsx"
)

# Create list of dataset filenames (without full path)
filenames <- list.files(
  here::here(datapath, paste0("Datasets_", versiondate)),
  pattern = "\\.csv$"
  )

# Map over all listed files or select specific datasets
# filenames <- filenames[filenames != "7. HCRU.csv"] or filenames[c(1, 4)]
# Will create new objects in global environment for each dataset
filenames |>
  purrr::set_names() |>
  purrr::map(\(x) import_datasets(x, keep_raw = T), .progress = T)

```

## Cleaning datasets

Additional cleaning functions can be used after import function to conduct range checks and create additional (e.g. categorical) variables for analysis.

***clean_demographics***

Derives asthma onset categories (\<12, 12-40, \>40 years), asthma duration (in years), and asthma duration categories (\<10, 10-\<25, ≥25) from labelled demographic data.

***clean_lifestyle***

Applies range checks and plausibility checks height and weight measures and cleans BMI measure accordingly. Derives categorical classifications for obese (BMI \<30 vs. ≥30 kg/m2).

***clean_comorbidities***

Updates labels and derives new variable for history of anxiety and/or depression.

***clean_asthma_control***

Recodes GINA asthma control responses to numeric values to calculate score and uses to derive GINA asthma control classification (Well controlled vs Partly controlled vs Uncontrolled). Derives categorical classifications for asthma control based on available ACQ and ACT scores.

***clean_spirometry***

Applies range checks and plausibility checks (FEV1 \>= FVC) to pre- and post-bronchodilator spirometry measures. Where post-bronchodilator measures are available at every timepoint for a patient, they are prioritised over pre-bronchodilator measures. Derives single composite FEV1, FVC, FEV1/FVC ratio, and percent predicted FEV1 measures, along with categorical classifications for FEV1 percent predicted (\<80% vs ≥80%) and FEV1/FVC ratio (\<0.70 vs ≥0.70).

*NB* - for longitudinal comparisons of FEV1, FVC, FEV1 percent predicted over time use pre- or post-bronchodilator measures consistently.

***clean_biologics***

Creates additional short biologic name variable that excludes product names. Creates biologic type variable that distinguishes between IL5 and IL5R based on product. Renames variables to shorter prefix "bx\_" in place of "bio\_" and expands start and end date variable names.

### Example workflow

```{r}

# Cleaning function already refers to spirometry_labelled dataframe
spirometry_clean <- clean_spirometry() |> 
  remove_empty_imputation_fields()

```

## Contributors

Please contact me with any questions or suggestions.

## License

Available for use under a CC BY-NC-SA 4.0 license (<https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode>).
