# ISARanalysis

Package contains tools to support cleaning and analysis of ISAR standardised research datasets.

## Download and install package

Download the library from Github:

```{r}
devtools::install_github("williameoswald/ISARanalysis")
```

## import_datasets

Function for importing list of ISAR standardised datasets and creating new "_labelled" dataframe objects with variable labels assigned, categorical variables converted to factors with assigned levels, and "YYYY-MM-DD" dates checked and converted to ymd() dates using lubridate package. Any date parsing errors will be reported and summarised in an additional object with "_date_parse_failures". All these cleaning steps use the "ISAR SRD Data Dictionary v1.0.xlsx" that must be imported as a "dictionary" object before use.

### Your inputs

Specify filename to import and whether you wish to keep a "\_raw" imported version of dataset without formatting.

## Example workflow

```{r}
# Location of data folder on Sharepoint
datapath <- "C:/path/to/folder/Dataset production"
# Current release date of datasets
versiondate <- "20260309"

# Create list of dataset filenames (without full path)
filenames <- list.files(here::here(datapath, paste0("Datasets_", versiondate)))
# Exclude .rds file
filenames <- filenames[filenames != "full_dataset.rds"]

# Import ISAR standardised research dataset dictionary as Microsoft Excel workbook using readxl package. NB - make sure file is closed before trying to import.
dictionary <- readxl::read_xlsx(
  "C:/path/to/folder/Dataset production/Data dictionary/ISAR SRD Data Dictionary v1.0.xlsx"
)

# Map over all listed files or select specific datasets
# filenames <- filenames[filenames != "7. HCRU.csv"] or filenames[c(1, 4)]
# Will create new objects in global environment for each dataset
filenames |>
  purrr::set_names() |>
  purrr::map(\(x) import_datasets(x, keep_raw = T), .progress = T)

```

## Contributors

Please contact me with any questions or suggestions.

## License

Available for use under a CC BY-NC-SA 4.0 license (<https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode>).
