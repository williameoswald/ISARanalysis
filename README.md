# ISARanalysis

Package contains tools to support cleaning and analysis of ISAR standardised research datasets.

## Download and install package
Download the library from Github:
```{r}
devtools::install_github("williameoswald/ISARanalysis")
```

## import_datasets

Function for importing list of ISAR standardised datasets and creating assigning new "_labelled" dataframe with variable labels assigned, converting categorical variables to factors with assigned levels, and checking and converting "YYYY-MM-DD" to ymd() dates using *lubridate* package. All these cleaning steps use the "ISAR SRD Data Dictionary v1.0.xlsx" that must be imported as a "dictionary" object before use. 

### Your inputs

Specify filename to import and whether you wish to keep a "_raw" imported version of dataset without formatting.

## Example workflow
```{r}
# Location of data folder on Sharepoint
datapath <- "C:/path/to/folder/Dataset production"
# Current release date of datasets
versiondate <- "20260309"

filenames <- list.files(here::here(datapath, paste0("Datasets_", versiondate)))
filenames <- filenames[filenames != "full_dataset.rds"]

# Import ISAR standardised research dataset dictionary as Microsoft Excel workbook using readxl
dictionary <- readxl::read_xlsx(
  "C:/Users/William.Oswald/Optimum Patient Care Global Limited/ISAR Team - Documents/Dataset production/Data dictionary/ISAR SRD Data Dictionary v1.0.xlsx"
)

# Map over all listed files
filenames |>
  purrr::set_names() |>
  purrr::map(\(x) import_datasets(x, keep_raw = T), .progress = T)

```

## Contributors

Please contact [me](william.oswald@optimumpatientcare.org) with any questions or suggestions.

## License

Available for use under a CC BY-NC-SA 4.0 license (https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode).
