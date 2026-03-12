#' Import ISAR standardised research datasets
#'
#' This function imports all listed datasets and creates a new "_labelled" version with all variables labelled, categorical variables formatted as factors with defined levels, and confirms and formats dates as defined in dictionary workbook
#' @param filename Filename of .csv as text
#' @param keep_raw Specify whether to keep "_raw" unformatted version of dataset object
#' @return Assigns object in global environment using abbreviated dataset name
#' @export

import_datasets <- function(filename, keep_raw = T) {
  library(tidyverse)
  library(here)
  library(janitor)

  if (!exists("dictionary", envir = .GlobalEnv)) {
    stop(glue::glue("Dataset dictionary not found in parent environment"))
  }

  #  Helper function to clean factor levels from excel sheet
  parse_levels <- function(cell_text) {
    if (is.na(cell_text)) {
      return(character(0))
    }

    # Extract all quoted strings, then strip the surrounding quotes
    levels <- str_extract_all(cell_text, '"[^"]+"')[[1]] |>
      str_replace_all('"', '') |>
      str_trim() |>
      str_squish()

    # Drop empty strings
    levels <- levels[levels != ""]

    return(levels)
  }

  #  Helper function to format categorical variables as factors with specified levels and label all
  factor_and_label_vars <- function(x) {
    variable <- cur_column()

    variable_label <- dict |>
      filter(variable_name == variable) |>
      pull(variable_label)

    variable_type <- dict |>
      filter(variable_name == variable) |>
      pull(variable_type)

    if (variable_type == "Categoric") {
      variable_levels <- dict |>
        filter(variable_name == variable) |>
        pull(variable_levels_parsed) |>
        pluck(1)

      structure(
        factor(x, levels = variable_levels),
        label = variable_label
      )
    } else if (variable_type == "Date") {
      structure(lubridate::ymd(x, quiet = T), label = variable_label)
    } else {
      structure(x, label = variable_label)
    }
  }

  dataset_name <- str_extract(filename, "(?<= ).*(?=\\.)")

  dataset_dict_name <- if_else(
    dataset_name %in% c("HCRU"),
    dataset_name,
    str_to_title(str_replace_all(dataset_name, "_", " "))
  )

  dict <- dictionary |>
    janitor::clean_names() |>
    filter(
      dataset_table == dataset_dict_name
    ) |>
    mutate(
      units = na_if(units, "N/A"),
      variable_label = if_else(
        is.na(units),
        variable_label,
        str_c(variable_label, units, sep = ", ")
      ),
      variable_levels = if_else(
        variable_type == "Categoric",
        variable_format,
        NA
      ),
      variable_levels_parsed = map(variable_levels, parse_levels)
    ) |>
    select(contains("variable_"))

  dict_vars <- dict |> pull(variable_name)

  df_raw <- read_csv(
    here(
      datapath,
      paste0(
        "Datasets_",
        versiondate
      ),
      filename
    ),
    col_types = cols(.default = col_character())
  )

  if (keep_raw) {
    assign(
      paste0(str_to_lower(dataset_name), "_raw"),
      df_raw,
      envir = .GlobalEnv
    )
  }
  #   Check date parsing
  date_vars <- dict |> filter(variable_type == "Date") |> pull(variable_name)

  if (length(date_vars) > 0) {
    date_df <- df_raw |> select(any_of(date_vars))

    failure_mask <- date_df |>
      mutate(across(everything(), \(x) {
        is.na(lubridate::ymd(x, quiet = TRUE)) & !is.na(x)
      }))

    parse_failures <- names(date_df) |>
      set_names() |>
      map(\(var) unique(date_df[[var]][failure_mask[[var]]])) |>
      keep(\(x) length(x) > 0)

    if (length(parse_failures) > 0) {
      message(glue::glue("Date parse failures in {dataset_name}:"))
      iwalk(parse_failures, \(vals, var) {
        message(glue::glue("  {var}: {paste(vals, collapse = ', ')}"))
      })

      assign(
        paste0(str_to_lower(dataset_name), "_date_parse_failures"),
        parse_failures,
        envir = .GlobalEnv
      )
    }
  }

  #   Check variables present
  missing_vars <- setdiff(dict_vars, names(df_raw))
  if (length(missing_vars) > 0) {
    warning(
      glue::glue(
        "The following dictionary variables were not found in {dataset_dict_name}: ",
        "{paste(missing_vars, collapse = ', ')}"
      ),
      call. = FALSE
    )
  }

  df <- df_raw |>
    mutate(across(any_of(dict_vars), factor_and_label_vars))

  assign(
    paste0(str_to_lower(dataset_name), "_labelled"),
    df,
    envir = .GlobalEnv
  )
}
