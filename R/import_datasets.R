#' Import ISAR standardised research datasets
#'
#' This function imports all listed datasets and creates a new "_labelled" version with all variables labelled, categorical variables formatted as factors with defined levels, and confirms and formats dates/numerics as defined in dictionary workbook
#' @param filename character. Name of the CSV file to import.
#' @param keep_raw logical. Whether to keep the raw unlabelled dataset. Default TRUE.
#' @return Assigns object in global environment using abbreviated dataset name
#' @importFrom dplyr filter pull mutate select any_of across everything cur_column if_else na_if
#' @importFrom purrr map compact pluck iwalk set_names
#' @importFrom stringr str_extract str_replace_all str_to_lower str_to_title str_trim str_squish str_c str_extract_all
#' @importFrom readr read_csv cols col_character
#' @importFrom lubridate ymd
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom here here
#'
#' @export
import_datasets <- function(filename, keep_raw = TRUE) {
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

    var_dict <- dict |>
      filter(variable_name == variable)

    variable_label <- var_dict |>
      pull(variable_label)

    variable_type <- var_dict |>
      pull(variable_type)

    if (variable_type == "Categoric") {
      variable_levels <- var_dict |>
        pull(variable_levels_parsed) |>
        pluck(1)

      structure(
        factor(x, levels = variable_levels),
        label = variable_label
      )
    } else if (variable_type == "Numeric") {
      structure(suppressWarnings(as.numeric(x)), label = variable_label)
    } else if (variable_type == "Date") {
      structure(lubridate::ymd(x, quiet = TRUE), label = variable_label)
    } else {
      structure(x, label = variable_label)
    }
  }

  #   Helper function to check date and numeric value parsing
  check_parse_failures <- function(df, type, dataset_name) {
    vars <- dict |> filter(variable_type == type) |> pull(variable_name)

    if (length(vars) == 0) {
      return(invisible(NULL))
    }

    if (type == "Date") {
      parse_fn <- function(x) {
        lubridate::ymd(x, quiet = TRUE)
      }
    } else if (type == "Numeric") {
      parse_fn <- function(x) {
        suppressWarnings(as.numeric(x))
      }
    }

    type_df <- df |> select(any_of(vars))

    failure_mask <- type_df |>
      mutate(across(everything(), \(x) is.na(parse_fn(x)) & !is.na(x)))

    failures <- names(type_df) |>
      set_names() |>
      map(\(var) unique(type_df[[var]][failure_mask[[var]]])) |>
      compact()

    if (length(failures) > 0) {
      message(glue::glue("{type} parse failures in {dataset_name}:"))
      iwalk(failures, \(vals, var) {
        message(glue::glue("  {var}: {paste(vals, collapse = ', ')}"))
      })
      assign(
        paste0(
          str_to_lower(dataset_name),
          "_",
          str_to_lower(type),
          "_parse_failures"
        ),
        failures,
        envir = .GlobalEnv
      )
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
    )

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

  #   Check parsing of dates and numerics
  check_parse_failures(df_raw, "Date", dataset_name)
  check_parse_failures(df_raw, "Numeric", dataset_name)

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
