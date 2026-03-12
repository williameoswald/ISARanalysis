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

  dataset <- str_to_lower(str_extract(filename, "(?<= ).*(?=\\.)"))

  dict <- dictionary |>
    janitor::clean_names() |>
    filter(
      dataset_table ==
        str_to_title(str_replace_all(
          str_extract(filename, "(?<= ).*(?=\\.)"),
          "_",
          " "
        ))
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
      paste0(dataset, "_raw"),
      df_raw,
      envir = .GlobalEnv
    )
  }
  #   Check date parsing
  date_vars <- dict |> filter(variable_type == "Date") |> pull(variable_name)

  parse_failures <- df_raw |>
    select(any_of(date_vars)) |>
    mutate(across(everything(), \(x) {
      parsed <- lubridate::ymd(x, quiet = TRUE)
      is.na(parsed) & !is.na(x)
    })) |>
    summarise(across(everything(), \(x) {
      list(unique(df_raw[[cur_column()]][x]))
    })) |>
    pivot_longer(
      everything(),
      names_to = "variable",
      values_to = "failed_values"
    ) |>
    filter(lengths(failed_values) > 0)

  if (nrow(parse_failures) > 0) {
    message(glue::glue("Date parse failures in {dataset}:"))
    message(glue::glue("Date converted to missing"))
    walk2(parse_failures$variable, parse_failures$failed_values, \(var, vals) {
      message(glue::glue("  {var}: {paste(vals, collapse = ', ')}"))
    })

    assign(
      paste0(dataset, "_date_parse_failures"),
      parse_failures,
      envir = .GlobalEnv
    )
  }

  df <- df_raw |>
    mutate(across(all_of(dict_vars), factor_and_label_vars))
  # (\(d) reduce(cat_vars, factor_and_label_vars, .init = d))()

  assign(
    paste0(dataset, "_labelled"),
    df,
    envir = .GlobalEnv
  )
}
