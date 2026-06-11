#' Query ISAR biomarkers dataset to get test results within period of time
#'
#' Identifies recorded biomarker test results (BEOS, FeNO, Ige) within a
#' defined time window preceding (or following) an anchor date. The function
#' joins test and date-specific (following clean_biomarkers) results against
#' patient-level query dates, and returns either the latest or highest results
#' within the search window.
#'
#' @param df A data frame of biomarker records. Must
#'   contain columns: `patient_id`, `visit_id`, `result_date`,
#'   `result_date_imputed`, `result_date_imputed_flag`, `test`, `result`,
#'   `result_imputed`, and `result_imputed_flag`.
#' @param query_dates A tibble with at minimum a `patient_id` column and a date
#'   column matching `search_anchor_date_col`. One row per patient. Patients
#'   present in `query_dates` but absent from `df` (i.e. no medication records)
#'   are dropped.
#' @param test_select Character scalar. The test to filter on,
#'   matched against the `test` column.
#' @param result_select Character scalar. The desired result to select for the
#'  patient within the search window, either `latest` (i.e. most recent result)
#'  or `highest` (i.e. maximum result). The output variable is derived from this
#'   value and `test_select` as `{paste0(str_to_lower(test_select),"_",result_select)}`
#'   (e.g. `"BEOS"` and `"highest"` produces `beos_highest`).
#' @param search_anchor_date_col Character scalar. Name of the date column in
#'   `query_dates` to use as the anchor for the search window. The window runs
#'   from `anchor - search_mos months` to `anchor` (inclusive). Defaults to
#'   `"index_date"`.
#' @param search_mos Integer. Number of months to look back from the anchor
#'   date. Negative values look forward. Month arithmetic uses
#'   [lubridate::%m-%][lubridate::`%m-%`] to handle end-of-month edge cases.
#'   Defaults to `12`.
#' @param keep_df Logical. If `TRUE`, assigns the full test and date-specific
#'  working data frame to the global environment for inspection. The assigned
#'  name is `{test_newvar}_df`. Defaults to `FALSE`.
#'
#' @details
#'
#' The search window is derived from `query_dates` using the column specified
#' by `search_anchor_date_col` as the index date:
#' search_start = anchor_date %m-% months(search_mos) and search_end = anchor_date
#'
#' \link[lubridate]{interval} allows search_start > search_end.
#'
#' Month arithmetic uses \link[lubridate]{\%m+\%} to handle end-of-month edge cases.
#' Always returns a date in the nth month after Date. If the new date would usually
#' spill over into the n + 1th month, \%m+\% will return the last day of the nth month.
#'
#' All test and date rows are first filtered to be \link[lubridate]{\%within\%}
#' search window and then sorted by descending date (most-recent first).
#' New result variable is created with selected result:
#'
#' \describe{
#'   \item{`result` (latest)}{Most recent test result within patient.
#'   `result_select == "latest" & row_number() == 1`}
#'   \item{`result` (highest)}{Maximum test result within patient and interval.
#'   `result_select == "highest" & result == max(result, na.rm = TRUE)`}
#'   \item{`NA`}{No condition matched}
#' }
#'
#' An additional `{test_newvar}_gap` variable is created with the calculated
#' interval in months between test date and search_end date.
#'
#' @return A filtered tibble with one row per patient containing
#' (Additional \link[dplyr]{distinct} step selects most recent if multiple
#' results are tied for maximum or same results):
#' \describe{
#'   \item{`patient_id`}{Patient identifier.}
#'   \item{`{test_newvar}`}{Numeric with single specified test results.}
#'   \item{`{test_newvar_gap}`}{Numeric with months between test and search index date.}
#' }
#'
#' @importFrom dplyr filter filter_out select distinct inner_join mutate bind_rows arrange
#' @importFrom lubridate interval int_overlaps `%m-%`
#' @importFrom stringr str_to_lower str_detect
#'
#' @examples
#' # Basic usage with a single anchor date column
#' query_biomarkers(
#'   df = clean_biomarkers(
#'     reduce_function = "max",
#'     drop_duplicate = TRUE,
#'     drop_rangefail = TRUE
#'     ) |>
#'   remove_empty_imputation_fields(),
#'   query_dates = query_dates,
#'   test_select = "beos",
#'   result_select = "highest",
#'   search_anchor_date_col = "bx_start_date",
#'   search_mos = 12,
#'   keep_df = TRUE
#' )
#'
#' # Use with a vector of test and result selection values (e.g. to create baseline dataset)
#' expand_grid(
#'   x = c("beos", "feno", "ige"),
#'   y = c("latest", "highest")
#' ) |>
#'   pmap(\(x, y) {
#'     query_biomarkers(
#'       df = biomarkers_clean,
#'       query_dates = query_dates_sample,
#'       test_select = x,
#'       result_select = y,
#'       search_anchor_date_col = "bx_start_date",
#'       search_mos = 12,
#'       keep_df = TRUE
#'     )
#'   }) |>
#'   purrr::reduce(left_join, by = join_by(patient_id)) |>
#'   dplyr::rename_with(.cols = -c("patient_id"), ~ paste0(., "_bl"))
#'
#'
#' @export
query_biomarkers <- function(
  df,
  query_dates,
  test_select,
  result_select,
  search_anchor_date_col = "index_date",
  search_mos = 12,
  keep_df = FALSE
) {
  if (!test_select %in% c("beos", "feno", "ige")) {
    stop('Selected test must be "beos", "feno", or "ige"')
  }
  if (!result_select %in% c("latest", "highest")) {
    stop('Result selection must be "latest" or "highest"')
  }

  test_newvar <- paste0(
    str_to_lower(test_select),
    "_",
    result_select
  )
  test_newvar_gap <- paste0(
    str_to_lower(test_select),
    "_",
    result_select,
    "_gap"
  )

  test_df <- df |>
    filter(test == test_select) |>
    rename_with(
      ~ str_replace(.x, "result_", ""),
      .cols = any_of(c("result_date"))
    ) |>
    inner_join(
      query_dates |>
        distinct() |>
        mutate(
          # https://lubridate.tidyverse.org/reference/mplus.html
          search_start = .data[[search_anchor_date_col]] %m-%
            months(search_mos),
          search_end = .data[[search_anchor_date_col]], # query date (inclusive)
          search_interval = interval(search_start, search_end)
        ) |>
        select(patient_id, contains("search_")),
      by = join_by(patient_id),
      relationship = "many-to-one"
    ) |>
    # Test results within search interval
    filter(
      date %within% search_interval
    ) |>
    arrange(patient_id, desc(date)) |>
    mutate(
      "{ test_newvar }" := case_when(
        result_select == "latest" &
          row_number() == 1 ~ result, # sorted by descending dates
        result_select == "highest" &
          result == max(result, na.rm = TRUE) ~ result,
        .default = NA
      ),
      "{ test_newvar }_date" := date,
      "{ test_newvar }_gap" := interval(date, search_end) / months(1),
      .by = patient_id
    )

  if (keep_df) {
    assign(
      paste0(test_newvar, "_df"),
      test_df,
      envir = .GlobalEnv
    )
  }

  # Pull patient-level measure
  test_df |>
    filter_out(is.na(.data[[test_newvar]])) |>
    select(
      patient_id,
      any_of(c(test_newvar, test_newvar_date, test_newvar_gap))
    ) |>
    # if tied for highest, then takes most recent
    distinct(patient_id, .data[[test_newvar]], .keep_all = TRUE)
}
