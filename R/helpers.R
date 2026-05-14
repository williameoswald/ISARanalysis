#' Helper function to remove unused (empty) imputation fields from ISAR standardised datasets.
#'
#' @importFrom dplyr filter_out mutate across if_else case_when ends_with where any_of
#'
#' @export
remove_empty_imputation_fields <- function(df) {
  # Find imputed columns where all values are 0
  all_zero_imputed <- df |>
    select(ends_with("_imputed")) |>
    select(where(\(x) all(x == 0, na.rm = TRUE) & any(!is.na(x)))) |>
    names()

  # Build full set of columns to drop (both _imputed and paired _imputed_flag)
  cols_to_drop <- c(
    all_zero_imputed,
    str_replace(all_zero_imputed, "_imputed$", "_imputed_flag")
  )

  print("No imputed values in columns:")
  print(cols_to_drop)
  print("Columns Will be dropped.")

  # Drop columns
  df |> select(-any_of(cols_to_drop))
}
