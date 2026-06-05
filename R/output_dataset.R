#' Save object as Rdata or Stata (version 14) and export dictionary in word or excel
#'
#' Processes specified dataframe to convert factors to labelled vectors using provided
#' helper function and \link[haven]{labelled}. Then saves dataframe as .dta
#' file using \link[haven]{write_dta}. The default is Stata version 14.
#'
#' @param df A data frame.
#' @param export_dictionary Logical. If `TRUE`, uses \link[labelled]{look_for}
#'  to create a data dictionary object that can then be exported as Microsoft Excel
#'  or Word (default) files.
#' @param excel_dictionary Logical. If `TRUE`, uses \link[writexl]{write_xlsx}
#' to export .xlsx file. If `FALSE` (default), uses \link[flextable]{save_as_docx}
#' to export .docx file.
#' @param add_isar Logical. If `TRUE`, adds "isar_" to front of file name.
#' @param export_loc Character vector. Specifies folder within project and
#' \link[here]{here} in which to save output files. Can also be full filepath.
#'
#' @importFrom dplyr select mutate across
#' @importFrom tidyselect where
#' @importFrom here here
#' @importFrom writexl write_xlsx
#' @importFrom haven labelled write_dta
#' @importFrom rlang set_names
#' @importFrom labelled look_for lookfor_to_long_format convert_list_columns_to_character
#' @importFrom here here
#' @importFrom flextable flextable align fontsize font bold set_table_properties height merge_v valign set_header_labels save_as_docx
#'
#' @examples
#' output_to_stata(
#'    bl_biologic,
#'    export_dictionary = T,
#'    excel_dictionary = F,
#'    add_isar = F,
#'    export_loc = "data_cleaning"
#' )
#'
#' @export
output_dataset <- function(
  df,
  export_dictionary = TRUE,
  excel_dictionary = FALSE,
  add_isar = TRUE,
  export_loc = "data_cleaning"
) {
  # factor -> labelled (keeps level order as 1..K; carries variable label if present)
  fct_to_labelled <- function(x) {
    if (!is.factor(x)) {
      return(x)
    }

    lvls <- levels(x)

    haven::labelled(
      x = as.integer(x),
      labels = rlang::set_names(seq_along(lvls), lvls),
      label = attr(x, "label", exact = TRUE)
    )
  }

  export_name <- paste0(
    if (add_isar) "isar_",
    deparse(substitute(df)),
    "_",
    format(Sys.Date(), "%Y%m%d")
  )

  # Save R Object
  save(df, file = here::here(export_loc, paste0(export_name, ".Rdata")))

  # Convert all factor variables to labelled vectors for output to Stata
  export_df <- df |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor), fct_to_labelled))

  # Write Stata dataset
  haven::write_dta(
    export_df,
    here::here(export_loc, paste0(export_name, ".dta"))
  )

  if (export_dictionary) {
    # Create dictionary object from data - labelled package uses labelled data, rather than raw factors, to get value labels

    dictionary <- bind_cols(
      df |>
        labelled::look_for() |>
        labelled::lookfor_to_long_format() |>
        labelled::convert_list_columns_to_character() |>
        dplyr::select(-value_labels),
      export_df |>
        labelled::look_for() |>
        labelled::lookfor_to_long_format() |>
        labelled::convert_list_columns_to_character() |>
        dplyr::select(value_labels)
    ) |>
      dplyr::select(
        pos,
        variable,
        col_type,
        label,
        r_levels = levels,
        stata_value_labels = value_labels,
        missing
      )

    if (excel_dictionary) {
      writexl::write_xlsx(
        dictionary,
        path = here::here(export_loc, paste0(export_name, ".xlsx"))
      )
    } else {
      dictionary |>
        flextable::flextable() |>
        flextable::align(align = "left", part = "header") |>
        flextable::fontsize(size = 12, part = "all") |>
        flextable::font(fontname = "Aptos", part = "all") |>
        flextable::bold(bold = TRUE, part = "header") |>
        flextable::bold(i = NULL, j = "variable", bold = TRUE, part = "body") |>
        flextable::set_table_properties(layout = "autofit") |>
        flextable::height(height = 0.7, part = "body", unit = "cm") |>
        flextable::merge_v(
          j = "variable",
          target = c("pos", "variable", "col_type", "label", "missing")
        ) |>
        flextable::valign(valign = "top", part = "body") |>
        flextable::set_header_labels(
          values = c(
            pos = "#",
            variable = "Variable",
            col_type = "Type",
            label = "Variable label",
            r_levels = "R levels",
            stata_value_labels = "Stata values",
            missing = "Missing values"
          )
        ) |>
        flextable::save_as_docx(
          path = here::here(export_loc, paste0(export_name, ".docx"))
        )
    }
  }
  return(dictionary)
}
