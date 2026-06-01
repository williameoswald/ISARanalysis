#' Helper function to remove unused (empty) imputation fields from ISAR standardised datasets.
#'
#' @importFrom dplyr filter_out mutate across if_else case_when ends_with where any_of select
#' @importFrom stringr str_replace
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

#' Calculate eosinophilic grade
#'
#' Assigns an eosinophilic grade to each patient based on the algorithm
#' described in Heaney et al. (2021). The grade reflects the likelihood of
#' eosinophilic asthma using blood eosinophil count (BEC) as the primary
#' classifier, with secondary features used to refine the grade where BEC is
#' in the intermediate range (150–300 cells/μL).
#'
#' Patients with missing \code{beos_highest} are returned as \code{NA}
#' regardless of other variables, as BEC is required by the algorithm.
#'
#' @param df A data frame containing the following columns:
#'   \describe{
#'     \item{beos_highest}{Numeric. Highest recorded blood eosinophil count
#'       (cells/μL).}
#'     \item{bx_type}{Character. Biologic treatment type; used to detect
#'       anti-IL-5/IL-5 receptor therapy via \code{"IL5"} string match.}
#'     \item{ltocs_yn}{Character. Whether the patient has used long-term
#'       oral corticosteroids (\code{"Yes"}/\code{"No"}).}
#'     \item{feno_highest}{Numeric. Highest recorded fractional exhaled nitric
#'       oxide (ppb). May be \code{NA}.}
#'     \item{crswnp}{Character. Chronic rhinosinusitis with nasal polyps
#'       diagnosis (\code{"Yes"}/\code{"No"}).}
#'     \item{asthma_onset}{Numeric. Age at asthma onset (years). May be
#'       \code{NA}.}
#'   }
#'
#' @return The input data frame with an additional column
#'   \code{eosinophilic_grade}: an ordered factor with levels
#'   \code{"Grade 3: Most likely"}, \code{"Grade 2: Likely"},
#'   \code{"Grade 1: Least likely"}, \code{"Grade 0: Unlikely/Noneosinophilic"},
#'   labelled \code{"Eosinophilic grade"}.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_detect
#'
#' @references
#' Heaney LG, et al. (2021). Eosinophilic and Noneosinophilic Asthma:
#' An Expert Consensus Framework to Characterize Phenotypes in a Global
#' Real-World Severe Asthma Cohort. \emph{Chest}, 160(3), 814–830.
#' \doi{10.1016/j.chest.2021.04.013}
#'
#' @examples
#' df <- tibble::tibble(
#'   beos_highest = c(350, 200, 200, 100, NA),
#'   bx_type      = c(NA, "IL5", NA, NA, NA),
#'   ltocs_yn     = c("No", "No", "Yes", "Yes", "No"),
#'   feno_highest = c(NA, NA, NA, 30, NA),
#'   crswnp       = c("No", "No", "No", "No", "Yes"),
#'   asthma_onset = c(25, NA, NA, NA, NA)
#' )
#' calculate_eosinophilic_grade(df)
#'
#' @export
calculate_eosinophilic_grade <- function(df) {
  # Based on Heaney et al 2021:
  # highest BEC ever (≥ 300 cells/mL, ≥ 150-300 cells/mL, or <150 cells/mL),
  # anti-IL-5/5 receptor treatment,
  # long-term OCS use ever
  # elevated FENO (≥ 25 parts per billion) ever,
  # nasal polyps diagnosis ever,
  # adult asthma onset (≥ 18 years)

  df |>
    mutate(
      eosinophilic_grade = structure(
        case_when(
          is.na(beos_highest) ~ NA,
          beos_highest >= 300 ~ "Grade 3: Most likely",
          stringr::str_detect(bx_type, "IL5") ~ "Grade 3: Most likely",
          ltocs_yn == "Yes" &
            (beos_highest >= 150 & beos_highest < 300) ~ "Grade 3: Most likely",
          ltocs_yn == "Yes" & (beos_highest < 150) ~ "Grade 2: Likely",
          beos_highest >= 150 &
            beos_highest < 300 &
            crswnp == "Yes" &
            !is.na(feno_highest) &
            feno_highest >= 25 &
            !is.na(asthma_onset) &
            asthma_onset >= 18 ~ "Grade 3: Most likely",
          beos_highest >= 150 &
            beos_highest < 300 &
            crswnp == "Yes" &
            !is.na(feno_highest) &
            feno_highest >= 25 ~ "Grade 3: Most likely",
          beos_highest >= 150 &
            beos_highest < 300 &
            crswnp == "Yes" &
            !is.na(asthma_onset) &
            asthma_onset >= 18 ~ "Grade 3: Most likely",
          beos_highest >= 150 &
            beos_highest < 300 &
            !is.na(feno_highest) &
            feno_highest >= 25 &
            !is.na(asthma_onset) &
            asthma_onset >= 18 ~ "Grade 3: Most likely",
          beos_highest >= 150 &
            beos_highest < 300 &
            (crswnp == "Yes" |
              (!is.na(feno_highest) & feno_highest >= 25) |
              (!is.na(asthma_onset) & asthma_onset >= 18)) ~ "Grade 2: Likely",
          beos_highest >= 150 & beos_highest < 300 ~ "Grade 1: Least likely",
          beos_highest < 150 &
            (crswnp == "Yes" |
              (!is.na(feno_highest) & feno_highest >= 25) |
              (!is.na(asthma_onset) &
                asthma_onset >= 18)) ~ "Grade 1: Least likely",
          beos_highest < 150 ~ "Grade 0: Unlikely/Noneosinophilic",
          .default = NA
        ) |>
          factor(
            levels = c(
              "Grade 3: Most likely",
              "Grade 2: Likely",
              "Grade 1: Least likely",
              "Grade 0: Unlikely/Noneosinophilic"
            )
          ),
        label = "Eosinophilic grade"
      )
    )
}
