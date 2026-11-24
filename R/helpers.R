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

#' Helper function to create SRD lifestyle variable "smoking_status". Applies
#' logic rules to clean smoking status over time, e.g. "Never" smoked status
#' following an earlier record indicating a smoker.
#'
#' @importFrom dplyr mutate arrange select if_else lag first
#' @importFrom tidyr fill
#'
#' @export
clean_smoking <- function(df) {
  # Check where duplicated visit dates
  # Check: df |> count(smoke, smoking_status_cln)

  # Fill NA with previous,
  # Ex-smoker > Current = ok, keep current
  # Ex-smoker > Never = ignore never, bring ex forward
  # Ex-smoker > Ex-smoker = ok, keep ex-smoker
  # Current > Current = ok
  # Current > Ex_smoker = ok
  # Current > Never = ignore never, bring current forward
  # Never smoker > Current = ok
  # Never smoker > Ex-smoker = never changes to ex-smoker, except never>current>ex, current left alone
  # Never smoker > Never = ok

  df |>
    mutate(smoke = as.character(smoke)) |>
    arrange(patient_id, visit_date) |>
    mutate(
      prior_smoked = dplyr::lag(
        cumany(smoke %in% c("Ex-smoker", "Current smoker")),
        default = FALSE
      ),
      # Blank out rows to be replaced (NA, or an impossible never)
      smoking_status_cln = if_else(
        smoke == "Never smoked" & prior_smoked,
        NA_character_,
        smoke
      ),
      .by = patient_id
    ) |>
    fill(smoking_status_cln, .direction = "down", .by = patient_id) |>
    mutate(
      # Find earliest ex/current record
      first_smk = first(smoke[smoke %in% c("Ex-smoker", "Current smoker")]),
      # If earliest ex/current record is "Ex-smoker" then replace "Never" with "Ex-smoker"
      # Leaves all "Never" and any "Current" alone
      smoking_status_cln = if_else(
        smoking_status_cln == "Never smoked" & first_smk %in% "Ex-smoker",
        "Ex-smoker",
        smoking_status_cln
      ),
      .by = patient_id
    ) |>
    select(-prior_smoked, -first_smk)
}

#
#' Helper function to flag outlying values using either Tukey’s IQR or 3*SD approach.
#'
#' @export
is_outlier <- function(
  x,
  method = c("iqr", "3sd"),
  low = TRUE,
  high = TRUE
) {
  method <- match.arg(method)
  # 3sd assumes normality, is influenced by outliers, and has small sample problems
  # iqr robust to outliers and normality and works with small samples

  if (!low && !high) {
    stop("At least one of `low` or `high` must be TRUE.")
  }

  if (method == "iqr") {
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
    spread <- 1.5 * (q[2] - q[1])
    lower <- q[1] - spread
    upper <- q[2] + spread
  } else {
    centre <- mean(x, na.rm = TRUE)
    spread <- 3 * sd(x, na.rm = TRUE)
    lower <- centre - spread
    upper <- centre + spread
  }

  is_low <- if (low) x < lower else FALSE
  is_high <- if (high) x > upper else FALSE

  is_low | is_high
}
