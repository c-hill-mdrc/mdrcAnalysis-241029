#### All Row Variable Testing
testthat::test_that("LM Extract Unit Testing",{

  # load ht_extract_sas output
  extract_sas <-
    readxl::read_excel(
      path = testthat::test_path("sas_tests", "lmglmcomparison.xlsx"))

  # Convert COHORT to a factor for analysis
  finalBif <- bif_final %>%
    dplyr::mutate(CohortFactor = factor(COHORT))

  # Create vector of dependent variables
  finalBif_dependents <- names(finalBif[setdiff(names(finalBif), c("SAMPLEID", "RA_CODE", "CohortFactor",
                                                                   "COHORT", "RA_DATE", "AGE",
                                                                   "DOB"))])
  finalBif_treatment  <- "RA_CODE"
  finalBif_covariates <- "CohortFactor"

  extract_r <- lm_extract(
    .dataset = finalBif,
    .dependents = finalBif_dependents,
    .treatment= finalBif_treatment,
    .covariates = finalBif_covariates,
    .confintalpha = .9)  # confidence interval - p value is .1 here.

  extract_r_matching <-
    extract_r %>%
    dplyr::select(
      Dependent, AdjMean_C, AdjMean_P, Impact_P_C, StdErr_P_C, ProbT_P_C,
      Stars_P_C, ProbF, Stars_ProbF, RSquare, NObs_C, NObs_P, NObs,
      Unadj_Impact_P_C, DepVarMean
      ) %>%
    dplyr::mutate(Dependent = toupper(Dependent)) %>%
    dplyr::filter(!is.na(AdjMean_P)) %>%
    dplyr::arrange(Dependent)

  names(extract_r_matching$ProbF) <- NULL

  extract_sas_matching <-
    extract_sas %>%
    dplyr::select(
      Dependent, AdjMean_C, AdjMean_P, Impact_P_C, StdErr_P_C, ProbT_P_C,
      Stars_P_C, ProbF, Stars_ProbF, RSquare, NObs_C, NObs_P, NObs,
      Unadj_Impact_P_C, DepVarMean
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("Stars"),
                                ~ifelse(is.na(.), " ", .)
                                )) %>%
    dplyr::filter(!stringr::str_detect(Dependent, "73PLUS")) %>%
    dplyr::arrange(Dependent)

  testthat::expect_equal(object = extract_r_matching,
                         expected = extract_sas_matching,
                         tolerance = 0.01
                         )

})

