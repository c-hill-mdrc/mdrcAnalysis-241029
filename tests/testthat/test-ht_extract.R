#### All Row Variable Testing
testthat::test_that("HT Unit Testing",{

  # Load SAS Extract
  ht_extract_sas <-
    readxl::read_excel(
      path = testthat::test_path("sas_tests", "htsubgroupcomparison.xlsx"))

  # Prep SAS output to match R
  ht_extract_sas$Dependent <- tolower(ht_extract_sas$Dependent)
  ht_extract_sas$UniqueId  <- tolower(ht_extract_sas$UniqueId)

  # filter ht extract without the full sample but just 1 and 2
  ht_extract_sas <-
    ht_extract_sas %>%
    dplyr::select_if(~!any(is.na(.))) %>%
    dplyr::filter(SubGrpVal != "Full") %>%
    dplyr::mutate(SubGrpVal  = as.numeric(SubGrpVal))

  # Prep Data for R Extract
  # Convert COHORT to a factor for analysis
  finalBif <- bif_final %>%
    dplyr::mutate(CohortFactor = factor(COHORT)) %>%
    dplyr::filter(!is.na(COHORT) & !is.na(RA_CODE))

  # Create vector of dependent variables
  finalBif_dependents <- names(finalBif[setdiff(names(finalBif),
                                                c("SAMPLEID", "RA_CODE", "CohortFactor",
                                                  "COHORT", "RA_DATE", "AGE", "DOB", "blcurhrs_73plus",
                                                  "blworkingyes", "blworkingno"))])
  finalBif_treatment  <- "RA_CODE"
  finalBif_covariates <- "CohortFactor"

  # Create R Extract
  ht_extract_r <-
    lm_extract(.dataset    = finalBif,
               .subgroup   = "COHORT",
               .dependents = finalBif_dependents,
               .treatment  = "RA_CODE",
               .covariates = NA_character_) %>%
    ht_extract()

  # Create list of matching columns in both
  commonvars <- setdiff(names(ht_extract_r),
                        setdiff(names(ht_extract_r),names(ht_extract_sas)))


  # Clean up R Extract
  ht_extract_r_matching <-
    ht_extract_r %>%
    dplyr::select(tidyselect::all_of(commonvars)) %>%
    dplyr::filter(Dependent != "sample_size") %>%
    dplyr::arrange(SubGrpVal, Dependent)

  ht_extract_sas_matching <-
    ht_extract_sas %>%
    dplyr::select(tidyselect::all_of(commonvars)) %>%
    dplyr::filter(Dependent %in% finalBif_dependents) %>%
    dplyr::arrange(SubGrpVal, Dependent)

  testthat::expect_equal(object = ht_extract_r_matching,
                         expected = ht_extract_sas_matching,
                         ignore_attr = TRUE,
                         tolerance = 0.01)

})
