#### All Row Variable Testing
testthat::test_that("Testing Regression with heteroscedasticity-robust standard errors",{

  # load regression withheteroscedasticity robust standard errors from sas
  sas_hetreg <-
    readxl::read_excel(
      path = testthat::test_path("sas_tests", "robustreg_simdata.xlsx"),
      sheet = "Not Clustered")

  # applying the lower function to dependents column
  sas_hetreg$Dependent <- tolower(sas_hetreg$Dependent)

  sas_hetreg <-
    sas_hetreg %>%
    dplyr::mutate(Stars_1_0 = ifelse(is.na(Stars_1_0), " ", Stars_1_0)) %>%
    dplyr::select(UniqueId, SubGrpVal, Dependent, AdjMean_1, AdjMean_0, Impact_1_0,
                  ProbT_1_0, Stars_1_0, StdErr_1_0, NObs_1, NObs_0, NObs,
                  Unadj_Mean_1 = UnAdj_Mean_1,
                  Unadj_Mean_0 = UnAdj_Mean_0, Unadj_Impact_1_0) %>%
    dplyr::arrange(UniqueId,
                   SubGrpVal) %>%
    janitor::clean_names()

  # running robust regression without clusters
  ## NOTE: Using HC1 to match results from SAS; HC3 is default in R
  r_hetreg <-
    hetreg_extract(sim_data_robust_reg,
                  .treatment  =   "treatment",
                  .dependents = c("employed_01",
                                  "employed_02"),
                  .subgroup   = c("eduLevel"),
                  .hcc_method  = "HC1") %>%
    dplyr::filter(UniqueId != "sample_size")

  # adjusting robust regression to match with sas robust regression outputs
  r_hetreg$SubGrpVal <- as.character(r_hetreg$SubGrpVal)

  r_hetreg <-
    r_hetreg %>%
    dplyr::select(UniqueId, SubGrpVal, Dependent, AdjMean_1, AdjMean_0,
                  Impact_1_0, ProbT_1_0, Stars_1_0, StdErr_1_0, NObs_1,
                  NObs_0, NObs, Unadj_Mean_1, Unadj_Mean_0,
                  Unadj_Impact_1_0) %>%
      dplyr::arrange(UniqueId,
                     SubGrpVal) %>%
    janitor::clean_names()

  testthat::expect_equal(object = r_hetreg,
                         expected = sas_hetreg,
                         tolerance = 0.01)

})

