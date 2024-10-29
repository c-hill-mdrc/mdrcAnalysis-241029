#### All Row Variable Testing
testthat::test_that("Testing Regression With Cluster SE Adjustment",{

  # load robust regression without cluster standard error adjustment from sas
  sas_robustreg_withCluster <-
    readxl::read_excel(
      path = testthat::test_path("sas_tests", "robustreg_simdata.xlsx"),
      sheet = "Clustered")

  # applying the lower subgrp value function
  sas_robustreg_withCluster$Dependent <- tolower(sas_robustreg_withCluster$Dependent)

  sas_robustreg <-
    sas_robustreg_withCluster %>%
    dplyr::select(UniqueId, SubGrpVal, Dependent, AdjMean_1, AdjMean_0, Impact_1_0,
                  StdErr_1_0, NObs_1, NObs_0, NObs,
                  Unadj_Mean_1 = UnAdj_Mean_1,
                  Unadj_Mean_0 = UnAdj_Mean_0, Unadj_Impact_1_0) %>%
    dplyr::arrange(UniqueId,
                   SubGrpVal)

  # running robust regression without clusters
  clusterreg <-
    clusterreg_extract(sim_data_robust_reg,
                      .treatment   = "treatment",
                      .dependents  = c("employed_01",
                                       "employed_02"),
                      .subgroup    = c("eduLevel"),
                      .cluster_var = "sites",
                      .cr_method   = "match_SAS") %>%
    dplyr::filter(UniqueId != "sample_size")

  # adjusting robust regression to match with sas robust regression outputs
  clusterreg$SubGrpVal <- as.character(clusterreg$SubGrpVal)

  clusterreg2 <-
    clusterreg %>%
    dplyr::select(UniqueId, SubGrpVal, Dependent, AdjMean_1, AdjMean_0, Impact_1_0,
                  StdErr_1_0, NObs_1, NObs_0, NObs,
                  Unadj_Mean_1, Unadj_Mean_0, Unadj_Impact_1_0) %>%
    dplyr::arrange(UniqueId,
                   SubGrpVal)


  # number of rows of robust regression without cluster
  nrow(sas_robustreg_withCluster)

  # number of rows of robust regression with cluster
  nrow(clusterreg)

  testthat::expect_equal(object = clusterreg2,
                         expected = sas_robustreg,
                         tolerance = 0.001)

  testthat::expect_equal(object = clusterreg$ProbT_1_0,
                         expected = sas_robustreg_withCluster$ProbT_1_0,
                         tolerance = .08)

})

