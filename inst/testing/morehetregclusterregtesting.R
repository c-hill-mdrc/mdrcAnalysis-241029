sim_data_weighted2 <- sim_data_weighted %>% dplyr::filter(sites != 12)

length(names(table(sim_data_weighted2$treatment)))

mdrcAnalysis::hetreg_extract(.dataset    = sim_data_weighted
              ,.dependents = c("employed_01", "employed_02")
              ,.treatment  = c("treatment")
              ,.covariates = c("sites")
              ,.subgroup   = c("eduLevel")
               )

finalBif <- bif_final %>%
  dplyr::mutate(CohortFactor = factor(COHORT))

hetreg_extract(.dataset    = finalBif,
               ,.dependents = c("blmale", "blfemale")
               ,.treatment  = c("RA_CODE")
               ,.covariates = c("AGE", "CohortFactor")
               ,.asfactorcovs = c("CohortFactor")
)

dplyr::count(sim_data_weighted, employed_01)
dplyr::count(sim_data_weighted, employed_02)
dplyr::count(sim_data_weighted, treatment)
dplyr::count(sim_data_weighted, eduLevel)
dplyr::count(sim_data_weighted, sites, treatment) %>% dplyr::group_by(sites) %>% dplyr::summarise(rows = dplyr::n()) %>% dplyr::filter(rows < 2)
dplyr::count(sim_data_weighted, genWeight)



clusterreg_extract(.dataset     = sim_data_weighted,
                   .dependents  = c("employed_01", "employed_02"),
                   .treatment   = c("treatment"),
                   .covariates  = c("eduLevel", "pre_income_scaled"),
                   .cluster_var = c("sites"),
                   .wt_var      = c("genWeight")
                   )
