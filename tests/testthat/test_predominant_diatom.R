#Load predominant_diatom function
predominant_diatom = function(p_contentration, si_concentration,
                              lake_temp,
                              p_min_ast = 0.01, p_min_cyc = 0.2,
                              si_min_ast = 1.9, si_min_cyc = 0.6,
                              min_lake_temp = 4, max_lake_temp = 28) {
  #error checking
  if (p_contentration < 0)
    return("nutrients concentration cannot be negative")

  if (si_concentration < 0)
    return("nutrients concentration cannot be negative")

  if (lake_temp > 50)
    return("lake temp over 50C? ---> check value")

  if (lake_temp >= min_lake_temp & lake_temp <=  max_lake_temp) {predominant = case_when (
    p_contentration >= p_min_cyc  & si_concentration >= si_min_cyc & si_concentration <= si_min_ast ~ "cyc_predominates", #Cyclotella predominates
    p_contentration >= p_min_ast & p_contentration <= p_min_cyc & si_concentration >= si_min_ast ~ "ast_predominates", #Asterionella predominates
    p_contentration > p_min_cyc & si_concentration > si_min_ast ~ "coexistence", #both Asterionella and Cyclotella coexist without predominance
    p_contentration < p_min_ast ~"no_survival_conditions", #No diatoms can survive
    si_concentration < si_min_cyc ~ "no_survival_conditions",
    p_contentration < p_min_cyc & si_concentration < si_min_ast ~ "no_survival_conditions")
  } else
    predominant = "no_survival_conditions"

  return(predominant)
}

#Test for predominant_diatom():
test_that("test_predominant_diatom",
          {diatom_data = as.data.frame(cbind(p_conc=c(0.12, 0.25, 0.13, 0.23), si_conc=c(2.16, 1.94, 1.76, 1.44), temp=rep(20, times=4), predominant_diatom = NA))

          expect_equal(predominant_diatom(diatom_data$p_conc[1], diatom_data$si_conc[1], diatom_data$temp[1]), "ast_predominates")
          expect_equal(predominant_diatom(diatom_data$p_conc[2], diatom_data$si_conc[2], diatom_data$temp[2]), "coexistence")
          expect_equal(predominant_diatom(diatom_data$p_conc[3], diatom_data$si_conc[3], diatom_data$temp[3]), "no_survival_conditions")
          expect_equal(predominant_diatom(diatom_data$p_conc[4], diatom_data$si_conc[4], diatom_data$temp[4]), "cyc_predominates")

          })



