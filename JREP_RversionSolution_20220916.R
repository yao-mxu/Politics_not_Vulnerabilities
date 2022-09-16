# Older R version solution
# Data Analysis
# Yao Xu
# 9/16/22

### Preparation------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse);library(stringr); library(devtools);library(readtext);library(xtable);library(estimatr)
library(ggplot2);library(reshape2);library(data.table);library(openxlsx);library(readxl);library(stargazer)
library(dotwhisker);library(dplyr);library(grid);library(gtable);library(ggpubr);library(cowplot);library(nnet)
library(gtsummary); library(gt); library(huxtable);library(plm);library(survey);library(scales)
# Setting directory and clearing workspace
#setwd("")
rm(list = ls())

# Load data
# load("")

# Define the controls in both waves
controls<-c("gender_id","race_id","age_binned","educ_binned","income_binned","Pct_nHAsian_wave1",
            "MHI_adj_wave1","PopDensity_sqkm_wave1","Pct_sameH_1yr_wave1","Pct_nHAsian_wave2",
            "MHI_adj_wave2","PopDensity_sqkm_wave2","Pct_sameH_1yr_wave2")

### Linear Regression Models with Threats (No Controls or Weights)---------------------------------------------------------------------------------------------------------------

# 1.1) Main effect, wave 1, no controls, no weights
main_wave1<-lm(dg_amount_wave1 ~ country_alter_wave1, data=waves_us_rev)
# 1.2) Main effect, wave 2, no controls, no weights
main_wave2<-lm(dg_amount_wave2 ~ country_alter_wave2, data=waves_us_rev)
# 1.3) Main effect, wave 1 that returned in wave 2, no controls, no weights
main_wave1on2<-lm(dg_amount_wave1 ~ country_alter_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 1.4）Main effect, wave 2 excluding who moved out of their counties, no controls, no weights
main_wave2stayed<-lm(dg_amount_wave2 ~ country_alter_wave2, data=waves_us_rev[waves_us_rev$wave2stayed==1,])


# 2.1) Health, wave 1, micro, no interaction, no controls, no weights
health_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1, data=waves_us_rev)
# 2.2) Health, wave 1, micro, with interaction, no controls, no weights
health_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1+country_alter_wave1*COVID_risk_wave1, data=waves_us_rev)
# 2.3) Health, wave 2, micro, no interaction, no controls, no weights
health_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+COVID_risk_wave2, data=waves_us_rev)
# 2.4) Health, wave 2, micro, with interaction, no controls, no weights
health_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+COVID_risk_wave2+country_alter_wave2*COVID_risk_wave2, data=waves_us_rev)
# 2.5) Health, wave 1 on 2, micro, no interaction, no controls, no weights
health_wave1on2_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 2.6) Health, wave 1 on 2, micro, with interaction, no controls, no weights
health_wave1on2_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1+country_alter_wave1*COVID_risk_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 3.1) Econ, wave 1, micro, no interaction, no controls, no weights
econ_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1, data=waves_us_rev)
# 3.2) Econ, wave 1, micro, with interaction, no controls, no weights
econ_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1+country_alter_wave1*empl_change_wave1, data=waves_us_rev)
# 3.3) Econ, wave 2, micro, no interaction, no controls, no weights
econ_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+empl_change_wave2, data=waves_us_rev)
# 3.4) Econ, wave 2, micro, with interaction, no controls, no weights
econ_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+empl_change_wave2+country_alter_wave2*empl_change_wave2, data=waves_us_rev)
# 3.5) Econ, wave 1 on 2, micro, no interaction, no controls, no weights
econ_wave1on2_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 3.6) Econ, wave 1 on 2, micro, with interaction, no controls, no weights
econ_wave1on2_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1+country_alter_wave1*empl_change_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 4.1) Symbolic, wave 1, micro, no interaction, no controls, no weights
symbo_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican, data=waves_us_rev)
# 4.2) Symbolic, wave 1, micro, with interaction, no controls, no weights
symbo_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican+country_alter_wave1*is_republican, data=waves_us_rev)
# 4.3) Symbolic, wave 2, micro, no interaction, no controls, no weights
symbo_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+is_republican, data=waves_us_rev)
# 4.4) Symbolic, wave 2, micro, with interaction, no controls, no weights
symbo_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+is_republican+country_alter_wave2*is_republican, data=waves_us_rev)
# 4.5) Symbolic, wave 1 on 2, micro, no interaction, no controls, no weights
symbo_wave1on2_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 4.6) Symbolic, wave 1 on 2, micro, with interaction, no controls, no weights
symbo_wave1on2_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican+country_alter_wave1*is_republican, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 5.1) Health, wave 1, meso, 1, no interaction, no controls, no weights
health_wave1_meso1<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1, data=waves_us_rev)
# 5.2) Health, wave 1, meso, 1, with interaction, no controls, no weights
health_wave1_meso1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1+country_alter_wave1*meso_COVIDdx_wave1, data=waves_us_rev)
# 5.3) Health, wave 2, meso, 1, no interaction, no controls, no weights
health_wave2_meso1<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdx_wave2, data=waves_us_rev)
# 5.4) Health, wave 2, meso, 1, with interaction, no controls, no weights
health_wave2_meso1_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdx_wave2+country_alter_wave2*meso_COVIDdx_wave2, data=waves_us_rev)
# 5.5) Health, wave 1 on 2, meso, 1, no interaction, no controls, no weights
health_wave1on2_meso1<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 5.6) Health, wave 1 on 2, meso, 1, with interaction, no controls, no weights
health_wave1on2_meso1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1+country_alter_wave1*meso_COVIDdx_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 6.1) Health, wave 1, meso, 2, no interaction, no controls, no weights
health_wave1_meso2<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1, data=waves_us_rev)
# 6.2) Health, wave 1, meso, 2, with interaction, no controls, no weights
health_wave1_meso2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1+country_alter_wave1*meso_COVIDdeath_wave1, data=waves_us_rev)
# 6.3) Health, wave 1, meso, 2, no interaction, no controls, no weights
health_wave2_meso2<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdeath_wave2, data=waves_us_rev)
# 6.4) Health, wave 1, meso, 2, with interaction, no controls, no weights
health_wave2_meso2_inter<-lm(dg_amount_wave2~ country_alter_wave2+meso_COVIDdeath_wave2+country_alter_wave2*meso_COVIDdeath_wave2, data=waves_us_rev)
# 6.5) Health, wave 1 on 2, meso, 2, no interaction, no controls, no weights
health_wave1on2_meso2<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 6.6) Health, wave 1 on 2, meso, 2, with interaction, no controls, no weights
health_wave1on2_meso2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1+country_alter_wave1*meso_COVIDdeath_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 7.1) Econ, wave 1, meso, no interaction, no controls, no weights
econ_wave1_meso<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1, data=waves_us_rev)
# 7.2) Econ, wave 1, meso, with interaction, no controls, no weights
econ_wave1_meso_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1+country_alter_wave1*meso_jobloss_wave1, data=waves_us_rev)
# 7.3) Econ, wave 2, meso, no interaction, no controls, no weights
econ_wave2_meso<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_jobloss_wave2, data=waves_us_rev)
# 7.4) Econ, wave 2, meso, with interaction, no controls, no weights
econ_wave2_meso_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_jobloss_wave2+country_alter_wave2*meso_jobloss_wave2, data=waves_us_rev)
# 7.5) Econ, wave 1 on 2, meso, no interaction, no controls, no weights
econ_wave1on2_meso<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 7.6) Econ, wave 1 on2 , meso, with interaction, no controls, no weights
econ_wave1on2_meso_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1+country_alter_wave1*meso_jobloss_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 8.1) Health, wave 1, macro, 1, no interaction, no controls, no weights
health_wave1_macro1<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1, data=waves_us_rev)
# 8.2) Health, wave 1, macro, 1, with interaction, no controls, no weights
health_wave1_macro1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1+country_alter_wave1*newcase_pc_wave1, data=waves_us_rev)
# 8.3) Health, wave 2, macro, 1, no interaction, no controls, no weights
health_wave2_macro1<-lm(dg_amount_wave2 ~ country_alter_wave2+newcase_pc_wave2, data=waves_us_rev)
# 8.4) Health, wave 2, macro, 1, with interaction, no controls, no weights
health_wave2_macro1_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+newcase_pc_wave2+country_alter_wave2*newcase_pc_wave2, data=waves_us_rev)
# 8.5) Health, wave 1 on 2, macro, 1, no interaction, no controls, no weights
health_wave1on2_macro1<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 8.6) Health, wave 1 on 2, macro, 1, with interaction, no controls, no weights
health_wave1on2_macro1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1+country_alter_wave1*newcase_pc_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 9.1) Health, wave 1, macro, 2, no interaction, no controls, no weights
health_wave1_macro2<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1, data=waves_us_rev)
# 9.2) Health, wave 1, macro, 2, with interaction, no controls, no weights
health_wave1_macro2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1+country_alter_wave1*newdeaths_pc_wave1, data=waves_us_rev)
# 9.3) Health, wave 2, macro, 2, no interaction, no controls, no weights
health_wave2_macro2<-lm(dg_amount_wave2 ~ country_alter_wave2+newdeaths_pc_wave2, data=waves_us_rev)
# 9.4) Health, wave 2, macro, 2, with interaction, no controls, no weights
health_wave2_macro2_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+newdeaths_pc_wave2+country_alter_wave2*newdeaths_pc_wave2, data=waves_us_rev)
# 9.5) Health, wave 1 on 2, macro, 2, no interaction, no controls, no weights
health_wave1on2_macro2<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 9.6) Health, wave 1 on 2, macro, 2, with interaction, no controls, no weights
health_wave1on2_macro2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1+country_alter_wave1*newdeaths_pc_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 10.1) Econ, wave 1, macro, no interaction, no controls, no weights
econ_wave1_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1, data=waves_us_rev)
# 10.2) Econ, wave 1, macro, with interaction, no controls, no weights
econ_wave1_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1+country_alter_wave1*Pct_liLoss_jun_wave1, data=waves_us_rev)
# 10.3) Econ, wave 2, macro, no interaction, no controls, no weights
econ_wave2_macro<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2, data=waves_us_rev)
# 10.4) Econ, wave 2, macro, with interaction, no controls, no weights
econ_wave2_macro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2+country_alter_wave2*Pct_liLoss_nov_wave2, data=waves_us_rev)
# 10.5) Econ, wave 1 on 2, macro, no interaction, no controls, no weights
econ_wave1on2_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 10.6) Econ, wave 1 on 2, macro, with interaction, no controls, no weights
econ_wave1on2_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1+country_alter_wave1*Pct_liLoss_jun_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 11.1) Symbolic, wave 1, macro, no interaction, no controls, no weights
symbo_wave1_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1, data=waves_us_rev)
# 11.2) Symbolic, wave 1, macro, with interaction, no controls, no weights
symbo_wave1_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1+country_alter_wave1*Pct_Vote_Rep_wave1, data=waves_us_rev)
# 11.3) Symbolic, wave 2, macro, no interaction, no controls, no weights
symbo_wave2_macro<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2, data=waves_us_rev)
# 11.4) Symbolic, wave 2, macro, with interaction, no controls, no weights
symbo_wave2_macro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2+country_alter_wave2*Pct_Vote_Rep_wave2, data=waves_us_rev)
# 11.5) Symbolic, wave 1 on 2, macro, no interaction, no controls, no weights
symbo_wave1on2_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 11.6) Symbolic, wave 1 on 2, macro, with interaction, no controls, no weights
symbo_wave1on2_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1+country_alter_wave1*Pct_Vote_Rep_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


### Linear Regression Models with Threats and Controls (No Weights)-------------------------------------------------------------------------------------------------------------------------
# 1.1) Main effect, wave 1, with controls, no weights
main_con_wave1<-lm(dg_amount_wave1 ~ country_alter_wave1
               +gender_id+race_id+age_binned+educ_binned+income_binned
               +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 1.2) Main effect, wave 2, with controls, no weights
main_con_wave2<-lm(dg_amount_wave2 ~ country_alter_wave2
                   +gender_id+race_id+age_binned+educ_binned+income_binned
                   +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 1.3) Main effect, wave 1 that returned in wave 2, with controls, no weights
main_con_wave1on2<-lm(dg_amount_wave1 ~ country_alter_wave1
                       +gender_id+race_id+age_binned+educ_binned+income_binned
                       +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 1.4) Main effect, wave 2 excluding the respondents for whom we have evidence about them moving out of their counties
main_con_wave2stayed<-lm(dg_amount_wave2 ~ country_alter_wave2
                   +gender_id+race_id+age_binned+educ_binned+income_binned
                   +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev[waves_us_rev$wave2stayed==1,])

# 2.1) Health, wave 1, micro, no interaction, with controls, no weights
health_con_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1
                       +gender_id+race_id+age_binned+educ_binned+income_binned
                       +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 2.2) Health, wave 1, micro, with interaction, with controls, no weights
health_con_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1+country_alter_wave1*COVID_risk_wave1
                             +gender_id+race_id+age_binned+educ_binned+income_binned
                             +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 2.3) Health, wave 2, micro, no interaction, with controls, no weights
health_con_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+COVID_risk_wave2
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 2.4) Health, wave 2, micro, with interaction, with controls, no weights
health_con_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+COVID_risk_wave2+country_alter_wave2*COVID_risk_wave2
                                 +gender_id+race_id+age_binned+educ_binned+income_binned
                                 +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 2.5) Health, wave 1 on 2, micro, no interaction, with controls, no weights
health_con_wave1on2_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 2.6) Health, wave 1 on 2, micro, with interaction, with controls, no weights
health_con_wave1on2_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1+country_alter_wave1*COVID_risk_wave1
                                 +gender_id+race_id+age_binned+educ_binned+income_binned
                                 +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])

# 3.1) Econ, wave 1, micro, no interaction, with controls, no weights
econ_con_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1
                     +gender_id+race_id+age_binned+educ_binned+income_binned
                     +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 3.2) Econ, wave 1, micro, with interaction, with controls, no weights
econ_con_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1+country_alter_wave1*empl_change_wave1
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 3.3) Econ, wave 2, micro, no interaction, with controls, no weights
econ_con_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+empl_change_wave2
                         +gender_id+race_id+age_binned+educ_binned+income_binned
                         +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 3.4) Econ, wave 2, micro, with interaction, with controls, no weights
econ_con_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+empl_change_wave2+country_alter_wave2*empl_change_wave2
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 3.5) Econ, wave 1 on 2, micro, no interaction, with controls, no weights
econ_con_wave1on2_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1
                         +gender_id+race_id+age_binned+educ_binned+income_binned
                         +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 3.6) Econ, wave 1 on 2, micro, with interaction, with controls, no weights
econ_con_wave1on2_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1+country_alter_wave1*empl_change_wave1
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 4.1) Symbolic, wave 1, micro, no interaction, with controls, no weights
symbo_con_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican
                      +gender_id+race_id+age_binned+educ_binned+income_binned
                      +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 4.2) Symbolic, wave 1, micro, with interaction, with controls, no weights
symbo_con_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican+country_alter_wave1*is_republican
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 4.3) Symbolic, wave 2, micro, no interaction, no controls, no weights
symbo_con_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+is_republican
                      +gender_id+race_id+age_binned+educ_binned+income_binned
                      +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 4.4) Symbolic, wave 2, micro, with interaction, no controls, no weights
symbo_con_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+is_republican+country_alter_wave2*is_republican
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 4.5) Symbolic, wave 1 on 2, micro, no interaction, with controls, no weights
symbo_con_wave1on2_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican
                          +gender_id+race_id+age_binned+educ_binned+income_binned
                          +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 4.6) Symbolic, wave 1 on 2, micro, with interaction, with controls, no weights
symbo_con_wave1on2_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican+country_alter_wave1*is_republican
                                +gender_id+race_id+age_binned+educ_binned+income_binned
                                +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 5.1) Health, wave 1, meso, 1, no interaction, with controls, no weights
health_con_wave1_meso1<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1
                       +gender_id+race_id+age_binned+educ_binned+income_binned
                       +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 5.2) Health, wave 1, meso, 1, with interaction, with controls, no weights
health_con_wave1_meso1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1+country_alter_wave1*meso_COVIDdx_wave1
                             +gender_id+race_id+age_binned+educ_binned+income_binned
                             +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 5.3) Health, wave 2, meso, 1, no interaction, with controls, no weights
health_con_wave2_meso1<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdx_wave2
                       +gender_id+race_id+age_binned+educ_binned+income_binned
                       +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 5.4) Health, wave 2, meso, 1, with interaction, with controls, no weights
health_con_wave2_meso1_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdx_wave2+country_alter_wave2*meso_COVIDdx_wave2
                             +gender_id+race_id+age_binned+educ_binned+income_binned
                             +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 5.5) Health, wave 1 on 2, meso, 1, no interaction, with controls, no weights
health_con_wave1on2_meso1<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 5.6) Health, wave 1 on 2, meso, 1, with interaction, with controls, no weights
health_con_wave1on2_meso1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1+country_alter_wave1*meso_COVIDdx_wave1
                                 +gender_id+race_id+age_binned+educ_binned+income_binned
                                 +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 6.1) Health, wave 1, meso, 2, no interaction, with controls, no weights
health_con_wave1_meso2<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 6.2) Health, wave 1, meso, 2, with interaction, with controls, no weights
health_con_wave1_meso2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1+country_alter_wave1*meso_COVIDdeath_wave1
                             +gender_id+race_id+age_binned+educ_binned+income_binned
                             +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 6.3) Health, wave 1, meso, 2, no interaction, with controls, no weights
health_con_wave2_meso2<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdeath_wave2
                       +gender_id+race_id+age_binned+educ_binned+income_binned
                       +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 6.4) Health, wave 1, meso, 2, with interaction, with controls, no weights
health_con_wave2_meso2_inter<-lm(dg_amount_wave2~ country_alter_wave2+meso_COVIDdeath_wave2+country_alter_wave2*meso_COVIDdeath_wave2
                             +gender_id+race_id+age_binned+educ_binned+income_binned
                             +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 6.5) Health, wave 1 on 2, meso, 2, no interaction, with controls, no weights
health_con_wave1on2_meso2<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 6.6) Health, wave 1 on 2, meso, 2, with interaction, with controls, no weights
health_con_wave1on2_meso2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1+country_alter_wave1*meso_COVIDdeath_wave1
                                 +gender_id+race_id+age_binned+educ_binned+income_binned
                                 +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 7.1) Econ, wave 1, meso, no interaction, with controls, no weights
econ_con_wave1_meso<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1
                    +gender_id+race_id+age_binned+educ_binned+income_binned
                    +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 7.2) Econ, wave 1, meso, with interaction, with controls, no weights
econ_con_wave1_meso_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1+country_alter_wave1*meso_jobloss_wave1
                          +gender_id+race_id+age_binned+educ_binned+income_binned
                          +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 7.3) Econ, wave 2, meso, no interaction, no controls, no weights
econ_con_wave2_meso<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_jobloss_wave2
                    +gender_id+race_id+age_binned+educ_binned+income_binned
                    +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 7.4) Econ, wave 2, meso, with interaction, no controls, no weights
econ_con_wave2_meso_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_jobloss_wave2+country_alter_wave2*meso_jobloss_wave2
                          +gender_id+race_id+age_binned+educ_binned+income_binned
                          +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 7.5) Econ, wave 1 on 2, meso, no interaction, with controls, no weights
econ_con_wave1on2_meso<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1
                        +gender_id+race_id+age_binned+educ_binned+income_binned
                        +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 7.6) Econ, wave 1 on 2, meso, with interaction, with controls, no weights
econ_con_wave1on2_meso_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1+country_alter_wave1*meso_jobloss_wave1
                              +gender_id+race_id+age_binned+educ_binned+income_binned
                              +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 8.1) Health, wave 1, macro, 1, no interaction, with controls, no weights
health_con_wave1_macro1<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1
                        +gender_id+race_id+age_binned+educ_binned+income_binned
                        +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 8.2) Health, wave 1, macro, 1, with interaction, with controls, no weights
health_con_wave1_macro1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1+country_alter_wave1*newcase_pc_wave1
                              +gender_id+race_id+age_binned+educ_binned+income_binned
                              +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 8.3) Health, wave 2, macro, 1, no interaction, with controls, no weights
health_con_wave2_macro1<-lm(dg_amount_wave2 ~ country_alter_wave2+newcase_pc_wave2
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 8.4) Health, wave 2, macro, 1, with interaction, with controls, no weights
health_con_wave2_macro1_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+newcase_pc_wave2+country_alter_wave2*newcase_pc_wave2
                                  +gender_id+race_id+age_binned+educ_binned+income_binned
                                  +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 8.5) Health, wave 1, macro, 1, no interaction, with controls, no weights
health_con_wave1on2_macro1<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 8.6) Health, wave 1, macro, 1, with interaction, with controls, no weights
health_con_wave1on2_macro1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1+country_alter_wave1*newcase_pc_wave1
                                  +gender_id+race_id+age_binned+educ_binned+income_binned
                                  +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 9.1) Health, wave 1, macro, 2, no interaction, with controls, no weights
health_con_wave1_macro2<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1
                        +gender_id+race_id+age_binned+educ_binned+income_binned
                        +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 9.2) Health, wave 1, macro, 2, with interaction, with controls, no weights
health_con_wave1_macro2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1+country_alter_wave1*newdeaths_pc_wave1
                              +gender_id+race_id+age_binned+educ_binned+income_binned
                              +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 9.3) Health, wave 2, macro, 2, no interaction, with controls, no weights
health_con_wave2_macro2<-lm(dg_amount_wave2 ~ country_alter_wave2+newdeaths_pc_wave2
                        +gender_id+race_id+age_binned+educ_binned+income_binned
                        +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 9.4) Health, wave 2, macro, 2, with interaction, with controls, no weights
health_con_wave2_macro2_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+newdeaths_pc_wave2+country_alter_wave2*newdeaths_pc_wave2
                              +gender_id+race_id+age_binned+educ_binned+income_binned
                              +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 9.1) Health, wave 1, macro, 2, no interaction, with controls, no weights
health_con_wave1on2_macro2<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 9.2) Health, wave 1, macro, 2, with interaction, with controls, no weights
health_con_wave1on2_macro2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1+country_alter_wave1*newdeaths_pc_wave1
                                  +gender_id+race_id+age_binned+educ_binned+income_binned
                                  +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])



# 10.1) Econ, wave 1, macro, no interaction, with controls, no weights
econ_con_wave1_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1
                     +gender_id+race_id+age_binned+educ_binned+income_binned
                     +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 10.2) Econ, wave 1, macro, with interaction, with controls, no weights
econ_con_wave1_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1+country_alter_wave1*Pct_liLoss_jun_wave1
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 10.3) Econ, wave 2, macro, no interaction, with controls, no weights
econ_con_wave2_macro<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2
                         +gender_id+race_id+age_binned+educ_binned+income_binned
                         +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 10.4) Econ, wave 2, macro, with interaction, with controls, no weights
econ_con_wave2_macro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2+country_alter_wave2*Pct_liLoss_nov_wave2
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 10.5) Econ, wave 1 on 2, macro, no interaction, with controls, no weights
econ_con_wave1on2_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1
                         +gender_id+race_id+age_binned+educ_binned+income_binned
                         +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 10.6) Econ, wave 1 on 2, macro, with interaction, with controls, no weights
econ_con_wave1on2_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1+country_alter_wave1*Pct_liLoss_jun_wave1
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])


# 11.1) Symbolic, wave 1, macro, no interaction, with controls, no weights
symbo_con_wave1_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1
                      +gender_id+race_id+age_binned+educ_binned+income_binned
                      +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 11.2) Symbolic, wave 1, macro, with interaction, with controls, no weights
symbo_con_wave1_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1+country_alter_wave1*Pct_Vote_Rep_wave1
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev)
# 11.3) Symbolic, wave 2, macro, no interaction, with controls, no weights
symbo_con_wave2_macro<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2
                      +gender_id+race_id+age_binned+educ_binned+income_binned
                      +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 11.4) Symbolic, wave 2, macro, with interaction, with controls, no weights
symbo_con_wave2_macro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2+country_alter_wave2*Pct_Vote_Rep_wave2
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev)
# 11.5) Symbolic, wave 1 on 2, macro, no interaction, with controls, no weights
symbo_con_wave1on2_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1
                          +gender_id+race_id+age_binned+educ_binned+income_binned
                          +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])
# 11.6) Symbolic, wave 1 on 2, macro, with interaction, with controls, no weights
symbo_con_wave1on2_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1+country_alter_wave1*Pct_Vote_Rep_wave1
                                +gender_id+race_id+age_binned+educ_binned+income_binned
                                +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,])

### Linear Regression Models with Weights (No Controls)---------------------------------------------------------------------------------------------------------------
# 1.1) Main effect, wave 1, no controls, with weights
main_wei_wave1<-lm(dg_amount_wave1 ~ country_alter_wave1, data=waves_us_rev, weights=weight_wave1)
# 1.2) Main effect, wave 2, no controls, with weights
main_wei_wave2<-lm(dg_amount_wave2 ~ country_alter_wave2, data=waves_us_rev, weights=weight_wave2)

### Linear Regression Models with Threats and Controls and Weights-------------------------------------------------------------------------------------------------------------------------
# 1.1) Main effect, wave 1, with controls, with weights
main_wei_con_wave1<-lm(dg_amount_wave1 ~ country_alter_wave1
                       +gender_id+race_id+age_binned+educ_binned+income_binned
                       +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 1.2) Main effect, wave 2, with controls, with weights
main_wei_con_wave2<-lm(dg_amount_wave2 ~ country_alter_wave2
                   +gender_id+race_id+age_binned+educ_binned+income_binned
                   +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 1.3) Main effect, wave 1 that returned in wave 2, with controls, with weights
# main_wei_con_wave1_2<-lm(dg_amount_wave1 ~ country_alter_wave1
#                    +gender_id+race_id+age_binned+educ_binned+income_binned
#                     +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,], weights=weight_wave1on2)


# 2.1) Health, wave 1, micro, no interaction, with controls, with weights
health_wei_con_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 2.2) Health, wave 1, micro, with interaction, with controls, with weights
health_wei_con_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+COVID_risk_wave1+country_alter_wave1*COVID_risk_wave1
                                     +gender_id+race_id+age_binned+educ_binned+income_binned
                                     +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 2.3) Health, wave 2, micro, no interaction, with controls, with weights
health_wei_con_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+COVID_risk_wave2
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 2.4) Health, wave 2, micro, with interaction, with controls, with weights
health_wei_con_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+COVID_risk_wave2+country_alter_wave2*COVID_risk_wave2
                                 +gender_id+race_id+age_binned+educ_binned+income_binned
                                 +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)


# 3.1) Econ, wave 1, micro, no interaction, with controls, with weights
econ_wei_con_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1
                             +gender_id+race_id+age_binned+educ_binned+income_binned
                             +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 3.2) Econ, wave 1, micro, with interaction, with controls, with weights
econ_wei_con_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+empl_change_wave1+country_alter_wave1*empl_change_wave1
                                   +gender_id+race_id+age_binned+educ_binned+income_binned
                                   +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 3.3) Econ, wave 2, micro, no interaction, with controls, with weights
econ_wei_con_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+empl_change_wave2
                         +gender_id+race_id+age_binned+educ_binned+income_binned
                         +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 3.4) Econ, wave 2, micro, with interaction, with controls, with weights
econ_wei_con_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+empl_change_wave2+country_alter_wave2*empl_change_wave2
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)


# 4.1) Symbolic, wave 1, micro, no interaction, with controls, with weights
symbo_wei_con_wave1_micro<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican
                              +gender_id+race_id+age_binned+educ_binned+income_binned
                              +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 4.2) Symbolic, wave 1, micro, with interaction, with controls, with weights
symbo_wei_con_wave1_micro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+is_republican+country_alter_wave1*is_republican
                                    +gender_id+race_id+age_binned+educ_binned+income_binned
                                    +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 4.3) Symbolic, wave 2, micro, no interaction, no controls, with weights
 symbo_wei_con_wave2_micro<-lm(dg_amount_wave2 ~ country_alter_wave2+is_republican
                          +gender_id+race_id+age_binned+educ_binned+income_binned
                          +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 4.4) Symbolic, wave 2, micro, with interaction, no controls, with weights
 symbo_wei_con_wave2_micro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+is_republican+country_alter_wave2*is_republican
                                +gender_id+race_id+age_binned+educ_binned+income_binned
                                +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)


# 5.1) Health, wave 1, meso, 1, no interaction, with controls, with weights
health_wei_con_wave1_meso1<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 5.2) Health, wave 1, meso, 1, with interaction, with controls, with weights
health_wei_con_wave1_meso1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1+country_alter_wave1*meso_COVIDdx_wave1
                                     +gender_id+race_id+age_binned+educ_binned+income_binned
                                     +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 5.3) Health, wave 2, meso, 1, no interaction, with controls, with weights
 health_wei_con_wave2_meso1<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdx_wave2
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 5.4) Health, wave 2, meso, 1, with interaction, with controls, with weights
 health_wei_con_wave2_meso1_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdx_wave2+country_alter_wave2*meso_COVIDdx_wave2
                                 +gender_id+race_id+age_binned+educ_binned+income_binned
                                 +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 5.5) Health, wave 1 on 2, meso, 1, no interaction, with controls, with weights
# health_wei_con_wave1on2_meso1<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1
#                              +gender_id+race_id+age_binned+educ_binned+income_binned
#                              +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,], weights=weight_wave1on2)
# 5.6) Health, wave 1 on 2, meso, 1, with interaction, with controls, with weights
# health_wei_con_wave1on2_meso1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdx_wave1+country_alter_wave1*meso_COVIDdx_wave1
#                                    +gender_id+race_id+age_binned+educ_binned+income_binned
#                                    +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev[waves_us_rev$sub_merge==1,], weights=weight_wave1on2)


# 6.1) Health, wave 1, meso, 2, no interaction, with controls, with weights
health_wei_con_wave1_meso2<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 6.2) Health, wave 1, meso, 2, with interaction, with controls, with weights
health_wei_con_wave1_meso2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_COVIDdeath_wave1+country_alter_wave1*meso_COVIDdeath_wave1
                                     +gender_id+race_id+age_binned+educ_binned+income_binned
                                     +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 6.3) Health, wave 1, meso, 2, no interaction, with controls, with weights
 health_wei_con_wave2_meso2<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_COVIDdeath_wave2
                           +gender_id+race_id+age_binned+educ_binned+income_binned
                           +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 6.4) Health, wave 1, meso, 2, with interaction, with controls, with weights
 health_wei_con_wave2_meso2_inter<-lm(dg_amount_wave2~ country_alter_wave2+meso_COVIDdeath_wave2+country_alter_wave2*meso_COVIDdeath_wave2
                                 +gender_id+race_id+age_binned+educ_binned+income_binned
                                 +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)

# 7.1) Econ, wave 1, meso, no interaction, with controls, with weights
econ_wei_con_wave1_meso<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 7.2) Econ, wave 1, meso, with interaction, with controls, with weights
econ_wei_con_wave1_meso_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+meso_jobloss_wave1+country_alter_wave1*meso_jobloss_wave1
                                  +gender_id+race_id+age_binned+educ_binned+income_binned
                                  +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 7.3) Econ, wave 2, meso, no interaction, with controls, with weights
 econ_wei_con_wave2_meso<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_jobloss_wave2
                        +gender_id+race_id+age_binned+educ_binned+income_binned
                        +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 7.4) Econ, wave 2, meso, with interaction, no controls, with weights
 econ_wei_con_wave2_meso_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+meso_jobloss_wave2+country_alter_wave2*meso_jobloss_wave2
                              +gender_id+race_id+age_binned+educ_binned+income_binned
                              +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)

# 8.1) Health, wave 1, macro, 1, no interaction, with controls, with weights
health_wei_con_wave1_macro1<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1
                                +gender_id+race_id+age_binned+educ_binned+income_binned
                                +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 8.2) Health, wave 1, macro, 1, with interaction, with controls, with weights
health_wei_con_wave1_macro1_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newcase_pc_wave1+country_alter_wave1*newcase_pc_wave1
                                      +gender_id+race_id+age_binned+educ_binned+income_binned
                                      +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 8.3) Health, wave 2, macro, 1, no interaction, with controls, with weights
 health_wei_con_wave2_macro1<-lm(dg_amount_wave2 ~ country_alter_wave2+newcase_pc_wave2
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 8.4) Health, wave 2, macro, 1, with interaction, with controls, with weights
 health_wei_con_wave2_macro1_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+newcase_pc_wave2+country_alter_wave2*newcase_pc_wave2
                                  +gender_id+race_id+age_binned+educ_binned+income_binned
                                  +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)

# 9.1) Health, wave 1, macro, 2, no interaction, with controls, with weights
health_wei_con_wave1_macro2<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1
                                +gender_id+race_id+age_binned+educ_binned+income_binned
                                +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 9.2) Health, wave 1, macro, 2, with interaction, with controls, with weights
health_wei_con_wave1_macro2_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+newdeaths_pc_wave1+country_alter_wave1*newdeaths_pc_wave1
                                      +gender_id+race_id+age_binned+educ_binned+income_binned
                                      +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 9.3) Health, wave 2, macro, 2, no interaction, with controls, with weights
 health_wei_con_wave2_macro2<-lm(dg_amount_wave2 ~ country_alter_wave2+newdeaths_pc_wave2
                            +gender_id+race_id+age_binned+educ_binned+income_binned
                            +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 9.4) Health, wave 2, macro, 2, with interaction, with controls, with weights
 health_wei_con_wave2_macro2_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+newdeaths_pc_wave2+country_alter_wave2*newdeaths_pc_wave2
                                  +gender_id+race_id+age_binned+educ_binned+income_binned
                                  +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)


# 10.1) Econ, wave 1, macro, no interaction, with controls, with weights
econ_wei_con_wave1_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1
                             +gender_id+race_id+age_binned+educ_binned+income_binned
                             +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 10.2) Econ, wave 1, macro, with interaction, with controls, with weights
econ_wei_con_wave1_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1+country_alter_wave1*Pct_liLoss_jun_wave1
                                   +gender_id+race_id+age_binned+educ_binned+income_binned
                                   +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 10.3) Econ, wave 2, macro, no interaction, with controls, with weights
 econ_wei_con_wave2_macro<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2
                         +gender_id+race_id+age_binned+educ_binned+income_binned
                         +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 10.4) Econ, wave 2, macro, with interaction, with controls, with weights
 econ_wei_con_wave2_macro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2+country_alter_wave2*Pct_liLoss_nov_wave2
                               +gender_id+race_id+age_binned+educ_binned+income_binned
                               +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)


# 11.1) Symbolic, wave 1, macro, no interaction, with controls, with weights
symbo_wei_con_wave1_macro<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1
                              +gender_id+race_id+age_binned+educ_binned+income_binned
                              +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 11.2) Symbolic, wave 1, macro, with interaction, with controls, with weights
symbo_wei_con_wave1_macro_inter<-lm(dg_amount_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1+country_alter_wave1*Pct_Vote_Rep_wave1
                                    +gender_id+race_id+age_binned+educ_binned+income_binned
                                    +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data=waves_us_rev,weights=weight_wave1)
# 11.3) Symbolic, wave 2, macro, no interaction, with controls, with weights
 symbo_wei_con_wave2_macro<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2
                          +gender_id+race_id+age_binned+educ_binned+income_binned
                          +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)
# 11.4) Symbolic, wave 2, macro, with interaction, with controls, with weights
 symbo_wei_con_wave2_macro_inter<-lm(dg_amount_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2+country_alter_wave2*Pct_Vote_Rep_wave2
                                +gender_id+race_id+age_binned+educ_binned+income_binned
                                +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data=waves_us_rev,weights=weight_wave2)


### Multinomial Logistic Regression-----------------------------
multinom_wave1 <- multinom(dg_category_wave1 ~ country_alter_wave1, data = waves_us_rev)
# multinom_wave1_pval <- (1 - pnorm(abs(summary(multinom_wave1)$coefficients/summary(multinom_wave1)$standard.errors), 0, 1)) * 2
# colnames(multinom_wave1_pval)<-c("(Intercept)", "p_value")
# multinom_wave1_pval<-as.data.frame(multinom_wave1_pval)
# cbind(summary(multinom_wave1)$coefficients, subset(multinom_wave1_pval,select=p_value))

multinom_wave2 <- multinom(dg_category_wave2 ~ country_alter_wave2, data = waves_us_rev)
# multinom_wave2_pval <- (1 - pnorm(abs(summary(multinom_wave2)$coefficients/summary(multinom_wave2)$standard.errors), 0, 1)) * 2
# colnames(multinom_wave2_pval)<-c("(Intercept)", "p_value")
# multinom_wave2_pval<-as.data.frame(multinom_wave2_pval)
# cbind(summary(multinom_wave2)$coefficients, subset(multinom_wave2_pval,select=p_value))

# Multinomial Wave 1 individual
health_multi_wave1_micro <- multinom(dg_category_wave1 ~ country_alter_wave1+COVID_risk_wave1, data = waves_us_rev)
econ_multi_wave1_micro <- multinom(dg_category_wave1 ~ country_alter_wave1+empl_change_wave1, data = waves_us_rev)
symbo_multi_wave1_micro <- multinom(dg_category_wave1 ~ country_alter_wave1+is_republican, data = waves_us_rev)

health_multi_wave1_micro_inter<- multinom(dg_category_wave1 ~ country_alter_wave1+COVID_risk_wave1+country_alter_wave1*COVID_risk_wave1, data = waves_us_rev)
econ_multi_wave1_micro_inter <- multinom(dg_category_wave1 ~ country_alter_wave1+empl_change_wave1+country_alter_wave1*empl_change_wave1, data = waves_us_rev)
symbo_multi_wave1_micro_inter <- multinom(dg_category_wave1 ~ country_alter_wave1+is_republican+country_alter_wave1*is_republican, data = waves_us_rev)

# Multinomial Wave 1 macro
health_multi_wave1_macro1 <- multinom(dg_category_wave1 ~ country_alter_wave1+newcase_pc_wave1, data = waves_us_rev)  
health_multi_wave1_macro2 <- multinom(dg_category_wave1 ~ country_alter_wave1+newdeaths_pc_wave1, data = waves_us_rev)
econ_multi_wave1_macro <- multinom(dg_category_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1, data = waves_us_rev)
symbo_multi_wave1_macro <- multinom(dg_category_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1, data = waves_us_rev)

health_multi_wave1_macro1_inter<- multinom(dg_category_wave1 ~ country_alter_wave1+newcase_pc_wave1+country_alter_wave1*newcase_pc_wave1, data = waves_us_rev)
health_multi_wave1_macro2_inter<- multinom(dg_category_wave1 ~ country_alter_wave1+newdeaths_pc_wave1+country_alter_wave1*newdeaths_pc_wave1, data = waves_us_rev)
econ_multi_wave1_macro_inter <- multinom(dg_category_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1+country_alter_wave1*Pct_liLoss_jun_wave1, data = waves_us_rev)
symbo_multi_wave1_macro_inter <- multinom(dg_category_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1+country_alter_wave1*Pct_Vote_Rep_wave1, data = waves_us_rev)

# Multinomial Wave 2 individual
health_multi_wave2_micro <- multinom(dg_category_wave2 ~ country_alter_wave2+COVID_risk_wave2, data = waves_us_rev)
econ_multi_wave2_micro <- multinom(dg_category_wave2 ~ country_alter_wave2+empl_change_wave2, data = waves_us_rev)
symbo_multi_wave2_micro <- multinom(dg_category_wave2 ~ country_alter_wave2+is_republican, data = waves_us_rev)

health_multi_wave2_micro_inter<- multinom(dg_category_wave2 ~ country_alter_wave2+COVID_risk_wave2+country_alter_wave2*COVID_risk_wave2, data = waves_us_rev)
econ_multi_wave2_micro_inter <- multinom(dg_category_wave2 ~ country_alter_wave2+empl_change_wave2+country_alter_wave2*empl_change_wave2, data = waves_us_rev)
symbo_multi_wave2_micro_inter <- multinom(dg_category_wave2 ~ country_alter_wave2+is_republican+country_alter_wave2*is_republican, data = waves_us_rev)

# Multinomial Wave 2 macro
health_multi_wave2_macro1 <- multinom(dg_category_wave2 ~ country_alter_wave2+newcase_pc_wave2, data = waves_us_rev)  
health_multi_wave2_macro2 <- multinom(dg_category_wave2 ~ country_alter_wave2+newdeaths_pc_wave2, data = waves_us_rev)
econ_multi_wave2_macro <- multinom(dg_category_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2, data = waves_us_rev)
symbo_multi_wave2_macro <- multinom(dg_category_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2, data = waves_us_rev)

health_multi_wave2_macro1_inter<- multinom(dg_category_wave2 ~ country_alter_wave2+newcase_pc_wave2+country_alter_wave2*newcase_pc_wave2, data = waves_us_rev)
health_multi_wave2_macro2_inter<- multinom(dg_category_wave2 ~ country_alter_wave2+newdeaths_pc_wave2+country_alter_wave2*newdeaths_pc_wave2, data = waves_us_rev)
econ_multi_wave2_macro_inter <- multinom(dg_category_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2+country_alter_wave2*Pct_liLoss_nov_wave2, data = waves_us_rev)
symbo_multi_wave2_macro_inter <- multinom(dg_category_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2+country_alter_wave2*Pct_Vote_Rep_wave2, data = waves_us_rev)

# Multinomial Wave 1 individual with controls
health_multi_con_wave1_micro <- multinom(dg_category_wave1 ~ country_alter_wave1+COVID_risk_wave1
                                     +gender_id+race_id+age_binned+educ_binned+income_binned
                                     +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
econ_multi_con_wave1_micro <- multinom(dg_category_wave1 ~ country_alter_wave1+empl_change_wave1
                                   +gender_id+race_id+age_binned+educ_binned+income_binned
                                   +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
symbo_multi_con_wave1_micro <- multinom(dg_category_wave1 ~ country_alter_wave1+is_republican
                                    +gender_id+race_id+age_binned+educ_binned+income_binned
                                    +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)

health_multi_con_wave1_micro_inter<- multinom(dg_category_wave1 ~ country_alter_wave1+COVID_risk_wave1+country_alter_wave1*COVID_risk_wave1
                                          +gender_id+race_id+age_binned+educ_binned+income_binned
                                          +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
econ_multi_con_wave1_micro_inter <- multinom(dg_category_wave1 ~ country_alter_wave1+empl_change_wave1+country_alter_wave1*empl_change_wave1
                                         +gender_id+race_id+age_binned+educ_binned+income_binned
                                         +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
symbo_multi_con_wave1_micro_inter <- multinom(dg_category_wave1 ~ country_alter_wave1+is_republican+country_alter_wave1*is_republican
                                          +gender_id+race_id+age_binned+educ_binned+income_binned
                                          +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)

# Multinomial Wave 1 macro with controls
health_multi_con_wave1_macro1 <- multinom(dg_category_wave1 ~ country_alter_wave1+newcase_pc_wave1
                                      +gender_id+race_id+age_binned+educ_binned+income_binned
                                      +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)  
health_multi_con_wave1_macro2 <- multinom(dg_category_wave1 ~ country_alter_wave1+newdeaths_pc_wave1
                                      +gender_id+race_id+age_binned+educ_binned+income_binned
                                      +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
econ_multi_con_wave1_macro <- multinom(dg_category_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1
                                   +gender_id+race_id+age_binned+educ_binned+income_binned
                                   +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
symbo_multi_con_wave1_macro <- multinom(dg_category_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1
                                    +gender_id+race_id+age_binned+educ_binned+income_binned
                                    +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)

health_multi_con_wave1_macro1_inter<- multinom(dg_category_wave1 ~ country_alter_wave1+newcase_pc_wave1+country_alter_wave1*newcase_pc_wave1
                                           +gender_id+race_id+age_binned+educ_binned+income_binned
                                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
health_multi_con_wave1_macro2_inter<- multinom(dg_category_wave1 ~ country_alter_wave1+newdeaths_pc_wave1+country_alter_wave1*newdeaths_pc_wave1
                                           +gender_id+race_id+age_binned+educ_binned+income_binned
                                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
econ_multi_con_wave1_macro_inter <- multinom(dg_category_wave1 ~ country_alter_wave1+Pct_liLoss_jun_wave1+country_alter_wave1*Pct_liLoss_jun_wave1
                                         +gender_id+race_id+age_binned+educ_binned+income_binned
                                         +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)
symbo_multi_con_wave1_macro_inter <- multinom(dg_category_wave1 ~ country_alter_wave1+Pct_Vote_Rep_wave1+country_alter_wave1*Pct_Vote_Rep_wave1
                                          +gender_id+race_id+age_binned+educ_binned+income_binned
                                          +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)

# Multinomial Wave 2 individual with controls
health_multi_con_wave2_micro <- multinom(dg_category_wave2 ~ country_alter_wave2+COVID_risk_wave2 
                                     +gender_id+race_id+age_binned+educ_binned+income_binned
                                     +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
econ_multi_con_wave2_micro <- multinom(dg_category_wave2 ~ country_alter_wave2+empl_change_wave2
                                   +gender_id+race_id+age_binned+educ_binned+income_binned
                                   +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
symbo_multi_con_wave2_micro <- multinom(dg_category_wave2 ~ country_alter_wave2+is_republican
                                    +gender_id+race_id+age_binned+educ_binned+income_binned
                                    +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)

health_multi_con_wave2_micro_inter<- multinom(dg_category_wave2 ~ country_alter_wave2+COVID_risk_wave2+country_alter_wave2*COVID_risk_wave2
                                          +gender_id+race_id+age_binned+educ_binned+income_binned
                                          +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
econ_multi_con_wave2_micro_inter <- multinom(dg_category_wave2 ~ country_alter_wave2+empl_change_wave2+country_alter_wave2*empl_change_wave2
                                         +gender_id+race_id+age_binned+educ_binned+income_binned
                                         +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
symbo_multi_con_wave2_micro_inter <- multinom(dg_category_wave2 ~ country_alter_wave2+is_republican+country_alter_wave2*is_republican
                                          +gender_id+race_id+age_binned+educ_binned+income_binned
                                          +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)

# Multinomial Wave 2 macro with controls

health_multi_con_wave2_macro1 <- multinom(dg_category_wave2 ~ country_alter_wave2+newcase_pc_wave2
                                      +gender_id+race_id+age_binned+educ_binned+income_binned
                                      +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)  
health_multi_con_wave2_macro2 <- multinom(dg_category_wave2 ~ country_alter_wave2+newdeaths_pc_wave2
                                      +gender_id+race_id+age_binned+educ_binned+income_binned
                                      +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
econ_multi_con_wave2_macro <- multinom(dg_category_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2
                                   +gender_id+race_id+age_binned+educ_binned+income_binned
                                   +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
symbo_multi_con_wave2_macro <- multinom(dg_category_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2
                                    +gender_id+race_id+age_binned+educ_binned+income_binned
                                    +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)

health_multi_con_wave2_macro1_inter<- multinom(dg_category_wave2 ~ country_alter_wave2+newcase_pc_wave2+country_alter_wave2*newcase_pc_wave2
                                           +gender_id+race_id+age_binned+educ_binned+income_binned
                                           +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
health_multi_con_wave2_macro2_inter<- multinom(dg_category_wave2 ~ country_alter_wave2+newdeaths_pc_wave2+country_alter_wave2*newdeaths_pc_wave2
                                           +gender_id+race_id+age_binned+educ_binned+income_binned
                                           +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
econ_multi_con_wave2_macro_inter <- multinom(dg_category_wave2 ~ country_alter_wave2+Pct_liLoss_nov_wave2+country_alter_wave2*Pct_liLoss_nov_wave2
                                         +gender_id+race_id+age_binned+educ_binned+income_binned
                                         +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)
symbo_multi_con_wave2_macro_inter <- multinom(dg_category_wave2 ~ country_alter_wave2+Pct_Vote_Rep_wave2+country_alter_wave2*Pct_Vote_Rep_wave2
                                          +gender_id+race_id+age_binned+educ_binned+income_binned
                                          +Pct_nHAsian_wave2+MHI_adj_wave2+PopDensity_sqkm_wave2+Pct_sameH_1yr_wave2, data = waves_us_rev)

### Making Large Coefficients Figures -------------------------------
# Tidy Micro Level Data  
tidydt_micro<-function(health_micro,health_micro_inter, 
                 econ_micro,econ_micro_inter, 
                 symbo_micro, symbo_micro_inter,is.control,wave){
  health_micro <- tidy(health_micro) %>% full_join(data.frame(term=tidy(health_micro_inter)$term), by = c("term")) %>% full_join(data.frame(term=tidy(econ_micro_inter)$term), by = c("term")) %>% full_join(data.frame(term=tidy(symbo_micro_inter)$term), by = c("term"))%>% mutate(model = "Health Risk")
  econ_micro <- tidy(econ_micro) %>%  full_join(data.frame(term=tidy(health_micro_inter)$term), by = c("term")) %>% full_join(data.frame(term=tidy(econ_micro_inter)$term), by = c("term")) %>% full_join(data.frame(term=tidy(symbo_micro_inter)$term), by = c("term")) %>% mutate(model = "Economic")
  symbo_micro <- tidy(symbo_micro) %>% full_join(data.frame(term=tidy(health_micro_inter)$term), by = c("term")) %>% full_join(data.frame(term=tidy(econ_micro_inter)$term), by = c("term")) %>% full_join(data.frame(term=tidy(symbo_micro_inter)$term), by = c("term")) %>% mutate(model = "Partisanship")
  
  if (wave==1){
  micro_plot<-rbind(rbind(health_micro,econ_micro),symbo_micro) %>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                           "COVID_risk_wave1Medium Risk" = "Medium Health Risk",
                                                                                                           "country_alter_wave1China:COVID_risk_wave1Medium Risk"="Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                           "COVID_risk_wave1High Risk"="High Health Risk",
                                                                                                           "country_alter_wave1China:COVID_risk_wave1High Risk" = "Chinese-born Recipient \u00D7 High Risk",
                                                                                                           "empl_change_wave11" = "Adverse Employment Change",          
                                                                                                           "country_alter_wave1China:empl_change_wave11" = "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                           "is_republicanRepublican"="Republican Partisan",
                                                                                                           "country_alter_wave1China:is_republicanRepublican"= "Chinese-born Recipient \u00D7 Republican Partisan"))%>% mutate(inter = "Without Interactions")
  
  micro_plot_inter<-rbind(rbind(tidy(health_micro_inter) %>% mutate(model = "Health Risk"),tidy(econ_micro_inter)%>% mutate(model = "Economic")),tidy(symbo_micro_inter)%>% mutate(model = "Partisanship"))%>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                "COVID_risk_wave1Medium Risk" = "Medium Health Risk",
                                                                                                                                                                                                                                                                "country_alter_wave1China:COVID_risk_wave1Medium Risk"="Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                                                                                                                                                                                "COVID_risk_wave1High Risk"="High Health Risk",
                                                                                                                                                                                                                                                                "country_alter_wave1China:COVID_risk_wave1High Risk" = "Chinese-born Recipient \u00D7 High Risk",
                                                                                                                                                                                                                                                                "empl_change_wave11" = "Adverse Employment Change",          
                                                                                                                                                                                                                                                                "country_alter_wave1China:empl_change_wave11" = "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                                                                                                                                                                                "is_republicanRepublican"="Republican Partisan",
                                                                                                                                                                                                                                                                "country_alter_wave1China:is_republicanRepublican"= "Chinese-born Recipient \u00D7 Republican Partisan"))%>% mutate(inter = "With Interactions")}
  else {micro_plot<-rbind(rbind(health_micro,econ_micro),symbo_micro)%>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                   "COVID_risk_wave2Medium Risk" = "Medium Health Risk",
                                                                                                                   "country_alter_wave2China:COVID_risk_wave2Medium Risk"="Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                                   "COVID_risk_wave2High Risk"="High Health Risk",
                                                                                                                   "country_alter_wave2China:COVID_risk_wave2High Risk" = "Chinese-born Recipient \u00D7 High Risk",
                                                                                                                   "empl_change_wave21" = "Adverse Employment Change",          
                                                                                                                   "country_alter_wave2China:empl_change_wave21" = "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                                   "is_republicanRepublican"="Republican Partisan",
                                                                                                                   "country_alter_wave2China:is_republicanRepublican"= "Chinese-born Recipient \u00D7 Republican Partisan"))%>% mutate(inter = "Without Interactions")
        micro_plot_inter<-rbind(rbind(tidy(health_micro_inter) %>% mutate(model = "Health Risk"),tidy(econ_micro_inter)%>% mutate(model = "Economic")),tidy(symbo_micro_inter)%>% mutate(model = "Partisanship"))%>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                             "COVID_risk_wave2Medium Risk" = "Medium Health Risk",
                                                                                                                                                                                                                                                             "country_alter_wave2China:COVID_risk_wave2Medium Risk"="Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                                                                                                                                                                             "COVID_risk_wave2High Risk"="High Health Risk",
                                                                                                                                                                                                                                                             "country_alter_wave2China:COVID_risk_wave2High Risk" = "Chinese-born Recipient \u00D7 High Risk",
                                                                                                                                                                                                                                                             "empl_change_wave21" = "Adverse Employment Change",          
                                                                                                                                                                                                                                                             "country_alter_wave2China:empl_change_wave21" = "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                                                                                                                                                                             "is_republicanRepublican"="Republican Partisan",
                                                                                                                                                                                                                                                             "country_alter_wave2China:is_republicanRepublican"= "Chinese-born Recipient \u00D7 Republican Partisan"))%>% mutate(inter = "With Interactions")}
  if (is.control == T) {
    plot_micro_wave1<-rbind(micro_plot,micro_plot_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "Participant") %>% filter(term %in% c("Chinese-born Recipient",
                           "Medium Health Risk",
                           "Chinese-born Recipient \u00D7 Medium Risk",
                           "High Health Risk",
                           "Chinese-born Recipient \u00D7 High Risk",
                           "Adverse Employment Change",
                           "Chinese-born Recipient \u00D7 Adverse Employment Change",
                           "Republican Partisan",
                           "Chinese-born Recipient \u00D7 Republican Partisan"))}
  else {plot_micro_wave1<-rbind(micro_plot,micro_plot_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "Participant")}
  return(plot_micro_wave1)
  }

# Tidy Meso Level Data  
tidydt_meso<-function(health_meso1, health_meso1_inter, 
                       health_meso2,health_meso2_inter, 
                       econ_meso,econ_meso_inter,is.control, wave){

  health_meso1 <- tidy(health_meso1) %>% full_join(data.frame(term=tidy(health_meso1_inter)$term), 
                                                               by = c("term")) %>% full_join(data.frame(term=tidy(health_meso2_inter)$term), 
                                                                                             by = c("term"))  %>%  full_join(data.frame(term=tidy(econ_meso_inter)$term), 
                                                                                                                             by = c("term"))%>% mutate(model = "County Health (Diagnoses)")
  health_meso2 <- tidy(health_meso2) %>% full_join(data.frame(term=tidy(health_meso1_inter)$term), 
                                                               by = c("term")) %>% full_join(data.frame(term=tidy(health_meso2_inter)$term), 
                                                                                             by = c("term"))  %>%  full_join(data.frame(term=tidy(econ_meso_inter)$term), 
                                                                                                                             by = c("term")) %>% mutate(model = "County Health (Deaths)")
  econ_meso <- tidy(econ_meso) %>% full_join(data.frame(term=tidy(health_meso1_inter)$term), 
                                                         by = c("term")) %>% full_join(data.frame(term=tidy(health_meso2_inter)$term), 
                                                                                       by = c("term"))  %>%  full_join(data.frame(term=tidy(econ_meso_inter)$term), 
                                                                                                                       by = c("term"))%>% mutate(model = "Economic")
  if (wave==1){
  plot_meso<-rbind(rbind(health_meso1,health_meso2),econ_meso) %>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                          "meso_COVIDdx_wave11+"="Network COVID-19 Diagnoses:1+",
                                                                                                          "country_alter_wave1China:meso_COVIDdx_wave11+" = "China*Network COVID Diagnoses:1+",
                                                                                                          "meso_COVIDdeath_wave11+" = "Network COVID-19 Deaths:1+",
                                                                                                          "country_alter_wave1China:meso_COVIDdeath_wave11+"="China*Network COVID Deaths:1+",
                                                                                                          "meso_jobloss_wave11+" = "Network COVID-19 Job Losses:1+",          
                                                                                                          "country_alter_wave1China:meso_jobloss_wave11+" = "China*Network COVID-19 Job Losses:1+"))%>% mutate(inter = "Without Interactions")
  
  plot_meso_inter<-rbind(rbind(tidy(health_meso1_inter) %>% mutate(model = "County Health (Diagnoses)"),tidy(health_meso2_inter)%>% mutate(model = "County Health (Deaths)")),tidy(econ_meso_inter)%>% mutate(model = "Economic"))%>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                               "meso_COVIDdx_wave11+"="Network COVID-19 Diagnoses:1+",
                                                                                                                                                                                                                                                                               "country_alter_wave1China:meso_COVIDdx_wave11+" = "China*Network COVID Diagnoses:1+",
                                                                                                                                                                                                                                                                               "meso_COVIDdeath_wave11+" = "Network COVID-19 Deaths:1+",
                                                                                                                                                                                                                                                                               "country_alter_wave1China:meso_COVIDdeath_wave11+"="China*Network COVID Deaths:1+",
                                                                                                                                                                                                                                                                               "meso_jobloss_wave11+" = "Network COVID-19 Job Losses:1+",          
                                                                                                                                                                                                                                                                               "country_alter_wave1China:meso_jobloss_wave11+" = "China*Network COVID-19 Job Losses:1+"))%>% mutate(inter = "With Interactions")}
  else{plot_meso<-rbind(rbind(health_meso1,health_meso2),econ_meso) %>%  relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                  "meso_COVIDdx_wave21+"="Network COVID-19 Diagnoses:1+",
                                                                                                                  "country_alter_wave2China:meso_COVIDdx_wave21+" = "China*Network COVID Diagnoses:1+",
                                                                                                                  "meso_COVIDdeath_wave21+" = "Network COVID-19 Deaths:1+",
                                                                                                                  "country_alter_wave2China:meso_COVIDdeath_wave21+"="China*Network COVID Deaths:1+",
                                                                                                                  "meso_jobloss_wave21+" = "Network COVID-19 Job Losses:1+",          
                                                                                                                  "country_alter_wave2China:meso_jobloss_wave21+" = "China*Network COVID-19 Job Losses:1+"))%>% mutate(inter = "Without Interactions")
      plot_meso_inter<-rbind(rbind(tidy(health_meso1_inter) %>% mutate(model = "County Health (Diagnoses)"),tidy(health_meso2_inter)%>% mutate(model = "County Health (Deaths)")),tidy(econ_meso_inter)%>% mutate(model = "Economic"))%>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                                   "meso_COVIDdx_wave21+"="Network COVID-19 Diagnoses:1+",
                                                                                                                                                                                                                                                                                   "country_alter_wave2China:meso_COVIDdx_wave21+" = "China*Network COVID Diagnoses:1+",
                                                                                                                                                                                                                                                                                   "meso_COVIDdeath_wave21+" = "Network COVID-19 Deaths:1+",
                                                                                                                                                                                                                                                                                   "country_alter_wave2China:meso_COVIDdeath_wave21+"="China*Network COVID Deaths:1+",
                                                                                                                                                                                                                                                                                   "meso_jobloss_wave21+" = "Network COVID-19 Job Losses:1+",          
                                                                                                                                                                                                                                                                                   "country_alter_wave2China:meso_jobloss_wave21+" = "China*Network COVID-19 Job Losses:1+"))%>% mutate(inter = "With Interactions")}
  if (is.control ==T) {
    plot_meso_wave1<-rbind(plot_meso,plot_meso_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "Meso")%>% filter(term %in% c("Chinese-born Recipient",
                                                                                                                                                                                            "Network COVID-19 Diagnoses:1+",
                                                                                                                                                                                            "China*Network COVID Diagnoses:1+",
                                                                                                                                                                                            "Network COVID-19 Deaths:1+",
                                                                                                                                                                                            "China*Network COVID Deaths:1+",
                                                                                                                                                                                            "Network COVID-19 Job Losses:1+",
                                                                                                                                                                                            "China*Network COVID-19 Job Losses:1+"))}
  else{plot_meso_wave1<-rbind(plot_meso,plot_meso_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "Meso")}
  return(plot_meso_wave1)
  }

# Tidy Macro Level Data  
tidydt_macro<-function(health_macro1,health_macro1_inter,
                         health_macro2,health_macro2_inter,
                         econ_macro,econ_macro_inter,
                         symbo_macro,symbo_macro_inter,is.control,wave){
  
  health_macro1 <- tidy(health_macro1) %>% full_join(data.frame(term=tidy(health_macro1_inter)$term), 
                                                                 by = c("term"))%>% full_join(data.frame(term=tidy(health_macro2_inter)$term), 
                                                                                              by = c("term")) %>% full_join(data.frame(term=tidy(econ_macro_inter)$term), 
                                                                                                                            by = c("term")) %>% full_join(data.frame(term=tidy(symbo_macro_inter)$term), 
                                                                                                                                                          by = c("term"))%>% mutate(model = "County Health (Diagnoses)")
  health_macro2 <- tidy(health_macro2) %>% full_join(data.frame(term=tidy(health_macro1_inter)$term), 
                                                                 by = c("term"))%>% full_join(data.frame(term=tidy(health_macro2_inter)$term), 
                                                                                              by = c("term")) %>% full_join(data.frame(term=tidy(econ_macro_inter)$term), 
                                                                                                                            by = c("term")) %>% full_join(data.frame(term=tidy(symbo_macro_inter)$term), 
                                                                                                                                                          by = c("term"))%>% mutate(model = "County Health (Deaths)")
  econ_macro <- tidy(econ_macro) %>% full_join(data.frame(term=tidy(health_macro1_inter)$term), 
                                                           by = c("term"))%>% full_join(data.frame(term=tidy(health_macro2_inter)$term), 
                                                                                        by = c("term")) %>% full_join(data.frame(term=tidy(econ_macro_inter)$term), 
                                                                                                                      by = c("term")) %>% full_join(data.frame(term=tidy(symbo_macro_inter)$term), 
                                                                                                                                                    by = c("term"))%>% mutate(model = "Economic")
  symbo_macro <- tidy(symbo_macro) %>% full_join(data.frame(term=tidy(health_macro1_inter)$term), 
                                                             by = c("term"))%>% full_join(data.frame(term=tidy(health_macro2_inter)$term), 
                                                                                          by = c("term")) %>% full_join(data.frame(term=tidy(econ_macro_inter)$term), 
                                                                                                                        by = c("term")) %>% full_join(data.frame(term=tidy(symbo_macro_inter)$term), 
                                                                                                                                                      by = c("term"))%>% mutate(model = "Partisanship")
  
  if (wave==1){
  plot_macro<-rbind(rbind(rbind(health_macro1,health_macro2),econ_macro),symbo_macro) %>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                                                       "newcase_pc_wave1"="County COVID-19 Diagnoses",
                                                                                                                                       "country_alter_wave1China:newcase_pc_wave1" = "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                       "newdeaths_pc_wave1" = "County COVID-19 Deaths",
                                                                                                                                       "country_alter_wave1China:newdeaths_pc_wave1"="Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                       "Pct_liLoss_jun_wave1" = "County Low Income Job Losses",          
                                                                                                                                       "country_alter_wave1China:Pct_liLoss_jun_wave1" = "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                       "Pct_Vote_Rep_wave1"="County Republican Vote Share",
                                                                                                                                       "country_alter_wave1China:Pct_Vote_Rep_wave1"="Chinese-born Recipient \u00D7 County Republican Vote Share"))%>% mutate(inter = "Without Interactions")
  
  plot_macro_inter<-rbind(rbind(rbind(tidy(health_macro1_inter) %>% mutate(model = "County Health (Diagnoses)"),tidy(health_macro2_inter)%>% mutate(model = "County Health (Deaths)")),tidy(econ_macro_inter) %>% mutate(model = "Economic")), tidy(symbo_macro_inter) %>% mutate(model = "Partisanship")) %>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                                                                                                          "newcase_pc_wave1"="County COVID-19 Diagnoses",
                                                                                                                                                                                                                                                                                                                                                          "country_alter_wave1China:newcase_pc_wave1" = "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                                                                                                                                                                                                                                          "newdeaths_pc_wave1" = "County COVID-19 Deaths",
                                                                                                                                                                                                                                                                                                                                                          "country_alter_wave1China:newdeaths_pc_wave1"="Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                                                                                                                                          "Pct_liLoss_jun_wave1" = "County Low Income Job Losses",          
                                                                                                                                                                                                                                                                                                                                                          "country_alter_wave1China:Pct_liLoss_jun_wave1" = "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                                                                                                                                                                                                                                          "Pct_Vote_Rep_wave1"="County Republican Vote Share",
                                                                                                                                                                                                                                                                                                                                                          "country_alter_wave1China:Pct_Vote_Rep_wave1"="Chinese-born Recipient \u00D7 County Republican Vote Share"))%>% mutate(inter = "With Interactions")}
  else{plot_macro<-rbind(rbind(rbind(health_macro1,health_macro2),econ_macro),symbo_macro) %>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                                               "newcase_pc_wave2"="County COVID-19 Diagnoses",
                                                                                                                                               "country_alter_wave2China:newcase_pc_wave2" = "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                               "newdeaths_pc_wave2" = "County COVID-19 Deaths",
                                                                                                                                               "country_alter_wave2China:newdeaths_pc_wave2"="Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                               "Pct_liLoss_nov_wave2" = "County Low Income Job Losses",          
                                                                                                                                               "country_alter_wave2China:Pct_liLoss_nov_wave2" = "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                               "Pct_Vote_Rep_wave2"="County Republican Vote Share",
                                                                                                                                               "country_alter_wave2China:Pct_Vote_Rep_wave2"="Chinese-born Recipient \u00D7 County Republican Vote Share"))%>% mutate(inter = "Without Interactions")
    
    
    plot_macro_inter<-rbind(rbind(rbind(tidy(health_macro1_inter) %>% mutate(model = "County Health (Diagnoses)"),tidy(health_macro2_inter)%>% mutate(model = "County Health (Deaths)")),tidy(econ_macro_inter) %>% mutate(model = "Economic")), tidy(symbo_macro_inter) %>% mutate(model = "Partisanship")) %>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                                                                                                              "newcase_pc_wave2"="County COVID-19 Diagnoses",
                                                                                                                                                                                                                                                                                                                                                              "country_alter_wave2China:newcase_pc_wave2" = "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                                                                                                                                                                                                                                              "newdeaths_pc_wave2" = "County COVID-19 Deaths",
                                                                                                                                                                                                                                                                                                                                                              "country_alter_wave2China:newdeaths_pc_wave2"="Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                                                                                                                                                                                                                                              "Pct_liLoss_nov_wave2" = "County Low Income Job Losses",          
                                                                                                                                                                                                                                                                                                                                                              "country_alter_wave2China:Pct_liLoss_nov_wave2" = "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                                                                                                                                                                                                                                              "Pct_Vote_Rep_wave2"="County Republican Vote Share",
                                                                                                                                                                                                                                                                                                                                                              "country_alter_wave2China:Pct_Vote_Rep_wave2"="Chinese-born Recipient \u00D7 County Republican Vote Share"))%>% mutate(inter = "With Interactions")}
  if (is.control == TRUE) {
  plot_macro_wave1<-rbind(plot_macro,plot_macro_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "County")%>% filter(term %in% c("Chinese-born Recipient",
                                                                                                                                                                                              "County COVID-19 Diagnoses",
                                                                                                                                                                                              "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                                                                              "County COVID-19 Deaths",
                                                                                                                                                                                              "Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                                                                              "County Low Income Job Losses",
                                                                                                                                                                                              "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                                                                              "County Republican Vote Share",
                                                                                                                                                                                              "Chinese-born Recipient \u00D7 County Republican Vote Share"))}
  else{plot_macro_wave1<-rbind(plot_macro,plot_macro_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "County")}
  return(plot_macro_wave1)
  }

# Tidy datasets to create coef figures
plot_micro_wave1<-tidydt_micro(health_wave1_micro,health_wave1_micro_inter, 
                               econ_wave1_micro,econ_wave1_micro_inter, 
                               symbo_wave1_micro, symbo_wave1_micro_inter,is.control=F, wave=1)

plot_meso_wave1<-tidydt_meso(health_wave1_meso1, health_wave1_meso1_inter, 
                             health_wave1_meso2,health_wave1_meso2_inter, 
                             econ_wave1_meso,econ_wave1_meso_inter,is.control=F, wave=1)


plot_macro_wave1<-tidydt_macro(health_wave1_macro1,health_wave1_macro1_inter,
                               health_wave1_macro2,health_wave1_macro2_inter,
                               econ_wave1_macro,econ_wave1_macro_inter,
                               symbo_wave1_macro,symbo_wave1_macro_inter,is.control=F, wave=1)

# Make a comprehensive legend from this plot, one-time execution
plot_wave1<-rbind(plot_micro_wave1,plot_meso_wave1,plot_macro_wave1) %>% mutate(level = factor(level,level = c("Participant","Meso","County")))
levels <- c("Health Risk", "County Health (Diagnoses)", "County Health (Deaths)","Economic","Partisanship")
plot_wave1$model <- factor(plot_wave1$model, levels = c("Health Risk", "County Health (Diagnoses)", "County Health (Deaths)","Economic","Partisanship"))
plot_wave1 <- plot_wave1[match(levels, plot_wave1$model),] 


main_legend<-dwplot(plot_wave1,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                    legend.position = "bottom",
                                                                    panel.grid.major = element_blank(),
                                                                    axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                    panel.grid.minor = element_blank(),
                                                                    panel.background = element_blank(),
                                                                    axis.line = element_line(colour = "black"),
                                                                    plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(-6,6)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","Health Risk"="#A730DC"))+scale_shape_manual(values=c(17, 25,24, 15, 16))

legend <- get_legend(main_legend)

# Write a function to make these figures
make_coef_figure<-function(plot_micro,plot_meso,plot_macro,xmin, xmax,nointerjobloss,intervoteshare, maskjobloss,no_legend){
  wave1_micro<-dwplot(ci = .90, plot_micro,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                      panel.grid.major = element_blank(),
                                                                      axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank(),
                                                                      axis.line = element_line(colour = "black"),
                                                                      plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","Health Risk"="#A730DC"))+scale_shape_manual(values=c(15,17,16))
  wave1_meso<-dwplot(ci = .90,plot_meso,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                      panel.grid.major = element_blank(),
                                                                      axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank(),
                                                                      axis.line = element_line(colour = "black"),
                                                                      plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC"))+scale_shape_manual(values=c(15,24,25, 16))
  if (maskjobloss==F){
    if (nointerjobloss==T & intervoteshare==F){
      wave1_macro<-dwplot(ci = .90,plot_macro,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                          panel.grid.major = element_blank(),
                                                                          axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                          panel.grid.minor = element_blank(),
                                                                          panel.background = element_blank(),
                                                                          axis.line = element_line(colour = "black"),
                                                                          plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","dotted"="#FF2C79"))+scale_shape_manual(values=c(24, 25,15, 16))+geom_segment(data=subset(plot_macro, inter == "With Interactions"),
                                                                                                                                                                                                                                                                                                                                                                            mapping=aes(x=xmin, xend=xmax, y="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                        yend="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                        color="dotted",linetype="dotted",lwd = "5"))+scale_linetype_manual(values ="dashed")+geom_segment(data=subset(plot_macro, inter == "With Interactions"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          mapping=aes(x=xmin, xend=xmax, y="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      yend="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      color="dotted",linetype="dotted",lwd = "5"))+geom_segment(data=subset(plot_macro, inter == "Without Interactions"), position = position_nudge(y = 0.07),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                mapping=aes(x=xmin, xend=xmax, y="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            yend="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            color="dotted",linetype="dotted",lwd = "5"))+scale_size_manual(values=c(0.05))}
    if (nointerjobloss==F & intervoteshare==F){
      wave1_macro<-dwplot(ci = .90,plot_macro,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                          panel.grid.major = element_blank(),
                                                                          axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                          panel.grid.minor = element_blank(),
                                                                          panel.background = element_blank(),
                                                                          axis.line = element_line(colour = "black"),
                                                                          plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","dotted"="#FF2C79"))+scale_shape_manual(values=c(24, 25,15, 16))+geom_segment(data=subset(plot_macro, inter == "With Interactions"), 
                                                                                                                                                                                                                                                                                                                                                                            mapping=aes(x=xmin, xend=xmax, y="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                        yend="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                        color="dotted",linetype="dotted",lwd = "5"))+scale_linetype_manual(values ="dashed")+geom_segment(data=subset(plot_macro, inter == "With Interactions"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          mapping=aes(x=xmin, xend=xmax, y="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      yend="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      color="dotted",linetype="dotted",lwd = "5"))+scale_size_manual(values=c(0.05))}
    
    if (nointerjobloss==F & intervoteshare==T){
      wave1_macro<-dwplot(ci = .90,plot_macro,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                          panel.grid.major = element_blank(),
                                                                          axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                          panel.grid.minor = element_blank(),
                                                                          panel.background = element_blank(),
                                                                          axis.line = element_line(colour = "black"),
                                                                          plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","dotted"="#FF2C79","dotted2"="#1DBEB9"))+scale_shape_manual(values=c(24, 25,15, 16))+geom_segment(data=subset(plot_macro, inter == "With Interactions"), 
                                                                                                                                                                                                                                                                                                                                                                                                mapping=aes(x=xmin, xend=xmax, y="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                            yend="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                            color="dotted",linetype="dotted",lwd = "5"))+scale_linetype_manual(values ="dashed")+geom_segment(data=subset(plot_macro, inter == "With Interactions"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              mapping=aes(x=xmin, xend=xmax, y="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          yend="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          color="dotted",linetype="dotted",lwd = "5"))+geom_segment(data=subset(plot_macro, inter == "With Interactions"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    mapping=aes(x=xmin, xend=xmax, y="Chinese-born Recipient \u00D7 County Republican Vote Share", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                yend="Chinese-born Recipient \u00D7 County Republican Vote Share", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                color="dotted2",linetype="dotted",lwd = "5"))+scale_size_manual(values=c(0.05))}}
  else{
    if (nointerjobloss==T & intervoteshare==F){
      wave1_macro<-dwplot(ci = .90,plot_macro,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                          panel.grid.major = element_blank(),
                                                                          axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                          panel.grid.minor = element_blank(),
                                                                          panel.background = element_blank(),
                                                                          axis.line = element_line(colour = "black"),
                                                                          plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","dotted"="#FF2C79"))+scale_shape_manual(values=c(24, 25,15, 16))+geom_segment(data=subset(plot_macro, inter == "With Interactions"),
                                                                                                                                                                                                                                                                                                                                                                            mapping=aes(x=xmin, xend=xmax, y="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                        yend="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                        color="dotted",linetype="dotted",lwd = "5"))+scale_linetype_manual(values ="dashed")+geom_segment(data=subset(plot_macro, inter == "Without Interactions"), position = position_nudge(y = 0.1),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                mapping=aes(x=xmin, xend=xmax, y="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            yend="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            color="dotted",linetype="dotted",lwd = "5"))+scale_size_manual(values=c(0.05))}
    if (nointerjobloss==F & intervoteshare==F){
      wave1_macro<-dwplot(ci = .90,plot_macro,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                          panel.grid.major = element_blank(),
                                                                          axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                          panel.grid.minor = element_blank(),
                                                                          panel.background = element_blank(),
                                                                          axis.line = element_line(colour = "black"),
                                                                          plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","dotted"="#FF2C79"))+scale_shape_manual(values=c(24, 25,15, 16))+geom_segment(data=subset(plot_macro, inter == "With Interactions"), 
                                                                                                                                                                                                                                                                                                                                                                            mapping=aes(x=xmin, xend=xmax, y="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                        yend="Chinese-born Recipient \u00D7 County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                        color="dotted",linetype="dotted",lwd = "5"))+scale_linetype_manual(values ="dashed")+scale_size_manual(values=c(0.05))}
    
    if (nointerjobloss==F & intervoteshare==T){
      wave1_macro<-dwplot(ci = .90,plot_macro,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, scales = "free", space="free")+
        geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                          panel.grid.major = element_blank(),
                                                                          axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                          panel.grid.minor = element_blank(),
                                                                          panel.background = element_blank(),
                                                                          axis.line = element_line(colour = "black"),
                                                                          plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","dotted"="#FF2C79","dotted2"="#1DBEB9"))+scale_shape_manual(values=c(24, 25,15, 16))+scale_linetype_manual(values ="dashed")+geom_segment(data=subset(plot_macro, inter == "With Interactions"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              mapping=aes(x=xmin, xend=xmax, y="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          yend="County Low Income Job Losses", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          color="dotted",linetype="dotted",lwd = "5"))+geom_segment(data=subset(plot_macro, inter == "With Interactions"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    mapping=aes(x=xmin, xend=xmax, y="Chinese-born Recipient \u00D7 County Republican Vote Share", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                yend="Chinese-born Recipient \u00D7 County Republican Vote Share", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                color="dotted2",linetype="dotted",lwd = "5"))+scale_size_manual(values=c(0.05))}
    
    
    
  }
  a<-plot_grid(wave1_micro+ theme(legend.position="none"), wave1_macro+ theme(legend.position="none"), ncol = 1, align = "v",axis="rlbt")
  #a<-plot_grid(wave1_micro+ theme(legend.position="none"),wave1_meso+ theme(legend.position="none"), wave1_macro+ theme(legend.position="none"), ncol = 1, align = "v",axis="rlbt") # this includes meso level
  plot<-plot_grid(a, legend,ncol = 1,rel_heights = c(2, .1),axis="r")
  if (no_legend==T){
    return(a)
  }else
  return(plot)
}


# Make wave 1 figure without controls
w1<-make_coef_figure(plot_micro_wave1,plot_meso_wave1,plot_macro_wave1,xmin=-10, xmax=10, nointerjobloss=F,intervoteshare=F,maskjobloss=T,no_legend=T)

# Make wave 1 figure with controls
# First let's tidy the data set
plot_con_micro_wave1<-tidydt_micro(health_con_wave1_micro,health_con_wave1_micro_inter, 
                               econ_con_wave1_micro,econ_con_wave1_micro_inter, 
                               symbo_con_wave1_micro, symbo_con_wave1_micro_inter,is.control = T, wave=1)

plot_con_meso_wave1<-tidydt_meso(health_con_wave1_meso1, health_con_wave1_meso1_inter, 
                             health_con_wave1_meso2,health_con_wave1_meso2_inter, 
                             econ_con_wave1_meso,econ_con_wave1_meso_inter,is.control = T, wave=1)


plot_con_macro_wave1<-tidydt_macro(health_con_wave1_macro1,health_con_wave1_macro1_inter,
                               health_con_wave1_macro2,health_con_wave1_macro2_inter,
                               econ_con_wave1_macro,econ_con_wave1_macro_inter,
                               symbo_con_wave1_macro,symbo_con_wave1_macro_inter,is.control = T, wave=1)

# Print figure wave 1 figure with controls
w1con<-make_coef_figure(plot_con_micro_wave1,plot_con_meso_wave1,plot_con_macro_wave1,xmin=-10, xmax=10, nointerjobloss=F,intervoteshare=F,maskjobloss=T,no_legend=T)

# Wave 1 with weights without controls
# Tidy datasets
#plot_wei_micro_wave1<-tidydt_micro(health_wei_wave1_micro,health_wei_wave1_micro_inter, 
#                               econ_wei_wave1_micro,econ_wei_wave1_micro_inter, 
#                               symbo_wei_wave1_micro, symbo_wei_wave1_micro_inter,is.control=F, wave=1)

#plot_wei_meso_wave1<-tidydt_meso(health_wei_wave1_meso1, health_wei_wave1_meso1_inter, 
#                             health_wei_wave1_meso2,health_wei_wave1_meso2_inter, 
#                             econ_wei_wave1_meso,econ_wei_wave1_meso_inter,is.control=F, wave=1)


#plot_wei_macro_wave1<-tidydt_macro(health_wei_wave1_macro1,health_wei_wave1_macro1_inter,
#                               health_wei_wave1_macro2,health_wei_wave1_macro2_inter,
#                               econ_wei_wave1_macro,econ_wei_wave1_macro_inter,
#                               symbo_wei_wave1_macro,symbo_wei_wave1_macro_inter,is.control=F, wave=1)

# make_coef_figure(plot_wei_micro_wave1,plot_wei_meso_wave1,plot_wei_macro_wave1,xmin=-10, xmax=10,nointerjobloss=F,intervoteshare=F,maskjobloss=T,no_legend=F)

# Make wave 1 returned figure without controls
plot_micro_wave1on2<-tidydt_micro(health_wave1on2_micro,health_wave1on2_micro_inter, 
                               econ_wave1on2_micro,econ_wave1on2_micro_inter, 
                               symbo_wave1on2_micro, symbo_wave1on2_micro_inter,is.control=F, wave=1)

plot_meso_wave1on2<-tidydt_meso(health_wave1on2_meso1, health_wave1on2_meso1_inter, 
                             health_wave1on2_meso2,health_wave1on2_meso2_inter, 
                             econ_wave1on2_meso,econ_wave1on2_meso_inter,is.control=F, wave=1)

plot_macro_wave1on2<-tidydt_macro(health_wave1on2_macro1,health_wave1on2_macro1_inter,
                               health_wave1on2_macro2,health_wave1on2_macro2_inter,
                               econ_wave1on2_macro,econ_wave1on2_macro_inter,
                               symbo_wave1on2_macro,symbo_wave1on2_macro_inter,is.control=F, wave=1)

# Make wave 1 returned figure without controls
w1on2_nc<-make_coef_figure(plot_micro_wave1on2,plot_meso_wave1on2,plot_macro_wave1on2,xmin=-10, xmax=10,nointerjobloss=F,intervoteshare=F,maskjobloss=T,no_legend=T)

# Make wave 1 returned figure with controls
plot_con_micro_wave1on2<-tidydt_micro(health_con_wave1on2_micro,health_con_wave1on2_micro_inter, 
                                  econ_con_wave1on2_micro,econ_con_wave1on2_micro_inter, 
                                  symbo_con_wave1on2_micro, symbo_con_wave1on2_micro_inter,is.control=T, wave=1)

plot_con_meso_wave1on2<-tidydt_meso(health_con_wave1on2_meso1, health_con_wave1on2_meso1_inter, 
                                health_con_wave1on2_meso2,health_con_wave1on2_meso2_inter, 
                                econ_con_wave1on2_meso,econ_con_wave1on2_meso_inter,is.control=T, wave=1)

plot_con_macro_wave1on2<-tidydt_macro(health_con_wave1on2_macro1,health_con_wave1on2_macro1_inter,
                                  health_con_wave1on2_macro2,health_con_wave1on2_macro2_inter,
                                  econ_con_wave1on2_macro,econ_con_wave1on2_macro_inter,
                                  symbo_con_wave1on2_macro,symbo_con_wave1on2_macro_inter,is.control=T, wave=1)

# Print figure wave 1 returned figure with controls
w1on2_c<-make_coef_figure(plot_con_micro_wave1on2,plot_con_meso_wave1on2,plot_con_macro_wave1on2,xmin=-10, xmax=10,nointerjobloss=T,intervoteshare=F,maskjobloss=T,no_legend=T)

# Wave 2 without controls
plot_micro_wave2<-tidydt_micro(health_wave2_micro,health_wave2_micro_inter, 
             econ_wave2_micro,econ_wave2_micro_inter, 
             symbo_wave2_micro, symbo_wave2_micro_inter,is.control=F,wave=2)


plot_meso_wave2<-tidydt_meso(health_wave2_meso1, health_wave2_meso1_inter, 
                             health_wave2_meso2,health_wave2_meso2_inter, 
                             econ_wave2_meso,econ_wave2_meso_inter,is.control=F,wave=2)


plot_macro_wave2<-tidydt_macro(health_wave2_macro1,health_wave2_macro1_inter,
                               health_wave2_macro2,health_wave2_macro2_inter,
                               econ_wave2_macro,econ_wave2_macro_inter,
                               symbo_wave2_macro,symbo_wave2_macro_inter,is.control=F,wave=2)

w2<-make_coef_figure(plot_micro_wave2,plot_meso_wave2,plot_macro_wave2,xmin=-10, xmax=10,nointerjobloss=T,intervoteshare=F,maskjobloss=F,no_legend=T)

# Wave 2 with controls
plot_con_micro_wave2<-tidydt_micro(health_con_wave2_micro,health_con_wave2_micro_inter, 
                               econ_con_wave2_micro,econ_con_wave2_micro_inter, 
                               symbo_con_wave2_micro, symbo_con_wave2_micro_inter,is.control=T,wave=2)


plot_con_meso_wave2<-tidydt_meso(health_con_wave2_meso1, health_con_wave2_meso1_inter, 
                             health_con_wave2_meso2,health_con_wave2_meso2_inter, 
                             econ_con_wave2_meso,econ_con_wave2_meso_inter,is.control=T,wave=2)


plot_con_macro_wave2<-tidydt_macro(health_con_wave2_macro1,health_con_wave2_macro1_inter,
                               health_con_wave2_macro2,health_con_wave2_macro2_inter,
                               econ_con_wave2_macro,econ_con_wave2_macro_inter,
                               symbo_con_wave2_macro,symbo_con_wave2_macro_inter,is.control=T,wave=2)

w2con<-make_coef_figure(plot_con_micro_wave2,plot_con_meso_wave2,plot_con_macro_wave2,xmin=-10, xmax=10,nointerjobloss=T,intervoteshare=F,maskjobloss=F,no_legend=T)

# Wave 1 with weights and controls
# Tidy datasets
plot_wei_con_micro_wave1<-tidydt_micro(health_wei_con_wave1_micro,health_wei_con_wave1_micro_inter, 
                                       econ_wei_con_wave1_micro,econ_wei_con_wave1_micro_inter, 
                                       symbo_wei_con_wave1_micro, symbo_wei_con_wave1_micro_inter,is.control=T, wave=1)

plot_wei_con_meso_wave1<-tidydt_meso(health_wei_con_wave1_meso1, health_wei_con_wave1_meso1_inter, 
                                     health_wei_con_wave1_meso2,health_wei_con_wave1_meso2_inter, 
                                     econ_wei_con_wave1_meso,econ_wei_con_wave1_meso_inter,is.control=T, wave=1)


plot_wei_con_macro_wave1<-tidydt_macro(health_wei_con_wave1_macro1,health_wei_con_wave1_macro1_inter,
                                       health_wei_con_wave1_macro2,health_wei_con_wave1_macro2_inter,
                                       econ_wei_con_wave1_macro,econ_wei_con_wave1_macro_inter,
                                       symbo_wei_con_wave1_macro,symbo_wei_con_wave1_macro_inter,is.control=T, wave=1)

plot_wei_wave1<-make_coef_figure(plot_wei_con_micro_wave1,plot_wei_con_meso_wave1,plot_wei_con_macro_wave1,xmin=-10, xmax=10,nointerjobloss=F,intervoteshare=F,maskjobloss=T,no_legend=T)

# Wave 2 with weights and controls
# Tidy datasets
plot_wei_con_micro_wave2<-tidydt_micro(health_wei_con_wave2_micro,health_wei_con_wave2_micro_inter, 
                                       econ_wei_con_wave2_micro,econ_wei_con_wave2_micro_inter, 
                                       symbo_wei_con_wave2_micro, symbo_wei_con_wave2_micro_inter,is.control=T, wave=2)

plot_wei_con_meso_wave2<-tidydt_meso(health_wei_con_wave2_meso1, health_wei_con_wave2_meso1_inter, 
                                     health_wei_con_wave2_meso2,health_wei_con_wave2_meso2_inter, 
                                     econ_wei_con_wave2_meso,econ_wei_con_wave2_meso_inter,is.control=T, wave=2)


plot_wei_con_macro_wave2<-tidydt_macro(health_wei_con_wave2_macro1,health_wei_con_wave2_macro1_inter,
                                       health_wei_con_wave2_macro2,health_wei_con_wave2_macro2_inter,
                                       econ_wei_con_wave2_macro,econ_wei_con_wave2_macro_inter,
                                       symbo_wei_con_wave2_macro,symbo_wei_con_wave2_macro_inter,is.control=T, wave=2)

plot_wei_wave2<-make_coef_figure(plot_wei_con_micro_wave2,plot_wei_con_meso_wave2,plot_wei_con_macro_wave2,xmin=-10, xmax=10,nointerjobloss=T,intervoteshare=F,maskjobloss=F,no_legend=T)

plot_wei_wave1<-annotate_figure(plot_wei_wave1,fig.lab ="A Wave 1 with weights",fig.lab.pos = c("top.left"),fig.lab.face="bold")
plot_wei_wave2<-annotate_figure(plot_wei_wave2,fig.lab ="B Wave 2 with weights",fig.lab.pos = c("top.left"),fig.lab.face="bold")

wei_w1w2<-ggarrange(plot_wei_wave1,plot_wei_wave2,ncol=1)
wei_w1w2<-plot_grid(wei_w1w2, legend, ncol = 1,rel_heights = c(2, .1), axis="r")

annotate_figure(wei_w1w2, bottom = text_grob("\n Whiskers denote 90% confidence intervals. \n Dashed whiskers represent 90% confidence intervals too wide to be printed with the current x-axis limits for visualization purposes. \n Controls include age, gender, race/ethnicity, educational attainment, and family income on the participant-level and \n % Asian, median household income, population density, and % residents in same house >= 1 year on the county-level. \n Data is weighted.", color = "black",
                                         hjust = 1, x = 1, face = "italic", size = 9))

# Saving the figures
# Adding the legend last
# No controls, wave1 and wave 2
# w1<-annotate_figure(w1,fig.lab ="Wave 1",fig.lab.pos = c("top.left"),fig.lab.face="bold")
# w2<-annotate_figure(w2,fig.lab ="Wave 2",fig.lab.pos = c("top.left"),fig.lab.face="bold")
# w1w2<-ggarrange(w1,w2,ncol=1)
# plot<-plot_grid(w1w2, legend,ncol = 1,rel_heights = c(2, .1), axis="r")

# annotate_figure(plot, bottom = text_grob("\n Whiskers denote 90% confidence intervals. \n Dashed whiskers represent 90% confidence intervals too wide to be printed with the current x-axis limits for visualization purposes.", color = "black",
#                                                     hjust = 1, x = 1, face = "italic", size = 9))

# With controls, wave 1 and wave 2 combined
w1con<-annotate_figure(w1con,fig.lab ="A Wave 1",fig.lab.pos = c("top.left"),fig.lab.face="bold")
w2con<-annotate_figure(w2con,fig.lab ="B Wave 2",fig.lab.pos = c("top.left"),fig.lab.face="bold")
w1w2con<-ggarrange(w1con,w2con,ncol=1)
plotcon<-plot_grid(w1w2con, legend,ncol = 1,rel_heights = c(2, .1), axis="r")

annotate_figure(plotcon, bottom = text_grob("\n Whiskers denote 90% confidence intervals. \n Dashed whiskers represent 90% confidence intervals too wide to be printed with the current x-axis limits for visualization purposes. \n Controls include age, gender, race/ethnicity, educational attainment, and family income on the participant-level and \n % Asian, median household income, population density, and % residents in same house >= 1 year on the county-level. \n Data is unweighted. The corresponding tables of regression output are SM Tables S3-S6. ", color = "black",hjust = 1, x = 1, face = "italic", size = 9))

# Wave 1 returned with controls
# w1on2_nc<-annotate_figure(w1on2_nc,fig.lab ="Wave 1 Returned without Controls",fig.lab.pos = c("top.left"),fig.lab.face="bold")
w1on2_c<-annotate_figure(w1on2_c,fig.lab ="Wave 1 Returned",fig.lab.pos = c("top.left"),fig.lab.face="bold")
# w1on2<-ggarrange(w1on2_nc,w1on2_c,ncol=1)
plotw1on2<-plot_grid(w1on2_c, legend,ncol = 1,rel_heights = c(2, .1), axis="r")

annotate_figure(plotw1on2, bottom = text_grob("\n Whiskers denote 90% confidence intervals. \n Dashed whiskers represent 90% confidence intervals too wide to be printed with the current x-axis limits for visualization purposes. \n Controls include age, gender, race/ethnicity, educational attainment, and family income on the participant-level and \n % Asian, median household income, population density, and % residents in same house >= 1 year on the county-level. \n Data is unweighted.", color = "black",
                                            hjust = 1, x = 1, face = "italic", size = 9))

# Making some meso figures for capstone [Not for Paper]
levels <- c("County Health (Diagnoses)", "County Health (Deaths)","Economic")
plot_meso<-plot_meso_wave1
plot_meso$model <- factor(plot_meso$model, levels =levels)
plot_meso <- plot_meso[match(levels, plot_meso$model),] 

make_meso_capstone_only<-function(plot_meso,xmin,xmax,legend){
  if(legend==T){
    wave1_meso<-dwplot(plot_meso,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, space="free")+
      geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                        panel.grid.major = element_blank(),
                                                                        legend.position = "bottom",
                                                                        axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                        panel.grid.minor = element_blank(),
                                                                        panel.background = element_blank(),
                                                                        axis.line = element_line(colour = "black"),
                                                                        plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC"))+scale_shape_manual(values=c(25,24, 15))
    
  }else{
    wave1_meso<-dwplot(plot_meso,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter, space="free")+
      geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                        panel.grid.major = element_blank(),
                                                                        legend.position = "none",
                                                                        axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                        panel.grid.minor = element_blank(),
                                                                        panel.background = element_blank(),
                                                                        axis.line = element_line(colour = "black"),
                                                                        plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC"))+scale_shape_manual(values=c(25,24,15))
    
  }
   
return(wave1_meso)
}
w1_meso_nc_legend<-make_meso_capstone_only(plot_meso,xmin=-2,xmax=3,legend=T)

# w1_meso_nc_legend<-make_meso_capstone_only(plot_meso_wave1,xmin=-2,xmax=3,legend=T)
w1_meso_nc<-make_meso_capstone_only(plot_meso_wave1,xmin=-2,xmax=3,legend=F)
w1_meso_c<-make_meso_capstone_only(plot_con_meso_wave1,xmin=-2,xmax=3,legend=F)

w2_meso_nc<-make_meso_capstone_only(plot_meso_wave2,xmin=-2,xmax=3,legend=F)
w2_meso_c<-make_meso_capstone_only(plot_con_meso_wave2,xmin=-2,xmax=3,legend=F)

w1on2_meso_nc<-make_meso_capstone_only(plot_meso_wave1on2,xmin=-2,xmax=3,legend=F)
w1on2_meso_c<-make_meso_capstone_only(plot_con_meso_wave1on2,xmin=-2,xmax=3,legend=F)

# Get meso legend
legend_meso <- get_legend(w1_meso_nc_legend)

# Wave 1 meso
w1_meso_nc<-annotate_figure(w1_meso_nc,fig.lab ="Wave 1 without Controls",fig.lab.pos = c("top.left"),fig.lab.face="bold")
w1_meso_c<-annotate_figure(w1_meso_c,fig.lab ="Wave 1 with Controls",fig.lab.pos = c("top.left"),fig.lab.face="bold")

w1_meso<-plot_grid(w1_meso_nc+ theme(legend.position="none"),w1_meso_c+ theme(legend.position="none"),ncol=1)
w1_meso<-plot_grid(w1_meso, legend_meso ,ncol = 1,rel_heights = c(2, .1), axis="r")

# annotate_figure(w1_meso, bottom = text_grob("\n Whiskers denote 90% confidence intervals.", color = "black",
#                                         hjust = 1, x = 1, face = "italic", size = 9))


# Wave 2 meso
w2_meso_nc<-annotate_figure(w2_meso_nc,fig.lab ="Wave 2 without Controls",fig.lab.pos = c("top.left"),fig.lab.face="bold")
w2_meso_c<-annotate_figure(w2_meso_c,fig.lab ="Wave 2 with Controls",fig.lab.pos = c("top.left"),fig.lab.face="bold")

w2_meso<-plot_grid(w2_meso_nc+ theme(legend.position="none"),w2_meso_c+ theme(legend.position="none"),ncol=1)
w2_meso<-plot_grid(w2_meso, legend_meso ,ncol = 1,rel_heights = c(2, .1), axis="r")

# annotate_figure(w2_meso, bottom = text_grob("\n Whiskers denote 90% confidence intervals.", color = "black",
#                                            hjust = 1, x = 1, face = "italic", size = 9))

# Wave 1 on 2 meso
w1on2_meso_nc<-annotate_figure(w1on2_meso_nc,fig.lab ="Wave 1 Returned without Controls",fig.lab.pos = c("top.left"),fig.lab.face="bold")
w1on2_meso_c<-annotate_figure(w1on2_meso_c,fig.lab ="Wave 1 Returned with Controls",fig.lab.pos = c("top.left"),fig.lab.face="bold")

w1on2_meso<-plot_grid(w1on2_meso_nc+ theme(legend.position="none"),w1on2_meso_c+ theme(legend.position="none"),ncol=1)
w1on2_meso<-plot_grid(w1on2_meso, legend_meso ,ncol = 1,rel_heights = c(2, .1), axis="r")

annotate_figure(w1on2_meso, bottom = text_grob("\n Whiskers denote 90% confidence intervals.", color = "black",
                                            hjust = 1, x = 1, face = "italic", size = 9))

### Making Large Coefficients Figures (Multinomial)-------------------------------

# Function to clean the coefficients to prep for the figure

# Tidy the micro level data (Function)
tidy_multinom_micro<-function(health_multi_micro,econ_multi_micro,symbo_multi_micro,health_multi_micro_inter,econ_multi_micro_inter,symbo_multi_micro_inter,wave,is.control){
  health_multi_micro <- tidy(health_multi_micro) %>% full_join(data.frame(term=tidy(health_multi_micro_inter)$term,y.level=tidy(health_multi_micro_inter)$y.level),
                                                                           by = c("y.level", "term")) %>% full_join(data.frame(term=tidy(econ_multi_micro_inter)$term,
                                                                                                                               y.level=tidy(econ_multi_micro_inter)$y.level),by = c("y.level", "term")) %>% full_join(data.frame(term=tidy(symbo_multi_micro_inter)$term,y.level=tidy(symbo_multi_micro_inter)$y.level),by = c("y.level", "term"))%>% mutate(model = "Health Risk")%>%distinct()
  econ_multi_micro <- tidy(econ_multi_micro)%>% full_join(data.frame(term=tidy(health_multi_micro_inter)$term,y.level=tidy(health_multi_micro_inter)$y.level),
                                                                      by = c("y.level", "term")) %>% full_join(data.frame(term=tidy(econ_multi_micro_inter)$term,
                                                                                                                          y.level=tidy(econ_multi_micro_inter)$y.level),by = c("y.level", "term")) %>% full_join(data.frame(term=tidy(symbo_multi_micro_inter)$term,y.level=tidy(symbo_multi_micro_inter)$y.level),by = c("y.level", "term")) %>% mutate(model = "Economic")%>%distinct()
  symbo_multi_micro  <- tidy(symbo_multi_micro ) %>% full_join(data.frame(term=tidy(health_multi_micro_inter)$term,y.level=tidy(health_multi_micro_inter)$y.level),
                                                                           by = c("y.level", "term")) %>% full_join(data.frame(term=tidy(econ_multi_micro_inter)$term,
                                                                                                                               y.level=tidy(econ_multi_micro_inter)$y.level),by = c("y.level", "term")) %>% full_join(data.frame(term=tidy(symbo_multi_micro_inter)$term,y.level=tidy(symbo_multi_micro_inter)$y.level),by = c("y.level", "term")) %>% mutate(model = "Partisanship")%>%distinct()
  
  if(wave==1){
    micro_plot<-rbind(rbind(health_multi_micro,econ_multi_micro),symbo_multi_micro) %>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                                               "COVID_risk_wave1Medium Risk" = "Medium Health Risk",
                                                                                                                               "country_alter_wave1China:COVID_risk_wave1Medium Risk"="Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                                               "COVID_risk_wave1High Risk"="High Health Risk",
                                                                                                                               "country_alter_wave1China:COVID_risk_wave1High Risk" = "Chinese-born Recipient \u00D7 High Risk",
                                                                                                                               "empl_change_wave11" = "Adverse Employment Change",          
                                                                                                                               "country_alter_wave1China:empl_change_wave11" = "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                                               "is_republicanRepublican"="Republican Partisan",
                                                                                                                               "country_alter_wave1China:is_republicanRepublican"= "Chinese-born Recipient \u00D7 Republican Partisan"))%>% mutate(inter = "Without Interactions")
    
    micro_plot_inter<-rbind(rbind(tidy(health_multi_micro_inter) %>% mutate(model = "Health Risk"),tidy(econ_multi_micro_inter)%>% mutate(model = "Economic")),tidy(symbo_multi_micro_inter)%>% mutate(model = "Partisanship"))%>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                                    "COVID_risk_wave1Medium Risk" = "Medium Health Risk",
                                                                                                                                                                                                                                                                                    "country_alter_wave1China:COVID_risk_wave1Medium Risk"="Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                                                                                                                                                                                                    "COVID_risk_wave1High Risk"="High Health Risk",
                                                                                                                                                                                                                                                                                    "country_alter_wave1China:COVID_risk_wave1High Risk" = "Chinese-born Recipient \u00D7 High Risk",
                                                                                                                                                                                                                                                                                    "empl_change_wave11" = "Adverse Employment Change",          
                                                                                                                                                                                                                                                                                    "country_alter_wave1China:empl_change_wave11" = "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                                                                                                                                                                                                    "is_republicanRepublican"="Republican Partisan",
                                                                                                                                                                                                                                                                                    "country_alter_wave1China:is_republicanRepublican"= "Chinese-born Recipient \u00D7 Republican Partisan"))%>% mutate(inter = "With Interactions")}
  if(wave==2){
    
    micro_plot<-rbind(rbind(health_multi_micro,econ_multi_micro),symbo_multi_micro) %>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                               "COVID_risk_wave2Medium Risk" = "Medium Health Risk",
                                                                                                                               "country_alter_wave2China:COVID_risk_wave2Medium Risk"="Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                                               "COVID_risk_wave2High Risk"="High Health Risk",
                                                                                                                               "country_alter_wave2China:COVID_risk_wave2High Risk" = "Chinese-born Recipient \u00D7 High Risk",
                                                                                                                               "empl_change_wave21" = "Adverse Employment Change",          
                                                                                                                               "country_alter_wave2China:empl_change_wave21" = "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                                               "is_republicanOther"="Other Partisan",
                                                                                                                               "country_alter_wave2China:is_republicanOther"= "Chinese-born Recipient \u00D7 Other Partisan",
                                                                                                                               "is_republicanRepublican"="Republican Partisan",
                                                                                                                               "country_alter_wave2China:is_republicanRepublican"= "Chinese-born Recipient \u00D7 Republican Partisan"))%>% mutate(inter = "Without Interactions")
    
    micro_plot_inter<-rbind(rbind(tidy(health_multi_micro_inter) %>% mutate(model = "Health Risk"),tidy(econ_multi_micro_inter)%>% mutate(model = "Economic")),tidy(symbo_multi_micro_inter)%>% mutate(model = "Partisanship"))%>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                                    "COVID_risk_wave2Medium Risk" = "Medium Health Risk",
                                                                                                                                                                                                                                                                                    "country_alter_wave2China:COVID_risk_wave2Medium Risk"="Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                                                                                                                                                                                                    "COVID_risk_wave2High Risk"="High Health Risk",
                                                                                                                                                                                                                                                                                    "country_alter_wave2China:COVID_risk_wave2High Risk" = "Chinese-born Recipient \u00D7 High Risk",
                                                                                                                                                                                                                                                                                    "empl_change_wave21" = "Adverse Employment Change",          
                                                                                                                                                                                                                                                                                    "country_alter_wave2China:empl_change_wave21" = "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                                                                                                                                                                                                    "is_republicanRepublican"="Republican Partisan",
                                                                                                                                                                                                                                                                                    "country_alter_wave2China:is_republicanRepublican"= "Chinese-born Recipient \u00D7 Republican Partisan"))%>% mutate(inter = "With Interactions")}
  
  
  
  if (is.control == T) {
    plot_micro<-rbind(micro_plot,micro_plot_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "Participant") %>% filter(term %in% c("Chinese-born Recipient",
                                                                                                                                                                                                "Medium Health Risk",
                                                                                                                                                                                                "Chinese-born Recipient \u00D7 Medium Risk",
                                                                                                                                                                                                "High Health Risk",
                                                                                                                                                                                                "Chinese-born Recipient \u00D7 High Risk",
                                                                                                                                                                                                "Adverse Employment Change",
                                                                                                                                                                                                "Chinese-born Recipient \u00D7 Adverse Employment Change",
                                                                                                                                                                                                "Republican Partisan",
                                                                                                                                                                                                "Chinese-born Recipient \u00D7 Republican Partisan"))}
  else {plot_micro<-rbind(micro_plot,micro_plot_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "Participant")}
  
  return(plot_micro)
}

# Tidy the macro level data (Function)
tidy_multinom_macro<-function(health_multi_macro1,health_multi_macro2,econ_multi_macro,symbo_multi_macro,health_multi_macro1_inter,health_multi_macro2_inter,econ_multi_macro_inter,symbo_multi_macro_inter,wave,is.control){
  
  health_multi_macro1 <- tidy(health_multi_macro1) %>% full_join(data.frame(term=tidy(health_multi_macro1_inter)$term,y.level=tidy(health_multi_macro1_inter)$y.level), 
                                                                             by = c("term","y.level"))%>% full_join(data.frame(term=tidy(health_multi_macro2_inter)$term,y.level=tidy(health_multi_macro2_inter)$y.level), 
                                                                                                                    by = c("term","y.level")) %>% full_join(data.frame(term=tidy(econ_multi_macro_inter)$term,y.level=tidy(econ_multi_macro_inter)$y.level), 
                                                                                                                                                            by = c("term","y.level")) %>% full_join(data.frame(term=tidy(symbo_multi_macro_inter)$term,y.level=tidy(symbo_multi_macro_inter)$y.level), 
                                                                                                                                                                                                    by = c("term","y.level"))%>% mutate(model = "County Health (Diagnoses)")%>%distinct()
  health_multi_macro2 <- tidy(health_multi_macro2) %>% full_join(data.frame(term=tidy(health_multi_macro1_inter)$term,y.level=tidy(health_multi_macro1_inter)$y.level), 
                                                                             by = c("term","y.level"))%>% full_join(data.frame(term=tidy(health_multi_macro2_inter)$term,y.level=tidy(health_multi_macro2_inter)$y.level), 
                                                                                                                    by = c("term","y.level")) %>% full_join(data.frame(term=tidy(econ_multi_macro_inter)$term,y.level=tidy(econ_multi_macro_inter)$y.level), 
                                                                                                                                                            by = c("term","y.level")) %>% full_join(data.frame(term=tidy(symbo_multi_macro_inter)$term,y.level=tidy(symbo_multi_macro_inter)$y.level), 
                                                                                                                                                                                                    by = c("term","y.level"))%>% mutate(model = "County Health (Deaths)")%>%distinct()
  econ_multi_macro <- tidy(econ_multi_macro)  %>% full_join(data.frame(term=tidy(health_multi_macro1_inter)$term,y.level=tidy(health_multi_macro1_inter)$y.level), 
                                                                        by = c("term","y.level"))%>% full_join(data.frame(term=tidy(health_multi_macro2_inter)$term,y.level=tidy(health_multi_macro2_inter)$y.level), 
                                                                                                               by = c("term","y.level")) %>% full_join(data.frame(term=tidy(econ_multi_macro_inter)$term,y.level=tidy(econ_multi_macro_inter)$y.level), 
                                                                                                                                                       by = c("term","y.level")) %>% full_join(data.frame(term=tidy(symbo_multi_macro_inter)$term,y.level=tidy(symbo_multi_macro_inter)$y.level), 
                                                                                                                                                                                               by = c("term","y.level"))%>% mutate(model = "Economic")%>%distinct()
  symbo_multi_macro <- tidy(symbo_multi_macro)  %>% full_join(data.frame(term=tidy(health_multi_macro1_inter)$term,y.level=tidy(health_multi_macro1_inter)$y.level), 
                                                                          by = c("term","y.level"))%>% full_join(data.frame(term=tidy(health_multi_macro2_inter)$term,y.level=tidy(health_multi_macro2_inter)$y.level), 
                                                                                                                 by = c("term","y.level")) %>% full_join(data.frame(term=tidy(econ_multi_macro_inter)$term,y.level=tidy(econ_multi_macro_inter)$y.level), 
                                                                                                                                                         by = c("term","y.level")) %>% full_join(data.frame(term=tidy(symbo_multi_macro_inter)$term,y.level=tidy(symbo_multi_macro_inter)$y.level), 
                                                                                                                                                                                                 by = c("term","y.level"))%>% mutate(model = "Partisanship")%>%distinct()
  
  if (wave==1){
    plot_macro<-rbind(rbind(rbind(health_multi_macro1,health_multi_macro2),econ_multi_macro),symbo_multi_macro) %>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                                                                                 "newcase_pc_wave1"="County COVID-19 Diagnoses",
                                                                                                                                                                 "country_alter_wave1China:newcase_pc_wave1" = "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                                                 "newdeaths_pc_wave1" = "County COVID-19 Deaths",
                                                                                                                                                                 "country_alter_wave1China:newdeaths_pc_wave1"="Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                                                 "Pct_liLoss_jun_wave1" = "County Low Income Job Losses",          
                                                                                                                                                                 "country_alter_wave1China:Pct_liLoss_jun_wave1" = "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                                                 "Pct_Vote_Rep_wave1"="County Republican Vote Share",
                                                                                                                                                                 "country_alter_wave1China:Pct_Vote_Rep_wave1"="Chinese-born Recipient \u00D7 County Republican Vote Share"))%>% mutate(inter = "Without Interactions")
    
    plot_macro_inter<-rbind(rbind(rbind(tidy(health_multi_macro1_inter) %>% mutate(model = "County Health (Diagnoses)"),tidy(health_multi_macro2_inter)%>% mutate(model = "County Health (Deaths)")),tidy(econ_multi_macro_inter) %>% mutate(model = "Economic")), tidy(symbo_multi_macro_inter) %>% mutate(model = "Partisanship")) %>% relabel_predictors(c("country_alter_wave1China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                                                                                                                                    "newcase_pc_wave1"="County COVID-19 Diagnoses",
                                                                                                                                                                                                                                                                                                                                                                                    "country_alter_wave1China:newcase_pc_wave1" = "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                                                                                                                                                                                                                                                                    "newdeaths_pc_wave1" = "County COVID-19 Deaths",
                                                                                                                                                                                                                                                                                                                                                                                    "country_alter_wave1China:newdeaths_pc_wave1"="Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                                                                                                                                                                    "Pct_liLoss_jun_wave1" = "County Low Income Job Losses",          
                                                                                                                                                                                                                                                                                                                                                                                    "country_alter_wave1China:Pct_liLoss_jun_wave1" = "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                                                                                                                                                                                                                                                                    "Pct_Vote_Rep_wave1"="County Republican Vote Share",
                                                                                                                                                                                                                                                                                                                                                                                    "country_alter_wave1China:Pct_Vote_Rep_wave1"="Chinese-born Recipient \u00D7 County Republican Vote Share"))%>% mutate(inter = "With Interactions")}
  else{plot_macro<-rbind(rbind(rbind(health_multi_macro1,health_multi_macro2),econ_multi_macro),symbo_multi_macro) %>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                                                                    "newcase_pc_wave2"="County COVID-19 Diagnoses",
                                                                                                                                                                    "country_alter_wave2China:newcase_pc_wave2" = "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                                                    "newdeaths_pc_wave2" = "County COVID-19 Deaths",
                                                                                                                                                                    "country_alter_wave2China:newdeaths_pc_wave2"="Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                                                    "Pct_liLoss_nov_wave2" = "County Low Income Job Losses",          
                                                                                                                                                                    "country_alter_wave2China:Pct_liLoss_nov_wave2" = "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                                                    "Pct_Vote_Rep_wave2"="County Republican Vote Share",
                                                                                                                                                                    "country_alter_wave2China:Pct_Vote_Rep_wave2"="Chinese-born Recipient \u00D7 County Republican Vote Share"))%>% mutate(inter = "Without Interactions")
  
  
  plot_macro_inter<-rbind(rbind(rbind(tidy(health_multi_macro1_inter) %>% mutate(model = "County Health (Diagnoses)"),tidy(health_multi_macro2_inter)%>% mutate(model = "County Health (Deaths)")),tidy(econ_multi_macro_inter) %>% mutate(model = "Economic")), tidy(symbo_multi_macro_inter) %>% mutate(model = "Partisanship")) %>% relabel_predictors(c("country_alter_wave2China" = "Chinese-born Recipient",      
                                                                                                                                                                                                                                                                                                                                                                                  "newcase_pc_wave2"="County COVID-19 Diagnoses",
                                                                                                                                                                                                                                                                                                                                                                                  "country_alter_wave2China:newcase_pc_wave2" = "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                                                                                                                                                                                                                                                                  "newdeaths_pc_wave2" = "County COVID-19 Deaths",
                                                                                                                                                                                                                                                                                                                                                                                  "country_alter_wave2China:newdeaths_pc_wave2"="Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                                                                                                                                                                                                                                                                  "Pct_liLoss_nov_wave2" = "County Low Income Job Losses",          
                                                                                                                                                                                                                                                                                                                                                                                  "country_alter_wave2China:Pct_liLoss_nov_wave2" = "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                                                                                                                                                                                                                                                                  "Pct_Vote_Rep_wave2"="County Republican Vote Share",
                                                                                                                                                                                                                                                                                                                                                                                  "country_alter_wave2China:Pct_Vote_Rep_wave2"="Chinese-born Recipient \u00D7 County Republican Vote Share"))%>% mutate(inter = "With Interactions")}
  if (is.control == TRUE) {
    plot_multi_macro_wave1<-rbind(plot_macro,plot_macro_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "County")%>% filter(term %in% c("Chinese-born Recipient",
                                                                                                                                                                                                      "County COVID-19 Diagnoses",
                                                                                                                                                                                                      "Chinese-born Recipient \u00D7 County COVID-19 Diagnoses",
                                                                                                                                                                                                      "County COVID-19 Deaths",
                                                                                                                                                                                                      "Chinese-born Recipient \u00D7 County COVID-19 Deaths",
                                                                                                                                                                                                      "County Low Income Job Losses",
                                                                                                                                                                                                      "Chinese-born Recipient \u00D7 County Low Income Job Losses",
                                                                                                                                                                                                      "County Republican Vote Share",
                                                                                                                                                                                                      "Chinese-born Recipient \u00D7 County Republican Vote Share"))}
  else{plot_multi_macro_wave1<-rbind(plot_macro,plot_macro_inter) %>% mutate(inter = factor(inter,level = c("Without Interactions","With Interactions")))%>% mutate(level = "County")}
  return(plot_multi_macro_wave1)
}

# Actually cleaning :) Micro and Macro levels only
plot_multi_micro_wave1<-tidy_multinom_micro(health_multi_wave1_micro,econ_multi_wave1_micro,symbo_multi_wave1_micro,health_multi_wave1_micro_inter,econ_multi_wave1_micro_inter,symbo_multi_wave1_micro_inter,wave=1,is.control=F)
plot_multi_con_micro_wave1<-tidy_multinom_micro(health_multi_con_wave1_micro,econ_multi_con_wave1_micro,symbo_multi_con_wave1_micro,health_multi_con_wave1_micro_inter,econ_multi_con_wave1_micro_inter,symbo_multi_con_wave1_micro_inter,wave=1,is.control=T)

plot_multi_micro_wave2<-tidy_multinom_micro(health_multi_wave2_micro,econ_multi_wave2_micro,symbo_multi_wave2_micro,health_multi_wave2_micro_inter,econ_multi_wave2_micro_inter,symbo_multi_wave2_micro_inter,wave=2,is.control=F)
plot_multi_con_micro_wave2<-tidy_multinom_micro(health_multi_con_wave2_micro,econ_multi_con_wave2_micro,symbo_multi_con_wave2_micro,health_multi_con_wave2_micro_inter,econ_multi_con_wave2_micro_inter,symbo_multi_con_wave2_micro_inter,wave=2,is.control=T)


plot_multi_macro_wave1<-tidy_multinom_macro(health_multi_wave1_macro1,health_multi_wave1_macro2,econ_multi_wave1_macro,symbo_multi_wave1_macro,health_multi_wave1_macro1_inter,health_multi_wave1_macro2_inter,econ_multi_wave1_macro_inter,symbo_multi_wave1_macro_inter,wave=1,is.control=F)
plot_multi_con_macro_wave1<-tidy_multinom_macro(health_multi_con_wave1_macro1,health_multi_con_wave1_macro2,econ_multi_con_wave1_macro,symbo_multi_con_wave1_macro,health_multi_con_wave1_macro1_inter,health_multi_con_wave1_macro2_inter,econ_multi_con_wave1_macro_inter,symbo_multi_con_wave1_macro_inter,wave=1,is.control=T)

plot_multi_macro_wave2<-tidy_multinom_macro(health_multi_wave2_macro1,health_multi_wave2_macro2,econ_multi_wave2_macro,symbo_multi_wave2_macro,health_multi_wave2_macro1_inter,health_multi_wave2_macro2_inter,econ_multi_wave2_macro_inter,symbo_multi_wave2_macro_inter,wave=2,is.control=F)
plot_multi_con_macro_wave2<-tidy_multinom_macro(health_multi_con_wave2_macro1,health_multi_con_wave2_macro2,econ_multi_con_wave2_macro,symbo_multi_con_wave2_macro,health_multi_con_wave2_macro1_inter,health_multi_con_wave2_macro2_inter,econ_multi_con_wave2_macro_inter,symbo_multi_con_wave2_macro_inter,wave=2,is.control=T)

# Double check
# multinom_wave1 <- multinom(dg_category_wave1 ~ country_alter_wave1, data = waves_us_rev)

# multinom_wave1_con <- multinom(dg_category_wave1 ~ country_alter_wave1
#                            +gender_id+race_id+age_binned+educ_binned+income_binned
#                           +Pct_nHAsian_wave1+MHI_adj_wave1+PopDensity_sqkm_wave1+Pct_sameH_1yr_wave1, data = waves_us_rev)

# summary(multinom_wave1)
# summary(multinom_wave1_con)

# Plotting the figures (Function)
levels <- c("Health Risk","Economic","Partisanship")
plot_multi_micro<-plot_multi_micro_wave1
plot_multi_micro$model <- factor(plot_multi_micro$model, levels =levels)
plot_multi_micro <-plot_multi_micro[match(levels, plot_multi_micro$model),] 

make_multi_micro<-function(plot, xmin, xmax, legend.b){
  if (legend.b==T){

  wave1_micro<-dwplot(ci = .90, plot,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter+y.level)+
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                      legend.position = "bottom",
                                                                      panel.grid.major = element_blank(),
                                                                      axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank(),
                                                                      axis.line = element_line(colour = "black"),
                                                                      plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","Health Risk"="#A730DC"))+scale_shape_manual(values=c("Health Risk"=17,"Partisanship" = 16,"Economic"=15))
  
  }else{
  wave1_micro<-dwplot(ci = .90, plot,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter+y.level)+
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                      legend.position="none",
                                                                      panel.grid.major = element_blank(),
                                                                      axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank(),
                                                                      axis.line = element_line(colour = "black"),
                                                                      plot.margin = unit(c(0, 0, 0, 0), "cm"))+xlim(xmin, xmax)+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","Health Risk"="#A730DC"))+scale_shape_manual(values=c("Health Risk"=17,"Partisanship" = 16,"Economic"=15))
  }  
  return(wave1_micro)
}
make_multi_macro<-function(plot, xmin, xmax, legend.b){
  if (legend.b==T){
  wave1_macro<-dwplot(ci = .90, plot,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter+y.level)+
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                      legend.position = "bottom",
                                                                      panel.grid.major = element_blank(),
                                                                      axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank(),
                                                                      axis.line = element_line(colour = "black"),
                                                                      plot.margin = unit(c(0, 0, 0, 0), "cm"))+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","dotted"="#FF2C79"))+scale_shape_manual(values=c(24, 25,15, 16))+xlim(xmin, xmax)
  }else{
    wave1_macro<-dwplot(ci = .90, plot,dot_args = list(aes(shape = model)))+facet_grid(factor(level,level = c("Participant","Meso","County"))~inter+y.level)+
      geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+theme(legend.title = element_blank(),
                                                                        legend.position = "none",
                                                                        panel.grid.major = element_blank(),
                                                                        axis.text.x=element_text(hjust=0.5,vjust=0.5),
                                                                        panel.grid.minor = element_blank(),
                                                                        panel.background = element_blank(),
                                                                        axis.line = element_line(colour = "black"),
                                                                        plot.margin = unit(c(0, 0, 0, 0), "cm"))+scale_color_manual(values = c("Partisanship" = "#1DBEB9", "Economic" = "#FF2C79","County Health (Deaths)"="#A730DC", "County Health (Diagnoses)"="#A730DC","dotted"="#FF2C79"))+scale_shape_manual(values=c(24, 25,15, 16))+xlim(xmin, xmax)
    
    
  }
  return(wave1_macro)
}

# Actually plotting :D

# Without controls in capstone [Not for Paper]
# multi_indiv_legend<-get_legend(make_multi_micro(plot_multi_micro,xmin=-2, xmax=4.5,legend.b=T))
# multi_micro_wave1<-make_multi_micro(plot_multi_micro_wave1,xmin=-2, xmax=4.5,legend.b=F)
# multi_micro_wave2<-make_multi_micro(plot_multi_micro_wave2,xmin=-2, xmax=4.5,legend.b=F)

# Print out micro level multinom figure
# Get the legend to share
multi_micro_wave1_con<-make_multi_micro(plot_multi_con_micro_wave1,legend.b=T,xmin=-3, xmax=3)
legend <- get_legend(multi_micro_wave1_con)

# Print out the figures individually
multi_micro_wave1_con<-make_multi_micro(plot_multi_con_micro_wave1,legend.b=F,xmin=-3, xmax=3)
multi_micro_wave2_con<-make_multi_micro(plot_multi_con_micro_wave2,legend.b=F,xmin=-3, xmax=3)

# Label the figures by waves
multi_micro_wave1<-annotate_figure(multi_micro_wave1_con,fig.lab ="A Wave 1",fig.lab.pos = c("top.left"),fig.lab.face="bold")
multi_micro_wave2<-annotate_figure(multi_micro_wave2_con,fig.lab ="B Wave 2",fig.lab.pos = c("top.left"),fig.lab.face="bold")

# Position the figures together
multi_micro<-plot_grid(multi_micro_wave1+theme(legend.position="none"), multi_micro_wave2+ theme(legend.position="none"), ncol=1,align=c("v"))

# Add legend and annotations
multi_micro<-plot_grid(multi_micro, legend ,ncol = 1,rel_heights = c(2, .1), axis="hv")
annotate_figure(multi_micro, bottom = text_grob("\n Whiskers denote 90% confidence intervals. \n Controls include age, gender, race/ethnicity, educational attainment, and family income on the participant-level and \n % Asian, median household income, population density, and % residents in same house >= 1 year on the county-level. \n Data is unweighted.", color = "black",
                                                hjust = 1, x = 1, face = "italic", size = 10))

# Print out macro level multinom figure
# First get the legend to share
multi_macro_wave1_con<-make_multi_macro(plot_multi_con_macro_wave1,legend.b=T,xmin=-8, xmax=15)
legend <- get_legend(multi_macro_wave1_con)

# Print out the figures individually
multi_macro_wave2_con<-make_multi_macro(plot_multi_con_macro_wave2,legend.b=F,xmin=-8, xmax=15)
multi_macro_wave1_con<-make_multi_macro(plot_multi_con_macro_wave1,legend.b=F,xmin=-8, xmax=15)

# Label the figures by waves
multi_macro_wave1<-annotate_figure(multi_macro_wave1_con,fig.lab ="A Wave 1",fig.lab.pos = c("top.left"),fig.lab.face="bold")
multi_macro_wave2<-annotate_figure(multi_macro_wave2_con,fig.lab ="B Wave 2",fig.lab.pos = c("top.left"),fig.lab.face="bold")

# Position the figures together
multi_macro<-plot_grid(multi_macro_wave1+theme(legend.position="none"), multi_macro_wave2+ theme(legend.position="none"), ncol=1,align=c("v"))

# Add legend and annotations
multi_macro<-plot_grid(multi_macro, legend ,ncol = 1,rel_heights = c(2, .1), axis="hv")
annotate_figure(multi_macro, bottom = text_grob("\n Whiskers denote 90% confidence intervals. \n Controls include age, gender, race/ethnicity, educational attainment, and family income on the participant-level and \n % Asian, median household income, population density, and % residents in same house >= 1 year on the county-level. \n Data is unweighted.", color = "black",
                                                hjust = 1, x = 1, face = "italic", size = 10))
