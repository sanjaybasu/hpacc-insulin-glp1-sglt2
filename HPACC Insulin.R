# Estimating the Impact of SGLT-2 Inhibitors and GLP1-Receptor Agonists on Insulin Use and Associated Quality of Life in Low and Middle-Income Countries: An Analysis Using the HPACC Data


setwd("~/Downloads")
library(readr)
library(tidyverse)
library(mice)
library(tableone)
library(survey)
# install.packages("devtools")
# devtools::install_github("boyercb/globorisk")
library(globorisk)

# Load the data
hpaccpart1 <- read_csv("HPACC_Maindata_Pt1_2023-06-30.csv")
hpaccpart2 <- read_csv("HPACC_Maindata_Pt2_2023-06-30.csv")


# Combine the data for person id, country, year, svy, psu, stratum, age, sex, weight, insulin, creatinine, and creatinine clearance, and remove missing values
hpaccpart1_sub <- hpaccpart1 %>% select(country, year, age, sex, wt, hbg, dia_med, insulin, insulin_new, fbg, hba1c_p, bg_med, ucrt, sbp_avg, csmoke, tchol_mgdl) 
hpaccpart2_sub <- hpaccpart2 %>% select(country, year, age, sex, wt, hbg, dia_med, insulin, insulin_new, fbg, hba1c_p, bg_med, ucrt, sbp_avg, csmoke, tchol_mgdl) 
hpacc <- rbind(hpaccpart1_sub, hpaccpart2_sub) %>% distinct()

#export hpacc to csv
#write.csv(hpacc, "hpacc.csv")


# for each year, replace '2005-2006' with 2005.5, '2007/8' with 2007.5, '2009-10' with 2009.5, '2009-11' with 2010, and '2015-2016' with 2016.5
hpacc <- hpacc %>% mutate(year = ifelse(year == "2005-2006", 2005.5, year))
hpacc <- hpacc %>% mutate(year = ifelse(year == "2007/8", 2007.5, year))
hpacc <- hpacc %>% mutate(year = ifelse(year == "2009-10", 2009.5, year))
hpacc <- hpacc %>% mutate(year = ifelse(year == "2009-11", 2010, year))
hpacc <- hpacc %>% mutate(year = ifelse(year == "2015-2016", 2016.5, year))

# convert year to numeric
hpacc <- hpacc %>% mutate(year = as.numeric(year))

# take the survey from the latest year in each country
hpacc <- hpacc %>% group_by(country) %>% filter(year == max(year))

# create a new unique id for each person
hpacc <- hpacc %>% mutate(id = row_number())

# make any values of wt > 200 into NA
hpacc <- hpacc %>% mutate(wt = ifelse(wt > 200, NA, wt))

# make any values of hba1c_p > 20 into NA
hpacc <- hpacc %>% mutate(hba1c_p = ifelse(hba1c_p > 20, NA, hba1c_p))

# recalculate age by removing values >100
hpacc <- hpacc %>% mutate(age = ifelse(age > 100, NA, age))

# make any values of ucrt > 20000 into NA
hpacc <- hpacc %>% mutate(ucrt = ifelse(ucrt > 20000, NA, ucrt))

# Determine who has diabetes: those who hbg = 1 or hbg = "Yes" or bg_med = 1 or bg_med = "Yes" or hba1c_p > 6.5 and <20 or dia_med = 1 or dia_med = "Yes" or fbg > 126
hpacc <- hpacc %>% mutate(diabetes = ifelse(fbg>126 | hbg == "1" | hbg == "Yes" | dia_med == "1" | dia_med == "Yes" | bg_med=="1" | bg_med=="Yes" | (insulin == "1" | insulin == "Yes" | insulin_new == "1" | insulin_new == "Yes") | (hba1c_p > 6.5 & hba1c_p < 20), 1, 0))

# Determine who is on insulin: those who have diabetes and insulin = 1 or insulin = "Yes" or insulin_new = 1 or insulin_new = "Yes"
hpacc <- hpacc %>% mutate(insulin = ifelse(diabetes == 1 & (insulin == 1 | insulin == "Yes" | insulin_new == 1 | insulin_new == "Yes"), 1, 0))

# estimate insulin dose based on body weight
hpacc <- hpacc %>% mutate(insulin_dose = ifelse(insulin == 1, wt * 0.64, 0))

# estimate low and high insulin dose confidence intervals based on IQR
hpacc <- hpacc %>% mutate(insulin_dose_low = ifelse(insulin == 1, wt * 0.37, 0))
hpacc <- hpacc %>% mutate(insulin_dose_high = ifelse(insulin == 1, wt * 0.84, 0))

# calculate the percent of the population with diabetes and insulin use in each country
hpacc <- hpacc %>% group_by(country) %>% mutate(percent_diabetes = 100* sum(diabetes, na.rm = T)/n())
hpacc <- hpacc %>% group_by(country) %>% mutate(percent_insulin = 100* sum(insulin, na.rm = T)/n())

# create a new database that only includes people with diabetes
hpacc_diabetes <- hpacc %>% filter(diabetes == 1)

# create a new database that only includes people with diabetes and insulin use
hpacc_diabetes_insulin <- hpacc_diabetes %>% filter(insulin == 1)

# subset the hpacc_diabetes_insulin data to only include those where the "country" has >100 individual rows
# hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% group_by(country) %>% filter(n() > 100)

# make a Table 1 of the diabetes data: using the tableone package to show the distribution of variables, and also show the values of each variable across all countries
table1 <- CreateTableOne(vars = c("age","sex","wt","insulin_dose","insulin_dose_low","insulin_dose_high", "percent_insulin"), data = hpacc_diabetes_insulin, strata = "country", factorVars = c("sex"), test = F, addOverall = T)

tab1Mat <- print(table1, nonnormal = c("age","wt","insulin_dose","insulin_dose_low","insulin_dose_high"))
## Save to a CSV file
write.csv(tab1Mat, file = "tab1.csv")

# Estimate the impact of GLP-1RAs on insulin use and associated health outcomes in low and middle-income countries
# Reduce insulin per person per day in GLP1 scenario based on sustain data
# Simulate: 17% decrease (95% CI: 14%, 19%)

hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_dose_new_glp = insulin_dose * 0.83)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_dose_new_glp_low = insulin_dose_low * 0.81)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_dose_new_glp_high = insulin_dose_high * 0.86)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(delta_insulin_dose_glp = insulin_dose - insulin_dose_new_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(delta_insulin_dose_glp_low = insulin_dose_low - insulin_dose_new_glp_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(delta_insulin_dose_glp_high = insulin_dose_high - insulin_dose_new_glp_high)


# make a Table 2 of the resulting estimates: using the tableone package to show the distribution of variables
table2 <- CreateTableOne(vars = c("insulin_dose_new_glp","insulin_dose_new_glp_low","insulin_dose_new_glp_high",
                                  "delta_insulin_dose_glp","delta_insulin_dose_glp_low","delta_insulin_dose_glp_high"), 
                         data = hpacc_diabetes_insulin, strata = "country", test = F, addOverall = T)

tab2Mat <- print(table2, nonnormal = c("insulin_dose_new_glp","insulin_dose_new_glp_low","insulin_dose_new_glp_high",
                                       "delta_insulin_dose_glp","delta_insulin_dose_glp_low","delta_insulin_dose_glp_high"))


## Save to a CSV file
write.csv(tab2Mat, file = "tab2.csv")

# Estimate the impact of SGLT-2 inhibitors on insulin use and associated health outcomes in low and middle-income countries
# reduce insulin use in SGLT2 scenario based on EMPA-REG BASALTM data
# Simulate: 11% decrease (95% CI: 6%, 16%)


hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_dose_new_sglt = insulin_dose * 0.89)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_dose_new_sglt_low = insulin_dose_low * 0.84)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_dose_new_sglt_high = insulin_dose_high * 0.94)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(delta_insulin_dose_sglt = insulin_dose - insulin_dose_new_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(delta_insulin_dose_sglt_low = insulin_dose_low - insulin_dose_new_sglt_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(delta_insulin_dose_sglt_high = insulin_dose_high - insulin_dose_new_sglt_high)


# make a Table 3 of the resulting estimates: using the tableone package to show the distribution of variables
table3 <- CreateTableOne(vars = c("insulin_dose_new_sglt","insulin_dose_new_sglt_low","insulin_dose_new_sglt_high",
                                  "delta_insulin_dose_sglt","delta_insulin_dose_sglt_low","delta_insulin_dose_sglt_high"), 
                         data = hpacc_diabetes_insulin, strata = "country", test = F, addOverall = T)

tab3Mat <- print(table3, nonnormal = c("insulin_dose_new_sglt","insulin_dose_new_sglt_low","insulin_dose_new_sglt_high",
                                       "delta_insulin_dose_sglt","delta_insulin_dose_sglt_low","delta_insulin_dose_sglt_high"))

## Save to a CSV file
write.csv(tab3Mat, file = "tab3.csv")


# reduced rate of severe hypoglycemia due to lower insulin use
# Adjusted for insulin type, sliding-scale insulin use, and albumin, creatinine, and hematocrit levels, the higher odds of hypoglycemia with increasing insulin doses remained (0.6–0.8 units/kg: odds ratio 2.10 [95% CI 1.08–4.09], P = 0.028; >0.8 units/kg: 2.95 [1.54–5.65], P = 0.001). The adjusted odds of hypoglycemia were not greater in patients who received 0.2–0.4 units/kg (1.08 [0.64–1.81], P = 0.78) or 0.4–0.6 units/kg (1.60 [0.90–2.86], P = 0.11). See https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3142056/ 
#  Baseline rate of severe hypoglycemia is 1.9 to 2.5 per 100 patient-years in Type 2 diabetes mellitus (T2DM). https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10241751/

# first calculate the units per kg per person per day in the old and new scenarios, for the variables insulin_dose, insulin_dose_new_glp, and insulin_dose_new_sglt and their associated _low and _high variables
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg = insulin_dose / wt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg_low = insulin_dose_low / wt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg_high = insulin_dose_high / wt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg_new_glp = insulin_dose_new_glp / wt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg_new_glp_low = insulin_dose_new_glp_low / wt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg_new_glp_high = insulin_dose_new_glp_high / wt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg_new_sglt = insulin_dose_new_sglt / wt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg_new_sglt_low = insulin_dose_new_sglt_low / wt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(insulin_units_per_kg_new_sglt_high = insulin_dose_new_sglt_high / wt)

# the reduction in the rate of severe hypoglycemia is 1 per 100 person years for each 0.1 unit/kg/day reduction in insulin dose, so we will calculate the reduction in the rate of severe hypoglycemia for each of the nine scenarios above
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_reduction_new_glp = 1/100 * delta_insulin_dose_glp * 10)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_reduction_new_glp_low = 1/100 * delta_insulin_dose_glp_low * 10)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_reduction_new_glp_high = 1/100 * delta_insulin_dose_glp_high * 10)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_reduction_new_sglt = 1/100 * delta_insulin_dose_sglt * 10)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_reduction_new_sglt_low = 1/100 * delta_insulin_dose_sglt_low * 10)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_reduction_new_sglt_high = 1/100 * delta_insulin_dose_sglt_high * 10)

# next calculate the  rate of severe hypoglycemia over 10 years for each of the nine scenarios above, where hypoglycemia_rate = 2.2 (varied from 1.9 to 2.5)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate = 2.2/100*10 * exp(3.39 * (insulin_units_per_kg) - 1.632))
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_low = 1.9/100*10 * exp(3.39 * (insulin_units_per_kg_low) - 1.632))
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_high = 2.5/100*10 * exp(3.39 * (insulin_units_per_kg_high) - 1.632))
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_new_glp = hypoglycemia_rate - hypoglycemia_rate_reduction_new_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_new_glp_low = hypoglycemia_rate_low - hypoglycemia_rate_reduction_new_glp_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_new_glp_high = hypoglycemia_rate_high - hypoglycemia_rate_reduction_new_glp_high)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_new_sglt = hypoglycemia_rate - hypoglycemia_rate_reduction_new_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_new_sglt_low = hypoglycemia_rate_low - hypoglycemia_rate_reduction_new_sglt_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(hypoglycemia_rate_new_sglt_high = hypoglycemia_rate_high - hypoglycemia_rate_reduction_new_sglt_high)

# now calculate the DALYs lost to hypoglycemia for each of the nine scenarios above, where DALYs = 0.1 * hypoglycemia_rate * 10 * 1/((1+0.03)^10)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs = 0.1 * hypoglycemia_rate * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_low = 0.1 * hypoglycemia_rate_low * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_high = 0.1 * hypoglycemia_rate_high * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp = 0.1 * hypoglycemia_rate_new_glp * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_low = 0.1 * hypoglycemia_rate_new_glp_low * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_high = 0.1 * hypoglycemia_rate_new_glp_high * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt = 0.1 * hypoglycemia_rate_new_sglt * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_low = 0.1 * hypoglycemia_rate_new_sglt_low * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_high = 0.1 * hypoglycemia_rate_new_sglt_high * 10 * 1/((1+0.03)^10) * percent_insulin)

# now for the GLP1RA scenarios, we will estimate the rate of new adverse events: GI side effects at a rate of 15-20% having diarrhea among 11% of people, with a disutility/DALY cost of 0.188 and lasting 3-4 days, and the risk of pancreatitis at a rate of 1.2 to 2.1 events per 1000 patient-years with a disutility of 0.324 and lasting 28 to 53 days
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(GI_side_effects_rate = 0.18 * 0.11)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(GI_side_effects_disutility = 0.188 * 3/365.25/10 * 1/((1+0.03)^10) )
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(pancreatitis_rate = 0.016)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(pancreatitis_disutility = 0.324 * 41/365.25/10 * 1/((1+0.03)^10))

# now let's update the DALYs in the glp scenarios to subtract the costs of GI side effects and pancreatitis
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp = DALYs_new_glp + (GI_side_effects_rate * GI_side_effects_disutility + pancreatitis_rate * pancreatitis_disutility))
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_low = DALYs_new_glp_low + (GI_side_effects_rate * GI_side_effects_disutility + pancreatitis_rate * pancreatitis_disutility))
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_high = DALYs_new_glp_high + (GI_side_effects_rate * GI_side_effects_disutility + pancreatitis_rate * pancreatitis_disutility))

# now for the SGLT2i scenarios, we will estimate the rate of new adverse events: UTIs at a rate of 87.4 per 1000 patient-years for women and 11.9 per 1000 patient-years for men with a disutility of 0.051 and lasting 3-14 days, and the risk of DKA at a rate of 0.6 to 4.9 events per 1000 patient-years with a disutility of 0.1 to 0.2 and lasting 3 to 7 days
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(UTI_rate = 0.0497)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(UTI_disutility = 0.051 * 8.5/365.25/10 * 1/((1+0.03)^10) *  percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DKA_rate = 0.00275)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DKA_disutility = 0.15 * 5/365.25/10 * 1/((1+0.03)^10) *  percent_insulin)

# now let's update the DALYs in the sglt scenarios to subtract the costs of UTIs and DKA
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt = DALYs_new_sglt + (UTI_rate * UTI_disutility + DKA_rate * DKA_disutility))
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_low = DALYs_new_sglt_low + (UTI_rate * UTI_disutility + DKA_rate * DKA_disutility))
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_high = DALYs_new_sglt_high + (UTI_rate * UTI_disutility + DKA_rate * DKA_disutility))

# now we're going to include the benefits of GLP1-RAs and SGLT2 inhibitors on cardiovascular outcomes
# first we calculate the CVD risk for each person in the dataset. then for GLP1-RA we reduce the risk by 18% and for SGLT2-i by 15%
# use the globorisk calculator to estimate the 10-year risk of CVD for each person in the dataset
# https://www.globorisk.org/
# first, make the iso codes for each country, Afghanistan - AFG
# Algeria - DZA
# Azerbaijan - AZE
# Bangladesh - BGD
# Belarus - BLR
# Benin - BEN
# Bhutan - BTN
# Botswana - BWA
# Brazil - BRA
# Burkina Faso - BFA
# Cabo Verde - CPV
# Cabo Verde 2020 - CPV 
# Cambodia - KHM
# Chile - CHL
# China - CHN
# Comoros - COM
# Costa Rica - CRI
# Ecuador - ECU
# Egypt - EGY
# Eritrea - ERI
# Ethiopia - ETH
# Fiji - FJI
# Gambia - GMB
# Georgia - GEO
# Ghana - GHA
# Grenada - GRD 
# Guyana - GUY
# Indonesia - IDN
# Iran - IRN
# Iraq - IRQ
# Jordan - JOR
# Kazakhstan - KAZ
# Kenya - KEN
# Kiribati - KIR
# Kyrgyzstan - KGZ
# Laos - LAO
# Lebanon - LBN
# Lesotho - LSO
# Liberia - LBR
# Libya - LBY
# Malawi - MWI
# Marshall Islands - MHL  
# Mexico - MEX
# Moldova - MDA
# Mongolia 2019 - MNG
# Morocco - MAR
# Myanmar - MMR
# Namibia - NAM
# Nauru - NRU
# Nepal - NPL
# Niger - NER
# Peru - PER
# Romania - ROU
# Russian Federation - RUS 
# Rwanda - RWA
# Samoa - WSM
# Sao Tome and Principe - STP 
# Seychelles - SYC
# Sierra Leone - SLE
# Solomon Islands - SLB
# South Africa - ZAF
# South Africa DHS - ZAF  
# Sri Lanka - LKA
# Sudan - SDN
# Swaziland - SWZ
# Tajikistan - TJK
# Tanzania - TZA 
# Timor Leste - TLS
# Togo - TGO
# Tokelau - TKL   
# Tonga - TON 
# Turkmenistan - TKM
# Tuvalu - TUV 
# Uganda - UGA
# Ukraine - UKR
# Vanuatu - VUT
# Vietnam - VNM
# Zambia - ZMB
# Zanzibar - TZA


hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(iso = case_when(country == "Afghanistan" ~ "AFG",
                                                                       country == "Algeria" ~ "DZA",
                                                                       country == "Azerbaijan" ~ "AZE",
                                                                       country == "Bangladesh" ~ "BGD",
                                                                       country == "Belarus" ~ "BLR",
                                                                       country == "Benin" ~ "BEN",
                                                                       country == "Bhutan" ~ "BTN",
                                                                       country == "Botswana" ~ "BWA",
                                                                       country == "Brazil" ~ "BRA",
                                                                       country == "Burkina Faso" ~ "BFA",
                                                                       country == "Cabo Verde" ~ "CPV",
                                                                       country == "Cambodia" ~ "KHM",
                                                                       country == "Chile" ~ "CHL",
                                                                       country == "China" ~ "CHN",
                                                                       country == "Comoros" ~ "COM",
                                                                       country == "Costa Rica" ~ "CRI",
                                                                       country == "Ecuador" ~ "ECU",
                                                                       country == "Egypt" ~ "EGY",
                                                                       country == "Eritrea" ~ "ERI",
                                                                       country == "Ethiopia" ~ "ETH",
                                                                       country == "Fiji" ~ "FJI",
                                                                       country == "Gambia" ~ "GMB",
                                                                       country == "Georgia" ~ "GEO",
                                                                       country == "Ghana" ~ "GHA",
                                                                       country == "Grenada" ~ "GRD",
                                                                       country == "Guyana" ~ "GUY",
                                                                       country == "Indonesia" ~ "IDN",
                                                                       country == "Iran" ~ "IRN",
                                                                       country == "Iraq" ~ "IRQ",
                                                                       country == "Jordan" ~ "JOR",
                                                                       country == "Kazakhstan" ~ "KAZ",
                                                                       country == "Kenya" ~ "KEN",
                                                                       country == "Kiribati" ~ "KIR",
                                                                       country == "Kyrgyzstan" ~ "KGZ",
                                                                       country == "Laos" ~ "LAO",
                                                                       country == "Lebanon" ~ "LBN",
                                                                       country == "Lesotho" ~ "LSO",
                                                                       country == "Liberia" ~ "LBR",
                                                                       country == "Libya" ~ "LBY",
                                                                       country == "Malawi" ~ "MWI",
                                                                       country == "Marshall Islands" ~ "VUT",
                                                                       country == "Mexico" ~ "MEX",
                                                                       country == "Moldova" ~ "MDA",
                                                                       country == "Mongolia" ~ "MNG",
                                                                       country == "Mongolia 2019" ~ "MNG",
                                                                       country == "Morocco" ~ "MAR",
                                                                       country == "Myanmar" ~ "MMR",
                                                                       country == "Namibia" ~ "NAM",
                                                                       country == "Nauru" ~ "VUT",
                                                                       country == "Nepal" ~ "NPL",
                                                                       country == "Niger" ~ "NER",
                                                                       country == "Peru" ~ "PER",
                                                                       country == "Romania" ~ "ROU",
                                                                       country == "Russian Federation" ~ "RUS",
                                                                       country == "Rwanda" ~ "RWA",
                                                                       country == "Samoa" ~ "WSM",
                                                                       country == "Sao Tome and Principe" ~ "STP",
                                                                       country == "Seychelles" ~ "SYC",
                                                                       country == "Sierra Leone" ~ "SLE",
                                                                       country == "Solomon Islands" ~ "SLB",
                                                                       country == "South Africa" ~ "ZAF",
                                                                       country == "Sri Lanka" ~ "LKA",
                                                                       country == "Sudan" ~ "SDN",
                                                                       country == "Swaziland" ~ "SWZ",
                                                                       country == "Tajikistan" ~ "TJK",
                                                                       country == "Tanzania" ~ "TZA",
                                                                       country == "Timor Leste" ~ "TLS",
                                                                       country == "Togo" ~ "TGO",
                                                                       country == "Tokelau" ~ "VUT",
                                                                       country == "Tonga" ~ "TON",
                                                                       country == "Turkmenistan" ~ "TKM",
                                                                       country == "Tuvalu" ~ "VUT",
                                                                       country == "Uganda" ~ "UGA",
                                                                       country == "Ukraine" ~ "UKR",
                                                                       country == "Vanuatu" ~ "VUT",
                                                                       country == "Vietnam" ~ "VNM",
                                                                       country == "Zambia" ~ "ZMB",
                                                                       country == "Zanzibar" ~ "TZA",
                                                                       TRUE ~ "NA"))


# now we're going to use the globorisk calculator to estimate the 10-year risk of CVD for each person in the dataset
# # estimate 10-year risk of CVD using laboratory calculator 
# globorisk(
#   sex = 1,
#   age = 52,
#   sbp = 140,
#   tc = 4.5,
#   dm = 1,
#   smk = 0,
#   iso = "ZAF",
#   year = 2020,
#   version = "lab",
#   type = "risk"
# )
# sex = sex, age = age, sbp = sbp_avg, tc = tchol_mgdl / 38.67, dm = 1, smk = csmoke, iso = iso, year = 2020, version = "lab", type = "risk"
# calculate the globorisk for each individual in the dataset given the above
# make all ages > 40 for the purposes of this calc
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(age = ifelse(age < 40, 40, age))
# for csmoke, all 1 and 'Yes' should be treated as 1
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(csmoke = ifelse(csmoke == "1" | csmoke == "Yes", 1, 0))
# for csmoke, values >100 and "No" should be treated as 0
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(csmoke = ifelse(csmoke > 100 | csmoke == "No", 0, csmoke))

# for values of sbp_avg > 300, set to 300
# for values of sbp_avg of NA, set to average
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(sbp_avg = ifelse(is.na(sbp_avg), mean(hpacc_diabetes_insulin$sbp_avg, na.rm = TRUE), sbp_avg))


hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(CVD_risk = globorisk(sex = sex,
                                                                                  age = age,
                                                                                  sbp = sbp_avg,
                                                                                  tc = tchol_mgdl / 38.67,
                                                                                  dm = 1,
                                                                                  smk = csmoke,
                                                                                  iso = iso,
                                                                                  year = 2020,
                                                                                  version = "lab",
                                                                                  type = "risk"))

# replace NA values with average
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(CVD_risk = ifelse(is.na(CVD_risk), mean(hpacc_diabetes_insulin$CVD_risk, na.rm = TRUE), CVD_risk))

# now we're going to include the benefits of GLP1-RAs and SGLT2 inhibitors on cardiovascular outcomes, which is 18% for GLP1-RAs (varied from 2% to 52%), and 15% for SGLT2 inhibitors (varied from 7% to 23%) 
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(CVD_risk_new_glp = CVD_risk * 0.82)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(CVD_risk_new_glp_low = CVD_risk * 0.68)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(CVD_risk_new_glp_high = CVD_risk * 0.98)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(CVD_risk_new_sglt = CVD_risk * 0.85)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(CVD_risk_new_sglt_low = CVD_risk * 0.77)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(CVD_risk_new_sglt_high = CVD_risk * 0.93)

# now calculate the DALYs lost to CVD for each of the nine scenarios above, where DALYs = CVD_risk * 0.072 * 10 * 1/((1+0.03)^10), where 0.072 goes to a low of 0.041 to a high of 0.179
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs = DALYs + CVD_risk * 0.072 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_low = DALYs_low + CVD_risk * 0.041 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_high = DALYs_high + CVD_risk * 0.179 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp = DALYs_new_glp + CVD_risk_new_glp * 0.072 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_low = DALYs_new_glp_low + CVD_risk_new_glp * 0.041 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_high = DALYs_new_glp_high + CVD_risk_new_glp * 0.179 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt = DALYs_new_sglt + CVD_risk_new_sglt * 0.072 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_low = DALYs_new_sglt_low + CVD_risk_new_sglt * 0.041 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_high = DALYs_new_sglt_high + CVD_risk_new_sglt * 0.179 * 10 * 1/((1+0.03)^10) * percent_insulin)

# now we're going to include the benefits of GLP1-RAs and SGLT2 inhibitors on renal outcomes, where the annual risk is 4.1 per 1000 person years (4.1 / 100 for a 10-year span), varied from 2.9 to 7.4, with a disutility of 0.338 (varied from 0.104 to 0.571)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(renal_risk = 0.0041)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(renal_risk_low = 0.0029)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(renal_risk_high = 0.0074)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(renal_disutility = 0.338 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(renal_disutility_low = 0.104 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(renal_disutility_high = 0.571 * 10 * 1/((1+0.03)^10) * percent_insulin)

# now let's update the DALYs to add the cost of renal outcomes for all scenarios, where GLP1RA reduces renal disutility by 21% (6% to 34%), and SGLT2i reduces renal disutility by 37% (31% to 42%)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs = DALYs + renal_risk*renal_disutility)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_low = DALYs_low + renal_risk_low*renal_disutility_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_high = DALYs_high + renal_risk_high*renal_disutility_high)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp = DALYs_new_glp + renal_risk*renal_disutility * 0.79)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_low = DALYs_new_glp_low + renal_risk_low*renal_disutility_low * 0.66)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_high = DALYs_new_glp_high + renal_risk_high*renal_disutility_high * 0.93)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt = DALYs_new_sglt + renal_risk*renal_disutility * 0.63)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_low = DALYs_new_sglt_low + renal_risk_low*renal_disutility_low * 0.58)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_high = DALYs_new_sglt_high + renal_risk_high*renal_disutility_high * 0.69)

# now we're going to include the benefits of GLP1-RAs and SGLT2 inhibitors on weight outcomes, where the weight loss on GLP1RA is 3.4 kg, varied from 2.3 to 4.5, and on SLGT2i is 1.8 kg, varied from 1.9 to 1.6, with a disutility of 0.00185 (varied from 0.00012 to 0.00441) per kg
# first calculate the weight loss amount
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_loss_new_glp = 3.4)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_loss_new_glp_low = 2.3)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_loss_new_glp_high = 4.5)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_loss_new_sglt = 1.8)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_loss_new_sglt_low = 1.6)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_loss_new_sglt_high = 1.9)

# now let's calculate the disutility of weight loss for each scenario, separately for GLP1RAs and for SGLT2is
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_disutility_glp = -0.00185 * weight_loss_new_glp * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_disutility_glp_low = -0.00441 * weight_loss_new_glp_low * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_disutility_glp_high = -0.00012 * weight_loss_new_glp_high * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_disutility_sglt = -0.00185 * weight_loss_new_sglt * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_disutility_sglt_low = -0.00441 * weight_loss_new_sglt_low * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(weight_disutility_sglt_high = -0.00012 * weight_loss_new_sglt_high * 10 * 1/((1+0.03)^10) * percent_insulin)

# now let's update the DALYs to add the cost of weight outcomes for all scenarios
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp = DALYs_new_glp + weight_disutility_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_low = DALYs_new_glp_low + weight_disutility_glp_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_high = DALYs_new_glp_high + weight_disutility_glp_high)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt = DALYs_new_sglt + weight_disutility_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_low = DALYs_new_sglt_low + weight_disutility_sglt_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_high = DALYs_new_sglt_high + weight_disutility_sglt_high)

# now we're going to include the benefits of GLP1-RAs and SGLT2 inhibitors on mortality, weight GLP1RA reducing death by 20% (varied from 5% to 33%), and for SGLT2i by 21% (varied from 12% to 30%)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(mortality_reduction_new_glp = 0.20)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(mortality_reduction_new_glp_low = 0.05)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(mortality_reduction_new_glp_high = 0.33)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(mortality_reduction_new_sglt = 0.21)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(mortality_reduction_new_sglt_low = 0.12)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(mortality_reduction_new_sglt_high = 0.30)

# now let's calculate the DALYs lost to mortality for each of the nine scenarios above, where DALYs = DALYs + mortality_reduction * mortality rate per year * 10 * 1/((1+0.03)^10), where mortality rate per 1000 person years = 1.769*e^ (0.0565 * age)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(mortality_rate = 1.769 * exp(0.0565 * age))
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs = DALYs + mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_low = DALYs_low +mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_high = DALYs_high + mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp = DALYs_new_glp + (1-mortality_reduction_new_glp) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_low = DALYs_new_glp_low + (1-mortality_reduction_new_glp_low) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_glp_high = DALYs_new_glp_high + (1-mortality_reduction_new_glp_high) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt = DALYs_new_sglt + (1-mortality_reduction_new_sglt) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_low = DALYs_new_sglt_low + (1-mortality_reduction_new_sglt_low) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_new_sglt_high = DALYs_new_sglt_high + (1-mortality_reduction_new_sglt_high) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)

hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% 
  mutate(
    DALYs = pmax(0, DALYs),
    DALYs_low = pmax(0, DALYs_low),
    DALYs_high = pmax(0, DALYs_high),
    DALYs_new_glp = pmax(0, DALYs_new_glp),
    DALYs_new_glp_low = pmax(0, DALYs_new_glp_low),
    DALYs_new_glp_high = pmax(0, DALYs_new_glp_high),
    DALYs_new_sglt = pmax(0, DALYs_new_sglt),
    DALYs_new_sglt_low = pmax(0, DALYs_new_sglt_low),
    DALYs_new_sglt_high = pmax(0, DALYs_new_sglt_high)
  )


# make a Table 4 of the resulting estimates: using the tableone package to show the distribution of variables
table4 <- CreateTableOne(vars = c("DALYs","DALYs_low","DALYs_high",
                                  "DALYs_new_glp","DALYs_new_glp_low","DALYs_new_glp_high",
                                  "DALYs_new_sglt","DALYs_new_sglt_low","DALYs_new_sglt_high"), 
                         data = hpacc_diabetes_insulin, strata = "country", test = F, addOverall = T)
tab4Mat <- print(table4, nonnormal = c("DALYs","DALYs_low","DALYs_high",
                                       "DALYs_new_glp","DALYs_new_glp_low","DALYs_new_glp_high",
                                       "DALYs_new_sglt","DALYs_new_sglt_low","DALYs_new_sglt_high"))

## Save to a CSV file
write.csv(tab4Mat, file = "tab4.csv")


# calculate what fraction of the difference in DALYs across the different scenarios was due to the DALYs saved from lower hypoglycemia events, versus the DALYs saved from lower CVD events or from lower kidney events or from lower overall mortality
# first calculate the DALY change from lower hypooglycemia in GLP1RA and SGLT2i scenarios
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_hypoglycemia_glp = 0.1 * (hypoglycemia_rate - hypoglycemia_rate_new_glp) * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_hypoglycemia_glp_low = 0.1 * (hypoglycemia_rate_low - hypoglycemia_rate_new_glp_low) * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_hypoglycemia_glp_high = 0.1 * (hypoglycemia_rate_high - hypoglycemia_rate_new_glp_high) * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_hypoglycemia_sglt = 0.1 * (hypoglycemia_rate - hypoglycemia_rate_new_sglt ) * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_hypoglycemia_sglt_low = 0.1 * (hypoglycemia_rate_low - hypoglycemia_rate_new_sglt_low ) * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_hypoglycemia_sglt_high = 0.1 * (hypoglycemia_rate_high - hypoglycemia_rate_new_sglt_high ) * 10 * 1/((1+0.03)^10) * percent_insulin)

# next calculate the DALY change from higher side-effects of GI and pancreatitis for glp1ra scenarios, and higher UTI and DKA in sglt2i scenarios
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_side_effects_glp = -GI_side_effects_rate * GI_side_effects_disutility + pancreatitis_rate * pancreatitis_disutility)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_side_effects_glp_low = -GI_side_effects_rate * GI_side_effects_disutility + pancreatitis_rate * pancreatitis_disutility)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_side_effects_glp_high = -GI_side_effects_rate * GI_side_effects_disutility + pancreatitis_rate * pancreatitis_disutility)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_side_effects_sglt = -UTI_rate * UTI_disutility + DKA_rate * DKA_disutility)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_side_effects_sglt_low = -UTI_rate * UTI_disutility + DKA_rate * DKA_disutility)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_side_effects_sglt_high = -UTI_rate * UTI_disutility + DKA_rate * DKA_disutility)


# next calculate the DALY change from lower CVD events in GLP1RA and SGLT2i scenarios
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_CVD_glp = (CVD_risk - CVD_risk_new_glp )* 0.072 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_CVD_glp_low = (CVD_risk - CVD_risk_new_glp_low )* 0.041 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_CVD_glp_high = (CVD_risk - CVD_risk_new_glp_high )* 0.179 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_CVD_sglt = (CVD_risk - CVD_risk_new_sglt )* 0.072 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_CVD_sglt_low = (CVD_risk - CVD_risk_new_sglt_low )* 0.041 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_CVD_sglt_high = (CVD_risk - CVD_risk_new_sglt_high )* 0.179 * 10 * 1/((1+0.03)^10) * percent_insulin)

# next calculate the DALY change from lower kidney events in GLP1RA and SGLT2i scenarios
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_kidney_glp = (renal_risk - 0.79 * renal_risk)* renal_disutility)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_kidney_glp_low = (renal_risk_low - 0.66 * renal_risk_low)* renal_disutility_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_kidney_glp_high = (renal_risk_high - 0.93 * renal_risk_high)* renal_disutility_high)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_kidney_sglt = (renal_risk - 0.63 * renal_risk)* renal_disutility)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_kidney_sglt_low = (renal_risk_low - 0.69 * renal_risk_low)* renal_disutility_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_kidney_sglt_high = (renal_risk_high - 0.58 * renal_risk_high)* renal_disutility_high)

# next calculate the DALY change from lower weight in GLP1RA and SGLT2i scenarios
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_weight_glp = -weight_disutility_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_weight_glp_low = -weight_disutility_glp_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_weight_glp_high = -weight_disutility_glp_high)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_weight_sglt = -weight_disutility_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_weight_sglt_low = -weight_disutility_sglt_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_weight_sglt_high = -weight_disutility_sglt_high)

# next calculate the DALY change from lower mortality in GLP1RA and SGLT2i scenarios
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_mortality_glp = (1-mortality_reduction_new_glp) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_mortality_glp_low = (1-mortality_reduction_new_glp_low) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_mortality_glp_high = (1-mortality_reduction_new_glp_high) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_mortality_sglt = (1-mortality_reduction_new_sglt) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_mortality_sglt_low = (1-mortality_reduction_new_sglt_low) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_mortality_sglt_high = (1-mortality_reduction_new_sglt_high) * mortality_rate/1000 * 10 * 1/((1+0.03)^10) * percent_insulin)


# sum all the DALY changes across the different causes of changes in DALYs in each of the GLP1RA and SGLT2i scenarios
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp = DALYs_hypoglycemia_glp + DALYs_side_effects_glp + DALYs_CVD_glp + DALYs_kidney_glp + DALYs_weight_glp + DALYs_mortality_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp_low = DALYs_hypoglycemia_glp_low + DALYs_side_effects_glp_low + DALYs_CVD_glp_low + DALYs_kidney_glp_low + DALYs_weight_glp_low + DALYs_mortality_glp_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp_high = DALYs_hypoglycemia_glp_high + DALYs_side_effects_glp_high + DALYs_CVD_glp_high + DALYs_kidney_glp_high + DALYs_weight_glp_high + DALYs_mortality_glp_high)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt = DALYs_hypoglycemia_sglt + DALYs_side_effects_sglt + DALYs_CVD_sglt + DALYs_kidney_sglt + DALYs_weight_sglt + DALYs_mortality_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt_low = DALYs_hypoglycemia_sglt_low + DALYs_side_effects_sglt_low + DALYs_CVD_sglt_low + DALYs_kidney_sglt_low + DALYs_weight_sglt_low + DALYs_mortality_sglt_low)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt_high = DALYs_hypoglycemia_sglt_high + DALYs_side_effects_sglt_high + DALYs_CVD_sglt_high + DALYs_kidney_sglt_high + DALYs_weight_sglt_high + DALYs_mortality_sglt_high)

# now calculate the fractional contribution (positive or negative) of each of the above causes of changes in DALYs in each of the GLP1RA and SGLT2i scenarios, to generate a plot of the results
# ignore the _low and _high and just have the main estimates
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp_frac_hypoglycemia = DALYs_hypoglycemia_glp / DALYs_delta_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp_frac_side_effects = DALYs_side_effects_glp / DALYs_delta_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp_frac_CVD = DALYs_CVD_glp / DALYs_delta_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp_frac_kidney = DALYs_kidney_glp / DALYs_delta_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp_frac_weight = DALYs_weight_glp / DALYs_delta_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_glp_frac_mortality = DALYs_mortality_glp / DALYs_delta_glp)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt_frac_hypoglycemia = DALYs_hypoglycemia_sglt / DALYs_delta_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt_frac_side_effects = DALYs_side_effects_sglt / DALYs_delta_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt_frac_CVD = DALYs_CVD_sglt / DALYs_delta_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt_frac_kidney = DALYs_kidney_sglt / DALYs_delta_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt_frac_weight = DALYs_weight_sglt / DALYs_delta_sglt)
hpacc_diabetes_insulin <- hpacc_diabetes_insulin %>% mutate(DALYs_delta_sglt_frac_mortality = DALYs_mortality_sglt / DALYs_delta_sglt)

# make a Table 5 of the resulting estimates: using the tableone package to show the distribution of variables
table5 <- CreateTableOne(vars = c("DALYs_delta_glp_frac_hypoglycemia","DALYs_delta_glp_frac_side_effects","DALYs_delta_glp_frac_CVD",
                                  "DALYs_delta_glp_frac_kidney","DALYs_delta_glp_frac_weight","DALYs_delta_glp_frac_mortality",
                                  "DALYs_delta_sglt_frac_hypoglycemia","DALYs_delta_sglt_frac_side_effects","DALYs_delta_sglt_frac_CVD",
                                  "DALYs_delta_sglt_frac_kidney","DALYs_delta_sglt_frac_weight","DALYs_delta_sglt_frac_mortality"), 
                         data = hpacc_diabetes_insulin, strata = "country", test = F, addOverall = T)

tab5Mat <- print(table5, nonnormal = c("DALYs_delta_glp_frac_hypoglycemia","DALYs_delta_glp_frac_side_effects","DALYs_delta_glp_frac_CVD",
                                       "DALYs_delta_glp_frac_kidney","DALYs_delta_glp_frac_weight","DALYs_delta_glp_frac_mortality",
                                       "DALYs_delta_sglt_frac_hypoglycemia","DALYs_delta_sglt_frac_side_effects","DALYs_delta_sglt_frac_CVD",
                                       "DALYs_delta_sglt_frac_kidney","DALYs_delta_sglt_frac_weight","DALYs_delta_sglt_frac_mortality"))


# Save to a CSV file
write.csv(tab5Mat, file = "tab5.csv")


