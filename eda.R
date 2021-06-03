library(tidyverse)
library(naniar)

country_data <- read_csv("config_files/FFG_Hackathon_Country_Level_Data.csv") %>% 
  janitor::clean_names()
miss_var_summary(country_data)
miss_case_summary(country_data)

 
  

country_data %>% 
  arrange(desc(en_atm_co2e_pc)) %>% 
  filter(en_atm_co2e_pc >= 21 | country_name=="United States" | country_name=="Luxembourg") %>% 
  group_by(country_name) %>% 
  ggplot(aes(x = year, color = country_name)) +
  geom_line(aes(y = en_atm_co2e_pc)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Year",
       y = "CO2 emissions per capita")


country_data %>% 
  arrange(desc(en_atm_co2e_kt)) %>% 
  filter(en_atm_co2e_kt >= 500000) %>% 
  group_by(country_name) %>% 
  ggplot(aes(x = year, color = country_name)) +
  geom_line(aes(y = en_atm_co2e_kt)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Year",
       y = "CO2 emissions (kt)")

country_data %>% 
  arrange(desc(en_atm_co2e_pp_gd)) %>% 
  filter(en_atm_co2e_pp_gd >= 0.75) %>% 
  group_by(country_name) %>% 
  ggplot(aes(x = year, color = country_name)) +
  geom_line(aes(y = en_atm_co2e_pp_gd)) +
  theme_minimal()


# models
require(tidyr)
country<-read.csv("config_files/FFG_Hackathon_Country_Level_Data.csv")
US<-rbind(country[124,],country[255,],country[386,],country[517,],country[517,],country[648,],country[779,],country[910,],country[1041,],country[1172,],country[1303,])
model <- lm(EN.ATM.CO2E.KT~.-Year-Country.Name-Country.Code-AG.LND.AGRI.K2-AG.LND.TOTL.K2-AG.SRF.TOTL.K2, data = US)
lm(EN.ATM.CO2E.KT~EG.FEC.RNEW.ZS, data = US) %>% 
  summary() # renewable consumption R^2=0.8456, p-val=0.0001659
lm(EN.ATM.CO2E.KT~EG.USE.COMM.CL.ZS, data = US) %>% 
  summary() # alternative & nuclear energy R^2=0.8664, p-val=9.228e-05
lm(EN.ATM.CO2E.KT~EG.ELC.RNEW.ZS, data = US) %>% 
  summary() # renewable output
lm(EN.ATM.CO2E.KT~EG.FEC.RNEW.ZS*EG.USE.COMM.CL.ZS*EG.ELC.RNEW.ZS, data = US) %>% 
  summary() # all 3



            