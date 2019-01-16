library(tidyverse)
library(here)

#load the dataset
#please have the file in the same folder as the R script
df <- read.csv(here("hibernation_active.csv")) %>% 
  filter(subject != "20181011")

room_temp = 21
flow = 500

#Add new columns to the dataframe, treatment and corrected oxygen value
df_groups <- mutate(df,
                    O2consumed = ((df$Inflow_O2...start.... - df$Outflow_CO2_..10.mins....)/100) * flow * STDP,
                    CO2consumed = ((df$Inflow_O2 - df$Outflow_O2)/100) * flow * STDP)
#summary table & confidence interval
avg_result <- group_by(df_groups, df$Temperature.treatment) %>%
  summarise(m_temp = mean(chamber_temp, na.rm = TRUE),
            m_weight = mean(bodyweight_g),
            m_rate_O2 = mean(rate_O2_T, na.rm = TRUE),
            sd_CO2 = sd(rate_CO2_T,na.rm = TRUE),
            N = n(),
            t_dist = qt(0.95, (N-1)), #95%
            CI = t_dist * sd_O2 / sqrt(N),
            CI = t_dist * sd_CO2 / sqrt(N)) #confidence interval



