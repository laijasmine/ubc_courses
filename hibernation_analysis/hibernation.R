library(tidyverse)
library(here)

#load the dataset
#please have the file in the same folder as the R script
df <- read.csv(here("hibernation_cold_warm.csv")) %>% 
  filter(subject != "20181121")
room_temp = 21
flow = 500

#Add new columns to the dataframe, treatment and corrected oxygen value
df_groups <- mutate(df,
                    O2consumed_T = ((df$Inflow_O2 - df$Outflow_CO2_Torpor)/100) * flow * STDP,
                    O2consumed_A = ((df$Inflow_O2 - df$Outflow_O2_Arousal)/100) * flow * STDP,
                    CO2consumed_T = ((df$Inflow_CO2 - df$Outflow_CO2_Torpor)/100) * flow * STDP,
                    CO2consumed_A = ((df$Inflow_CO2 - df$Outflow_CO2_Arousal)/100) * flow * STDP,
                    rate_O2 = abs(O2consumed_A - O2consumed_T) / df$Time_to_arousal_min,
                    rate_CO2 = abs(CO2consumed_A - CO2consumed_T) / df$Time_to_arousal_min,
                    consumption_g = rate_O2 / df$bodyweight_g)
#summary table & confidence interval
avg_result <- group_by(df_groups, df$Temperature.treatment) %>%
  summarise(m_temp = mean(chamber_temp, na.rm = TRUE),
            m_weight = mean(bodyweight_g),
            m_rate_O2 = mean(rate_O2, na.rm = TRUE),
            m_consumption = mean(consumption_g),
            sd_g = sd(consumption_g ,na.rm = TRUE),
            m_rate_CO2 = mean(rate_CO2, na.rm = TRUE),
            sd_O2 = sd(rate_O2, na.rm = TRUE),
            sd_CO2 = sd(rate_CO2, na.rm = TRUE),
            N = n(),
            t_dist = qt(0.95, (N-1)), #95%
            CI_O2 = t_dist * sd_O2 / sqrt(N),
            CI_CO2 = t_dist * sd_CO2 / sqrt(N),
            CI_g = t_dist * sd_g / sqrt(N)) #confidence interval

#save data as 
#write.csv(avg_result, "summary_stats.csv")


#data frame used to graph
graph_df <- avg_result

hiber_O2 <- ggplot(graph_df, aes(graph_df$`df$Temperature.treatment`, m_rate_O2,
                                 fill = graph_df$`df$Temperature.treatment`)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = (m_rate_O2 - CI_O2) , ymax = (m_rate_O2 + CI_O2)), width = 0.2) +
  labs(x = "Treatment",
       y = "Mean Rate of Oxygen Consumption ((ml/min)/min STPD)") +
  scale_fill_manual(values = c("#0097a7", "#ffab40")) +
  theme_minimal() +
  guides(fill=FALSE) #removes legends

hiber_CO2 <- ggplot(graph_df, aes(graph_df$`df$Temperature.treatment`, m_rate_CO2,
                                  fill = graph_df$`df$Temperature.treatment`)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = (m_rate_CO2 - CI_CO2) , ymax = (m_rate_CO2 + CI_CO2)), width = 0.2) +
  labs(x = "Treatment",
       y = "Mean Rate of Carbon Dioxide Production ((ml/min)/min STPD)") +
  scale_fill_manual(values = c("#0097a7", "#ffab40")) +
  theme_minimal() +
  guides(fill=FALSE) #removes legends

#graphs arranged and saved
ggsave("hibernation_O2.jpg", hiber_O2, height =6)
ggsave("hibernation_CO2.jpg", hiber_CO2, height =6)
