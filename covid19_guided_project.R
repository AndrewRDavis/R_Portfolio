library(tidyverse)
covid_df <- read_csv("covid19.csv")
#dim(covid_df)
vector_cols <- colnames(covid_df)
#print(vector_cols)
head(covid_df)
#glimpse(covid_df)

##filter All States
covid_df_all_states <- covid_df %>%
  filter(Province_State == "All States") %>%
  select(-Province_State)

##select columns
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, 
         daily_positive)

##group by Country and sum each field descending by tested rate
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarise(tested = sum(daily_tested),
            positive = sum(daily_positive),
            active = sum(active),
            hospitalized = sum(hospitalizedCurr)) %>%
  arrange(desc(tested))

#top 10 countries for covid testing
covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
#print(covid_top_10)

countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

#positivity rate
ratio <- positive_cases / tested_cases
#print(ratio)

positive_tested_top_3 <- c("United Kingdom" = 0.113, "United States" = 0.109,
                           "Turkey" = 0.081)

#print(positive_tested_top_3)

united_kingdom <- c(0.113, 1473672, 166909, 0, 0)
united_states <- c(0.109, 17282363, 1877179, 0, 0)
turkey <- c(0.081, 2031192, 163941, 2980960, 0)

covid_mat <- rbind(united_kingdom, united_states, turkey)
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
#print(covid_mat)


question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)
datasets <- list(original = covid_df,
                 allstates = covid_df_all_states,
                 daily = covid_df_all_states_daily,
                 top_10 = covid_top_10)
matrix <- list(covid_mat)
vector <- list(vector_cols, countries)
data_structure_list <- list("dataframe" = datasets, "matrix" = matrix, "vector" =
                              vector)
covid_analysis_list <- list(question, answer, data_structure_list)
print(covid_analysis_list[[2]])
