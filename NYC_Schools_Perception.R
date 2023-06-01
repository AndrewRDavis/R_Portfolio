library(tidyverse)
library(ggpubr)

combined <- read.csv("combined.csv")

gened <- read_tsv("masterfile11_gened_final.txt", show_col_types = FALSE)
d75 <- read_tsv("masterfile11_d75_final.txt", show_col_types = FALSE)

##filter gened data for only High Schools and select columns
gened <- gened %>%
  filter(schooltype == "High School") %>%
  select(dbn:aca_tot_11)

d75 <- d75 %>%
  select(dbn:aca_tot_11)

##combine gened and d75 data
gened_d75 <- gened %>%
  bind_rows(d75)

gened_d75 <- gened_d75 %>%
  rename(DBN = dbn)

##join combined data with the gened_d75 data
tot_combined <- combined %>%
  left_join(
    gened_d75,
    by = "DBN"
  )

##find correlation of SAT scores to scoring
cor_mat <- tot_combined %>%
  select(avg_sat_score, saf_p_11:aca_tot_11) %>%
  cor(use = "pairwise.complete.obs")

##convert to tibble
cor_tib <- cor_mat %>%
  as_tibble(rownames = "variable")

##filter for only highly positive or negatively correlated
avg_score_core <- cor_tib %>%
  select(variable, avg_sat_score) %>%
  filter(avg_sat_score > 0.25 | avg_sat_score < -0.25)

x_var <- avg_score_core$variable
questions <- c(
  "saf_t_11" = "Satisfaction of Teacher's Responses",
  "saf_s_11" = "Satisfaction of Student's Responses",
  "aca_s_11" = "Academic Expectations of Student's Responses",
  "saf_tot_11" = "Safety and Respect Total Score"
)

##pivot longer to questions
tot_combined_selectstrong <- tot_combined %>%
  select(x_var) %>%
  pivot_longer(
    cols = saf_t_11:saf_tot_11,
    names_to = "question",
    values_to = "score")

tot_combined_selectstrong$question <- str_replace_all(
  tot_combined_selectstrong$question, questions)

##plot review score to average SAT score  
graph_group <- tot_combined_selectstrong %>%  
  ggplot(aes(x = score, y = avg_sat_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(vars(question)) +
  stat_cor() +
  labs(
    x = "Review Score",
    y = "Average SAT Score"
  )

##determine response type and replot based on feedback
combined_survey_gather <- tot_combined %>%
  pivot_longer(cols = saf_p_11:aca_tot_11,
               names_to = "survey_question",
               values_to = "score")

combined_survey_gather <- combined_survey_gather %>%
  mutate(response_type = str_sub(survey_question, 4, 6),
         question = str_sub(survey_question, 1,3))

combined_survey_gather <- combined_survey_gather %>%
  mutate(response_type = ifelse(response_type == "_p_", "parent",
                                ifelse(response_type == "_t_", "teacher",
                                       ifelse(response_type == "_s_", "student",
                                              ifelse(response_type == "_to", "total", "NA")))))

final_plot <- combined_survey_gather %>%
  filter(response_type != "total") %>%
  ggplot(aes(x = question, y = score, fill = response_type)) +
  geom_boxplot()