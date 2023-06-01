library(tidyverse)
library(lubridate)
sales_2019 <- read.csv("sales2019.csv")


##filter out NAs for user submitted reviews
sales_2019 <- sales_2019 %>%
  filter(!is.na(user_submitted_review))

##calculate purchased means
purchased_mean <- sales_2019 %>%
  filter(!is.na(total_purchased)) %>%
  pull(total_purchased) %>%
  mean

purchased_mean <- as.integer(round(purchased_mean, digits = 0))

sales_2019 <- sales_2019 %>%
  mutate(
    revised_purchase = if_else(is.na(total_purchased), 
                               purchased_mean, 
                               total_purchased)
  )

#print(distinct(sales_2019["user_submitted_review"]))

##function to determine review sentiment
is_positive <- function(review) {
  review_positive = case_when(
    str_detect(review, "Awesome") ~ TRUE,
    str_detect(review, "OK") ~ TRUE,
    str_detect(review, "Never") ~ TRUE,
    str_detect(review, "a lot") ~ TRUE,
    TRUE ~ FALSE
  )
}

sales_2019 <- sales_2019 %>%
  mutate(
    review_sentiment = unlist(map(user_submitted_review, is_positive))
  )

##reformat date and determine pre or post sales
sales_2019 <- sales_2019 %>%
  mutate(
    date = mdy(date)
  )

sales_2019 <- sales_2019 %>%
  mutate(
    review_system = if_else(date < "2019-07-01", "Pre", "Post")
  )

review_system_summary_table <- sales_2019 %>%
  group_by(review_system, customer_type, review_sentiment) %>%
  summarise(
    sum_of_reviews = sum(revised_purchase)
  )

print(review_system_summary_table)