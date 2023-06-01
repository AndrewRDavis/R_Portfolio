library(tidyverse)
forestfires <- read.csv("forestfires.csv")

#create reporting order by month
month_order <- c("jan", "feb", "mar", "apr", "may", "jun",
                 "jul", "aug", "sep", "oct", "nov", "dec")

#create reporting order by day of the week
dow_order <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

#assign order to the month and day
forestfires <- forestfires %>%
  mutate(
    month = factor(month, levels = month_order),
    day = factor(day, levels = dow_order)
  )

##forest fires per month
forestfires_count_month <- forestfires %>%
  group_by(month) %>%
  summarise(total_fires = n())

##forest fires per day
forestfires_count_day <- forestfires %>%
  group_by(day) %>%
  summarise(total_fires = n())

##create plot of Forest Fires by Month
p_fcm <- forestfires_count_month %>%
  ggplot(aes(x = month, y = total_fires)) +
  geom_col() +
  labs(title = "Forest Fires by Month") +
  xlab("Month") +
  ylab("Number of Forest Fires")

##create plot of Forest Fires by Day of the Week
p_fcd <- forestfires_count_day %>%
  ggplot(aes(x = day, y = total_fires)) +
  geom_col() +
  labs(title = "Forest Fires by Day of the Week") +
  xlab("Day") +
  ylab("Number of Forest Fires")

##long pivot forest fires data
forestfires_long <- forestfires %>%
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

##plot variable changes over the month
ff_l <- forestfires_long %>%
  ggplot(aes(x = month, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(title = "Variable Changes over Month") +
  xlab("Month") +
  ylab("Variable Value")

##plot relationship of other variables to area burned
ff_a <- forestfires_long %>%
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scale = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Value of Column",
    y = "Area Burned (hectare)"
  )

##same plot with areas less than 300
ff_af <- forestfires_long %>%
  filter(area < 300) %>%
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scale = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Value of Column",
    y = "Area Burned (hectare)"
  )

