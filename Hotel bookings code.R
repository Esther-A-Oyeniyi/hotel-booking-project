#install the required packages
library(tidyverse)
library(skimr)
library(janitor)

# import and view data/ data structure
bookings_df <- read_csv("hotel_bookings.csv")
head(bookings_df)
str(bookings_df)
glimpse(bookings_df)
colnames(bookings_df)
skim_without_charts(bookings_df)

#clean data
trimmed_df <- bookings_df %>% 
  select(hotel, is_canceled, lead_time)
View(trimmed_df)
trimmed_df %>% 
  select(hotel, is_canceled, lead_time) %>% 
  rename(hotel_type = hotel)
example_df_1 <- bookings_df %>%
  select(arrival_date_year, arrival_date_month) %>% 
  unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = " ")
example_df <- bookings_df %>%
  mutate(guests = adults + children + babies)


#Great. Now it's time to analyze the data, start by calculating some summary statistics! 

#Calculate the total number of canceled bookings and the average lead time for booking. start your code after the %>% symbol 
 
#Make a column called 'number_canceled' to represent the total number of canceled bookings. 
#Then, make a column called 'average_lead_time' to represent the average lead time. 
#Use the `summarize()` function to do this 

example_df_2 <- bookings_df %>%
  summarize(number_canceled = sum(is_canceled),
  average_lead_time = mean(lead_time))

#further analysis using visualization


#how time time does it take to have children booked? 
#how many children stays over the weekend?
example_df %>%
  group_by(hotel) %>%
  arrange(lead_time)
ggplot(data = example_df) + geom_point(mapping = aes(x = lead_time, y = children ))
ggplot(data = example_df) + geom_point(mapping = aes(x = stays_in_weekend_nights, y = children ))


#which year is the earliest number of bookings?
"mindate" <- min(example_df$arrival_date_year, na.rm = FALSE)

#which year is the latest number of bookings? 
"maxdate" <- max(example_df$arrival_date_year, na.rm = FALSE)

#what are the frequently booked hotel?
#compare hotel bookings based on market segment and hotel type
#which segment has the highest/lowest number of bookings?
#which hotel type has the highest/lowest number of bookings?
ggplot(data = example_df) + geom_bar(mapping = aes(x = market_segment, fill = market_segment)) +
  facet_wrap(~hotel) + 
  labs(title="Hotel bookings based on market segment and hotel type",
       caption =paste0("Data from:",  mindate, " to ", maxdate, 
                       "         Chart Created_by Esther Anthony"),
       x ="Market_Segment",
       y ="Number of Bookings")

#save charts
ggsave('example_df_chart.png',
       width =16,
       height =8)

ggsave('example_df_chart.png')

## EXPORT THE DATA
write.csv(trimmed_df,"C://Users//User//Desktop//PORTFOLIO WORKS//hotel_trimmed_data.csv")
write.csv(example_df,"C://Users//User//Desktop//PORTFOLIO WORKS//example_df.csv")
write.csv(example_df_1,"C://Users//User//Desktop//PORTFOLIO WORKS//example_df_1.csv")
write.csv(example_df_2,"C://Users//User//Desktop//PORTFOLIO WORKS//example_df_2.csv")