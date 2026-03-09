library(stringr)
library(janitor)
library(tidyverse)
library(readr)

# Load the 2019 data
Divvy_Trips_2019_Q1 <- read_csv("Divvy_Trips_2019_Q1.csv")
# Load the 2020 data
Divvy_Trips_2020_Q1 <- read_csv("Divvy_Trips_2020_Q1.csv")

#To check for duplicate trip_id
Divvy_Trips_2019_Q1 %>%
  count(trip_id) %>%
  filter(n > 1)
#There are no duplicates

#To ensure no spaces in column names and all column names are in lowercase
clean_names(Divvy_Trips_2019_Q1) 

#To view the new cleaned dataframe
clean1_Divvy_Trips_2019_Q1 <- clean_names(Divvy_Trips_2019_Q1)

#To count the number of rows with empty cells
Divvy_Trips_2019_Q1 %>%
  filter(if_any(everything(), is.na)) %>%
  nrow()
#There are 19712 rows


#To delete empty cells
clean2_Divvy_Trips_2019_Q1 <- drop_na(clean1_Divvy_Trips_2019_Q1)


# To detect inconsistent suffixes using the full name of observed abbreviations prominent in the dataframe
patterns <- c("\\bRoad\\b", "\\bStreet\\b", "\\bParkway\\b", "\\bAvenue\\b",
              "\\bBoulevard\\b", "\\bPlace\\b", "\\bDrive\\b")


# Combine into one regex
regex_pattern <- paste(patterns, collapse = "|")


# Filter rows where either start or end station has any of these exact patterns
clean3_Divvy_Trips_2019_Q1 <- clean2_Divvy_Trips_2019_Q1 %>%
  filter(
    str_detect(from_station_name, regex_pattern) |
      str_detect(to_station_name, regex_pattern)
  )
#Inconsistency detected in place appearing as pl in most places and place in a certain station name, Mccormick Place


#Make the naming format consistent as "Pl" using global substitution
clean4_Divvy_Trips_2019_Q1 <- clean2_Divvy_Trips_2019_Q1
clean4_Divvy_Trips_2019_Q1$from_station_name <- 
  gsub("Place", "Pl", clean2_Divvy_Trips_2019_Q1$from_station_name)


clean5_Divvy_Trips_2019_Q1 <- clean4_Divvy_Trips_2019_Q1
clean5_Divvy_Trips_2019_Q1$to_station_name <-
  gsub("Place", "Pl", clean4_Divvy_Trips_2019_Q1$to_station_name)


#To confirm fixed inconsistent suffixes
patterns <- c("\\bPlace\\b")


# Combine into one regex
regex_pattern <- paste(patterns, collapse = "|")


# Filter rows where either start or end station has any of these exact patterns
clean5_Divvy_Trips_2019_Q1 %>%
  filter(
    str_detect(from_station_name, regex_pattern) |
      str_detect(to_station_name, regex_pattern)
  ) %>%
  select(from_station_name, to_station_name)
#result is empty meaning it has been consistently formatted.


clean6_Divvy_Trips_2019_Q1 <- clean5_Divvy_Trips_2019_Q1 %>%
  mutate(across(where(is.character), ~ str_to_title(str_squish(trimws(str_remove_all(., "\\(.*?\\)"))))))
#To remove any leading or trailing spaces and remove the Asterisk appearing in some street address across all addresses.
#library(dplyr) loads dplyr, which gives you functions like mutate(), across(), %>% (the pipe).
#library(stringr) loads stringr, which has handy string functions like str_squish().
#cleaned_Divvy_Trips_2019_data %>% ... the %>%  takes what you have written in the first code line and calls information from it with the next one
#mutate(...) is saying “create new columns or change existing ones.”
#across(where(is.character), ~ str_squish(trimws(.)))
#across lets you apply a function to multiple columns at once rather than having to call out each columns one after the other.
#where(is.character) picks all columns that are text/strings.
#~ str_squish(...) implies for each cell in those columns:
#str_to_title sets texts to title case
#trimws(.) removes spaces at the start or end of the text
#str_squish(.)removes extra spaces inside the text, leaving just one space between words


#Rename from_station_name to start_station_name
#Rename to_station_name to end_station_name
#Rename from_station_id to start_station_id
#Rename to_station_name to end_station_id


clean6_Divvy_Trips_2019_Q1 <- clean6_Divvy_Trips_2019_Q1 %>%
  rename(start_station_name = from_station_name, 
         end_station_name = to_station_name,
         start_station_id = from_station_id, 
         end_station_id = to_station_id)


#Longitude and latitude values were validated by confirming that latitude values
# fell within the expected range (−90 to 90) and longitude values within (−180 to 180).
# The observed negative longitude values are consistent with the study area being 
#located west of the Prime Meridian.”
clean7_Divvy_Trips_2019_Q1 <- clean6_Divvy_Trips_2019_Q1%>%
  select(-gender,-birthyear, -bikeid)


#To confirm that trip_duration is stored in seconds
tripduration_check <- clean7_Divvy_Trips_2019_Q1 %>%
  mutate(tripduration_seconds = as.numeric(difftime(end_time, start_time, units = "secs")))


#To confirm that their calculation is correct
tripduration_check <- tripduration_check %>%
  mutate(duration_diff = tripduration - tripduration_seconds)

tripduration_check %>%
  filter(duration_diff != 0) %>%
  nrow()
#16 diferences noted, Hence we use our calculated field


clean8_Divvy_Trips_2019_Q1 <- tripduration_check%>%
  select(-tripduration, - duration_diff)



#To save the cleaning steps
write.csv(clean1_Divvy_Trips_2019_Q1,
          "clean1_Divvy_Trips_2019_Q1.csv",
          row.names = FALSE)

write.csv(clean2_Divvy_Trips_2019_Q1,
          "clean2_Divvy_Trips_2019_Q1.csv",
          row.names = FALSE)

write.csv(clean3_Divvy_Trips_2019_Q1,
          "clean3_Divvy_Trips_2019_Q1.csv",
          row.names = FALSE)


write.csv(clean4_Divvy_Trips_2019_Q1,
          "clean4_Divvy_Trips_2019_Q1.csv",
          row.names = FALSE)

write.csv(clean5_Divvy_Trips_2019_Q1,
          "clean5_Divvy_Trips_2019_Q1.csv",
          row.names = FALSE)


write.csv(clean6_Divvy_Trips_2019_Q1,
          "clean6_Divvy_Trips_2019_Q1.csv",
          row.names = FALSE)

write.csv(clean7_Divvy_Trips_2019_Q1,
          "clean7_Divvy_Trips_2019_Q1.csv",
          row.names = FALSE)


write.csv(clean8_Divvy_Trips_2019_Q1,
          "clean8_Divvy_Trips_2019_Q1.csv",
          row.names = FALSE)

#for divvy_trips_2020
#To check for duplicate ride_id
Divvy_Trips_2020_Q1 %>%
  count(ride_id)%>%
  filter(n>1)


#To check through the dataframe and count how many rows have at least one empty cell
Divvy_Trips_2020_Q1%>%
  filter(if_any(everything(), is.na))%>%
  nrow()


#To delete empty cells
clean1_Divvy_Trips_2020_Q1 <- drop_na(Divvy_Trips_2020_Q1)


#To remove any leading or trailing spaces and remove the observed Asterisk appearing in some street address across all addresses.
clean2_Divvy_Trips_2020_Q1 <- clean1_Divvy_Trips_2020_Q1%>%
  mutate(across(where(is.character), ~str_to_title(str_squish(trimws(str_remove_all(., "\\(.*?\\)"))))))


#To ensure that there are only characters, letters and underscores in the dataframe
clean3_Divvy_Trips_2020_Q1 <- clean_names(clean2_Divvy_Trips_2020_Q1)



# To detect inconsistent suffixes using the full name of observed abreviations prominent in the dataframe
patterns <- c("\\bRoad\\b", "\\bStreet\\b", "\\bParkway\\b", "\\bAvenue\\b",
              "\\bBoulevard\\b", "\\bPlace\\b", "\\bDrive\\b")

# Combine into one regex
regex_pattern <- paste(patterns, collapse = "|")

# Filter rows where either start or end station has any of these exact patterns
clean3_Divvy_Trips_2020_Q1 %>%
  filter(
    str_detect(start_station_name, regex_pattern) |
      str_detect(end_station_name, regex_pattern)
  ) %>%
  select(start_station_name, end_station_name)


#This shows that place was formatted inconsistently as pl in most places and as place in some others. The others didn't appear in the dataframe
#This shows 565 rows


#Rectify this by using global substitution, gsub()


clean4_Divvy_Trips_2020_Q1 <- clean3_Divvy_Trips_2020_Q1
clean4_Divvy_Trips_2020_Q1$start_station_name <-
  gsub("Place", "Pl", clean3_Divvy_Trips_2020_Q1$start_station_name)

clean5_Divvy_Trips_2020_Q1 <- clean4_Divvy_Trips_2020_Q1
clean5_Divvy_Trips_2020_Q1$end_station_name <-
  gsub("Place", "Pl", clean3_Divvy_Trips_2020_Q1$end_station_name)

#To confirm fixed inconsistent suffixes
patterns <- c("\\bPlace\\b")


# Combine into one regex
regex_pattern <- paste(patterns, collapse = "|")


# Filter rows where either start or end station has any of these exact patterns
clean5_Divvy_Trips_2020_Q1 %>%
  filter(
    str_detect(start_station_name, regex_pattern) |
      str_detect(end_station_name, regex_pattern)
  ) %>%
  select(start_station_name, end_station_name)
#result is empty meaning it has been consistently formatted.

#Longitude and latitude values were validated by confirming that latitude values
# fell within the expected range (−90 to 90) and longitude values within (−180 to 180).
# The observed negative longitude values are consistent with the study area being 
#located west of the Prime Meridian.”

#Rename started_at to start_time and ended_at to end_time

clean6_Divvy_Trips_2020_Q1 <- clean5_Divvy_Trips_2020_Q1 %>%
  rename(start_time = started_at, 
         end_time = ended_at,
         usertype = member_casual,
         trip_id = ride_id)

#Remove columns for merging purpose
clean7_Divvy_Trips_2020_Q1 <- clean6_Divvy_Trips_2020_Q1%>%
  select(-rideable_type, -start_lat, -start_lng, -end_lat, -end_lng)

clean8_Divvy_Trips_2020_Q1 <- clean7_Divvy_Trips_2020_Q1 %>%
  mutate(tripduration_seconds = as.numeric(difftime(end_time, start_time, units = "secs")))

write.csv(clean1_Divvy_Trips_2020_Q1,
          "clean1_Divvy_Trips_2020_Q1.csv",
          row.names = FALSE)

write.csv(clean2_Divvy_Trips_2020_Q1,
          "clean2_Divvy_Trips_2020_Q1.csv",
          row.names = FALSE)

write.csv(clean3_Divvy_Trips_2020_Q1,
          "clean3_Divvy_Trips_2020_Q1.csv",
          row.names = FALSE)

write.csv(clean4_Divvy_Trips_2020_Q1,
          "clean4_Divvy_Trips_2020_data.csv",
          row.names = FALSE)

write.csv(clean5_Divvy_Trips_2020_Q1,
          "clean5_Divvy_Trips_2020_Q1.csv",
          row.names = FALSE)

write.csv(clean6_Divvy_Trips_2020_Q1,
          "clean6_Divvy_Trips_2020_Q1.csv",
          row.names = FALSE)

write.csv(clean7_Divvy_Trips_2020_Q1,
          "clean7_Divvy_Trips_2020_Q1.csv",
          row.names = FALSE)

write.csv(clean8_Divvy_Trips_2020_Q1,
          "clean8_Divvy_Trips_2020_Q1.csv",
          row.names = FALSE)




#To make both datatypes character_datatype 
clean8_Divvy_Trips_2019_Q1 <- clean8_Divvy_Trips_2019_Q1 %>%
  mutate(trip_id = as.character(trip_id))

clean8_Divvy_Trips_2020_Q1 <- clean8_Divvy_Trips_2020_Q1 %>%
  mutate(trip_id = as.character(trip_id))


merged_divvy_data1 <-
  bind_rows(clean8_Divvy_Trips_2019_Q1, clean8_Divvy_Trips_2020_Q1)


#To change name of cell from customer to casual and from subscribers to members
merged_divvy_data2 <- merged_divvy_data1 %>%
  mutate(usertype = recode(usertype,
                           "Customer" = "Casual",
                           "Subscriber" = "Member"))


#To rearrange columns
merged_divvy_data3 <- merged_divvy_data2%>%
  relocate(tripduration_seconds, .before = start_station_id)


#Add day of week for the start_time and end_time
library(lubridate)

merged_divvy_data4 <- merged_divvy_data3 %>%
  mutate(
    start_day_of_the_week = wday(start_time, label = TRUE),
    end_day_of_the_week   = wday(end_time,   label = TRUE)
  )


#I arranged rows in descending order to check for false tripduration_seconds 
#and spotted some -ve values and some values less than 60s which I use as my benchmark for rides
#I also noticed some data with outrageous amounts of time spent on rides when u arranged by clicking on the 
#arrow in the r source(that place that shows the datasets) and so I set a benchmark to remove all hours greater than 24 hrs that is 86400seconds

Original_merged_rows <- nrow(merged_divvy_data4)


merged_divvy_data5 <- merged_divvy_data4 %>%
  filter(tripduration_seconds > 0) %>%   # remove negative durations
  filter(tripduration_seconds >= 60) %>%    # remove very short trips
  filter(tripduration_seconds <= 86400)


cleaned_merged_rows <- nrow(merged_divvy_data5)

removed_merged_rows <- Original_merged_rows - cleaned_merged_rows

removed_percent <- (removed_merged_rows / Original_merged_rows) * 100
#1.048% of the data  

#Rearranged datasets again
merged_divvy_data6 <- merged_divvy_data5%>%
  relocate(start_day_of_the_week, end_day_of_the_week, .before = start_station_id)


#ANALYSES PHASE

#mean and median trip duration for each usertyes
summary_tripduration <- merged_divvy_data6 %>%
  group_by(usertype) %>%
  summarise(
    avg_tripduration_secs   = mean(tripduration_seconds),
    median_tripduration_secs = median(tripduration_seconds))

#in minutes
summary_tripduration %>%
  mutate(avg_tripduration_mins = avg_tripduration_secs / 60,
         median_tripduration_mins = median_tripduration_secs / 60)%>%
  select(-avg_tripduration_secs, -median_tripduration_secs)

#The avg_trip duration for casual users was 39 minutes and from member was 11 minutes
#However, the median trip duration is 22.8 minutes for casual users and 8.5 minutes for members.
#This implies that the distribution of trip durations is moderately positively skewed.
#, and this results from the presence of longer trips that increase the overall average, which is typical in bike-share usage data.
#This also implies that casual riders use bike for a longer duration than member.


#We now dive deeper to observe how these two users differ by days of the week(dow)
#I realised I still had two columns and decided to work with a sinlge column for dow but then 
#going through my data I realised that some hrs crossed over from one day into another and so
#I picked the start day as my day of week and proceeded to exploring the differment bewteen users by dow


all(merged_divvy_data6$start_day_of_the_week ==
      merged_divvy_data6$end_day_of_the_week)

merged_divvy_data6 %>%
  filter(start_day_of_the_week != end_day_of_the_week) %>%
  nrow()

merged_divvy_data6 %>%
  filter(start_day_of_the_week != end_day_of_the_week) %>%
  select(start_time, end_time, start_day_of_the_week, end_day_of_the_week)


#To remove end day of week and change start day to trip_day
merged_divvy_data7 <- merged_divvy_data6 %>%
  select(- end_day_of_the_week)%>%
  rename(trip_day = start_day_of_the_week )


#Compare the difference in usage of bikes for each usertype by trip_day
daily_summary_casual <- merged_divvy_data7 %>%
  filter(usertype == "Casual") %>%
  group_by(trip_day) %>%
  summarise(
    avg_tripduration_min = mean(tripduration_seconds/60),
    median_tripduration_min = median(tripduration_seconds/60),
    .groups = "drop"
    )

daily_summary_member <- merged_divvy_data7 %>%
  filter(usertype == "Member") %>%
  group_by(trip_day) %>%
  summarise(
    avg_tripduration_min = mean(tripduration_seconds)/60,
    median_tripduration_min = median(tripduration_seconds/60),
    .groups = "drop"
  )


daily_summary <- merged_divvy_data7 %>%
  group_by(usertype, trip_day) %>%
  summarise(avg_tripduration = mean(tripduration_seconds) / 60)

ggplot(daily_summary, aes(trip_day, avg_tripduration, fill = usertype)) +
  geom_col(position = "dodge") +
  labs(title = "Average Trip Duration by Day of Week",
       x = "Day of Week",
       y = "Average Trip Duration (minutes)")

#Casual riders consistently take longer trips across all days, 
#with peaks on weekends. Members show stable, shorter ride durations during weekdays,
#reinforcing commuter behavior.
#The highest average trip duration for both casual and members were on sundays. 
#For both groups, the mean is greater than the median showing that the data is slighlty skewed.
#Casual users consistently took longer trips than members, with average trip durations 
#peaking on Sundays. Median values show a similar pattern, 
#confirming that casual users’ rides are both longer and slightly more variable.
#Lets see if this will 
#be same for the count per day to see which day bike is being used the most.


merged_divvy_data8 <- merged_divvy_data7 %>%
  mutate(
    day_type = ifelse(trip_day %in% c("Sat", "Sun"),
                      "Weekend", "Weekday"))%>%
  relocate(day_type, .after = trip_day) 


#daily trip
daily_trip_counts <- merged_divvy_data8 %>%
  group_by(usertype, day_type, trip_day) %>%
  summarise(
    trip_count = n(),
    .groups = "drop"
  )

#avg trip count by day_type
avg_day_type_trip_count <- merged_divvy_data8 %>%
  group_by(usertype, trip_day, day_type) %>%
  summarise(daily_trip_count = n(), .groups = "drop") %>%
  group_by(usertype, day_type) %>%
  summarise(
    avg_daily_trip_count = round(mean(daily_trip_count), 0),
    .groups = "drop"
  )


library(ggplot2)
library(scales)

ggplot(daily_trip_counts,
       aes(x = trip_day,
           y = trip_count,
           fill = usertype)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Daily Trip Count by Day of the Week",
    x = "Day of the Week",
    y = "Number of Trips",
    fill = "User Type"
  ) +
  theme_minimal()




# Plot the average trip counts
ggplot(avg_day_type_trip_count, aes(x = day_type, y = avg_daily_trip_count, fill = usertype)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Daily Trip Count by User Type: Weekday vs Weekend",
    x = "Day Type",
    y = "Average Number of Trips",
    fill = "User Type"
  ) +
  theme_minimal()


combined_summary <- merged_divvy_data8 %>%
  group_by(usertype, day_type) %>%
  summarise(
    avg_trip_count = n() / n_distinct(trip_day),
    avg_trip_duration_min = mean(tripduration_seconds) / 60,
    .groups = "drop"
  )

combined_summary

#Rearranged the dataset for plotting purposes
combined_long <- combined_summary %>%
  pivot_longer(
    cols = c(avg_trip_count, avg_trip_duration_min),
    names_to = "metric",
    values_to = "value"
  )

combined_long


#plot
library(ggplot2)
library(scales)

ggplot(combined_long,
       aes(x = day_type, y = value, fill = usertype)) +
  geom_col(position = "dodge") +
  facet_wrap(~ metric, scales = "free_y",
             labeller = as_labeller(c(
               avg_trip_count = "Average Trip Count",
               avg_trip_duration_min = "Average Trip Duration (Minutes)"
             ))) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Trip Frequency and Duration by User Type and Day Type",
    x = "Day Type",
    y = "Value",
    fill = "User Type"
  ) +
  theme_minimal()
#Casual riders use bikes mostly on weekends with the amount of times bikes 
#where ordered on saturday and sunday as 15729 and 8748 times. There was a huge drop on weekdays 
#Members use bikes at a high count rate on weekdays, even about 7 times more than the casual riders use it on weekends
#On average no of bikes picked up by members on weekdays are 119137 bikes and 59119 on weekends
#While avg count of bikes casual users is  12238 on weekend and, 5150 on weekdays
#we observed that bikes are used mostly by members than by casual riders. 
#While long trips are made by casual users they don't use bikes as often as the members
#who use it very often but take shorter trip in terms of trip length


#We now dive deeper to see which hour users choose to ride by usertype
#i.e which is the most recorded hour for the start_time for each users

merged_divvy_data9 <- merged_divvy_data8 %>%
  mutate(start_hour = hour(start_time)) %>%
  relocate(start_hour, .before = trip_day)


#see trip count  by start hour
trips_by_hour_overall <- merged_divvy_data9 %>%
  group_by(start_hour) %>%
  summarise(trip_count = n()) %>%
  arrange(start_hour)


#Overall, most trips occurs between 7am - 8 am and 4pm - 5pm. The least occurs at 3am. I predict that the
#dataset might be skewed by members, in terms of when most rides occur so we dive deeper to see if this is so for each user
#To see if there's a difference in hourly behaviour 


#trip Count by usertype and start hour o

hourly_trip_by_usertype <- merged_divvy_data9 %>%
  group_by(usertype, start_hour) %>%
  summarise(trip_count = n(), .groups = "drop") %>%
  arrange(usertype, desc(trip_count))
#Casual users peak around 13–17 (1pm - 5pm), that is, afternoon/early evening
#casual riders rarely ride at 0-5(12am - 5am), that is, at night / very early morning
#Members peak around 7am - 8am and again around 4pm - 6pm, that is,  mostly daytime commuter hours
#late morning 9am and midday rides 12am - 3am exists but are not as intense as commuting hours
#members similarly to casual riders ride least at night from 12am - 4am with a slightly higher amunt from 



#we now see if there are stations that are popular for members and for casual riders
# Top start stations overall
top_start_stations <- merged_divvy_data9 %>%
  group_by(start_station_name) %>%        # Group by start station
  summarise(station_count = n()) %>%        # Count trips per station
  arrange(desc(station_count))              # Sort descending


top_start_stations_usertype <- merged_divvy_data9 %>%
  group_by(usertype, start_station_name) %>% # Group by user type + start station
  summarise(station_count = n()) %>%
  slice_max(station_count, n = 10)%>%
  arrange(usertype, desc(station_count)) 

#we now see if there are stations that are popular for members and for casual riders
# Top start stations overall
top_end_stations <- merged_divvy_data9 %>%
  group_by(end_station_name) %>%        # Group by start station
  summarise(station_count = n()) %>%        # Count trips per station
  arrange(desc(station_count))              # Sort descending


top_end_stations_usertype <- merged_divvy_data9 %>%
  group_by(usertype, end_station_name) %>% # Group by user type + start station
  summarise(station_count = n()) %>%
  slice_max(station_count, n = 10)%>%
  arrange(usertype, desc(station_count)) 


write.csv(merged_divvy_data1,
          "merged_divvy_data1.csv",
          row.names = FALSE)

write.csv(merged_divvy_data2,
          "merged_divvy_data2.csv",
          row.names = FALSE)

write.csv(merged_divvy_data3,
          "merged_divvy_data3.csv",
          row.names = FALSE)

write.csv(merged_divvy_data4,
          "merged_divvy_data4.csv",
          row.names = FALSE)

write.csv(merged_divvy_data5,
          "merged_divvy_data5.csv",
          row.names = FALSE)

write.csv(merged_divvy_data6,
          "merged_divvy_data6.csv",
          row.names = FALSE)

write.csv(merged_divvy_data7,
          "merged_divvy_data7.csv",
          row.names = FALSE)

write.csv(merged_divvy_data8,
          "merged_divvy_data8.csv",
          row.names = FALSE)

write.csv(merged_divvy_data9,
          "merged_divvy_data9.csv",
          row.names = FALSE)


write.csv(summary_tripduration,
          "summary_tripduration.csv",
          row.names = FALSE)

write.csv(daily_summary_casual,
          "daily_summary_casual.csv",
          row.names = FALSE)

write.csv(daily_summary_member,
          "daily_summary_member.csv",
          row.names = FALSE)

write.csv(daily_trip_counts,
          "daily_trip_counts.csv",
          row.names = FALSE)

write.csv(avg_day_type_trip_count,
          "avg_day_type_trip_count.csv",
          row.names = FALSE)

write.csv(trips_by_hour_overall,
          "trips_by_hour_overall.csv",
          row.names = FALSE)

write.csv(hourly_trip_by_usertype,
          "hourly_trip_by_usertype.csv",
          row.names = FALSE)

write.csv(top_start_stations,
          "top_start_stations.csv",
          row.names = FALSE)

write.csv(top_start_stations_usertype,
          "top_start_stations_usertype).csv",
          row.names = FALSE)

write.csv(top_end_stations,
          "top_end_stations.csv",
          row.names = FALSE)

write.csv(top_end_stations_usertype,
          "top_end_stations_usertype).csv",
          row.names = FALSE)


