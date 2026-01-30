  library(stringr)
  library(janitor)
  library(tidyverse)
  
  #To check for duplicate trip_id
  Divvy_Trips_2019_Q1 %>%
    count(trip_id) %>%
    filter(n > 1)
  #There are no duplicates
  
  
  #To ensure no spaces in column names and all column names are in lowercase
  clean_names(Divvy_Trips_2019_Q1) 
  
  
  #To view the new cleaned dataframe
  clean1_Divvy_Trips_2019_Q1 <- clean_names(Divvy_Trips_2019_Q1)
  View(clean1_Divvy_Trips_2019_Q1)
  
  
  #To count the number of rows with empty cells
  Divvy_Trips_2019_Q1 %>%
    filter(if_any(everything(), is.na)) %>%
    nrow()
  #There are 19712 rows
  
  
  #To delete empty cells
  clean2_Divvy_Trips_2019_Q1 <- drop_na(clean1_Divvy_Trips_2019_Q1)
  View(clean2_Divvy_Trips_2019_Q1)
  
  
  # To detect inconsistent suffixes using the full name of observed abreviations prominent in the dataframe
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
  View(clean3_Divvy_Trips_2019_Q1)
  #Inconsistency detected in place appearing as pl in most places and place in a certain station name, Mccormick Place
  
  
  #Make the naming format consistent as "Pl" using global substitution
  clean4_Divvy_Trips_2019_Q1 <- clean2_Divvy_Trips_2019_Q1
  clean4_Divvy_Trips_2019_Q1$from_station_name <- 
  gsub("Place", "Pl", clean2_Divvy_Trips_2019_Q1$from_station_name)
  View(clean4_Divvy_Trips_2019_Q1)
  
  
  clean5_Divvy_Trips_2019_Q1 <- clean4_Divvy_Trips_2019_Q1
  clean5_Divvy_Trips_2019_Q1$to_station_name <-
  gsub("Place", "Pl", clean4_Divvy_Trips_2019_Q1$to_station_name)
  View(clean5_Divvy_Trips_2019_Q1)
  
  
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
  View(clean6_Divvy_Trips_2019_Q1)
  
  
  #Longitude and latitude values were validated by confirming that latitude values
  # fell within the expected range (−90 to 90) and longitude values within (−180 to 180).
  # The observed negative longitude values are consistent with the study area being 
  #located west of the Prime Meridian.”
  clean7_Divvy_Trips_2019_Q1 <- clean6_Divvy_Trips_2019_Q1%>%
    select(-gender,-birthyear, -bikeid)
  View(clean7_Divvy_Trips_2019_Q1)
  
  
  #To confirm that trip_duration is stored in seconds
  tripduration_check <- clean7_Divvy_Trips_2019_Q1 %>%
    mutate(tripduration_seconds = as.numeric(difftime(end_time, start_time, units = "secs")))
  View(tripduration_check)
  
  
  #To confirm that their calculation is correct
  tripduration_check <- tripduration_check %>%
    mutate(duration_diff = tripduration - tripduration_seconds)
    View(tripduration_check)
  
  tripduration_check %>%
    filter(duration_diff != 0) %>%
    nrow()
  #16 diferences noted, Hence we use our calculated field
  
  
  clean8_Divvy_Trips_2019_Q1 <- tripduration_check%>%
    select(-tripduration, - duration_diff)
  View(clean8_Divvy_Trips_2019_Q1)
  
  
  
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
  View(clean1_Divvy_Trips_2020_Q1)
  
  
  #To remove any leading or trailing spaces and remove the observed Asterisk appearing in some street address across all addresses.
  clean2_Divvy_Trips_2020_Q1 <- clean1_Divvy_Trips_2020_Q1%>%
    mutate(across(where(is.character), ~str_to_title(str_squish(trimws(str_remove_all(., "\\(.*?\\)"))))))
  View(clean2_Divvy_Trips_2020_Q1)
  
  
  #To ensure that there are only characters, letters and underscores in the dataframe
  clean3_Divvy_Trips_2020_Q1 <- clean_names(clean2_Divvy_Trips_2020_Q1)
  View(clean3_Divvy_Trips_2020_Q1)
  
  
  
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
  View(clean4_Divvy_Trips_2020_Q1)
  
  clean5_Divvy_Trips_2020_Q1 <- clean4_Divvy_Trips_2020_Q1
  clean5_Divvy_Trips_2020_Q1$end_station_name <-
    gsub("Place", "Pl", clean3_Divvy_Trips_2020_Q1$end_station_name)
  View(clean5_Divvy_Trips_2020_Q1)
  
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
  View(clean6_Divvy_Trips_2020_Q1)
  
  #Remove columns for merging purpose
  clean7_Divvy_Trips_2020_Q1 <- clean6_Divvy_Trips_2020_Q1%>%
    select(-rideable_type, -start_lat, -start_lng, -end_lat, -end_lng)
  View(clean7_Divvy_Trips_2020_Q1)
  
  clean8_Divvy_Trips_2020_Q1 <- clean7_Divvy_Trips_2020_Q1 %>%
    mutate(tripduration_seconds = as.numeric(difftime(end_time, start_time, units = "secs")))
  View(clean8_Divvy_Trips_2020_Q1)
  
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
  
  write.csv(clean6_Divvy_Trips_2020_data,
            "clean6_Divvy_Trips_2020_data.csv",
            row.names = FALSE)
  
  write.csv(clean7_Divvy_Trips_2020_data,
            "clean7_Divvy_Trips_2020_data.csv",
            row.names = FALSE)
  
  write.csv(clean8_Divvy_Trips_2020_data,
            "clean8_Divvy_Trips_2020_data.csv",
            row.names = FALSE)
  
  
  
  
  #To make both datatypes character_datatype 
  clean8_Divvy_Trips_2019_Q1 <- clean8_Divvy_Trips_2019_Q1 %>%
    mutate(trip_id = as.character(trip_id))
  
  clean8_Divvy_Trips_2020_Q1 <- clean8_Divvy_Trips_2020_Q1 %>%
    mutate(trip_id = as.character(trip_id))
  
  
  merged_divvy_data1 <-
  bind_rows(clean8_Divvy_Trips_2019_Q1, clean8_Divvy_Trips_2020_Q1)
  View(merged_divvy_data1)
  
  
  #To change name of cell from customer to casual and from subscribers to members
  merged_divvy_data2 <- merged_divvy_data1 %>%
    mutate(usertype = recode(usertype,
                             "Customer" = "Casual",
                             "Subscriber" = "Member"))
  View(merged_divvy_data2)
  
  
  #To rearrange columns
  merged_divvy_data3 <- merged_divvy_data2%>%
    relocate(tripduration_seconds, .before = start_station_id)
  View(merged_divvy_data3)
  
  
  #Add day of week for the start_time and end_time
  library(lubridate)
  
  merged_divvy_data4 <- merged_divvy_data3 %>%
    mutate(
      start_day_of_the_week = wday(start_time, label = TRUE),
      end_day_of_the_week   = wday(end_time,   label = TRUE)
    )
  View(merged_divvy_data4)
  
  
  #I arranged rows in descending order to check for false tripduration_seconds 
  #and spoted some -ve values and some vakues less thn 60s which i use as my bech benchmark for rides
  #I also noticed some data with aoutrageous amounts of time spent on rides when u arranged by clicking on the 
  #arro in the r sorce(aspa that place that shows the satasets) and so i set a benc hmark to emove all greater than 3 hrs tha is 10800seconds
  
  Original_merged_rows <- nrow(merged_divvy_data4)
  View(Original_merged_rows)
  
  
  merged_divvy_data5 <- merged_divvy_data4 %>%
    filter(tripduration_seconds > 0) %>%   # remove negative durations
    filter(tripduration_seconds >= 60) %>%    # remove very short trips
    filter(tripduration_seconds <= 10800)
  View(merged_divvy_data5)
  
  
  cleaned_merged_rows <- nrow(merged_divvy_data5)
  View(cleaned_merged_rows)
  
  removed_merged_rows <- Original_merged_rows - cleaned_merged_rows
  View(removed_merged_rows)
  
  removed_percent <- (removed_merged_rows / Original_merged_rows) * 100
  View(removed_percent)
  #1.238% of the data  
  
  #Rearranged datasets again
  merged_divvy_data6 <- merged_divvy_data5%>%
    relocate(start_day_of_the_week, end_day_of_the_week, .before = start_station_id)
  View(merged_divvy_data6)
  
  
  #ANALYSES PHASE (I GUESS...LOL)
  
  #mean and median trip duration for each usertyes
  summary_tripduration <- merged_divvy_data6 %>%
    group_by(usertype) %>%
    summarise(
      avg_tripduration_secs   = mean(tripduration_seconds),
      median_tripduration_secs = median(tripduration_seconds)
    )
  View(summary_tripduration)
  
  
  #in minutes
  summary_tripduration %>%
    mutate(avg_tripduration_mins = avg_tripduration_secs / 60,
           median_tripduration_mins = median_tripduration_secs / 60)%>%
    select(-avg_tripduration_secs, -median_tripduration_secs)
  
  #The avg_trip duration for casual users was 31minutes and from member was 11 min
  #This was closely related to the median values of 22.3 minutes for casual users and 
  #8.48 minutes for members.
  #This implies that casual riders use bike for a longer duration than members
  
  
  #We now dive deeper to observe how these two users differs by days of the week
  #I realised i still had two columns and decided to work with a sinlge colium for dow but then 
  #going through my data i realised that some hrs crossed over from one day into another and so
  #I picked the start day as my day of week and proceeded to exploring the differement bewteen users by dow
  
  
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
  View(merged_divvy_data7)
  
  
  #Compare the difference in usage of bikesfor each usertype
  daily_summary_casual <- merged_divvy_data7 %>%
    filter(usertype == "Casual") %>%
    group_by(trip_day) %>%
    summarise(
      avg_tripduration_min = mean(tripduration_seconds/60),
      median_tripduration_min = median(tripduration_seconds/60),
      .groups = "drop"
    )
  View(daily_summary_casual)
  
  daily_summary_member <- merged_divvy_data7 %>%
    filter(usertype == "Member") %>%
    group_by(trip_day) %>%
    summarise(
      avg_tripduration_min = mean(tripduration_seconds)/60,
      median_tripduration_min = median(tripduration_seconds/60),
      .groups = "drop"
    )
  View(daily_summary_member)
  
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
  #For both groups, the mean is greater than the median showing that tthe datsa is  slighlty skwed.
  #Casual users consistently took longer trips than members, with average trip durations 
  #peaking on Sundays. Median values show a similar pattern, 
  #confirming that casual users’ rides are both longer and slightly more variable.
  #for Lets see if this will 
  #be same for the count per day to see which day bike is being used the most
 
  
  
  
  
  #daiy trip
  daily_trip_counts <- merged_divvy_data7 %>%
    group_by(usertype, trip_day) %>%
    summarise(
      trip_count = n(),
      .groups = "drop"
    )
  
  View(daily_trip_counts)
  
  #Add a column for day_type to calculate avg count by day of the week
  merged_divvy_data8 <- merged_divvy_data7 %>%
    mutate(
      day_type = ifelse(trip_day %in% c("Sat", "Sun"),
                        "Weekend", "Weekday"))%>%
    relocate(day_type, .after = trip_day)
  View(merged_divvy_data8)
  
  #avg trip count by daytype
  avg_day_type_trip_count <- merged_divvy_data8 %>%
    group_by(usertype, trip_day, day_type) %>%
    summarise(daily_trip_count = n(), .groups = "drop") %>%
    group_by(usertype, day_type) %>%
    summarise(
      avg_daily_trip_count = mean(daily_trip_count),
      .groups = "drop"
    )
  View(avg_day_type_trip_count)
  
  
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
 #Casual riders use bikes the mostly on weekends with the amount of times bokes 
  #where order on satand subn as 15507 and 8582 times.There was a huge drop on weekdays 
  #Members use bikes at a high count rate on weeddays, even about 7 times more than the caual iridders use it on weekends
  #On average no of bikes picked up by members on weeekdays are 119046 bikes and 59059 on weeekends
  #While avg  count of bikes casual users is  12044 on weekend and , 5047 on weeekdays
  #we observed that bikes are used mostly bike members tha by casual riders. 
  #While long trips are made by casual users they dont use bikes as much as often as the members
  #who use it very often but take shorter trip in term of trip length
  
  
  #We now dive deeper  to see which hours for each users , which hour  users chose to ride
  #i.e which is the most recorded hrs for the start_time for each users
  
  merged_divvy_data9 <- merged_divvy_data8 %>%
    mutate(start_hour = hour(start_time)) %>%
    relocate(start_hour, .before = trip_day)
  View(merged_divvy_data9)
  
  
  #see trip count  by start hour
  trips_by_hour_overall <- merged_divvy_data9 %>%
    group_by(start_hour) %>%
    summarise(trip_count = n()) %>%
    arrange(start_hour)
  View(trips_by_hour_overall)
  
  
  #Overall , most trips occurs between 7am - 8 am and 4pm -5pm . The east occrs a 3ami predict that the
  #members might be skewing this dataset in terms of when most rides ocur so we dive deper to seeing if this is so for each user
  #To see if theres a  difference in hourly behaviour 
  
  
  #trip Count by usertype and start hour o
  
  hourly_trip_by_usertype <- merged_divvy_data9 %>%
    group_by(usertype, start_hour) %>%
    summarise(trip_count = n(), .groups = "drop") %>%
    arrange(usertype, desc(trip_count))
  View(hourly_trip_by_usertype)
  #Casual users peak around 13–17 (1 PM–5 PM) → afternoon/early evening
  #casual riders raraely ride at 0-5(12am-5am) that is at night / very early morning
  #Members peak around 7–8am and again around 4-6pm → mostly daytime commuter hours
  #late morning 9am and midday rides 12-3 exists but are not as intense at commuting hours
  #members similarly to cusal rideers ride least at night from 0-4 am with a slighly higher amunt from 
  
  
  
  #we now see if theres stations that are popular for members and for casual riders
  # Top start stations overall
  top_start_stations <- merged_divvy_data9 %>%
    group_by(start_station_name) %>%        # Group by start station
    summarise(station_count = n()) %>%        # Count trips per station
    arrange(desc(station_count))              # Sort descending
  View(top_start_stations)
  
  
  top_start_stations_usertype <- merged_divvy_data9 %>%
    group_by(usertype, start_station_name) %>% # Group by user type + start station
    summarise(station_count = n()) %>%
    slice_max(station_count, n = 10)%>%
    arrange(usertype, desc(station_count)) 
  View(top_start_stations_usertype)
  
  #we now see if theres stations that are popular for members and for casual riders
  # Top start stations overall
  top_end_stations <- merged_divvy_data9 %>%
    group_by(end_station_name) %>%        # Group by start station
    summarise(station_count = n()) %>%        # Count trips per station
    arrange(desc(station_count))              # Sort descending
  View(top_end_stations)
  
  
  top_end_stations_usertype <- merged_divvy_data9 %>%
    group_by(usertype, end_station_name) %>% # Group by user type + start station
    summarise(station_count = n()) %>%
    slice_max(station_count, n = 10)%>%
    arrange(usertype, desc(station_count)) 
  View(top_end_stations_usertype)
  
  
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
  
  

  