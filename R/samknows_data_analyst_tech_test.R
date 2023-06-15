#Loading all Libraries
library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(dlookr)
library(ggplot2)
library(psych)

#Loading all CSV's
person = fread("C:/Users/siddh/Desktop/Data Science/Task/SamKnows/data-analyst-test-master/data-analyst-test-master/data/details_for_each_person.csv")
download = fread("C:/Users/siddh/Desktop/Data Science/Task/SamKnows/data-analyst-test-master/data-analyst-test-master/data/download_speed_measurements.csv")
upload = fread("C:/Users/siddh/Desktop/Data Science/Task/SamKnows/data-analyst-test-master/data-analyst-test-master/data/upload_speed_measurements.csv")

#filtering dataframe on variable 'did_test_complete_successfully' = 'True', 
#city considered = 'Samsville' and 'Databury', and tests run in the month of 'January 2021'
#and consolidating data for each person based on average of download and upload speed

#Filtering download_speed_measurements.csv with 'did_test_complete_successfully == TRUE' and month "January'
download_filtered <- download %>% filter(did_test_complete_successfully == TRUE) %>% mutate(time_of_measurement = ymd_hms(time_of_measurement)) %>%
  filter(month(time_of_measurement) == 1 & year(time_of_measurement) == 2021)

##Filtering upload_speed_measurements.csv with 'did_test_complete_successfully == TRUE' and month "January'
upload_filtered <- upload %>% filter(did_test_complete_successfully == TRUE) %>% mutate(time_of_measurement = ymd_hms(time_of_measurement)) %>%
  filter(month(time_of_measurement) == 1 & year(time_of_measurement) == 2021)

#Creating dataframe with filtered data from download_speed_measurements.csv and upload_speed_measurements.csv and filtering with City "Samsville", "Databury".
df <- person %>%
  filter(city %in% c("Samsville", "Databury")) %>%
  left_join(download_filtered, by = "person_id") %>%
  left_join(upload_filtered, by = "person_id") %>%
  group_by(person_id, city, type_of_broadband_connection, name_of_isp) %>%
  summarize(average_download_speed = mean(measured_download_speed_in_Mbps, na.rm = TRUE),
            average_upload_speed = mean(measured_upload_speed_in_Mbps, na.rm = TRUE))
View(df)


#Calculating the average download speed each day, with 60th percentile.

#dataframe with average speed per day of each user
df_daily_speeds <- download_filtered %>%
  group_by(person_id, date = date(time_of_measurement)) %>%
  summarize(average_download_speed = mean(measured_download_speed_in_Mbps, na.rm = TRUE))
View(df_daily_speeds)

#60th percentile of their average download speed
df_percentile <- df_daily_speeds %>%
  group_by(person_id) %>%
  summarize(avg_download_speed_60percentile = quantile(average_download_speed, 0.6))
View(df_percentile)

#Joining the dataframes in Final Dataframe
final_table <- inner_join(df, df_percentile, by = "person_id") %>%
  select(person_id, city, type_of_broadband_connection, name_of_isp, average_download_speed, average_upload_speed, avg_download_speed_60percentile)
View(final_table)


#DATA QUALITY

#Checking for basic summary of dataframe's such as data types, missing values, outliers, unique values 
dim(person)
diagnose(person)
describe_person <- describe(person)
print(describe_person)

dim(download)
diagnose(download)
describe_download <- describe(download)
print(describe_download)

dim(upload)
diagnose(upload)
describe_upload <- describe(upload)
print(describe_upload)

dim(final_table)
diagnose(final_table)
describe_final_table <- describe(final_table)
print(describe_final_table)

#count of people in different cities
city_sum <- df %>%
  group_by(city) %>%
  summarise(total_people = n_distinct(person_id))

#bar plot of the number of people in each city
ggplot(city_sum, aes(x = city, y = total_people)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = total_people), vjust = -0.5, color = "black", size = 6) +
  labs(x = "City", y = "Number of People", title = "Number of People in Different Cities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(size = 12, face = "bold"))


#Checking for Outliers
boxplot(download$measured_download_speed_in_Mbps,
        xlab = "Measured Download Speed (Mbps)",
        main = "Boxplot of Measured Download Speed")

boxplot(final_table$average_download_speed,
        xlab = "Measured Download Speed (Mbps)",
        main = "Boxplot of Average Measured Download Speed")

boxplot(upload$measured_upload_speed_in_Mbps,
        xlab = "Measured Upload Speed (Mbps)",
        main = "Boxplot of Measured Upload Speed")

boxplot(final_table$average_upload_speed,
        xlab = "Measured Upload Speed (Mbps)",
        main = "Boxplot of Average Measured Upload Speed")


#checking download dataframe whose test were successful 
#but did not produce any data i.e 0

#Creating a dataframe 'download1' with values 0 in 'measured_download_speed_in_Mbps' column and 'did_test_complete_successfully' is 'TRUE'
download1 <- download %>%
  filter(did_test_complete_successfully == TRUE & (measured_download_speed_in_Mbps == 0 | is.na(measured_download_speed_in_Mbps)))

#Now with download1 dataframe we are applying summation to know how much test with value 0 is for particular person
grouped_download <- download1 %>%
  group_by(person_id) %>%
  summarise(successful_tests_with_value_0 = sum(did_test_complete_successfully))

#merging the 'successful_tests' column with the 'df' dataframe and storing in new dataframe 'df1'
df1 <- final_table %>%
  left_join(grouped_download, by = "person_id")
View(df1)

#Sorting the df1 dataframe
sorted_df <- df1[order(df1$successful_tests_with_value_0), ]
View(sorted_df)

#Calculating count and sum of error test values
error_counts <- sorted_df %>%
  group_by(type_of_broadband_connection) %>%
  summarize(successful_tests_with_value_0 = n())

error_sum <- sorted_df %>%
  group_by(type_of_broadband_connection) %>%
  summarise(sum_error_test = sum(successful_tests_with_value_0))

merged_count_sum <- inner_join(error_counts, error_sum, by="type_of_broadband_connection") %>%
  select(type_of_broadband_connection, successful_tests_with_value_0, sum_error_test)

View(merged_count_sum)

#Barplot for count of error test value
ggplot(merged_count_sum, aes(x = type_of_broadband_connection, y = successful_tests_with_value_0)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = successful_tests_with_value_0), vjust = -0.5, color = "black", size = 6) +
  labs(x = "Type of Broadband Connection", y = "Count of Successful Tests",
       title = "Count of Successful Tests with 0 Value by Broadband Connection") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Barplot for Sum of error test value
ggplot(merged_count_sum, aes(x = type_of_broadband_connection, y = sum_error_test)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = sum_error_test), vjust = -0.5, color = "black", size = 6) +
  labs(x = "Type of Broadband Connection", y = "Sum of Successful Tests",
       title = "Sum of Successful Tests with 0 Value by Broadband Connection") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#In both cities the VSDL broadband connection with ISP Useus have more than 130+ error test values
#average_download_speed with 180+ Mbps is provided with 'Fibre' Broadband and error test value is less than equal to 12.


#finding mislabeled data
#with the download speed available for each broadband connection we are categorizing them into their highest and lowest download speed
expected_speed_range <- data.frame(type_of_broadband_connection = c("ADSL", "VDSL", "Fibre"),
                                   expected_speed_low = c(1, 10, 100),
                                   expected_speed_high = c(10, 100, 1000))


# Merge the expected speed ranges with the original dataframe
mislabeled_data_broadband <- merge(final_table, expected_speed_range, by = "type_of_broadband_connection")
View(mislabeled_data_broadband)

# Identify mislabeled data
mislabeled_data_broadband <- mislabeled_data_broadband %>%
  mutate(mislabeled = ifelse(average_download_speed < expected_speed_low | average_download_speed > expected_speed_high, "Yes", "No"))

# View the mislabeled data
View(mislabeled_data_broadband)

# Plot the mislabeled data
ggplot(mislabeled_data_broadband, aes(x = type_of_broadband_connection, fill = mislabeled)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "#009E73")) +
  labs(x = "Connection Type", y = "Count", fill = "Mislabeled") +
  ggtitle("Mislabeled Data by Connection Type")


#DATA VISUALIZATION
#creating summary of download and upload speed
download_summary <- sorted_df %>%
  group_by(city, type_of_broadband_connection, name_of_isp) %>%
  summarise(average_download_speed = mean(average_download_speed, na.rm = TRUE)) %>%
  pivot_wider(names_from = name_of_isp, values_from = average_download_speed) %>%
  mutate(type_of_broadband_connection = factor(type_of_broadband_connection,
                                               levels = c("ADSL", "VDSL", "Fibre")))
cat("Summary Table: Average Download Speed by ISP and Connection Type\n")
print(download_summary)

upload_summary <- sorted_df %>%
  group_by(city, type_of_broadband_connection, name_of_isp) %>%
  summarise(average_upload_speed = mean(average_upload_speed, na.rm = TRUE)) %>%
  pivot_wider(names_from = name_of_isp, values_from = average_upload_speed) %>%
  mutate(type_of_broadband_connection = factor(type_of_broadband_connection,
                                               levels = c("ADSL", "VDSL", "Fibre")))
cat("Summary Table: Average Download Speed by ISP and Connection Type\n")
print(upload_summary)

avg_download_speed <- sorted_df %>%
  group_by(city, type_of_broadband_connection, name_of_isp) %>%
  summarise(avg_down_speed = mean(average_download_speed))
View(avg_download_speed)

my_colors <- c("orange", "#0000FF")
ggplot(avg_download_speed, aes(x = interaction(type_of_broadband_connection, city), y = avg_down_speed, fill = name_of_isp)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = my_colors) +
  labs(x = "Connection Type - City", y = "Average Download Speed (Mbps)", fill = "ISP") +
  ggtitle("Average Download Speeds by ISP, City, and Connection Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Which ISP is showing better average speed in Databury
# Filter the data for Databury and Fibre connection
filtered_data <- sorted_df %>% filter(city == "Databury", type_of_broadband_connection == "Fibre")

# Calculate the average download speed for Fibrelicious in Databury
fibrelicious_speed <- filtered_data %>% filter(name_of_isp == "Fibrelicious") %>% summarise(avg_download_speed = mean(average_download_speed))

# Calculate the average download speed for Useus in Databury
useus_speed <- filtered_data %>% filter(name_of_isp == "Useus") %>% summarise(avg_download_speed = mean(average_download_speed))

# Compare the average download speeds
speed_comparison <- fibrelicious_speed$avg_download_speed - useus_speed$avg_download_speed

# Print the comparison
if (speed_comparison > 0) {
  cat("If you have a Fibre connection in Databury, Fibrelicious provides a better download speed by approximately ", round(speed_comparison, 2), " Mbps compared to Useus.")
} else if (speed_comparison < 0) {
  cat("If you have a Fibre connection in Databury, Useus provides a better download speed by approximately ", abs(round(speed_comparison, 2)), " Mbps compared to Fibrelicious.")
} else {
  cat("If you have a Fibre connection in Databury, both Fibrelicious and Useus provide similar download speeds.")
}


#Barchart on average download speed per hour by ISP
# Merge the person and download speed data based on person_id
merged_df <- inner_join(person, download, by = "person_id")
View(merged_df)

merged_df$time_of_measurement <- as.POSIXct(merged_df$time_of_measurement)

merged_df$hour <- format(merged_df$time_of_measurement, "%H")
View(merged_df)

filtered_data <- merged_df %>% filter(name_of_isp %in% c("Fibrelicious", "Useus"))
View(filtered_data)

avg_speed <- filtered_data %>%
  group_by(name_of_isp, hour) %>%
  summarise(avg_download_speed = mean(measured_download_speed_in_Mbps, na.rm = TRUE))
View(avg_speed)

avg_speed$hour <- factor(avg_speed$hour)

ggplot(avg_speed, aes(x = hour, y = avg_download_speed, fill = name_of_isp)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Hour of the Day", y = "Average Download Speed (Mbps)", fill = "ISP") +
  ggtitle("Average Download Speeds per Hour by both ISP") +
  theme_minimal()

