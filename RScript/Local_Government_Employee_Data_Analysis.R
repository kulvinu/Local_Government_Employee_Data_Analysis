library(dplyr)
library(ggplot2)

# Importing dataset
staff_info <- read.csv("assessment-1-k2443219-1.csv")

# Custom color scale
cbp2 <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00")
cbp3 <- c("#648FFF", "#785EF0", "#DC267F", "#FE6100", 
          "#FFB000", "#009E73")

# Looking into the distributions
# Distribution of 'department'
ggplot(staff_info, aes(x = department, fill = department)) +
  geom_bar() +
  scale_fill_manual(values = cbp2) + # Use the custom color palette
  scale_y_continuous(breaks = seq(0, 200, by = 20)) + # Scaled y-axis with breaks
  labs(
    title = "Distribution of Departments",
    x = "Department",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    #panel.grid.major.x = element_blank(), # Remove vertical gridlines
    #panel.grid.major.y = element_line(color = "gray", linetype = "dashed"), # Dashed horizontal gridlines
    #panel.grid.minor = element_blank() # Remove minor gridlines
  )

# Distribution of 'time'
ggplot(staff_info, aes(x = factor(time, levels = c("Less than 1 year", "1 year", "2 years", "3-5 years", "6-10 years", "More than 10 years")), fill = time)) +
  geom_bar() +
  scale_fill_manual(values = cbp2)+
  scale_y_continuous(breaks = seq(0, 100, by = 20))+
  labs(
    title = "Distribution of Time Employed",
    x = "Time Employed",
    y = "Count"
  ) +
  theme_minimal()+
  theme(
    legend.position = "none")

# Distribution of 'sick_days'
ggplot(staff_info, aes(x = sick_days)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red", size = 1, adjust=1) +
  labs(title = "Distribution of Sick Days", x = "Sick Days", y = "Frequency") +
  theme_minimal()

# Create a histogram with KDE overlay for 'sick_days'
ggplot(staff_info, aes(x = sick_days)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +  # Histogram
  geom_density(kernel='gaussian' ,color = "red", size = 1, adjust = 2) +  # KDE line
  labs(title = "Distribution of Sick Days", x = "Sick Days", y = "Frequency") +
  theme_minimal()

# Distribution of 'response_target'
ggplot(staff_info, aes(x = response_target)) +
  geom_histogram(binwidth = 2, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "blue", size = 1) +
  labs(title = "Distribution of Response Target", x = "Response Target (%)", y = "Frequency") +
  theme_minimal()

# Distribution of 'ftime'
ggplot(staff_info, aes(x = factor(ftime, levels = c("Less than 1 year", "1 year", "2 years", "3-5 years", "6-10 years", "More than 10 years")), fill = ftime)) +
  geom_bar() +
  scale_fill_manual(values = cbp3)+
  scale_y_continuous(breaks = seq(0, 100, by = 20))+
  labs(title = "Distribution of Full-Time Duration", x = "Full-Time Duration", y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "none")

# Question 2
# Numerical Variables
summary(staff_info[c("sick_days", "response_target")]) -> stats
# Adding IQR to the summary table
IQR_sick_days <- IQR(staff_info$sick_days)
IQR_response_target <- IQR(staff_info$response_target, na.rm = TRUE)

# Display the summary and IQR values
stats <- rbind(stats, 
               c("IQR" = IQR_sick_days, IQR_response_target))

# Categorical Variables
staff_info |> count(department)
staff_info |> count(time)
staff_info |> count(ftime)

# Question 3
# Group by department
response_by_department <- staff_info |> 
  group_by(department) |> 
  summarise(
    mean_response = mean(response_target, na.rm = TRUE),
    sd_response = sd(response_target, na.rm = TRUE),
    Median_Response_Target = median(response_target, na.rm = TRUE),
    IQR_Response_Target = IQR(response_target, na.rm = TRUE),
    Q1 = quantile(response_target, 0.25, na.rm = TRUE),
    Q3 = quantile(response_target, 0.75, na.rm = TRUE)
  )
print(response_by_department)

# Group by time
response_by_time <- staff_info |> 
  group_by(time) |> 
  summarise(
    mean_response = mean(response_target, na.rm = TRUE),
    Median_Response_Target = median(response_target, na.rm = TRUE),
    IQR_Response_Target = IQR(response_target, na.rm = TRUE),
    sd_response = sd(response_target, na.rm = TRUE),
    Q1 = quantile(response_target, 0.25, na.rm = TRUE),
    Q3 = quantile(response_target, 0.75, na.rm = TRUE)
  )
print(response_by_time)

summary_stats <- staff_info %>%
  group_by(department, time) %>%
  summarise(
    Mean_Response_Target = mean(response_target, na.rm = TRUE),
    Median_Response_Target = median(response_target, na.rm = TRUE),
    IQR_Response_Target = IQR(response_target, na.rm = TRUE),
    SD_Response_Target = sd(response_target, na.rm = TRUE),
    .groups = 'drop'
  )

# Print the summary table
print(summary_stats)

# Question 4
staff_info$time <- factor(staff_info$time, levels = c("Less than 1 year", "1 year", "2 years", "3-5 years", "6-10 years", "More than 10 years"))
ggplot(staff_info, aes(x = time, y = response_target)) +
  geom_boxplot(fill = "#117733", color = "black") +
  labs(title = "Response Target by Employment Time", x = "Time", y = "Response Target (%)") +
  theme_minimal()

# Question 5
ggplot(staff_info, aes(x = sick_days, y = response_target)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Relationship Between Sick Days and Response Target",
       x = "Sick Days",
       y = "Response Target (%)") +
  theme_minimal()


summary_data <- staff_info %>%
  group_by(ftime) %>%
  summarise(mean_response = mean(response_target, na.rm = TRUE))

# Create the line chart
ggplot(summary_data, aes(x = factor(ftime,levels = c("Less than 1 year", "1 year", "2 years", "3-5 years", "6-10 years", "More than 10 years")), y = mean_response)) +
  geom_line(group = 1, color = "blue", size = 1) +  # Line connecting the points
  geom_point(color = "red", size = 3) +             # Points on the line
  labs(title = "Average Response Target by Full-Time Duration",
       x = "Full-Time Duration (Years)",
       y = "Average Response Target (%)") +
  scale_x_discrete(labels = c("Less than 1 year", "1 year", "2 years", "3-5 years", "6-10 years", "More than 10 years")) +
  theme_minimal()
