#-----------1.Bar Chart-------------

install.packages("readxl")
install.packages("ggplot2")

# Load necessary library
library(readxl)
library(ggplot2)

file_path <- "cleaning_opendatarelease.xlsx" 
data <- read_excel(file_path)

#Access Method Frequency Bar Chart

# Create a data frame with access method counts
access_methods <- data.frame(
  Method = c("Online", "Telephone", "Email", "By Post", "In Person"),
  Count = c(524, 231, 200, 75, 154)
)

# Generate the bar chart
ggplot(access_methods, aes(x = Method, y = Count, fill = Method)) +
  geom_bar(stat = "identity", colour = "black") + # Add black borders
  scale_fill_manual(
    values = c(
      "Online" = "#FF4500",   # Highlight Online with a bright color
      "Telephone" = "gray70",
      "Email" = "gray70",
      "By Post" = "gray70",
      "In Person" = "gray70"
    )
  ) +
  labs(
    title = "Access Methods Frequency",
    subtitle = "This graph shows the number of respondents using each access method for services.",
    x = "Access Method",
    y = "Number of Respondents",
    caption = "Data Source: https://www.data.gov.uk/dataset/9710aaaf-b360-4946-b8b2-ede8d98bc748/improving-public-engagement-survey-january-2016"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none" # Removes the legend
  )

#----------2.Pie Chart------------

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Load the dataset
file_path <- "cleaning_opendatarelease.xlsx" 
data <- read_excel(file_path)

# Extract relevant columns for activities
relevant_columns <- c("Activity_Look_Info", "Activity_Pay_Services",
                      "Activity_Report_Issues", "Activity_Request_Services", 
                      "Activity_Send_Comments", "Activity_Watch webcasts")
service_data <- data[, relevant_columns]

# Reshape the data into long format
service_long <- service_data %>%
  pivot_longer(cols = everything(), names_to = "Activity", values_to = "Response")

# Count "YES" responses for each activity
service_activity <- service_long %>%
  filter(Response == "YES") %>%
  group_by(Activity) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Activity = recode(Activity,
                           "Activity_Look_Info" = "Looking for Information",
                           "Activity_Pay_Services" = "Paying for Service",
                           "Activity_Report_Issues" = "Reporting Issues",
                           "Activity_Request_Services" = "Requesting Services",
                           "Activity_Send_Comments" = "Sending Comments",
                           "Activity_Watch webcasts" = "Watching Webcasts"))

# Generate the pie chart
ggplot(service_activity, aes(x = "", y = Count, fill = Activity)) +
  geom_bar(stat = "identity", width = 1, colour = "black") +
  coord_polar(theta = "y", start = 0) + # Convert to pie chart and start at top
  labs(
    title = "Service Activity Distribution",
    subtitle = "Proportion of various service activities",
    caption = "Data Source: https://www.data.gov.uk/dataset/9710aaaf-b360-4946-b8b2-ede8d98bc748/improving-public-engagement-survey-january-2016"
  ) +
  theme_void() + # Simplify chart layout for pie
  theme(
    legend.position = "right",
    plot.caption = element_text(hjust = 0, size = 10) # Align caption to the right
  ) +
  geom_text(aes(label = paste0(round(Count / sum(Count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4) + # Add percentage labels
  scale_fill_manual(
    values = c(
      "Looking for Information" = "#FF4500", 
      "Paying for Service" = "#FFA07A", 
      "Reporting Issues" = "gray70", 
      "Requesting Services" = "#8da0cb", 
      "Sending Comments" = "#66c2a5", 
      "Watching Webcasts" = "#FFD700" # Add distinct colors for each activity
    )
  )



#-----------3.Stack Bar--------------

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Load the dataset
file_path <- "cleaning_opendatarelease.xlsx" 
data <- read_excel(file_path)

# Extract relevant columns for Age Group and Device responses
relevant_columns <- c("Age_Group", "PC", "Mobile phone", "Tablet")
data <- data[, relevant_columns]

# Reshape data into long format
data_long <- data %>%
  pivot_longer(cols = c(PC, `Mobile phone`, Tablet), 
               names_to = "Device", 
               values_to = "Response")

# Count "YES" responses for each device by Age Group
data_count <- data_long %>%
  filter(Response == "YES") %>%
  group_by(Age_Group, Device) %>%
  summarise(Count = n(), .groups = "drop")

# Normalize the data to percentages by Age Group
data_percentage <- data_count %>%
  group_by(Age_Group) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Rename devices for the legend
data_percentage <- data_percentage %>%
  mutate(Device = recode(Device, 
                         "PC" = "Computer",
                         "Mobile phone" = "Mobile",
                         "Tablet" = "Tablet"))

# Plot the percentage stacked bar chart with black borders
ggplot(data_percentage, aes(x = Age_Group, y = Percentage, fill = Device)) +
  geom_bar(stat = "identity", position = "stack", color = "black") + # Add black border
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  labs(
    title = "Percentage of Device Usage by Age Group",
    subtitle = "Distribution of devices used for online access across age groups",
    caption = "Data Source: https://www.data.gov.uk/dataset/9710aaaf-b360-4946-b8b2-ede8d98bc748/improving-public-engagement-survey-january-2016",
    x = "Age Group",
    y = "Percentage of Device Usage for Online Access",
    fill = "Device"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Computer" = "#FF4500", 
                               "Mobile" = "#8da0cb", 
                               "Tablet" = "#66c2a5"))


#-------------4.Heat map--------------

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl) # To load Excel files

file_path <- "cleaning_opendatarelease.xlsx" # Replace with your actual file path
data <- read_excel(file_path)

# Update relevant_columns based on actual column names
relevant_columns <- c("Age_Group", "Activity_Look_Info", "Activity_Pay_Services",
                      "Activity_Report_Issues", "Activity_Request_Services", 
                      "Activity_Send_Comments", "Activity_Watch webcasts") # Corrected the column name

# Filter the dataset for the relevant columns
data <- data[, relevant_columns]

# Reshape data into long format
data_long <- data %>%
  pivot_longer(cols = -Age_Group, names_to = "Activity", values_to = "Response")

# Rename activities to more descriptive names
data_long <- data_long %>%
  mutate(Activity = recode(Activity,
                           "Activity_Look_Info" = "Looking for Information",
                           "Activity_Pay_Services" = "Paying for Service",
                           "Activity_Report_Issues" = "Reporting Issues",
                           "Activity_Request_Services" = "Requesting Services",
                           "Activity_Send_Comments" = "Sending Comments",
                           "Activity_Watch webcasts" = "Watching Webcasts")) # Corrected column name and renamed

# Filter only "YES" responses and count them by Age_Group and Activity
activity_counts <- data_long %>%
  filter(Response == "YES") %>%
  group_by(Age_Group, Activity) %>%
  summarise(Count = n(), .groups = "drop")

# Ensure all combinations of Age_Group and Activity are included
all_combinations <- expand.grid(
  Age_Group = unique(data$Age_Group),
  Activity = unique(data_long$Activity)
)

# Merge the full combinations with the actual counts, filling missing values with 0
activity_counts_full <- all_combinations %>%
  left_join(activity_counts, by = c("Age_Group", "Activity")) %>%
  mutate(Count = replace_na(Count, 0))

# Create the heatmap
ggplot(activity_counts_full, aes(x = Activity, y = Age_Group, fill = Count)) +
  geom_tile(color = "white") +  # Add white borders for better visibility
  scale_fill_gradient(low = "#8da0cb", high = "#FF4500", name = "Count") +
  labs(
    title = "Intensity of Activities by Age Group",
    subtitle = "Heatmap showing the count of each activity performed by respondents",
    caption = "Data Source: https://www.data.gov.uk/dataset/9710aaaf-b360-4946-b8b2-ede8d98bc748/improving-public-engagement-survey-january-2016",
    x = "Activity",
    y = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid = element_blank(),  # Remove gridlines for a cleaner look
    plot.caption = element_text(hjust = 1)  # Align caption to the left
  )

#-------------END------------------