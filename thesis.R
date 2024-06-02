# This is the code for the paper
# R1 is the response for the Government, and R3 is for the Family. 
# All R1 – R8, SG1 – SG8 has been changed from the Excel. If you want to follow this code, data change from the excel should be done before this code.

# Packages needed
library(dplyr)
library(ggplot2)
library(rddtools)
library(cowplot)


# Read data from 2013 to 2022
data_list <- lapply(2013:2022, function(year) {
  data <- read.csv(“your address”)
  data$year <- year  # Add the year variable to each data frame
  data
})
names(data_list) <- paste0("data_", 2013:2022)

# Combine all the data from data_list into a single data frame
all_data <- bind_rows(data_list)

# Graph total time line
# Calculate the weighted average for each year
weighted_averages <- all_data %>%
  group_by(year) %>%
  summarise(weighted_average = weighted.mean(R1, w = wt2, na.rm = TRUE))

# Create the combined plot
combined_plot <- ggplot() +
  geom_smooth(data = subset(all_data, year < 2017), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = 0.8) +
  geom_smooth(data = subset(all_data, year >= 2017), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = 0.8) +
  geom_vline(xintercept = 2016.5, color = "green", size = 0.8, linetype = "dashed") +
  geom_point(data = weighted_averages, aes(x = year, y = weighted_average), color = "blue", size = 2) +
  labs(title = "Title you want", x = "Year", y = "Weighted Response Score") +
  theme_minimal() +
  coord_cartesian(ylim = c(number you want to see, number you want to see)) +
  scale_x_continuous(breaks = 2013:2022)

# Display the combined plot
print(combined_plot)

# Graph without COVID timeline
# Filter the data for the shorter timeline (2013-2019)
all_data_short <- all_data %>% filter(year >= 2013 & year <= 2019)

# Calculate the weighted average for each year in the shorter timeline
weighted_averages_short <- all_data_short %>%
  group_by(year) %>%
  summarise(weighted_average = weighted.mean(R1, w = wt2, na.rm = TRUE))

# Create the combined plot for the shorter timeline
combined_plot_short <- ggplot() +
  geom_smooth(data = subset(all_data_short, year < 2017), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = 0.8) +
  geom_smooth(data = subset(all_data_short, year >= 2017), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = 0.8) +
  geom_vline(xintercept = 2016.5, color = "green", size = 0.8, linetype = "dashed") +
  geom_point(data = weighted_averages_short, aes(x = year, y = weighted_average), color = "blue", size = 2) +
  labs(title = "title you want", x = "Year", y = "Weighted Response Score") +
  theme_minimal() +
  coord_cartesian(ylim = c(number you want to see, number you want to see)) +
  scale_x_continuous(breaks = 2013:2019)

# Display the combined plot for the shorter timeline
print(combined_plot_short)

# RDD static for Full timeline

# Create the RDD data object without weights
rdd_data_obj <- rdd_data(y = all_data$R1, x = all_data$year, cutpoint = 2016.5)

# Perform the weighted linear RDD regression
rdd_result <- rdd_reg_lm(rdd_data_obj, order = 1, slope = "separate", weights = all_data$wt2)

# View the summary of the RDD regression for the entire dataset
cat("RDD Result for the Entire Dataset:\n")
summary(rdd_result)

# RDD static for without COVID
# Filter the data for the shorter timeline (2013-2019)
all_data_short <- all_data %>% filter(year >= 2013 & year <= 2019)

# Create the RDD data object with the cutoff point at 2016.5
rdd_data_obj <- rdd_data(y = all_data_short$R1, x = all_data_short$year, cutpoint = 2016.5)

# Perform the weighted linear RDD regression
rdd_result <- rdd_reg_lm(rdd_data_obj, order = 1, slope = "separate", weights = all_data_short$wt2)

# View the summary of the RDD regression for the shorter timeline
cat("RDD Result for the Shorter Timeline (2013-2019):\n")
summary(rdd_result)

# For subgroup1 graph Full timeline
# Calculate the weighted average for each year and subgroup
weighted_averages_subgroups <- all_data %>%
  group_by(year, SG1) %>%
  summarise(weighted_average = weighted.mean(R1, w = wt2, na.rm = TRUE)) %>%
  mutate(subgroup = as.character(SG1),
         subgroup = case_when(
           subgroup == "1" ~ "Male",
           subgroup == "2" ~ "Female",
           TRUE ~ "Total"
         ))

# Create the combined plot with zoomed limits
line_thickness <- 0.8
ggplot() +
  geom_smooth(data = subset(all_data, year < 2017 & SG1 == 1), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG1 == 1), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data, year < 2017 & SG1 == 2), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG1 == 2), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_vline(xintercept = 2016.5, color = "green", size = line_thickness, linetype = "dashed") +
  geom_point(data = weighted_averages_subgroups, aes(x = year, y = weighted_average, color = subgroup), size = 2) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red", "Total" = "black")) +
  labs(title = "title you want", x = "Year", y = "Weighted Response Score", color = "Subgroup") +
  theme_minimal() +
  coord_cartesian(ylim = c(number you want to see, number you want to see)) +  # Adjust the y-axis limits to zoom in
  scale_x_continuous(breaks = 2013:2022)

# For subgroup1 graph without COVID timeline

# Filter the data for the shorter timeline (2013-2019)
all_data_short <- all_data %>% filter(year >= 2013 & year <= 2019)

# Calculate the weighted average for each year and subgroup in the shorter timeline
weighted_averages_subgroups_short <- all_data_short %>%
  group_by(year, SG1) %>%
  summarise(weighted_average = weighted.mean(R1, w = wt2, na.rm = TRUE)) %>%
  mutate(subgroup = as.character(SG1),
         subgroup = case_when(
           subgroup == "1" ~ "Male",
           subgroup == "2" ~ "Female",
           TRUE ~ "Total"
         ))

# Create the combined plot with zoomed limits for the shorter timeline
line_thickness <- 0.8
ggplot() +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG1 == 1), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG1 == 1), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG1 == 2), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG1 == 2), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_vline(xintercept = 2016.5, color = "green", size = line_thickness, linetype = "dashed") +
  geom_point(data = weighted_averages_subgroups_short, aes(x = year, y = weighted_average, color = subgroup), size = 2) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red", "Total" = "black")) +
  labs(title = "title you want", x = "Year", y = "Weighted Response Score", color = "Subgroup") +
  theme_minimal() +
  coord_cartesian(ylim = c(number you want to see, number you want to see)) + # Adjust the y-axis limits to zoom in
  scale_x_continuous(breaks = 2013:2019)

# Function to perform RDD regression for a subgroup
run_rdd_subgroup <- function(subgroup_data) {
  rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
  rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 1, weights = subgroup_data$wt2)
  return(rdd_result)
}

# Split the data by subgroup
subgroup_data <- split(all_data, all_data$SG1)

# Run RDD regression for each subgroup
rdd_results <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup
for (i in seq_along(rdd_results)) {
  subgroup_name <- names(rdd_results)[i]
  cat("\nRDD Result for Subgroup", subgroup_name, ":\n")
  print(summary(rdd_results[[i]]))
}
# without COVID timeline subgroup RDD 
robust_data <- all_data[all_data$year >= 2013 & all_data$year <= 2019, ]

# Function to perform RDD regression for a subgroup
run_rdd_subgroup <- function(subgroup_data) {
  rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
  rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 1, weights = subgroup_data$wt2)
  return(rdd_result)
} short_data

# Split the robust data by subgroup
subgroup_data <- split(robust_data, robust_data$SG1)

# Run RDD regression for each subgroup using the robust data
rdd_results_robust <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup using the robust data
for (i in seq_along(rdd_results_robust)) {
  subgroup_name <- names(rdd_results_robust)[i]
  cat("\nRDD Result for Subgroup", subgroup_name, "(Robust Timeline):\n")
  print(summary(rdd_results_robust[[i]]))
}

# Other subgroups are done in a similar way
# A few changes were made in some subgroups for better understanding and for the scope of the study
# For example, subgroup 5’s scale were added with similar groups

# RDD for the without COVID timeline for SG5
all_data_short <- all_data %>% filter(year >= 2013 & year <= 2019)

# Function to perform RDD regression for a subgroup (shorter timeline)
run_rdd_subgroup <- function(subgroup_data) {
  if (nrow(subgroup_data) > 0 && length(unique(subgroup_data$year)) >= 2) {
    rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
    rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 2)
    return(rdd_result)
  } else {
    return(NULL)
  }
}

# Split the data by subgroup (shorter timeline)
subgroup_data <- split(all_data_short, all_data_short$SG5)

# Run RDD regression for each subgroup (shorter timeline)
rdd_results <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup (shorter timeline)
for (i in seq_along(rdd_results)) {
  subgroup_name <- names(rdd_results)[i]
  if (!is.null(rdd_results[[i]])) {
    cat("\nRDD Result for Subgroup", subgroup_name, "(2013-2019):\n")
    print(summary(rdd_results[[i]]))
  } else {
    cat("\nInsufficient data for Subgroup", subgroup_name, "(2013-2019). RDD analysis not performed.\n")
  }
}

# Function to perform RDD regression for a subgroup SG5
run_rdd_subgroup <- function(subgroup_data) {
  rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
  rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 2)
  return(rdd_result)
}

# Split the data by subgroup
subgroup_data <- split(all_data, all_data$SG5)

# Run RDD regression for each subgroup
rdd_results <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup
for (i in seq_along(rdd_results)) {
  subgroup_name <- names(rdd_results)[i]
  cat("\nRDD Result for Subgroup", subgroup_name, ":\n")
  print(summary(rdd_results[[i]]))
}

# Graph for subgroup5 without COVID timeline
# Filter the data for the shorter timeline (2013-2019)
all_data_short <- all_data %>% filter(year >= 2013 & year <= 2019)

# Combine education levels into groups
all_data_short <- all_data_short %>%
  mutate(SG5_combined = case_when(
    SG5 %in% c("1", "2", "3") ~ "Pre-College",
    SG5 %in% c("4", "5") ~ "Undergraduate",
    SG5 %in% c("6", "7") ~ "Graduate",
    TRUE ~ NA_character_  # Set non-matching values to NA
  ))

# Calculate weighted averages for the combined education levels in the shorter timeline
weighted_averages_subgroups_short <- all_data_short %>%
  group_by(year, SG5_combined) %>%
  summarise(weighted_average = weighted.mean(R1, w = wt2, na.rm = TRUE))

# Create the combined plot with zoomed limits for the shorter timeline
line_thickness <- 0.8
ggplot() +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG5_combined == "Pre-College"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG5_combined == "Pre-College"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG5_combined == "Undergraduate"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG5_combined == "Undergraduate"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG5_combined == "Graduate"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "green", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG5_combined == "Graduate"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "green", size = line_thickness) +
  geom_vline(xintercept = 2016.5, color = "black", size = line_thickness, linetype = "dashed") +
  geom_point(data = subset(weighted_averages_subgroups_short, !is.na(SG5_combined)), aes(x = year, y = weighted_average, color = SG5_combined), size = 2) +
  scale_color_manual(values = c("Pre-College" = "red", "Undergraduate" = "blue", "Graduate" = "green")) +
  labs(title = "title you want", x = "Year", y = "Weighted Response Score", color = "Education Level") +
  theme_minimal() +
  coord_cartesian(ylim = c(number you want to see, number you want to see)) + # Adjust the y-axis limits to zoom in
  scale_x_continuous(breaks = 2013:2019)

# Graph of SG5 Full timeline 
# Combine education levels into groups
all_data <- all_data %>%
  mutate(SG5_combined = case_when(
    SG5 %in% c("1", "2", "3") ~ "Pre-College",
    SG5 %in% c("4", "5") ~ "Undergraduate",
    SG5 %in% c("6", "7") ~ "Graduate",
    TRUE ~ NA_character_  # Set non-matching values to NA
  ))

# Calculate weighted averages for the combined education levels
weighted_averages_subgroups <- all_data %>%
  group_by(year, SG5_combined) %>%
  summarise(weighted_average = weighted.mean(R1, w = wt2, na.rm = TRUE))

# Create the combined plot with zoomed limits
line_thickness <- 0.8
ggplot() +
  geom_smooth(data = subset(all_data, year < 2017 & SG5_combined == "Pre-College"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG5_combined == "Pre-College"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data, year < 2017 & SG5_combined == "Undergraduate"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG5_combined == "Undergraduate"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data, year < 2017 & SG5_combined == "Graduate"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "green", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG5_combined == "Graduate"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "green", size = line_thickness) +
  geom_vline(xintercept = 2016.5, color = "black", size = line_thickness, linetype = "dashed") +
  geom_point(data = subset(weighted_averages_subgroups, !is.na(SG5_combined)), aes(x = year, y = weighted_average, color = SG5_combined), size = 2) +
  scale_color_manual(values = c("Pre-College" = "red", "Undergraduate" = "blue", "Graduate" = "green")) +
  labs(title = "title you want", x = "Year", y = "Weighted Response Score", color = "Education Level") +
  theme_minimal() +
  coord_cartesian(ylim = c(number you want to see, number you want to see)) + # Adjust the y-axis limits to zoom in
  scale_x_continuous(breaks = 2013:2022)

# RDD for the SG5
# Combine education levels into groups
all_data <- all_data %>%
  mutate(SG5_combined = case_when(
    SG5 %in% c("1", "2", "3") ~ "Pre-College",
    SG5 %in% c("4", "5") ~ "Undergraduate",
    SG5 %in% c("6", "7") ~ "Graduate",
    TRUE ~ "Total"
  ))

# Split the data by the combined education subgroups
subgroup_data <- split(all_data, all_data$SG5_combined)

# Run RDD regression for each subgroup
rdd_results <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup
for (i in seq_along(rdd_results)) {
  subgroup_name <- names(rdd_results)[i]
  cat("\nRDD Result for Subgroup", subgroup_name, ":\n")
  print(summary(rdd_results[[i]]))
}

# RDD of without COVID SG5
# Filter the data for the shorter timeline (2013-2019)
all_data_short <- all_data %>% filter(year >= 2013 & year <= 2019)

########robust test for female and male
# Function to perform RDD regression for a subgroup
run_rdd_subgroup <- function(subgroup_data) {
  rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
  rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 1, weights = subgroup_data$wt2)
  return(rdd_result)
}

# Filter the data for the shorter timeline (2013-2019)
short_data <- all_data[all_data$year >= 2013 & all_data$year <= 2019, ]

# Combine education levels into groups
short_data <- short_data %>%
  mutate(SG5_combined = case_when(
    SG5 %in% c("1", "2", "3") ~ "Pre-College",
    SG5 %in% c("4", "5") ~ "Undergraduate",
    SG5 %in% c("6", "7") ~ "Graduate",
    TRUE ~ NA_character_  # Set non-matching values to NA
  ))

# Split the short data by the combined education subgroups
subgroup_data <- split(short_data, short_data$SG5_combined)

# Run RDD regression for each subgroup using the short data
rdd_results_short <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup using the short data
for (i in seq_along(rdd_results_short)) {
  subgroup_name <- names(rdd_results_short)[i]
  cat("\nRDD Result for Subgroup", subgroup_name, "(Short Timeline):\n")
  print(summary(rdd_results_short[[i]]))
}

# re-scale was done for SG8 too
# Read data from 2013 to 2022
data_list <- lapply(2013:2022, function(year) {
  data <- read.csv(paste0("ss", year, ".csv"))
  data$year <- year  # Add the year variable to each data frame
  data
})
names(data_list) <- paste0("data_", 2013:2022)

# Change 99 to NA because it is unknown
data_list$data_2013$SG8[data_list$data_2013$SG8 == 99] <- NA
data_list$data_2014$SG8[data_list$data_2014$SG8 == 99] <- NA

# Change 'no money' to 99
data_list$data_2014$SG8[data_list$data_2014$SG8 == 1] <- 99
data_list$data_2015$SG8[data_list$data_2015$SG8 == 1] <- 99
data_list$data_2016$SG8[data_list$data_2016$SG8 == 1] <- 99
data_list$data_2017$SG8[data_list$data_2017$SG8 == 1] <- 99
data_list$data_2018$SG8[data_list$data_2018$SG8 == 1] <- 99
data_list$data_2019$SG8[data_list$data_2019$SG8 == 1] <- 99
data_list$data_2020$SG8[data_list$data_2020$SG8 == 1] <- 99
data_list$data_2021$SG8[data_list$data_2021$SG8 == 1] <- 99
data_list$data_2022$SG8[data_list$data_2022$SG8 == 1] <- 99

# Change 2 to 1
data_list$data_2014$SG8[data_list$data_2014$SG8 == 2] <- 1
data_list$data_2015$SG8[data_list$data_2015$SG8 == 2] <- 1
data_list$data_2016$SG8[data_list$data_2016$SG8 == 2] <- 1
data_list$data_2017$SG8[data_list$data_2017$SG8 == 2] <- 1
data_list$data_2018$SG8[data_list$data_2018$SG8 == 2] <- 1
data_list$data_2019$SG8[data_list$data_2019$SG8 == 2] <- 1
data_list$data_2020$SG8[data_list$data_2020$SG8 == 2] <- 1
data_list$data_2021$SG8[data_list$data_2021$SG8 == 2] <- 1
data_list$data_2022$SG8[data_list$data_2022$SG8 == 2] <- 1

# Change 3 to 1 since there are too many layers
data_list$data_2013$SG8[data_list$data_2013$SG8 == 2] <- 1
data_list$data_2014$SG8[data_list$data_2014$SG8 == 3] <- 1
data_list$data_2015$SG8[data_list$data_2015$SG8 == 3] <- 1
data_list$data_2016$SG8[data_list$data_2016$SG8 == 3] <- 1
data_list$data_2017$SG8[data_list$data_2017$SG8 == 3] <- 1
data_list$data_2018$SG8[data_list$data_2018$SG8 == 3] <- 1
data_list$data_2019$SG8[data_list$data_2019$SG8 == 3] <- 1
data_list$data_2020$SG8[data_list$data_2020$SG8 == 3] <- 1
data_list$data_2021$SG8[data_list$data_2021$SG8 == 3] <- 1
data_list$data_2022$SG8[data_list$data_2022$SG8 == 3] <- 1

# Change 4 and 5 to 2 since there are too many layers
data_list$data_2013$SG8[data_list$data_2013$SG8 == 3] <- 2
data_list$data_2013$SG8[data_list$data_2013$SG8 == 4] <- 2
data_list$data_2014$SG8[data_list$data_2014$SG8 == 4] <- 2
data_list$data_2015$SG8[data_list$data_2015$SG8 == 4] <- 2
data_list$data_2016$SG8[data_list$data_2016$SG8 == 4] <- 2
data_list$data_2017$SG8[data_list$data_2017$SG8 == 4] <- 2
data_list$data_2018$SG8[data_list$data_2018$SG8 == 4] <- 2
data_list$data_2019$SG8[data_list$data_2019$SG8 == 4] <- 2
data_list$data_2020$SG8[data_list$data_2020$SG8 == 4] <- 2
data_list$data_2021$SG8[data_list$data_2021$SG8 == 4] <- 2
data_list$data_2022$SG8[data_list$data_2022$SG8 == 4] <- 2
data_list$data_2014$SG8[data_list$data_2014$SG8 == 5] <- 2
data_list$data_2015$SG8[data_list$data_2015$SG8 == 5] <- 2
data_list$data_2016$SG8[data_list$data_2016$SG8 == 5] <- 2
data_list$data_2017$SG8[data_list$data_2017$SG8 == 5] <- 2
data_list$data_2018$SG8[data_list$data_2018$SG8 == 5] <- 2
data_list$data_2019$SG8[data_list$data_2019$SG8 == 5] <- 2
data_list$data_2020$SG8[data_list$data_2020$SG8 == 5] <- 2
data_list$data_2021$SG8[data_list$data_2021$SG8 == 5] <- 2
data_list$data_2022$SG8[data_list$data_2022$SG8 == 5] <- 2

# Change 6 and 7 to 3 since there are too many layers
data_list$data_2013$SG8[data_list$data_2013$SG8 == 5] <- 3
data_list$data_2013$SG8[data_list$data_2013$SG8 == 6] <- 3
data_list$data_2014$SG8[data_list$data_2014$SG8 == 6] <- 3
data_list$data_2015$SG8[data_list$data_2015$SG8 == 6] <- 3
data_list$data_2016$SG8[data_list$data_2016$SG8 == 6] <- 3
data_list$data_2017$SG8[data_list$data_2017$SG8 == 6] <- 3
data_list$data_2018$SG8[data_list$data_2018$SG8 == 6] <- 3
data_list$data_2019$SG8[data_list$data_2019$SG8 == 6] <- 3
data_list$data_2020$SG8[data_list$data_2020$SG8 == 6] <- 3
data_list$data_2021$SG8[data_list$data_2021$SG8 == 6] <- 3
data_list$data_2022$SG8[data_list$data_2022$SG8 == 6] <- 3
data_list$data_2014$SG8[data_list$data_2014$SG8 == 7] <- 3
data_list$data_2015$SG8[data_list$data_2015$SG8 == 7] <- 3
data_list$data_2016$SG8[data_list$data_2016$SG8 == 7] <- 3
data_list$data_2017$SG8[data_list$data_2017$SG8 == 7] <- 3
data_list$data_2018$SG8[data_list$data_2018$SG8 == 7] <- 3
data_list$data_2019$SG8[data_list$data_2019$SG8 == 7] <- 3
data_list$data_2020$SG8[data_list$data_2020$SG8 == 7] <- 3
data_list$data_2021$SG8[data_list$data_2021$SG8 == 7] <- 3
data_list$data_2022$SG8[data_list$data_2022$SG8 == 7] <- 3

# Change 8 and 9 to 4 since there are too many layers
data_list$data_2013$SG8[data_list$data_2013$SG8 == 7] <- 4
data_list$data_2013$SG8[data_list$data_2013$SG8 == 8] <- 4
data_list$data_2014$SG8[data_list$data_2014$SG8 == 8] <- 4
data_list$data_2015$SG8[data_list$data_2015$SG8 == 8] <- 4
data_list$data_2016$SG8[data_list$data_2016$SG8 == 8] <- 4
data_list$data_2017$SG8[data_list$data_2017$SG8 == 8] <- 4
data_list$data_2018$SG8[data_list$data_2018$SG8 == 8] <- 4
data_list$data_2019$SG8[data_list$data_2019$SG8 == 8] <- 4
data_list$data_2020$SG8[data_list$data_2020$SG8 == 8] <- 4
data_list$data_2021$SG8[data_list$data_2021$SG8 == 8] <- 4
data_list$data_2022$SG8[data_list$data_2022$SG8 == 8] <- 4
data_list$data_2014$SG8[data_list$data_2014$SG8 == 9] <- 4
data_list$data_2015$SG8[data_list$data_2015$SG8 == 9] <- 4
data_list$data_2016$SG8[data_list$data_2016$SG8 == 9] <- 4
data_list$data_2017$SG8[data_list$data_2017$SG8 == 9] <- 4
data_list$data_2018$SG8[data_list$data_2018$SG8 == 9] <- 4
data_list$data_2019$SG8[data_list$data_2019$SG8 == 9] <- 4
data_list$data_2020$SG8[data_list$data_2020$SG8 == 9] <- 4
data_list$data_2021$SG8[data_list$data_2021$SG8 == 9] <- 4
data_list$data_2022$SG8[data_list$data_2022$SG8 == 9] <- 4

# Change 10, 11, and 12 to 5
data_list$data_2014$SG8[data_list$data_2014$SG8 == 10] <- 5
data_list$data_2015$SG8[data_list$data_2015$SG8 == 10] <- 5
data_list$data_2016$SG8[data_list$data_2016$SG8 == 10] <- 5
data_list$data_2017$SG8[data_list$data_2017$SG8 == 10] <- 5
data_list$data_2018$SG8[data_list$data_2018$SG8 == 10] <- 5
data_list$data_2019$SG8[data_list$data_2019$SG8 == 10] <- 5
data_list$data_2020$SG8[data_list$data_2020$SG8 == 10] <- 5
data_list$data_2021$SG8[data_list$data_2021$SG8 == 10] <- 5
data_list$data_2022$SG8[data_list$data_2022$SG8 == 10] <- 5
data_list$data_2014$SG8[data_list$data_2014$SG8 == 11] <- 5
data_list$data_2015$SG8[data_list$data_2015$SG8 == 11] <- 5
data_list$data_2016$SG8[data_list$data_2016$SG8 == 11] <- 5
data_list$data_2017$SG8[data_list$data_2017$SG8 == 11] <- 5
data_list$data_2018$SG8[data_list$data_2018$SG8 == 11] <- 5
data_list$data_2019$SG8[data_list$data_2019$SG8 == 11] <- 5
data_list$data_2020$SG8[data_list$data_2020$SG8 == 11] <- 5
data_list$data_2021$SG8[data_list$data_2021$SG8 == 11] <- 5
data_list$data_2022$SG8[data_list$data_2022$SG8 == 11] <- 5
data_list$data_2014$SG8[data_list$data_2014$SG8 == 12] <- 5
data_list$data_2015$SG8[data_list$data_2015$SG8 == 12] <- 5
data_list$data_2016$SG8[data_list$data_2016$SG8 == 12] <- 5
data_list$data_2017$SG8[data_list$data_2017$SG8 == 12] <- 5
data_list$data_2018$SG8[data_list$data_2018$SG8 == 12] <- 5
data_list$data_2019$SG8[data_list$data_2019$SG8 == 12] <- 5
data_list$data_2020$SG8[data_list$data_2020$SG8 == 12] <- 5
data_list$data_2021$SG8[data_list$data_2021$SG8 == 12] <- 5
data_list$data_2022$SG8[data_list$data_2022$SG8 == 12] <- 5
#
# Change 99 to 6
data_list$data_2014$SG8[data_list$data_2014$SG8 == 99] <- 6
data_list$data_2015$SG8[data_list$data_2015$SG8 == 99] <- 6
data_list$data_2016$SG8[data_list$data_2016$SG8 == 99] <- 6
data_list$data_2017$SG8[data_list$data_2017$SG8 == 99] <- 6
data_list$data_2018$SG8[data_list$data_2018$SG8 == 99] <- 6
data_list$data_2019$SG8[data_list$data_2019$SG8 == 99] <- 6
data_list$data_2020$SG8[data_list$data_2020$SG8 == 99] <- 6
data_list$data_2021$SG8[data_list$data_2021$SG8 == 99] <- 6
data_list$data_2022$SG8[data_list$data_2022$SG8 == 99] <- 6

# Combine all the data from data_list into a single data frame
all_data <- bind_rows(data_list)

# withtou COVID timeline RDD for SG8
all_data_short <- all_data %>% filter(year >= 2013 & year <= 2019)

# Function to perform RDD regression for a subgroup (shorter timeline)
run_rdd_subgroup <- function(subgroup_data) {
  if (nrow(subgroup_data) > 0 && length(unique(subgroup_data$year)) >= 2) {
    rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
    rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 2)
    return(rdd_result)
  } else {
    return(NULL)
  }
}

# Split the data by subgroup (shorter timeline)
subgroup_data <- split(all_data_short, all_data_short$SG8)

# Run RDD regression for each subgroup (shorter timeline)
rdd_results <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup (shorter timeline)
for (i in seq_along(rdd_results)) {
  subgroup_name <- names(rdd_results)[i]
  if (!is.null(rdd_results[[i]])) {
    cat("\nRDD Result for Subgroup", subgroup_name, "(2013-2019):\n")
    print(summary(rdd_results[[i]]))
  } else {
    cat("\nInsufficient data for Subgroup", subgroup_name, "(2013-2019). RDD analysis not performed.\n")
  }
}

# Full timeline RDD for SG8
# Function to perform RDD regression for a subgroup
run_rdd_subgroup <- function(subgroup_data) {
  rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
  rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 2)
  return(rdd_result)
}

# Split the data by subgroup
subgroup_data <- split(all_data, all_data$SG8)

# Run RDD regression for each subgroup
rdd_results <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup
for (i in seq_along(rdd_results)) {
  subgroup_name <- names(rdd_results)[i]
  cat("\nRDD Result for Subgroup", subgroup_name, ":\n")
  print(summary(rdd_results[[i]]))
}

# SG8 graph for full timeline
# Combine education levels into groups
all_data <- all_data %>%
  mutate(SG8_combined = case_when(
    SG8 %in% c("1", "6") ~ "~2 Million",
    SG8 %in% c("2") ~ "2-4 Million",
    SG8 %in% c("3") ~ "4-6 Million",
    SG8 %in% c("4", "5") ~ "6 Million~",
    TRUE ~ NA_character_  # Set non-matching values to NA
  ))

# Calculate weighted averages for the combined education levels
weighted_averages_subgroups <- all_data %>%
  group_by(year, SG8_combined) %>%
  summarise(weighted_average = weighted.mean(R1, w = wt2, na.rm = TRUE))

# Create the combined plot with zoomed limits
line_thickness <- 0.8
ggplot() +
  geom_smooth(data = subset(all_data, year < 2017 & SG8_combined == "~2 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG8_combined == "~2 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data, year < 2017 & SG8_combined == "2-4 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG8_combined == "2-4 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data, year < 2017 & SG8_combined == "4-6 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "green", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG8_combined == "4-6 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "green", size = line_thickness) +
  geom_smooth(data = subset(all_data, year < 2017 & SG8_combined == "6 Million~"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "purple", size = line_thickness) +
  geom_smooth(data = subset(all_data, year >= 2017 & SG8_combined == "6 Million~"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "purple", size = line_thickness) +
  
  geom_vline(xintercept = 2016.5, color = "black", size = line_thickness, linetype = "dashed") +
  geom_point(data = subset(weighted_averages_subgroups, !is.na(SG8_combined)), aes(x = year, y = weighted_average, color = SG8_combined), size = 2) +
  scale_color_manual(values = c("~2 Million" = "red", "2-4 Million" = "blue", "4-6 Million" = "green", "6 Million~" ="purple")) +
  labs(title = "title you want", x = "Year", y = "Weighted Response Score", color = "Income Level") +
  theme_minimal() +
  coord_cartesian(ylim = c(number you want to see, number you want to see)) + # Adjust the y-axis limits to zoom in
  scale_x_continuous(breaks = 2013:2022)

# without COVID timeline graph for the SG8
# Filter the data for the shorter timeline (2013-2019)
all_data_short <- all_data %>% filter(year >= 2013 & year <= 2019)

# Combine income levels into groups
all_data_short <- all_data_short %>%
  mutate(SG8_combined = case_when(
    SG8 %in% c("1", "6") ~ "~2 Million",
    SG8 %in% c("2") ~ "2-4 Million", 
    SG8 %in% c("3") ~ "4-6 Million",
    SG8 %in% c("4", "5") ~ "6 Million~",
    TRUE ~ NA_character_  # Set non-matching values to NA
  ))

# Calculate weighted averages for the combined income levels in the shorter timeline
weighted_averages_subgroups_short <- all_data_short %>%
  group_by(year, SG8_combined) %>%
  summarise(weighted_average = weighted.mean(R1, w = wt2, na.rm = TRUE))

# Create the combined plot with zoomed limits for the shorter timeline
line_thickness <- 0.8
ggplot() +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG8_combined == "~2 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG8_combined == "~2 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "red", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG8_combined == "2-4 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG8_combined == "2-4 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG8_combined == "4-6 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "green", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG8_combined == "4-6 Million"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "green", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year < 2017 & SG8_combined == "6 Million~"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "purple", size = line_thickness) +
  geom_smooth(data = subset(all_data_short, year >= 2017 & SG8_combined == "6 Million~"), aes(x = year, y = R1, weight = wt2), method = "lm", formula = y ~ x, se = FALSE, color = "purple", size = line_thickness) +
  geom_vline(xintercept = 2016.5, color = "black", size = line_thickness, linetype = "dashed") +
  geom_point(data = subset(weighted_averages_subgroups_short, !is.na(SG8_combined)), aes(x = year, y = weighted_average, color = SG8_combined), size = 2) +
  scale_color_manual(values = c("~2 Million" = "red", "2-4 Million" = "blue", "4-6 Million" = "green", "6 Million~" = "purple")) +
  labs(title = "title you want", x = "Year", y = "Weighted Response Score", color = "Income Level") +
  theme_minimal() +
  coord_cartesian(ylim = c(number you want to see, number you want to see)) + # Adjust the y-axis limits to zoom in
  scale_x_continuous(breaks = 2013:2019)

# Function to perform RDD regression for a subgroup
run_rdd_subgroup <- function(subgroup_data) {
  rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
  rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 1, weights = subgroup_data$wt2)
  return(rdd_result)
}

# Combine education levels into groups
all_data <- all_data %>%
  mutate(SG8_combined = case_when(
    SG8 %in% c("1", "6") ~ "~2 Million",
    SG8 %in% c("2") ~ "2-4 Million",
    SG8 %in% c("3") ~ "4-6 Million",
    SG8 %in% c("4", "5") ~ "6 Million~",
    TRUE ~ NA_character_  # Set non-matching values to NA
  ))
# Split the data by the combined education subgroups
subgroup_data <- split(all_data, all_data$SG8_combined)

# Run RDD regression for each subgroup
rdd_results <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup
for (i in seq_along(rdd_results)) {
  subgroup_name <- names(rdd_results)[i]
  cat("\nRDD Result for Subgroup", subgroup_name, ":\n")
  print(summary(rdd_results[[i]]))
}

# without COVID timeline RDD for SG8
# Function to perform RDD regression for a subgroup
run_rdd_subgroup <- function(subgroup_data) {
  rdd_data_obj <- rdd_data(y = subgroup_data$R1, x = subgroup_data$year, cutpoint = 2016.5)
  rdd_result <- rdd_reg_lm(rdd_data_obj, slope = "separate", order = 1, weights = subgroup_data$wt2)
  return(rdd_result)
}

# Filter the data for the shorter timeline (2013-2019)
short_data <- all_data %>% filter(year >= 2013 & year <= 2019)

# Combine income levels into groups
short_data <- short_data %>%
  mutate(SG8_combined = case_when(
    SG8 %in% c("1", "6") ~ "~2 Million",
    SG8 %in% c("2") ~ "2-4 Million",
    SG8 %in% c("3") ~ "4-6 Million",
    SG8 %in% c("4", "5") ~ "6 Million~",
    TRUE ~ NA_character_  # Set non-matching values to NA
  ))

# Split the short data by the combined income subgroups
subgroup_data <- split(short_data, short_data$SG8_combined)

# Run RDD regression for each subgroup using the short data
rdd_results_short <- lapply(subgroup_data, run_rdd_subgroup)

# Print the summary for each subgroup using the short data
for (i in seq_along(rdd_results_short)) {
  subgroup_name <- names(rdd_results_short)[i]
  cat("\nRDD Result for Subgroup", subgroup_name, "(Short Timeline):\n")
  print(summary(rdd_results_short[[i]]))
}
