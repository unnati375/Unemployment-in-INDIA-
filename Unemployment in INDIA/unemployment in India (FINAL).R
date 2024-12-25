library(dplyr)
library(ggplot2)
library(stats)
data <- read.csv("C:/Users/UTTATI/Downloads/Unemployment in India.csv")
head(data)
#countplot of Area
area_counts <- data %>%
  group_by(Area) %>%
  summarize(count = n())
ggplot(area_counts, aes(x = Area, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Count of Unemployment Rates by Area",
       x = "Area (Rural or Urban)",
       y = "Count") +
  theme_minimal()

# Create a count plot of regions with an overview of area
ggplot(data, aes(x = Region, fill = Area)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Unemployment Rates by Region",
       x = "Region",
       y = "Count of Records",
       fill = "Area (Rural or Urban)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

install.packages("lubridate")
library(lubridate)
data$year <- year(dmy(data$Date))

# Assuming your dataset is named 'unemployment_data'
# Create the boxplot
ggplot(data, aes(x = factor(year), y =  EstimatedUnemploymentRate)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Unemployment Rate by Year",
       x = "Year",
       y = "Estimated Unemployment Rate") +
  theme_minimal() 

# Create the scatterplot
ggplot(data, aes(x = EstimatedUnemploymentRate, y = factor(year))) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot of Unemployment Rate by Year",
       x = "Estimated Unemployment Rate",
       y = "Year") +
  theme_minimal() 

# Create the scatterplot
ggplot(data, aes(x =  EstimatedLabourParticipationRate, y = factor(Region))) +
  geom_point(color = "green") +
  labs(title = "Scatterplot of Labor Participation Rate by Region",
       x = "Estimated Labor Participation Rate",
       y = "Region") +
  theme_minimal()


# Create the scatterplot
ggplot(data, aes(x = EstimatedUnemploymentRate, y = factor(Region), color = factor(year))) +
  geom_point(size = 2, alpha = 0.7) +  # Adjust size and transparency of points
  labs(title = "Scatterplot of Unemployment Rate by Region (Colored by Year)",
       x = "Estimated Unemployment Rate",
       y = "Region",
       color = "Year") +  # Add legend title
  theme_minimal() +
  theme(legend.position = "right")  # Adjust legend position if needed

# Convert year to numeric if it's not already
data$year <- as.numeric(as.character(data$year))

# Calculate the correlation
correlation <- cor(data$year,data$EstimatedUnemploymentRate, use = "complete.obs")
print(correlation)

# Perform the t-test
t_test_result <- t.test(EstimatedUnemploymentRate ~ Area, data = data)
print(t_test_result)

# Perform the ANOVA test
anova_result <- aov(EstimatedUnemploymentRate ~ Region, data = data)
summary(anova_result)

install.packages("corrplot")
library(corrplot)
# Assuming your dataset is named 'unemployment_data'
# Select only numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Plot the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numeric Variables", mar = c(0,0,1,0))


# Create the line plot
ggplot(data, aes(x = year, y =  EstimatedLabourParticipationRate, color = Area, group = Area)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Labor Participation Rate Over the Years by Area",
       x = "Year",
       y = "Estimated Labor Participation Rate",
       color = "Area") +
  theme_minimal()

correlation_lp_ur <- cor(data$EstimatedLabourParticipationRate, data$EstimatedUnemploymentRate, use="complete.obs")
print(paste("Correlation between Labor Participation Rate and Unemployment Rate:", correlation_lp_ur))

correlation_emp_ur <- cor(data$Estimated.Employed, data$EstimatedUnemploymentRate, use="complete.obs")
print(paste("Correlation between Employment and Unemployment Rate:", correlation_emp_ur))

two_way_anova <- aov(EstimatedUnemploymentRate ~ Region * Area, data = data)
summary(two_way_anova) 

t_test_result <- t.test(EstimatedUnemploymentRate ~ year, data = data)
print(t_test_result)