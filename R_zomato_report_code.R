#Loading Necessary Libraries
library(tidyr)    # For data tidying
library(dplyr)    # For data manipulation
library(ggplot2)  # For data visualization

#Loading the Data
df = read.csv("C:/Users/dshre/Downloads/zomato.csv")

#Checking Column Names
colnames(df)

#Dropping Unnecessary Columns
columns_to_drop = c("Unnamed..0.1", "Unnamed..0")
existing_columns = columns_to_drop[columns_to_drop %in% colnames(df)]
df = df %>% select(-all_of(existing_columns))

#Quick Data Scan
glimpse(df)
summary(df)

#Handling Missing Values
missing_values <- df %>% summarise(across(everything(), ~sum(is.na(.))))
print("Missing Values per Column:")
print(missing_values)

df <- df %>%
  mutate(across(c(`rate..out.of.5.`, `avg.cost..two.people.`), ~replace_na(., mean(., na.rm = TRUE))))

#Checking for Duplicates:
duplicate_count = sum(duplicated(df))
print(paste("Number of duplicate rows:", duplicate_count))

#Visualizations and Insights

ggplot(df %>% count(restaurant.type, sort = TRUE) %>% head(10),
       aes(x = reorder(restaurant.type, n), y = n)) +
  geom_bar(stat = "identity",fill="Blue") + coord_flip() +
  labs(title = "Top 10 Most Common Restaurant Types")

ggplot(df %>% arrange(desc(avg.cost..two.people.)) %>% head(10),
       aes(x = reorder(restaurant.name, avg.cost..two.people.), y = avg.cost..two.people.)) +
  geom_bar(stat = "identity",fill="red") + coord_flip() +
  labs(title = "Top 10 Most Expensive Restaurants")

ggplot(df, aes(x = num.of.ratings)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Number of Ratings", x = "Number of Ratings", y = "Frequency") +
  theme_minimal()


# Count the frequency of each cuisine type and select the top 10
top_cuisines <- df %>%
  count(cuisines.type, sort = TRUE) %>%
  top_n(10, n)

# Create a bar plot of the top 10 cuisines by frequency
ggplot(top_cuisines, aes(x = reorder(cuisines.type, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  coord_flip() +
  labs(title = "Top Cuisines by Frequency", x = "Cuisine Type", y = "Frequency") +
  theme_minimal()


ggplot(df %>% arrange(avg.cost..two.people.) %>% head(10),
       aes(x = reorder(restaurant.name, avg.cost..two.people.), y = avg.cost..two.people.)) +
  geom_bar(stat = "identity",fill="blue") + coord_flip() +
  labs(title = "Top 10 Cheapest Restaurants")

ggplot(df, aes(x = "", fill = factor(online_order))) +
  geom_bar(width = 1) + coord_polar("y") +
  labs(title = "Online Order Distribution")

# Count the distribution of table booking options
table_booking_dist <- df %>%
  count(table.booking)

# Create a pie chart
ggplot(table_booking_dist, aes(x = "", y = n, fill = factor(table.booking))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Table Booking Distribution", fill = "Table Booking") +
  theme_void()


ggplot(df %>% count(area, sort = TRUE) %>% head(10),
       aes(x = reorder(area, n), y = n)) +
  geom_bar(stat = "identity",fill="skyblue") + coord_flip() +
  labs(title = "Top 10 Areas with Most Restaurants")

ggplot(df, aes(x = avg.cost..two.people., y = rate..out.of.5.)) +
  geom_point() +
  labs(title = "Ratings vs. Average Cost")


# Calculate specific descriptive statistics for a column
avg_cost_stats <- df %>%
  summarise(
    mean_cost = mean(avg.cost..two.people., na.rm = TRUE),
    median_cost = median(avg.cost..two.people., na.rm = TRUE),
    sd_cost = sd(avg.cost..two.people., na.rm = TRUE)
  )
print(avg_cost_stats)

# Simple linear regression model
linear_model <- lm(rate..out.of.5. ~ avg.cost..two.people., data = df)
summary(linear_model)

# Visualize the linear regression line
ggplot(df, aes(x = avg.cost..two.people., y = rate..out.of.5.)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Regression: Rating vs. Average Cost", x = "Average Cost for Two People", y = "Rating (Out of 5)")


# Multiple linear regression model
multiple_linear_model <- lm(rate..out.of.5. ~ avg.cost..two.people. + num.of.ratings, data = df)
summary(multiple_linear_model)

# Plot residuals to check the fit
ggplot(data = data.frame(residuals = residuals(multiple_linear_model)), aes(x = residuals)) +
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  labs(title = "Residuals of Multiple Linear Regression Model", x = "Residuals", y = "Frequency")

# New data to predict ratings
new_data <- data.frame(avg.cost..two.people. = c(300, 500), num.of.ratings = c(100, 50))

# Predict ratings based on new data
predictions <- predict(multiple_linear_model, newdata = new_data)
print(predictions)
