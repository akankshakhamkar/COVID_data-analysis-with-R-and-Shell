---
title: "As1"
output:
  word_document: default
  pdf_document: default
  html_document: default
date: "2023-07-20"
---

```{r}

# Install the required packages 
install.packages("tidyverse")
install.packages("lubridate")
install.packages("naniar")
install.packages("simputation")

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
 BiocManager::install("impute", version = "3.17")



# Load the packages
library(tidyverse)
library(lubridate)


#Loading datasets into R
library(readxl)
WorldwideVaccineData <- read_excel("~/Library/Mobile Documents/com~apple~Numbers/Documents/WorldwideVaccineData.xlsx")
library(readxl)
CountryLockdowndates <- read_excel("~/Downloads/CountryLockdowndates.xls")

library(readxl)
Covid_data <- read_excel("~/Library/Mobile Documents/com~apple~Numbers/Documents/Covid-data.xlsx")
View(Covid_data)


```


Exploring the datasets

```{r}
head(Covid_data)
summary(Covid_data)
str(Covid_data)
nrow(Covid_data)

head(WorldwideVaccineData)
summary(WorldwideVaccineData)
str(WorldwideVaccineData)
nrow(WorldwideVaccineData)

head(CountryLockdowndates)
summary(CountryLockdowndates)
str(CountryLockdowndates)
nrow(CountryLockdowndates)


```
Finding missing values in the three datasets

```{r}

library(naniar)
library(simputation)

missing_values <- sapply(WorldwideVaccineData, function(x) sum(is.na(x)))
print(missing_values)# No missing values found

missing_values_country <- sapply(CountryLockdowndates, function(x) sum(is.na(x)))
print(missing_values_country)

missing_values_covid <- sapply(Covid_data, function(x) sum(is.na(x)))
print(missing_values_covid)

```


Handling the missing datasets - 
CountryLockdownDates datasset has missing values in the "Province" column. I have decided to keep the missing values as they are because they may indicate that information about the province or territory is not available. The filter() function from the dplyr package is used to remove rows where the "Date" column has NA values. The !is.na(Date) part inside the filter() function checks for non-missing values in the "Date" column. Rows that meet this condition will be kept, and rows with missing dates will be removed.


```{r}

#Handling missing data in countrylockdowndates dataframe

sum(is.na(CountryLockdowndates$Date))

# Remove rows with missing dates in the "Date" column
CountryLockdowndates <- CountryLockdowndates %>%
  filter(!is.na(Date))


# Handling missing data in the Covid_data dataframe

# Sort the dataset based on 'date' (if not already sorted)
Covid_data <- Covid_data[order(Covid_data$total_cases), ]

# Identify missing values in 'new_deaths' and 'total_deaths'
missing_new_deaths <- which(is.na(Covid_data$new_deaths))
missing_total_deaths <- which(is.na(Covid_data$total_deaths))

# Handling missing data using median

Covid_data <- Covid_data %>% 
  mutate(new_deaths  = if_else(is.na(new_deaths), median(new_deaths, na.rm = TRUE), new_deaths),
         total_deaths = if_else(is.na(total_deaths), median(total_deaths, na.rm = TRUE), total_deaths))





```{r}
#The "Date" column is currently in character format. We can convert it to the date format using the as.Date() function in R. Convert the "Date" column to a proper date format for two datasets containing Date

CountryLockdowndates$Date <- as.Date(CountryLockdowndates$Date, format = "%Y/%m/%d")
Covid_data$date <- as.Date(Covid_data$date, format = "%Y-%m-%d")



#For CountryLockdowndates dataset we can remove the reference column as it is not needed for the analysis.

CountryLockdowndates$Reference <- NULL
CountryLockdowndates$Type <- NULL


#Saving this new cleaned dataset to new file

write.csv(CountryLockdowndates, "CountryLockdowndates_cleaned.csv", row.names = FALSE)
```

Standardising the location for Italy, Iran, United Kingdom and United States 

```{r}
# Load the required libraries
library(dplyr)

# Correct misspelled country names
Covid_data$location <- ifelse(tolower(Covid_data$location) == "iran", "Iran", Covid_data$location)
Covid_data$location <- ifelse(tolower(Covid_data$location) == "itly", "Italy", Covid_data$location)
Covid_data$location <- gsub("UnitedKingdom", "United Kingdom", Covid_data$location)
Covid_data$location <- gsub("United Stats", "United States", Covid_data$location)


# Group the data by 'Country/Region' in CountryLockdowndates dataset
filtered_data <- CountryLockdowndates[order(CountryLockdowndates$Date), ] %>%
  group_by(`Country/Region`) %>%
  summarise(Smallest_Date = min(Date),
            Province = first(Province))

# Print the result
print(filtered_data)


#Standardising titles for the three datasets

CountryLockdowndates <- CountryLockdowndates %>%
  rename(Country = `Country/Region`)

Covid_data <- Covid_data %>%
  rename(Country = `location`)
filtered_data <- filtered_data %>%
  rename(lockdown_date = `Smallest_Date`)

filtered_data <- filtered_data %>%
  rename(Country = `Country/Region`)

#Capitalise date in Covid_data dataset
colnames(Covid_data)[colnames(Covid_data) == "date"] <- "Date"


```

Joining datasets

```{r}
# Join covid_data with vaccination_data based on "Country" and "Date"
merged_data <- left_join(Covid_data, filtered_data, by = c("Country"))

# Join merged_data with population_data based on "Country" and "Date"
final_data <- left_join(merged_data, WorldwideVaccineData, by = c("Country"))

```

Tidying the joined data set

Explanation of Tidy Data Techniques:

Removing duplicates: We use the distinct() function to remove duplicate rows based on all columns, keeping only the first occurrence of each unique row.

Handling missing values: In this example, we use na.omit() to remove rows with any missing values. However, the approach for handling missing data can vary depending on the dataset and analysis requirements. Other techniques like imputation can be applied when appropriate.

Rearranging columns: We use the select() function from the dplyr package to reorder the columns as per the expected format. We assign new column names using the = operator.

```{r}
#Remove Province column
final_data <- final_data[, !names(final_data) %in% c("Province", "% of population vaccinated")]


# Check for duplicate rows
duplicate_rows <- final_data[duplicated(final_data), ]

#Check for missing values
missing_values <- colSums(is.na(final_data))
if (sum(missing_values) > 0) {
  cat("Warning: There are missing values in the dataset.\n")
}
final_data23 <- na.omit(final_data1)

# Remove duplicate rows (if any)
final_data <- final_data[!duplicated(final_data), ]

colnames(final_data23)[colnames(final_data23) == "Total doses administered"] <- "Total_doses_administered"
colnames(final_data23)[colnames(final_data23) == "% of population fully vaccinated"] <-"percent_population_fully_vaccinated"

head(final_data23)

# Rearrange the columns
library(dplyr)

final_data1 <- final_data23 %>%
  select(
    location = Country,
    Date = Date,
    total_cases = total_cases,
    new_cases = new_cases,
    total_deaths = total_deaths,
    new_deaths = new_deaths,
    gdp_per_capita = gdp_per_capita,
    population = population,
    lockdown_date = lockdown_date,
    Total_doses_administered = Total_doses_administered,
    percent_population_fully_vaccinated = %_population_fully_vaccinated 
  )

```

Creating plots for each country 


```{r}
# Assuming the joined and tidied dataset is named "final_data" and "percent_fully_vaccinated" column has been added

# Load required library
library(ggplot2)

# Create a line plot for each country
plot_new_cases <- ggplot(final_data1, aes(x = as.Date(Date), y = new_cases, group = location, color = location)) +
  geom_line() +
  labs(x = "Date", y = "New Cases", title = "Trend of New COVID-19 Cases by Country",
       subtitle = "Line Plot showing how the number of new cases has evolved over time for each country",
       caption = "Data Source: COVID-19 Dataset") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank()) +
  facet_wrap(~location, scales = "free_y")

# Print the line plot
print(plot_new_cases)


# Assuming the joined dataset is named "final_data"

library(ggplot2)

# Filter data for Australia
australia_data <- final_data1 %>% filter(location == "Australia")

# Create a line plot for new cases in Australia
ggplot(australia_data, aes(x = Date, y = new_cases)) +
  geom_line(color = "blue") +
  labs(title = "COVID New Cases in Australia",
       x = "Date",
       y = "New Cases") +
  theme_minimal()


# Filter data for France
france_data <- final_data1 %>% filter(location == "France")

# Create a line plot for new cases in France
ggplot(france_data, aes(x = Date, y = new_cases)) +
  geom_line(color = "yellow") +
  labs(title = "COVID New Cases in France",
       x = "Date",
       y = "New Cases") +
  theme_minimal()

# Filter data for Iran
Iran_data <- final_data1 %>% filter(location == "Iran")

# Create a line plot for new cases in Iran
ggplot(Iran_data, aes(x = Date, y = new_cases)) +
  geom_line(color = "green") +
  labs(title = "COVID New Cases in Iran",
       x = "Date",
       y = "New Cases") +
  theme_minimal()


# Filter data for Italy
Italy_data <- final_data1 %>% filter(location == "Italy")

# Create a line plot for new cases in Italy
ggplot(Italy_data, aes(x = Date, y = new_cases)) +
  geom_line(color = "red") +
  labs(title = "COVID New Cases in Italy",
       x = "Date",
       y = "New Cases") +
  theme_minimal()

# Filter data for Spain
Spain_data <- final_data1 %>% filter(location == "Spain")

# Create a line plot for new cases in Spain
ggplot(Spain_data, aes(x = Date, y = new_cases)) +
  geom_line(color = "pink") +
  labs(title = "COVID New Cases in Spain",
       x = "Date",
       y = "New Cases") +
  theme_minimal()

```

The exploratory analysis of COVID trends in different countries showed some interesting results. The line plot was used to depict the new cases over time for each country. It showed the progression of the pandemic and the impact it had on various events and case numbers. While analysing the plots for different countries, there is a sharp increase in new cases in Australia, France, Italy and Spain was observed. This could possibly mean that there were events with large gatherings, leading to a spread of the COVID-19 virus. Sharp reductions in plots for Australia, Frabce, Italy and Spain could be explained by timely and strict lockdown measures, increased number of individuals getting vaccinated or other public health interventions. 

Different countries showed different trends of new cases due to the varied government responses, population complaince with lockdowns and vaccinatons measures, and healthcare infrasturcture. Countries such as Australia had quicker declines in peaks because they implemented early and strict lockdowns. However, countries such as France, Italy and Spain had prolonged peaks because of the delay in lockdown measures. Vaccination rollouts played a crucial role in shaping the trend as well, countries having high vaccination rates had better control of the virus.

Furthermore, some countries such as Iran showed fluctuating trends, with periodic spikes in cases, which could be attributed to new variants of the virus or relaxed public adherence to preventive guidelines during certain periods. 

In conclusion, it is evident that a proactive approach, rapid response to emerging challenges, and a collective effort from governments and individuals are essential in managing and mitigating the impact of such global health crises.




#  The relation between the death rate or new case numbers with the GDP of each country

```{r}
# Calculate the mean GDP of all the countries

mean_gdp <- mean(final_data1$gdp_per_capita)
# Add 'GDP_Status' column to final_data1
final_data1$GDP_Status <- ifelse(final_data1$gdp_per_capita >= mean_gdp, "Higher", "Lower")

# Calculate the daily infected case rate and daily death rate for each country
final_data1$daily_infected_case_rate <- final_data1$new_cases / final_data1$population
final_data1$daily_death_rate <- final_data1$new_deaths / final_data1$population

# Round the daily infected case rate to 2 decimal places
final_data1$daily_infected_case_rate <- signif(final_data1$daily_infected_case_rate, 3)

# Round the daily death rate to 2 decimal places
final_data1$daily_death_rate <- signif(final_data1$daily_death_rate, 3)

# Load necessary libraries for plotting
library(ggplot2)

# Plot representing relationship between the daily infected case rate and GDP groups for Australia

Australia_infected_data <- final_data1 %>% filter(location == "Australia")

ggplot(Australia_infected_data, aes(x = Date, y = daily_infected_case_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Infected Case Rate for Australia",
       x = "Date",
       y = "Daily Infected Case Rate",
       color = "GDP Status") +
  theme_minimal()

# Plot representing relationship between the daily infected case rate and GDP groups for France

France_infected_data <- final_data1 %>% filter(location == "France")

ggplot(France_infected_data, aes(x = Date, y = daily_infected_case_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Infected Case Rate for France",
       x = "Date",
       y = "Daily Infected Case Rate",
       color = "GDP Status") +
  theme_minimal()

# Plot representing relationship between the daily infected case rate and GDP groups for Italy

Italy_infected_data <- final_data1 %>% filter(location == "Italy")

ggplot(Italy_infected_data, aes(x = Date, y = daily_infected_case_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Infected Case Rate for Italy",
       x = "Date",
       y = "Daily Infected Case Rate",
       color = "GDP Status") +
  theme_minimal()

# Plot representing relationship between the daily infected case rate and GDP groups for Iran

Iran_infected_data <- final_data1 %>% filter(location == "Iran")

ggplot(Iran_infected_data, aes(x = Date, y = daily_infected_case_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Infected Case Rate for Iran",
       x = "Date",
       y = "Daily Infected Case Rate",
       color = "GDP Status") +
  theme_minimal()

# Plot representing relationship between the daily infected case rate and GDP groups for Spain
Spain_infected_data <- final_data1 %>% filter(location == "Spain")

ggplot(Spain_infected_data, aes(x = Date, y = daily_infected_case_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Infected Case Rate for Spain",
       x = "Date",
       y = "Daily Infected Case Rate",
       color = "GDP Status") +
  theme_minimal()


# Plot representing relationship between the daily death rate and GDP status for Australia

Australia_deathrate_data <- final_data1 %>% filter(location == "Australia")

ggplot(Australia_deathrate_data, aes(x = Date, y = daily_death_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Death Rate within Australia",
       x = "Date",
       y = "Daily Death Rate",
       color = "GDP Status") +
  theme_minimal()

# Plot representing relationship between the daily death rate and GDP status for France
France_deathrate_data <- final_data1 %>% filter(location == "France")

ggplot(France_deathrate_data, aes(x = Date, y = daily_death_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Death Rate within France",
       x = "Date",
       y = "Daily Death Rate",
       color = "GDP Status") +
  theme_minimal()


# Plot representing relationship between the daily death rate and GDP status for Iran
Iran_deathratedata <- final_data1 %>% filter(location == "Iran")

ggplot(Iran_deathratedata, aes(x = Date, y = daily_death_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Death Rate within Iran",
       x = "Date",
       y = "Daily Death Rate",
       color = "GDP Status") +
  theme_minimal()

# Plot representing relationship between the daily death rate and GDP status for Italy

Italy_deathratedata <- final_data1 %>% filter(location == "Italy")

ggplot(Italy_deathratedata, aes(x = Date, y = daily_death_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Death Rate within Italy",
       x = "Date",
       y = "Daily Death Rate",
       color = "GDP Status") +
  theme_minimal()

# Plot representing relationship between the daily death rate and GDP status for Spain

Spain_deathrate_data <- final_data1 %>% filter(location == "Spain")

ggplot(Spain_deathrate_data, aes(x = Date, y = daily_death_rate, color = GDP_Status )) +
  geom_line() +
  labs(title = "Daily Death Rate within Spain",
       x = "Date",
       y = "Daily Death Rate",
       color = "GDP Status") +
  theme_minimal()

```

Daily Infected Case Rate by GDP Group:
In the High GDP group (countries with GDP per capita higher than or equal to the average), the daily infected case rate tends to fluctuate at a lower level over time. In the Low GDP group (countries with GDP per capita lower than the average), the daily infected case rate exhibits more variability and shows higher peaks compared to the High GDP group.


Daily Death Rate by GDP Group:
Similarly, in the High GDP group, the daily death rate remains relatively stable and starts to become lower over time.
Whereas, in the Low GDP group, the daily death rate shows a lot higher fluctuations and can reach higher levels during certain periods.

The findings suggest that there is a relationship between a country's GDP and its COVID-19 outbreak dynamics. Countries with higher GDPs generally have more robust healthcare systems, better access to medical facilities, advanced healthcare infrastructure, and higher healthcare spending. These factors can contribute to a more effective response to the pandemic, including widespread testing, contact tracing, and timely medical interventions, which can help control the spread of the virus and reduce the number of newly infected cases and deaths. In contrast, countries with lower GDPs may face several challenges in dealing with the pandemic. They might have limited healthcare resources, inadequate testing capabilities, and difficulties in implementing widespread public health measures. These factors can lead to a less effective response, resulting in higher infection rates and, consequently, higher death rates.

It is essential to consider that the relationship between GDP and COVID-19 outcomes is not solely deterministic, as other factors can also play significant roles. For example, government policies, public adherence to preventive measures, population density, and the timing of interventions can all influence the progression of the pandemic. It is important to remember that each country's situation is unique, and multiple factors contribute to the final outcomes.

PART 3:
To explore the relationships among newly infected cases, newly deaths, total doses administered, and % of population fully vaccinated.


```{r}

# Install and Load necessary libraries
install.packages("corrplot")
library(corrplot)

# Select the relevant columns for correlation analysis
cor_data <- final_data1[, c("new_cases", "new_deaths", "Total_doses_administered", "percentage_of_population_fully_vaccinated")]

# Calculate the correlation matrix
correlation_matrix <- cor(cor_data, method = "pearson")

# Create a correlation plot to visualize the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black", tl.srt = 45)

```

From the plot above we can say that there is. strong positive correlation between new cases and new deaths. This suggests that as the number of new cases increases in a country, there is a higher likelihood of an increase in the number of deaths. This aligns with the fact that higher rates of infections can lead to more fatalities.

We also observe a strong negative correlation between total doses administered and percentage of population fully vaccinated. This means that the when the total number doses being administered increases, the percent of population being fully vaccinated decreases. This finding was a little surprising but it can be explained by the time lag that occurs between administering the the first and second doses of a two dose vaccine. As more doses are given, the percentage fully vaccinated might not increase proportionally until individuals receive their second doses.  

Features that were least influential shown by the weak correlation was new cases and total doses administered. There is a positive weak correlation between them which means that the places with higher infection rates may also be prioritising vaccination efforts but relationship is not strong. A weak positive correlation is also observed between new deaths and total number of vaccine doses administered. This could imply that high death rates might increase the vaccination campaigns but this correlation is not strong.

Overall,correlation matrix helps us identify potential associations between variables, it does not indicate causative relationships. Further research and analysis are necessary to understand the underlying factors driving these correlations.


PART 4: 

```{r}



```








