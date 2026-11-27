# =====================================================
# PROJECT: INVESTIGATING GHANA'S INFLATION TRENDS (CPI)
# ORGANISATION: GHANA STATISTICAL SERVICE (GSS)
# SOFTWARE: R
# DATA SOURCE: GHANA STATISTICAL SERVICE - STATSBANK
# AUTHOR : ASAMOAH SAMUELLA APAW ABENA
# =====================================================

# OBJECTIVE:
# To analyse Ghana's monthly CPI inflation data.
# The analysis will examine headline inflation,
# product categories, local and imported inflation,
# and inflation trends across different decades.
# Load the packages required for data manipulation
install.packages(c("readr", "dplyr", "tibble"))
library(readr)
library(dplyr)
library(tibble)
"readr" %in% rownames(installed.packages())
"tibble" %in% rownames(installed.packages())
library(readr)
library(readr)
# Import the original Ghana CPI dataset
# Store the imported CPI dataset under a simple name
cpi_data <- CPI_20261125_103744
# =====================================================
# PROJECT DATA IMPORT
# =====================================================

# Load the original Ghana CPI Excel dataset
cpi_data <- as.data.frame(
  readxl::read_excel(file.choose())
)


# QUESTION 1: ANALYSING HEADLINE INFLATION

# Select records for All products to represent headline inflation
headline <- cpi_data[cpi_data$items == "All products", ]
# Check the number of headline inflation observations
dim(headline)
# Identify the highest headline inflation rate
max(headline$all_sources, na.rm = TRUE)
# Identify the month and year of the highest inflation rate
headline[which.max(headline$all_sources), c("months", "all_sources")]
# Identify the lowest headline inflation rate
min(headline$all_sources, na.rm = TRUE)
# Identify the lowest headline inflation rate
min(headline$all_sources, na.rm = TRUE)
min(headline$all_sources, na.rm = TRUE)
headline[which.min(headline$all_sources), c("months", "all_sources")]
# Identify the five months with the highest headline inflation
head(headline[order(-headline$all_sources), c("months", "all_sources")], 5)
# Plot the movement of headline inflation over time
plot(headline$month_date, headline$all_sources,
     type = "l",
     main = "Ghana Headline Inflation (1999–2026)",
     xlab = "Year",
     ylab = "Inflation Rate (%)")
# Convert the month variable into a proper date format
headline$month_date <- as.Date(
  paste0(gsub("MO", "-", headline$months), "-01"),
  format = "%Y-%m-%d"
)
# Plot headline inflation trends from 1999 to 2026
plot(headline$month_date, headline$all_sources,
     type = "l",
     main = "Ghana Headline Inflation (1999–2026)",
     xlab = "Year",
     ylab = "Inflation Rate (%)")
head(headline$month_date, 10)
sum(is.na(headline$month_date))
# Convert month codes into proper dates
headline$month_date <- as.Date(
  paste0(
    substr(headline$months, 1, 4), "-",
    sprintf("%02d", as.numeric(sub(".*MO", "", headline$months))),
    "-01"
  )
)
# Calculate the average headline inflation rate
mean(headline$all_sources, na.rm = TRUE)
# Calculate the median headline inflation rate
median(headline$all_sources, na.rm = TRUE)
# Identify the lowest headline inflation rate
min(headline$all_sources, na.rm = TRUE)

# Identify the month when the lowest rate occurred
headline[which.min(headline$all_sources), c("months", "all_sources")]
# Convert month codes into proper dates
month_number <- as.numeric(sub(".*MO", "", headline$months))

year_number <- as.numeric(substr(headline$months, 1, 4))

headline$month_date <- as.Date(
  sprintf("%04d-%02d-01", year_number, month_number)
)
# Extract the year and month from the month codes
year_number <- as.numeric(substr(headline$months, 1, 4))
month_number <- as.numeric(sub(".*MO", "", headline$months))

# Combine the year and month into proper dates
headline$month_date <- as.Date(
  sprintf("%04d-%02d-01", year_number, month_number)
)
# Check that the dates were converted successfully
sum(is.na(headline$month_date))
# Extract the year and month from the month codes
year_number <- as.numeric(substr(headline$months, 1, 4))
month_number <- as.numeric(sub(".*MO", "", headline$months))

# Combine the year and month into proper dates
headline$month_date <- as.Date(
  sprintf("%04d-%02d-01", year_number, month_number)
)
head(headline, 3)
# Convert month codes into proper dates
month_number <- as.numeric(sub(".*mo", "", headline$months))
year_number <- as.numeric(substr(headline$months, 1, 4))

headline$month_date <- as.Date(
  sprintf("%04d-%02d-01", year_number, month_number)
)
head(headline$month_date, 5)
# Arrange headline inflation from oldest to newest
headline <- headline[order(headline$months), ]

# Plot headline inflation trends
plot(headline$all_sources,
     type = "l",
     main = "Ghana Headline Inflation (1999–2026)",
     xlab = "Monthly observations (1999–2026)",
     ylab = "Inflation Rate (%)")
# Prepare the headline data for a chronological time-series graph

# Extract the year from the month variable
headline$year <- as.numeric(substr(headline$months, 1, 4))

# Extract the month number
headline$month_number <- as.numeric(sub(".*mo", "", headline$months))

# Create a chronological time index
headline$time_index <- headline$year * 12 + headline$month_number

# Arrange the observations from oldest to newest
headline <- headline[order(headline$time_index), ]
# Recreate the headline dataset
headline <- cpi_data[cpi_data$items == "All products", ]

# Extract year and month from the month code
headline$year <- as.numeric(substr(headline$months, 1, 4))
headline$month_number <- as.numeric(sub(".*mo", "", headline$months))

# Create a proper date index
headline$time_index <- headline$year * 12 + headline$month_number

# Keep valid headline observations from January 1999
headline <- headline[!is.na(headline$all_sources) & headline$year >= 1999, ]

# Arrange from oldest to newest
headline <- headline[order(headline$time_index), ]

# Draw the final headline inflation graph
plot(headline$time_index, headline$all_sources,
     type = "l",
     xaxt = "n",
     main = "Ghana Headline Inflation Trend (1999–2026)",
     xlab = "Year",
     ylab = "Headline Inflation Rate (%)")

# Display calendar years on the horizontal axis
year_positions <- seq(1999, 2026, by = 1)
axis(1,
     at = year_positions * 12 + 1,
     labels = year_positions)
# Create a chronological date variable
headline$date <- as.Date(
  paste(headline$year, headline$month_number, "01", sep = "-")
)

# Plot headline inflation with calendar years
plot(headline$date, headline$all_sources,
     type = "l",
     xaxt = "n",
     main = "Ghana Headline Inflation Trend (1999–2026)",
     xlab = "Year",
     ylab = "Headline Inflation Rate (%)")

# Add calendar-year labels
axis.Date(1,
          at = seq(as.Date("1999-01-01"),
                   as.Date("2026-01-01"),
                   by = "2 years"),
          format = "%Y")
plot(headline$date, headline$all_sources,
     type = "l",
     xaxt = "n",
     main = "Ghana Headline Inflation Trend (1999–2026)",
     xlab = "Year",
     ylab = "Headline Inflation Rate (%)")

axis.Date(1,
          at = seq(as.Date("1999-01-01"),
                   as.Date("2026-01-01"),
                   by = "2 years"),
          # =====================================================
          # QUESTION 1: ANALYSING HEADLINE INFLATION
          # =====================================================
          
          # Select All products to represent headline inflation
          headline <- cpi_data[cpi_data$items == "All products", ]
          
          # Extract the year from the month code, e.g. 2026M01
          headline$year <- as.integer(substr(headline$months, 1, 4))
          
          # Extract the month number, e.g. 01 from 2026M01
          headline$month_number <- as.integer(substr(headline$months, 6, 7))
          
          # Convert the year and month into a proper date
          headline$date <- as.Date(
            sprintf("%04d-%02d-01", headline$year, headline$month_number)
          )
          
          # Keep valid headline inflation observations from January 1999
          headline_plot <- headline[
            !is.na(headline$all_sources) &
              !is.na(headline$date) &
              headline$date >= as.Date("1999-01-01"), ]
          
          # Arrange the observations chronologically
          headline_plot <- headline_plot[order(headline_plot$date), ]
          
          # Identify the highest and lowest headline inflation rates
          headline_plot[which.max(headline_plot$all_sources),
                        c("months", "all_sources")]
          
          headline_plot[which.min(headline_plot$all_sources),
                        c("months", "all_sources")]
          
          # Calculate the average and median headline inflation
          mean(headline_plot$all_sources)
          median(headline_plot$all_sources)
          
          # Plot headline inflation using calendar dates
          plot(headline_plot$date, headline_plot$all_sources,
               type = "l",
               xaxt = "n",
               main = "Ghana Headline Inflation Trend (1999–2026)",
               xlab = "Year",
               ylab = "Headline Inflation Rate (%)")
          
          # Add calendar-year labels to the horizontal axis
          axis.Date(1,
                    at = seq(as.Date("1999-01-01"),
                             as.Date("2026-01-01"),
                             by = "2 years"),
                    format = "%Y")
          # Identify the five months with the highest headline inflation
          head(headline_plot[
            order(headline_plot$all_sources, decreasing = TRUE),
            c("months", "all_sources")
          ], 5)
          format = "%Y")
# =====================================================
# QUESTION 1: ANALYSING HEADLINE INFLATION
# =====================================================

# Select All products to represent headline inflation
headline <- cpi_data[cpi_data$items == "All products", ]

# Extract the year from month codes such as 2026M01
headline$year <- as.integer(substr(headline$months, 1, 4))

# Extract the month number
headline$month_number <- as.integer(substr(headline$months, 6, 7))

# Convert the year and month into a proper date
headline$date <- as.Date(
  sprintf("%04d-%02d-01", headline$year, headline$month_number)
)

# Remove missing inflation values and keep observations from 1999
headline_plot <- headline[
  !is.na(headline$all_sources) &
    !is.na(headline$date) &
    headline$date >= as.Date("1999-01-01"), ]

# Arrange the observations from oldest to newest
headline_plot <- headline_plot[order(headline_plot$date), ]

# Identify the highest headline inflation rate and its month
headline_plot[which.max(headline_plot$all_sources),
              c("months", "all_sources")]

# Identify the lowest headline inflation rate and its month
headline_plot[which.min(headline_plot$all_sources),
              c("months", "all_sources")]

# Calculate the mean headline inflation rate
mean(headline_plot$all_sources)

# Calculate the median headline inflation rate
median(headline_plot$all_sources)

# Identify the five months with the highest inflation rates
head(headline_plot[
  order(headline_plot$all_sources, decreasing = TRUE),
  c("months", "all_sources")
], 5)

# Plot the headline inflation trend
plot(headline_plot$date, headline_plot$all_sources,
     type = "l",
     xaxt = "n",
     main = "Ghana Headline Inflation Trend (1999–2026)",
     xlab = "Year",
     ylab = "Headline Inflation Rate (%)")

# Add calendar-year labels to the horizontal axis
axis.Date(1,
          at = seq(as.Date("1999-01-01"),
                   as.Date("2026-01-01"),
                   by = "2 years"),
          format = "%Y")
# =====================================================


# ================================================================
# GHANA STATISTICAL SERVICE (GSS)
# PROJECT: INVESTIGATING GHANA'S INFLATION TRENDS (CPI)
#
# QUESTION 2: ANALYSIS OF INFLATION BY PRODUCT CATEGORY
#
# RESEARCH QUESTION:
# Which product categories have had the highest inflation
# over the long run and over the last five years?
#
# OBJECTIVE:
# To identify and rank the 13 detailed CPI product categories
# according to their average year-on-year inflation rates.
#
# PERIODS OF ANALYSIS:
# 1. Long-run period: January 1999 – January 2026
# 2. Recent five-year period: February 2021 – January 2026
#
# METHOD:
# Data filtering, grouping, descriptive statistics and ranking.
# ================================================================


# ------------------------------------------------
# STEP 1: PREPARE THE CPI DATA
# ------------------------------------------------

# Extract the year from the month code (e.g. 2026M01)
cpi_data$year <- as.integer(
  substr(cpi_data$months, 1, 4)
)

# Extract the month number (e.g. 01 from 2026M01)
cpi_data$month_number <- as.integer(
  substr(cpi_data$months, 6, 7)
)

# Convert the year and month into a proper calendar date
# This allows the observations to be filtered by time period
cpi_data$date <- as.Date(
  sprintf("%04d-%02d-01",
          cpi_data$year,
          cpi_data$month_number)
)


# ------------------------------------------------
# STEP 2: SELECT THE 13 DETAILED PRODUCT CATEGORIES
# ------------------------------------------------

# Exclude All products, Food and Non-food because
# they are broad aggregates and would cause double-counting.
# Retain only observations with available inflation rates
# from January 1999 onwards.

category_data <- cpi_data[
  !(cpi_data$items %in% c("All products", "Food", "Non-food")) &
    !is.na(cpi_data$all_sources) &
    !is.na(cpi_data$date) &
    cpi_data$date >= as.Date("1999-01-01"),
]

# Verify the number of detailed product categories
cat("\nQUESTION 2: NUMBER OF DETAILED PRODUCT CATEGORIES\n")
print(length(unique(category_data$items)))


# ------------------------------------------------
# STEP 3: ANALYSE LONG-RUN CATEGORY INFLATION
# PERIOD: JANUARY 1999 – JANUARY 2026
# ------------------------------------------------

# Group the observations by product category.
# Calculate the mean, median and number of observations
# to summarise the long-run inflation pattern.

long_run_ranking <- dplyr::summarise(
  dplyr::group_by(category_data, items),
  
  mean_inflation = mean(all_sources, na.rm = TRUE),
  
  median_inflation = median(all_sources, na.rm = TRUE),
  
  observations = dplyr::n(),
  
  .groups = "drop"
)

# Arrange categories from the highest to the lowest
# average inflation rate.
long_run_ranking <- long_run_ranking[
  order(-long_run_ranking$mean_inflation),
]

# Select the five categories with the highest
# long-run average inflation rates.
top_five_long_run <- head(long_run_ranking, 5)

# Display the long-run ranking
cat("\nQUESTION 2: TOP FIVE CATEGORIES BY LONG-RUN AVERAGE INFLATION\n")
print(top_five_long_run)


# ------------------------------------------------
# STEP 4: ANALYSE RECENT FIVE-YEAR CATEGORY INFLATION
# PERIOD: FEBRUARY 2021 – JANUARY 2026
# ------------------------------------------------

# Select the latest 60 monthly observations
# for each of the 13 detailed product categories.

recent_category_data <- category_data[
  category_data$date >= as.Date("2021-02-01") &
    category_data$date <= as.Date("2026-01-01"),
]

# Group the recent observations by product category.
# Calculate the mean, median and number of observations.

recent_ranking <- dplyr::summarise(
  dplyr::group_by(recent_category_data, items),
  
  mean_inflation = mean(all_sources, na.rm = TRUE),
  
  median_inflation = median(all_sources, na.rm = TRUE),
  
  observations = dplyr::n(),
  
  .groups = "drop"
)

# Arrange categories from the highest to the lowest
# recent average inflation rate.
recent_ranking <- recent_ranking[
  order(-recent_ranking$mean_inflation),
]

# Select the five categories with the highest
# average inflation over the recent five-year period.
top_five_recent <- head(recent_ranking, 5)

# Display the recent five-year ranking
cat("\nQUESTION 2: TOP FIVE CATEGORIES BY RECENT AVERAGE INFLATION\n")
print(top_five_recent)


# ------------------------------------------------
# STEP 5: CREATE A NAMED NUMERIC VECTOR
# ------------------------------------------------

# Store the five highest recent category averages
# in a named numeric vector.
# Each product category is the name of its corresponding value.

top_categories_vector <- setNames(
  as.numeric(top_five_recent$mean_inflation),
  as.character(top_five_recent$items)
)

# Display the named vector
cat("\nQUESTION 2: NAMED VECTOR OF TOP FIVE RECENT CATEGORIES\n")
print(top_categories_vector)


# ------------------------------------------------
# STEP 6: VERIFY THE ANALYSIS
# ------------------------------------------------

# Confirm that 13 detailed categories were included
cat("\nVerification: Number of detailed categories\n")
print(length(unique(category_data$items)))

# Confirm the number of observations in the long-run analysis
cat("\nVerification: Long-run observations per category\n")
print(unique(long_run_ranking$observations))

# Confirm the number of observations in the recent analysis
cat("\nVerification: Recent five-year observations per category\n")
print(unique(recent_ranking$observations))


# ================= END OF QUESTION 2 =================



# -----------------------------------------------------
# B. AVERAGE INFLATION OVER THE LAST FIVE YEARS
# -----------------------------------------------------

# Select observations from January 2021 to January 2026
recent_category_data <- category_data %>%
  filter(
    date >= as.Date("2021-01-01"),
    date <= as.Date("2026-01-01")
  )

# Calculate average inflation for each category
recent_ranking <- recent_category_data %>%
  group_by(items) %>%
  summarise(
    mean_inflation = mean(all_sources, na.rm = TRUE),
    median_inflation = median(all_sources, na.rm = TRUE),
    observations = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_inflation))

# Display the full recent ranking
print(recent_ranking, n = Inf)

# Identify the five categories with the highest recent averages
top_five_recent <- head(recent_ranking, 5)

print(top_five_recent)
# QUESTION 2: REBUILD CATEGORY ANALYSIS

# Create proper dates from month codes
cpi_data$year <- as.integer(substr(cpi_data$months, 1, 4))
cpi_data$month_number <- as.integer(substr(cpi_data$months, 6, 7))

cpi_data$date <- as.Date(
  sprintf("%04d-%02d-01", cpi_data$year, cpi_data$month_number)
)

# Keep only the 13 detailed categories
category_data <- cpi_data[
  !(cpi_data$items %in% c("All products", "Food", "Non-food")) &
    !is.na(cpi_data$all_sources) &
    !is.na(cpi_data$date), ]

# Rank categories over the full period
long_run_ranking <- dplyr::summarise(
  dplyr::group_by(category_data, items),
  mean_inflation = mean(all_sources, na.rm = TRUE),
  median_inflation = median(all_sources, na.rm = TRUE),
  observations = dplyr::n(),
  .groups = "drop"
)

long_run_ranking <- long_run_ranking[
  order(-long_run_ranking$mean_inflation), ]

# Display the five highest long-run averages
top_five_long_run <- head(long_run_ranking, 5)
print(top_five_long_run)

# Select January 2021 to January 2026
recent_category_data <- category_data[
  category_data$date >= as.Date("2021-01-01") &
    category_data$date <= as.Date("2026-01-01"), ]

# Rank categories over the last five years
recent_ranking <- dplyr::summarise(
  dplyr::group_by(recent_category_data, items),
  mean_inflation = mean(all_sources, na.rm = TRUE),
  median_inflation = median(all_sources, na.rm = TRUE),
  observations = dplyr::n(),
  .groups = "drop"
)

recent_ranking <- recent_ranking[
  order(-recent_ranking$mean_inflation), ]

# Display the five highest recent averages
top_five_recent <- head(recent_ranking, 5)
print(top_five_recent)
# =====================================================
# QUESTION 2: PRODUCT CATEGORY INFLATION ANALYSIS
# =====================================================

# Create proper dates from the month codes
cpi_data$year <- as.integer(substr(cpi_data$months, 1, 4))
cpi_data$month_number <- as.integer(substr(cpi_data$months, 6, 7))

cpi_data$date <- as.Date(
  sprintf("%04d-%02d-01",
          cpi_data$year,
          cpi_data$month_number)
)

# Select the 13 detailed product categories
# Exclude All products, Food and Non-food aggregates
category_data <- cpi_data[
  !(cpi_data$items %in% c("All products", "Food", "Non-food")) &
    !is.na(cpi_data$all_sources) &
    !is.na(cpi_data$date) &
    cpi_data$date >= as.Date("1999-01-01"), ]

# Check the number of detailed categories
n_distinct(category_data$items)

# -----------------------------------------------------
# A. LONG-RUN CATEGORY INFLATION
# -----------------------------------------------------

# Calculate average and median inflation for each category
long_run_ranking <- dplyr::summarise(
  dplyr::group_by(category_data, items),
  mean_inflation = mean(all_sources, na.rm = TRUE),
  median_inflation = median(all_sources, na.rm = TRUE),
  observations = dplyr::n(),
  .groups = "drop"
)

# Rank categories from highest to lowest average inflation
long_run_ranking <- long_run_ranking[
  order(-long_run_ranking$mean_inflation), ]

# Display the five highest categories
top_five_long_run <- head(long_run_ranking, 5)
print(top_five_long_run)

# -----------------------------------------------------
# B. RECENT FIVE-YEAR CATEGORY INFLATION
# -----------------------------------------------------

# Select January 2021 to January 2026
recent_category_data <- category_data[
  category_data$date >= as.Date("2021-01-01") &
    category_data$date <= as.Date("2026-01-01"), ]

# Calculate average and median inflation for each category
recent_ranking <- dplyr::summarise(
  dplyr::group_by(recent_category_data, items),
  mean_inflation = mean(all_sources, na.rm = TRUE),
  median_inflation = median(all_sources, na.rm = TRUE),
  observations = dplyr::n(),
  .groups = "drop"
)

# Rank recent inflation from highest to lowest
recent_ranking <- recent_ranking[
  order(-recent_ranking$mean_inflation), ]

# Display the five highest recent categories
top_five_recent <- head(recent_ranking, 5)
print(top_five_recent)recent_category_data <- category_data[
  category_data$date >= as.Date("2021-02-01") &
    category_data$date <= as.Date("2026-01-01"), ]

recent_ranking <- dplyr::summarise(
  dplyr::group_by(recent_category_data, items),
  mean_inflation = mean(all_sources, na.rm = TRUE),
  median_inflation = median(all_sources, na.rm = TRUE),
  observations = dplyr::n(),
  .groups = "drop"
)

recent_ranking <- recent_ranking[
  order(-recent_ranking$mean_inflation), ]

top_five_recent <- head(recent_ranking, 5)

print(top_five_recent, n = 5, width = Inf)
# =====================================================
# QUESTION 3: LATEST MONTH CATEGORY ANALYSIS
# =====================================================

# Identify the latest month available in the dataset
latest_month <- max(category_data$date, na.rm = TRUE)

# Select the 13 detailed categories for that month
latest_category_data <- category_data[
  category_data$date == latest_month &
    !is.na(category_data$all_sources), ]

# Rank categories from highest to lowest inflation
latest_ranking <- latest_category_data[
  order(latest_category_data$all_sources, decreasing = TRUE),
  c("items", "all_sources")
]

# Identify the five categories with the highest inflation
top_five_latest <- head(latest_ranking, 5)

# Identify the five categories with the lowest inflation
bottom_five_latest <- head(
  latest_ranking[order(latest_ranking$all_sources), ],
  5
)

# Display the latest month analysed
print(format(latest_month, "%B %Y"))

# Display the five highest inflation categories
print(top_five_latest, row.names = FALSE)

# Display the five lowest inflation categories
print(bottom_five_latest, row.names = FALSE)
# =====================================================
# QUESTION 4: LOCAL VERSUS IMPORTED INFLATION
# =====================================================

# Select All products observations from January 2022
local_import_data <- cpi_data[
  cpi_data$items == "All products" &
    cpi_data$date >= as.Date("2022-01-01") &
    !is.na(cpi_data$local) &
    !is.na(cpi_data$import), ]

# Calculate the difference between local and imported inflation
# A positive gap means local inflation is higher
local_import_data$gap <- (
  local_import_data$local - local_import_data$import
)

# Calculate average inflation rates
mean(local_import_data$local)
mean(local_import_data$import)

# Count months when local inflation was higher
sum(local_import_data$gap > 0)

# Count months when imported inflation was higher
sum(local_import_data$gap < 0)

# Identify the average difference
mean(local_import_data$gap)

# Identify the highest local inflation rate
local_import_data[
  which.max(local_import_data$local),
  c("months", "local")
]

# Identify the highest imported inflation rate
local_import_data[
  which.max(local_import_data$import),
  c("months", "import")
]

# Compare the latest local and imported inflation rates
tail(local_import_data[
  , c("months", "local", "import", "gap")
], 1)
# =====================================================




# Display the decade comparison
print(decade_summary, n = Inf)
# =====================================================
# QUESTION 4: LOCAL VERSUS IMPORTED INFLATION
# =====================================================

# Load the corrected local and imported inflation data
local_import_data <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE
)

# Convert dates into proper calendar dates
local_import_data$date <- as.Date(local_import_data$date)

# Arrange observations chronologically
local_import_data <- local_import_data[
  order(local_import_data$date), ]

# Calculate the difference between local and imported inflation
# Positive gap means local inflation is higher
local_import_data$gap <-
  local_import_data$local - local_import_data$import

# Confirm the number of monthly observations
nrow(local_import_data)

# Calculate average inflation rates
mean(local_import_data$local)
mean(local_import_data$import)

# Count months when local inflation was higher
sum(local_import_data$gap > 0)

# Count months when imported inflation was higher
sum(local_import_data$gap < 0)

# Calculate average difference
mean(local_import_data$gap)

# Identify peak local inflation
local_import_data[
  which.max(local_import_data$local),
  c("months", "local")
]

# Identify peak imported inflation
local_import_data[
  which.max(local_import_data$import),
  c("months", "import")
]

# Display January 2026 comparison
tail(local_import_data[
  , c("months", "local", "import", "gap")
], 1)

# -----------------------------------------------------
# GRAPH: LOCAL VERSUS IMPORTED INFLATION
# -----------------------------------------------------

# Plot local inflation
plot(local_import_data$date,
     local_import_data$local,
     type = "l",
     col = "blue",
     lwd = 2,
     ylim = range(c(local_import_data$local,
                    local_import_data$import)),
     xaxt = "n",
     main = "Local and Imported Inflation in Ghana (2022–2026)",
     xlab = "Year",
     ylab = "Inflation Rate (%)")

# Add imported inflation
lines(local_import_data$date,
      local_import_data$import,
      col = "red",
      lwd = 2)

# Add calendar-year labels
axis.Date(1,
          at = seq(as.Date("2022-01-01"),
                   as.Date("2026-01-01"),
                   by = "6 months"),
          format = "%b %Y",
          las = 2)

# Add legend
legend("topright",
       legend = c("Local inflation", "Imported inflation"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2)
# =====================================================
# QUESTION 5: COMPARING INFLATION ACROSS DECADES
# =====================================================

# Select headline inflation observations
decade_data <- cpi_data[
  cpi_data$items == "All products" &
    !is.na(cpi_data$all_sources), ]

# Extract the year from the month code
decade_data$year <- as.integer(
  substr(as.character(decade_data$months), 1, 4)
)

# Keep observations from January 1999 onwards
decade_data <- decade_data[
  !is.na(decade_data$year) &
    decade_data$year >= 1999, ]

# Group observations into periods
decade_data$period <- ifelse(
  decade_data$year == 1999, "1999 only",
  ifelse(decade_data$year <= 2009, "2000s",
         ifelse(decade_data$year <= 2019, "2010s", "2020s")
  )
)

# Define the order of the periods
period_order <- c("1999 only", "2000s", "2010s", "2020s")

# Calculate summary statistics for each period
decade_summary <- do.call(rbind, lapply(
  period_order,
  function(p) {
    
    # Select inflation rates for the current period
    rates <- decade_data$all_sources[
      decade_data$period == p
    ]
    
    # Calculate descriptive statistics
    data.frame(
      period = p,
      observations = length(rates),
      mean_inflation = mean(rates),
      median_inflation = median(rates),
      minimum_inflation = min(rates),
      maximum_inflation = max(rates),
      months_below_10_percent = mean(rates < 10) * 100
    )
  }
))

# Display the decade comparison
print(decade_summary, row.names = FALSE)
# -----------------------------------------------------
# GRAPH: COMPARISON OF INFLATION ACROSS PERIODS
# -----------------------------------------------------

# Create a bar chart of average headline inflation
bar_positions <- barplot(
  decade_summary$mean_inflation,
  names.arg = decade_summary$period,
  main = "Average Headline Inflation Across Periods",
  xlab = "Period",
  ylab = "Average Inflation Rate (%)",
  ylim = c(0, 25),
  col = "steelblue"
)

# Add the average inflation values above the bars
text(
  x = bar_positions,
  y = decade_summary$mean_inflation,
  labels = round(decade_summary$mean_inflation, 2),
  pos = 3
)# =====================================================
# FINAL SECTION: CREATING R DATA STRUCTURES
# =====================================================

# Create a named vector of the five highest recent
# category inflation averages
top_categories_vector <- setNames(
  top_five_recent$mean_inflation,
  top_five_recent$items
)

# Create a vector containing headline peak, trough
# and latest inflation rates
headline_rates_vector <- c(
  peak = max(headline_plot$all_sources, na.rm = TRUE),
  trough = min(headline_plot$all_sources, na.rm = TRUE),
  latest = headline_plot$all_sources[
    which.max(headline_plot$date)
  ]
)

# Display both vectors
print(top_categories_vector)
print(headline_rates_vector)
# =====================================================
# CREATE A MATRIX OF CATEGORY INFLATION RATES
# =====================================================

# Select five January observations for comparison
selected_months <- c(
  "2022M01", "2023M01", "2024M01",
  "2025M01", "2026M01"
)

# Select the 13 detailed product categories
matrix_data <- cpi_data[
  !(cpi_data$items %in% c("All products", "Food", "Non-food")) &
    cpi_data$months %in% selected_months &
    !is.na(cpi_data$all_sources),
  c("items", "months", "all_sources")
]

# Convert the data into a wide-format table
matrix_wide <- tidyr::pivot_wider(
  matrix_data,
  names_from = months,
  values_from = all_sources
)

# Convert the table into a numeric matrix
category_inflation_matrix <- as.matrix(
  matrix_wide[, -1]
)

# Give the rows the names of the product categories
rownames(category_inflation_matrix) <- matrix_wide$items

# Display the matrix
print(category_inflation_matrix)
# =====================================================
# FINAL SECTION: COMBINING RESULTS INTO A NAMED LIST
# =====================================================

# Create the named vector of the five highest recent
# category inflation averages
top_categories_vector <- setNames(
  top_five_recent$mean_inflation,
  top_five_recent$items
)

# Create the headline inflation rates vector
headline_rates_vector <- c(
  peak = max(headline_plot$all_sources, na.rm = TRUE),
  trough = min(headline_plot$all_sources, na.rm = TRUE),
  latest = headline_plot$all_sources[
    which.max(headline_plot$date)
  ]
)

# Summarise local and imported inflation
local_import_summary <- list(
  mean_local = mean(local_import_data$local),
  mean_import = mean(local_import_data$import),
  months_local_higher = sum(local_import_data$gap > 0),
  months_import_higher = sum(local_import_data$gap < 0),
  average_gap = mean(local_import_data$gap),
  peak_local = local_import_data[
    which.max(local_import_data$local),
    c("months", "local")
  ],
  peak_import = local_import_data[
    which.max(local_import_data$import),
    c("months", "import")
  ]
)

# Combine all major findings into one named list
final_results <- list(
  overview = list(
    start_date = min(headline_plot$date),
    end_date = max(headline_plot$date),
    headline_observations = nrow(headline_plot),
    product_categories = length(unique(category_data$items))
  ),
  headline_rates = headline_rates_vector,
  top_categories = top_categories_vector,
  long_run_ranking = top_five_long_run,
  recent_ranking = top_five_recent,
  latest_month_highest = top_five_latest,
  latest_month_lowest = bottom_five_latest,
  local_import_comparison = local_import_summary,
  decade_comparison = decade_summary,
  category_inflation_matrix = category_inflation_matrix
)

# Inspect the structure of the final results list
str(final_results)
# =====================================================
# FINAL SECTION: COMBINING ALL ANALYSIS RESULTS
# =====================================================

# 1. Create a named vector of the five highest recent
# product category inflation averages
top_categories_vector <- setNames(
  as.numeric(top_five_recent$mean_inflation),
  as.character(top_five_recent$items)
)

# 2. Create a vector containing headline peak, trough
# and latest inflation rates
headline_rates_vector <- c(
  peak = max(headline_plot$all_sources, na.rm = TRUE),
  trough = min(headline_plot$all_sources, na.rm = TRUE),
  latest = headline_plot$all_sources[
    which.max(headline_plot$date)
  ]
)

# 3. Calculate the local and imported inflation gap
# Positive values indicate higher local inflation
local_import_data$gap <-
  local_import_data$local - local_import_data$import

# 4. Summarise local and imported inflation
local_import_summary <- list(
  mean_local = mean(local_import_data$local, na.rm = TRUE),
  mean_import = mean(local_import_data$import, na.rm = TRUE),
  months_local_higher = sum(local_import_data$gap > 0),
  months_import_higher = sum(local_import_data$gap < 0),
  average_gap = mean(local_import_data$gap, na.rm = TRUE)
)

# 5. Combine all the findings into one named list
final_results <- list(
  
  # General information about the analysis
  overview = list(
    start_date = min(headline_plot$date),
    end_date = max(headline_plot$date),
    headline_observations = nrow(headline_plot),
    product_categories = length(unique(category_data$items))
  ),
  
  # Headline inflation results
  headline_rates = headline_rates_vector,
  
  # Product category rankings
  top_categories = top_categories_vector,
  long_run_ranking = top_five_long_run,
  recent_ranking = top_five_recent,
  
  # Latest-month category results
  latest_month_highest = top_five_latest,
  latest_month_lowest = bottom_five_latest,
  
  # Local and imported inflation comparison
  local_import_comparison = local_import_summary,
  
  # Decade comparison
  decade_comparison = decade_summary,
  
  # Category inflation matrix
  category_inflation_matrix = category_inflation_matrix
)

# 6. Inspect the structure of the final results list
str(final_results)
