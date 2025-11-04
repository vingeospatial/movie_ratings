

# 1) INSTALL & LOAD PACKAGES

library(tidyverse)     # Core data science tools: dply, ggplot2, readr, etc
library(lubridate)     # Makes it easy to work with date columns
library(skimr)         # Provides quick summary stats of your data
library(janitor)       # Cleans messy column names, helps explore categorical vars
library(broom)          # Tidies up model output for easier analysis
library(GGally)        # Optional : for quick EDA (PAIR PLOTS)
library(pROC)          # Used to evaluate classification model(AUC, ROC)

# 2) READ THE DATA

# Set your file name (change the path if needed)
movie_raw <- read.csv("data/Movie-Dataset-Latest.csv")    # Load the CSV into R as a tibble
# Take a quick look
glimpse(movie_raw)
head(movie_raw, 3)
View(movie_raw)

# 3) CLEANING & BASIC PREPARATION (robust)    

# peek at the raw names so you can see what's actually there     
names(movie_raw)

movies <- movie_raw |> 
  janitor::clean_names() |>       # Convert column names to snake_case
  select(-tidyselect::any_of(c("unnamed_0", "x"))) |>   # Drop unwanted columns if they exist
  mutate(
    # Parse release_date safely(ymd ignores if already Date)   
    release_date = lubridate::ymd(release_date),
    year         = lubridate::year(release_date),   # Extract release year
    decade       = floor(year / 10) * 10,           # Derive decade from year 
    # convert 'video' to factor if it exists    
    # video        = if ("video" %in% names(.)) as.factor(video) else NULL
    video         = if ("video" %in% names(cur_data())) as.factor(video) else NULL
  )
 
glimpse(movies) # check cleaned structure and new columns
  
# check missing counts across all columns    
movies |> 
  summarise(across(everything(), ~ sum(is.na(.)))) |> 
  t()  #Transpose for easier reading
  
# DISTRIBUTION OF RATINGS     

ggplot(movies, aes(x = vote_average)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +  # Histogram with 30 bins
  labs(
    title = "Distribution of Movie Ratings", # Chart title 
    x  = "Avereage Rating",   # X-axis label
    y = "Number of Movies"    # Y-axis label
  ) +
  theme_minimal()   # Clean, minimal visual style
  
  
# 4.2 MOST VOTES MOVIES   
movies |> 
  arrange(desc(vote_count)) |>    # sort movies by highest vote count 
  slice_head(n = 15) |>           # Take the top 15
  ggplot(aes(x = reorder(title, vote_count), y = vote_count)) + # Reorder titles by vote
  geom_col(fill = "darkorange") +   # Bar chart with orange bars  
  coord_flip() +                    # Flip coordinates for horizontal bar
  labs(
    title = "Top 15 Most Voted Movies",      # Chart title
    x = "Movie Title",                       # X-axis label
    y = "Vote count"                         # Y-axis label
  ) +
  theme_minimal()                            # Clean visual style
  
  
  
  
 # STEP 7: TOP RATED MOVIES WITH MINIMUM 1000 VOTES

min_votes <- 1000 # to avoid including unknown films with 1 vote
top_rated <- movies |> 
  filter(vote_count >= min_votes) |>                                   # Keep only movies with 1000+ votes
  arrange(desc(vote_average)) |>                                      # Sort by highest rating
  slice_head(n = 15)                                                  # Pick top 15
ggplot(top_rated, aes(x = reorder(title, vote_average), y = vote_average)) +
  geom_col(fill = "seagreen") +                                       # green horizontal bars
  coord_flip() +                                                      # Flip for horizontal layout
  labs(
    title = paste("Top 15 Highest Rated Movies (vote_count >=", min_votes, ")"),
    x = "Movie Title",                                                # X-axis label
    y = "Average Rating"                                              # Y-axis label
  ) +
  theme_minimal()                                                     # Clean style
  
  
# AVERAGE MOVIE RATING OVER TIME   

rating_year <- movies |> 
  filter(!is.na(year)) |>                  # Remove rows with missing year
  group_by(year) |>                        # Group by release year
  summarise(
    avg_rating = mean(vote_average, na.rm = TRUE), # Calculate average rating
    n = n()                                        # Count movies per year
  ) |> 
  filter(n >= 5)   # Keep years with at least 5 movies to avoid unreliable averages
ggplot(rating_year, aes(x = year, y = avg_rating)) + 
  geom_line(color = "steelblue") +                                # Line for rating trend
  geom_point() +                                                 # Add points for each year
  labs(
    title = "Average Movie rating by Year",                      # Chart title
    x = "Year",                                                  # X-axis label
    y = "Average Rating"                                         # Y-axis
  ) +
  theme_minimal()                                                # clean visual theme
  


# POPULARITY VS VOTE COUNT 
ggplot(movies, aes(x = popularity, y = vote_count)) +
  geom_point(alpha = 0.5, color = "purple") +           # Scatter plot with semi-transparent purple points
  scale_x_log10() +                                     # Log scale for popularity (x-axis)
  scale_y_log10() +                                     # Log scale for vote count (y-axis)
  labs(
    title = "Popularity vs Vote Count (log-log scale)", # Chart title
    x = "Popularity (log)",                             # X-axis label
    y = "Vote Count (log)"                              # Y-axis label
  ) +
  theme_minimal()                                       # clean theme


# FEATURE ENGINEERING   
movies_fe <- movies |> 
  mutate(
    log_vote_count = log1p(vote_count),              # use log(1 + x) to avoid log(0) issues
    high_rating    = vote_average >= 8,              # TRUE if rating is 8 or higher (for classification)
    rating_bucket  = case_when(                      # Group ratings into categories
      vote_average < 5 ~ "Low",
      vote_average < 7 ~ "Average",
      TRUE             ~ "High"
    )
  ) |> 
  mutate(rating_bucket = factor(rating_bucket, levels = c("Low", "Average", "High")))   # Ordered factor
# Check the new structure  
glimpse(movies_fe)  # See new columns
# Count how many movies fall into each rating bucket
movies_fe |> 
  count(rating_bucket)



# STEP 11: distribution of vote count(Raw vs log transformed)   

# DISTRIBUTION OF VOTE COUNTS   
par(mfrow = c(1, 2))   # show two plots side by side 

# Histogram of raw vote counts     
hist(movies$vote_count, breaks = 30,
     main = "Vote Count (Raw)", col = "tomato", xlab = "vote_count")

# Histogram of log-transformed vote counts  
hist(movies_fe$log_vote_count, breaks = 30,
     main = "Vote Count (Log Transformed)", col = "steelblue", xlab = "log_vote_count")

par(mfrow = c(1, 1))   # Reset plot layout default 





# STEP 12: RATING BUCKET COUNTS



  
  
  
  
  
  
  
  
  
  
  