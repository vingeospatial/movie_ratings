

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
  janitor::clean_names() |>       # Conver column names to snake_case
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
  
  
  
  
  
  
  