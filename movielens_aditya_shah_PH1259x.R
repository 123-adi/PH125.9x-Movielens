##### Movielens Capstone - Aditya Shah 

##### Course material code: downloads movielens and splits into edx and validation sets ##### 

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##### My project code #####

#### Packages
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(lubridate)
library(scales)
library(ggplot2)

#### Overview, Introduction

#### 1. Data exploration/Analysis

### 1.1 Initial exploration and manipulation (preparing the dataset)

head(edx)
glimpse(edx)

# convert timestamp to date and extract rating year
edx <- edx %>%
  mutate(date = as_datetime(timestamp),
         rated_year = year(as_datetime(timestamp))) 

# extract movie release year
edx <- edx %>% 
  mutate(release_year = str_extract(title, pattern = "(\\(\\d{4}\\))"), # includes brackets
         release_year = str_extract(release_year, pattern = "(\\d{4})"), # remove brackets
         release_year = as.numeric(release_year)) 

# measure difference between year released and year rated
edx <- edx %>%
  mutate(diff_year = rated_year - release_year)

# see range of differences
edx %>%
  arrange(desc(diff_year)) %>%
  select(diff_year)

# remove rows with negative differences
edx <- edx %>%
  filter(diff_year >= 0)

# extract weekday
edx <- edx %>%
  mutate(weekday = weekdays(date))

# drop raw timestamp column
edx <- edx %>%
  select(-timestamp)

### 1.2 Exploring data

## 1.2.1 Summary statistics

# Ratings:

edx %>%
  select(rating, rated_year, release_year, diff_year) %>%
  summary

# ratings between 0.5-5; mean 3.5
# movie rated year between 1995-2009
# movie release year between 1915-2008
# range of difference between release year and rated year 0-93 years; mean 12 years

# Users and Movies:

# unique users 
total_users <- n_distinct(edx$userId) # 69878

# unique movies
total_movies <- n_distinct(edx$movieId) # 10677

# total ratings 
total_no_ratings <- nrow(edx) # 8999880

# Ratings per user/movie:

# mean no. of ratings per user 
avg_ratings_per_user <- nrow(edx)/n_distinct(edx$userId) # 128.79

# no. of ratings as % of total possibilities
total_no_ratings/(total_users*total_movies) # 1%

# range of no. of ratings per user
ratings_per_user <- edx %>%
  group_by(userId) %>%
  summarise(n = n())

range(ratings_per_user$n) # 10-6616 

# mean no. of ratings per movie
avg_ratings_per_movie <- nrow(edx)/n_distinct(edx$movieId) # 842.92

# range of no. of ratings per movie
ratings_per_movie <- edx %>%
  group_by(movieId) %>%
  summarise(n = n())

range(ratings_per_movie$n) # 1-31362 

# Genres (combinations):

# genres (genre combinations)
n_distinct(edx$genres) # 797

## 1.2.2 Further analysis

# 1.2.2.1 Ratings

# ratings table count
edx %>%
  group_by(rating) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 
# integer ratings more likely; 3 or 4 most common

# visualising the ratings table
edx %>%
  group_by(rating) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  scale_y_continuous(labels = comma) +
  geom_col() 
# most ratings seem to be 3 or above

## 1.2.2.2 Ratings & Users

# no. of ratings by user ID
edx %>%
  group_by(userId) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = userId, y = count)) +
  geom_col() +
  ylim(0, 1200) +
  labs(title = "1 Users and no. of ratings",
       y = "no. of ratings") +
  geom_hline(yintercept = avg_ratings_per_user, color="red", linetype="dashed") # avg no. ratings/user line
# a lot of variability in the number of ratings by a user

# histogram of no. of ratings per user
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 100) +
  scale_x_log10() +
  labs(title = "2 No. of ratings per user", 
       x = "no. of ratings (log scale)",
       y = "no. of users")+
  geom_vline(xintercept = avg_ratings_per_user, color="red", linetype="dashed") # avg no. ratings/user line
# no. of ratings below average for most users; a small proportion of prolific raters seem to be pushing up the average

# top 10 users by number of ratings
edx %>%
  group_by(userId) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)

# mean rating
avg_rating <- mean(edx$rating) # 3.512

# no. of ratings and mean rating by user
edx %>%
  group_by(userId) %>%
  summarise(count = n(),
            mean_rating  = mean(rating)) %>%
  ggplot(aes(x = mean_rating, y = count)) +
  geom_point() +
  labs(title = "3 No. of ratings and mean rating by user",
       y = "no. of ratings") +
  geom_vline(xintercept = avg_rating, color="blue", linetype="dashed")

## 1.2.2.3 Ratings & Movies

# no. of ratings by movie ID
edx %>%
  group_by(movieId) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = movieId, y = count)) +
  geom_col() +
  ylim(0, 6000) +
  labs(title = "1 Movies and no. of ratings",
       y = "no. of ratings") +
  geom_hline(yintercept = avg_ratings_per_movie, color="red", linetype="dashed") # avg no. ratings/movie line
# a lot of variability in the number of ratings a movie has

# histogram of no. of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 100) +
  scale_x_log10() +
  labs(title = "2 Ratings per movie",
       x = "no. of ratings (log scale)",
       y = "no. of movies") +
  geom_vline(xintercept = avg_ratings_per_movie, color="red", linetype="dashed") # avg no. ratings/movie line
# no. of ratings below average for most movies; a small proportion of movies (blockbusters) have a lot more than the average

# top 10 movies by number of ratings
edx %>%
  group_by(movieId) %>%
  summarise(n = n(),
            title = title[1]) %>%
  top_n(10, n) %>%
  arrange(desc(n))
# all blockbusters

# no. of ratings and mean rating by movie
edx %>%
  group_by(movieId) %>%
  summarise(count = n(),
            mean_rating  = mean(rating)) %>%
  ggplot(aes(x = mean_rating, y = count)) +
  geom_point() +
  labs(title = "3 No. of ratings and mean rating by movie",
       y = "no. of ratings") +
  geom_vline(xintercept = avg_rating, color="blue", linetype="dashed")

## 1.2.2.4 Ratings & Weekdays

# table of no. of ratings by weekday
edx %>%
  group_by(weekday) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 

# visualising no. of ratings by weekday
edx %>%
  group_by(weekday) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = weekday, y = count)) + 
  geom_col() +
  labs(title = "1 No. of ratings by day of the week") # highest no. of ratings on Mondays & Tuesdays 
# more movies rated on Mondays and Tuesdays; overall not much variability across the days

# weekday and mean rating 
edx %>%
  group_by(weekday) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(x = weekday, y = mean_rating)) +
  geom_point() + 
  expand_limits(y = 0) +
  labs(title = "2 Day of the week and mean rating") +
  geom_hline(yintercept = avg_rating, color="blue", linetype="dashed") 
# no discernible variability; avg. rating by weekday similar to overall avg. rating

## 1.2.2.5 Ratings & Time difference when rated

# time difference when rated and mean rating
edx %>%
  group_by(diff_year) %>%
  summarise(mean_rating = mean(rating)) %>%
  ggplot(aes(x = diff_year, y = mean_rating)) +
  geom_point() +
  labs(title = "1 Time difference vs mean rating",
       x = "time difference (years) when rated") +
  geom_hline(yintercept = avg_rating, color="blue", linetype="dashed")

# ratings within a few years of release get an avg rating below the overall avg and seems to improve as the time between release and rating increases. maybe when movies are newer there is a lot of promotion and buzz so people watch it for fear of missing out but don't always enjoy it or doesn't live up to the hype whereas for older movies people make an informed choice based on existing reviews and seem to enjoy more what they choose to watch.

# time difference and no. of ratings
edx %>%
  group_by(diff_year) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = diff_year, y = count)) +
  scale_y_continuous(labels = comma) +
  geom_col() +
  labs(title = "2 Time difference when rated and no. of ratings",
       x = "Time difference when rated",
       y = "no. of ratings")
# ratings after movie release get a lot of ratings and decrease as time passes

edx %>%
  group_by(diff_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 
# highest number of ratings within 3 years of release

## 1.2.2.6 Ratings & Genre (combinations)

# no. of ratings and mean rating by genre
genre_rating <- edx %>%
  group_by(genres) %>%
  summarise(count = n(),
            mean_rating  = mean(rating)) 

range(genre_rating$count) # 2-733246
range(genre_rating$mean_rating) # 1.45-4.71

# mean rating by genre
edx %>%
  group_by(genres) %>%
  summarise(mean_rating  = mean(rating)) %>%
  mutate(genres = reorder(genres ,mean_rating)) %>%
  ggplot(aes(x = genres, y = mean_rating)) +
  geom_point() + 
  expand_limits(y = 0) +
  geom_hline(yintercept = avg_rating, color="blue", linetype="dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "1 Genres and their mean ratings")
# a lot of variability of avg rating across genres

# only genres with 2000 or more ratings
edx %>%
  group_by(genres) %>%
  summarise(mean_rating = mean(rating), n = n()) %>%
  filter(n >= 2000) %>%
  mutate(genres = reorder(genres, mean_rating)) %>%
  ggplot(aes(x = genres, y = mean_rating)) +
  geom_point() +
  geom_hline(yintercept = avg_rating, color="blue", linetype="dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "2 Genres (with 2000 or more ratings) and their mean ratings")

# number of ratings and mean rating by genre
genre_rating %>%
  ggplot(aes(x = mean_rating, y = count)) +
  geom_point() +
  scale_y_continuous(labels = comma) +
  labs(title = "3 No. of ratings and mean rating by genre",
       y = "no. of ratings") +
  geom_vline(xintercept = avg_rating, color="blue", linetype="dashed")

# although the no. of ratings and avg rating for a genre doesn't seem to go hand in hand, genres with a high no. of ratings seem to have an avg rating that is around the overall avg rating.

genre_rating %>%
  ggplot(aes(x = mean_rating)) +
  geom_histogram() +
  labs(title = "4 Genre mean rating",
       y = "no. of genres") +
  geom_vline(xintercept = avg_rating, color="blue", linetype="dashed")
# most genres seem to have an avg rating around 3-4

#### 2. Modeling/Training Algorithms

# target: RMSE < 0.86490

### 2.1 Split edx into training and test sets

# test set will be 10% of edx
set.seed(1, sample.kind="Rounding")
index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-index,]
temp <- edx[index,]

# Make sure userId and movieId in test set are also in train set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test)
train <- rbind(train, removed)

rm(temp, removed)

### 2.2 Loss function - defining the RMSE function (course material)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### 2.3 Models

## 2.3.1 Model 1: Basic mu_hat (mean rating; course material)

# overall mean rating
mu_hat <- mean(train$rating) 
mu_hat

# rmse
model_1_rmse <- RMSE(test$rating, mu_hat)

# create result table
rmse_results <- tibble(method = "1 Basic (mean rating)", RMSE = model_1_rmse)

## 2.3.2 Model 2: Mean + Movie b_i (course material)

# adding movie effect 
movie_avg <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat))

# model
predict_model_2 <- mu_hat + test %>% 
  left_join(movie_avg, by='movieId') %>%
  pull(b_i)

# rmse
model_2_rmse <- RMSE(test$rating, predict_model_2)

# update rmse result table
rmse_results <- rmse_results %>% add_row(method = "2 Mean + Movie", RMSE = model_2_rmse)

## 2.3.3 Model 3: Mean + User b_u

# adding user effect to mean
user_avg <- train %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu_hat))

# model
predict_model_3 <- mu_hat + test %>% 
  left_join(user_avg, by='userId') %>%
  pull(b_u)

# rmse
model_3_rmse <- RMSE(test$rating, predict_model_3)

# update rmse result table
rmse_results <- rmse_results %>% add_row(method = "3 Mean + User", RMSE = model_3_rmse)

## 2.3.4 Model 4: Mean + Movie + User (course material)

# adding user effect to mean + movie
movie_user_avg <- train %>% 
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

# model
predict_model_4 <- test %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(movie_user_avg, by='userId') %>%
  mutate(predict = mu_hat + b_i + b_u) %>%
  pull(predict)

# rmse
model_4_rmse <- RMSE(test$rating, predict_model_4)

# update rmse result table
rmse_results <- rmse_results %>% add_row(method = "4 Mean + Movie + User", RMSE = model_4_rmse)

## 2.3.5 Model 5: Mean + Movie (regularized; course material)

# find optimum lambda

lambdas <- seq(0, 10, 0.25)

rmses_bi <- sapply(lambdas, function(l){
  
  mu <- mean(train$rating)
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$rating))
})

qplot(lambdas, rmses_bi)  

lambda_bi <- lambdas[which.min(rmses_bi)] #2.5

# adding regularized movie effect
movie_avg_r <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda_bi), n_i = n()) 

# model
predict_model_5 <- test %>% 
  left_join(movie_avg_r, by = "movieId") %>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)

# rmse
model_5_rmse <- RMSE(test$rating, predict_model_5)

# update rmse result table
rmse_results <- rmse_results %>% add_row(method = "5 Mean + Movie(r)", RMSE = model_5_rmse)

## 2.3.6 Model 6: Mean + Movie (r) + User (r) (course material)

# find optimum lambda

rmses_bi_bu <- sapply(lambdas, function(l){
  
  mu <- mean(train$rating)
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$rating))
})

qplot(lambdas, rmses_bi_bu)  

lambda_bi_bu <- lambdas[which.min(rmses_bi_bu)] #5

# adding regularized user effect to mean + movie (r)
movie_r_user_r_avg <- train %>% 
  left_join(movie_avg_r, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu_hat - b_i)/(n()+lambda_bi_bu), n_i = n())

# model
predict_model_6 <- test %>% 
  left_join(movie_avg_r, by='movieId') %>%
  left_join(movie_r_user_r_avg, by='userId') %>%
  mutate(predict = mu_hat + b_i + b_u) %>%
  pull(predict)

# rmse
model_6_rmse <- RMSE(test$rating, predict_model_6)

# update rmse result table
rmse_results <- rmse_results %>% add_row(method = "6 Mean + Movie(r) + User(r)", RMSE = model_6_rmse)

## 2.3.7 Model 7: Mean + Movie (r) + User (r) + Time diff 

# adding time difference effect (difference between rated year and release year) to Mean + Movie (r) + User (r)
movie_age_avg <- train %>% 
  left_join(movie_avg_r, by='movieId') %>%
  left_join(movie_r_user_r_avg, by='userId') %>%
  group_by(diff_year) %>%
  summarize(b_y = mean(rating - mu_hat - b_i - b_u))

# model
predict_model_7 <- test %>% 
  left_join(movie_avg_r, by='movieId') %>%
  left_join(movie_r_user_r_avg, by='userId') %>%
  left_join(movie_age_avg, by='diff_year') %>%
  mutate(predict = mu_hat + b_i + b_u + b_y) %>%
  pull(predict)

# rmse
model_7_rmse <- RMSE(test$rating, predict_model_7)

# update rmse result table
rmse_results <- rmse_results %>% add_row(method = "7 Mean + Movie(r) + User(r) + Time difference", RMSE = model_7_rmse)

## 2.3.8 Model 8: Mean + Movie(r) + User(r) + Time diff + Weekday

# adding weekday effect to Mean + Movie(r) + User(r) + Time diff
weekday_avg <- train %>%
  left_join(movie_avg_r, by='movieId') %>%
  left_join(movie_r_user_r_avg, by='userId') %>%
  left_join(movie_age_avg, by='diff_year') %>%
  group_by(weekday) %>%
  summarize(b_w = mean(rating - mu_hat - b_i - b_u - b_y))

# model
predict_model_8 <- test %>% 
  left_join(movie_avg_r, by='movieId') %>%
  left_join(movie_r_user_r_avg, by='userId') %>%
  left_join(movie_age_avg, by='diff_year') %>%
  left_join(weekday_avg, by="weekday") %>%
  mutate(predict = mu_hat + b_i + b_u + b_y + b_w) %>%
  pull(predict)

# rmse
model_8_rmse <- RMSE(test$rating, predict_model_8)

# update rmse result table
rmse_results <- rmse_results %>% add_row(method = "8 Mean + Movie(r) + User(r) + Time diff + Weekday", RMSE = model_8_rmse)

## 2.3.9 Model 9: Mean + Movie(r) + User(r) + Time diff + Genres

# adding genre effect to Mean + Movie(r) + User(r) + Time diff
genre_avg <- train %>%
  left_join(movie_avg_r, by='movieId') %>%
  left_join(movie_r_user_r_avg, by='userId') %>%
  left_join(movie_age_avg, by='diff_year') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u - b_y))

# model
predict_model_9 <- test %>% 
  left_join(movie_avg_r, by='movieId') %>%
  left_join(movie_r_user_r_avg, by='userId') %>%
  left_join(movie_age_avg, by='diff_year') %>%
  left_join(genre_avg, by="genres") %>%
  mutate(predict = mu_hat + b_i + b_u + b_y + b_g) %>%
  pull(predict)

# rmse
model_9_rmse <- RMSE(test$rating, predict_model_9)

# update rmse result table
rmse_results <- rmse_results %>% add_row(method = "9 Mean + Movie(r) + User(r) + Time diff + Genre", RMSE = model_9_rmse)

### 2.4 Validation

## 2.4.1 Prepare validation dataset (repeat same manipulation as edx)

# convert timestamp to date and extract rating year
validation_prep <- validation %>%
  mutate(date = as_datetime(timestamp),
         rated_year = year(as_datetime(timestamp))) 

# extract movie release year
validation_prep <- validation_prep %>% 
  mutate(release_year = str_extract(title, pattern = "(\\(\\d{4}\\))"), # includes brackets
         release_year = str_extract(release_year, pattern = "(\\d{4})"), # remove brackets
         release_year = as.numeric(release_year)) 

# measure difference between year released and year rated
validation_prep <- validation_prep %>%
  mutate(diff_year = rated_year - release_year)

# remove rows with negative differences
validation_prep <- validation_prep %>%
  filter(diff_year >= 0)

# drop raw timestamp column
validation_prep <- validation_prep %>%
  select(-timestamp)

## 2.4.2 predicted ratings for validation (same approach as model 9)

validate_pred_ratings <- validation_prep %>% 
  left_join(movie_avg_r, by='movieId') %>%
  left_join(movie_r_user_r_avg, by='userId') %>%
  left_join(movie_age_avg, by='diff_year') %>%
  left_join(genre_avg, by="genres") %>%
  mutate(predict = mu_hat + b_i + b_u + b_y + b_g) %>%
  pull(predict)

# target rmse < 0.86490

validate_pred_ratings_rmse <- RMSE(validation_prep$rating, validate_pred_ratings)
RMSE(validation_prep$rating, validate_pred_ratings) # 0.864424 (< target)

#### 3. Results

rmse_results %>%
  mutate(across(-method, num, digits = 8))

#### 4. Conclusion


