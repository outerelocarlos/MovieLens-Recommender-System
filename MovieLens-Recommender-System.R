
##### Project Libraries #####
  
  # Installing the required libraries (if they are not installed already)
  if(!require(caret)) {
    install.packages("caret")
  }
  if(!require(cowplot)) {
    install.packages("cowplot")
  }
  if(!require(data.table)) {
    install.packages("data.table")
  }
  if(!require(dplyr)) {
    install.packages("dplyr")
  }
  if(!require(ggplot2)) {
    install.packages("ggplot2")
  }
  if(!require(ggthemes)) {
    install.packages("ggthemes")
  }
  if(!require(lubridate)) {
    install.packages("lubridate")
  }
  if(!require(mltools)) {
    install.packages("mltools")
  }
  if(!require(recosystem)) {
    install.packages("recosystem")
  }
  if(!require(scales)) {
    install.packages("scales")
  }
  if(!require(stringr)) {
    install.packages("stringr")
  }
  if(!require(tibble)) {
    install.packages("tibble")
  }
  if(!require(tidyr)) {
    install.packages("tidyr")
  }
  
  # Loading the required libraries
  library(caret)
  library(cowplot)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  library(lubridate)
  library(mltools)
  library(recosystem)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)

##### Data import/loading #####

  # The dataset ZIP file is downloaded
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  # The ratings.dat is imported into the workspace
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                  col.names = c("userId", "movieId", "rating", "timestamp"))
  class(ratings)
  head(ratings)

  # The movies.dat is imported into the workspace
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  class(movies)
  head(movies)
  
  # The movies object is converted into a dataframe
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                              title = as.character(title),
                                              genres = as.character(genres))
  class(movies)
  head(movies)
  
  # Ratings and movies dataframes are joined by "movieId"
  movielens <- left_join(ratings, movies, by = "movieId")
  class(movielens)
  head(movielens)

##### Validation set construction #####

  # Using built-in R functions to create the working and validation sets
  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
  validation_index <- sample(1:nrow(movielens), 0.1*nrow(movielens))
  working_set <- movielens[-validation_index,]
  temp <- movielens[validation_index,]

  tibble(Dataset = c("movielens", "working_set", "temp"),
        "Number of ratings" = c(nrow(movielens), nrow(working_set), nrow(temp)))
  
  # Using createDataPartition to create the working and validation sets
  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  working_set <- movielens[-test_index,]
  temp <- movielens[test_index,]

  tibble(Dataset = c("movielens", "working_set", "temp"),
        "Number of ratings" = c(nrow(movielens), nrow(working_set), nrow(temp)))
  
  # Make sure userId and movieId in validation set are also in working_set set
  validation <- temp %>% 
        semi_join(working_set, by = "movieId") %>%
        semi_join(working_set, by = "userId")

  # Add rows removed from validation set back into working_set set
  removed <- anti_join(temp, validation)
  working_set <- rbind(working_set, removed)

##### Data Exploration #####

  dim(working_set)
  class(working_set)
  str(working_set, vec.len = 2)
  head(working_set)
  summary(working_set)

##### Ratings Exploration #####

  # Rating count
  working_set %>%
    group_by(rating) %>%
    summarize(count = n())
  
  # Rating distribution plot
  working_set %>%
    group_by(rating) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = rating, y = count)) +
    geom_bar(stat = "identity", fill = "#8888ff") +
    ggtitle("Rating Distribution") +
    xlab("Rating") +
    ylab("Occurrences Count") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

##### Timestamps Exploration #####

  # as_datetime() showcase
  sample(as_datetime(working_set$timestamp, origin = "1970-01-01"), replace = TRUE, size = 20)
  
  # Yearly rating count
  working_set %>% 
    mutate(year = year(as_datetime(timestamp, origin = "1970-01-01"))) %>%
    group_by(year) %>%
    summarize(count = n())
  
  # Ratings per year plot
  working_set %>% 
    mutate(year = year(as_datetime(timestamp, origin = "1970-01-01"))) %>%
    ggplot(aes(x = year)) +
    geom_bar(fill = "#8888ff") + 
    ggtitle("Ratings per year") +
    xlab("Year") +
    ylab("Number of ratings") +
    scale_y_continuous(labels = comma) + 
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))
  
  # Average rating per year plot
  working_set %>% 
    mutate(year = year(as_datetime(timestamp, origin = "1970-01-01"))) %>%
    group_by(year) %>%
    summarize(avg = mean(rating)) %>%
    ggplot(aes(x = year, y = avg)) +
    geom_bar(stat = "identity", fill = "#8888ff") + 
    ggtitle("Average rating per year") +
    xlab("Year") +
    ylab("Average rating") +
    scale_y_continuous(labels = comma) + 
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

##### Movies Exploration #####

  # Movie popularity count
  working_set %>% 
    group_by(movieId) %>% 
    summarize(count = n()) %>%
    slice_head(n = 10)
  
  # Movie popularity summary
  summary(working_set %>% group_by(movieId) %>% summarize(count = n()) %>% select(count))
  
  # Ratings per movie plot
  working_set %>%
    group_by(movieId) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = movieId, y = count)) +
    geom_point(alpha = 0.2, color = "#4020dd") +
    geom_smooth(color = "red") +
    ggtitle("Ratings per movie") +
    xlab("Movies") +
    ylab("Number of ratings") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))
  
  # Movies' rating histogram
  working_set %>%
    group_by(movieId) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = count)) +
    geom_histogram(fill = "#8888ff", color = "#4020dd") +
    ggtitle("Movies' rating histogram") +
    xlab("Rating count") +
    ylab("Number of movies") +
    scale_y_continuous(labels = comma) +
    scale_x_log10(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

##### Users Exploration #####

  # User rating count (activity measure)
  working_set %>% 
    group_by(userId) %>% 
    summarize(count = n()) %>%
    slice_head(n = 10)
  
  # User rating summary
  summary(working_set %>% group_by(userId) %>% summarize(count = n()) %>% select(count))
  
  # Ratings per user plot
  working_set %>%
    group_by(userId) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = userId, y = count)) +
    geom_point(alpha = 0.2, color = "#4020dd") +
    geom_smooth(color = "red") +
    ggtitle("Ratings per user") +
    xlab("Users") +
    ylab("Number of ratings") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))
  
  # Users' rating histogram
  working_set %>%
    group_by(userId) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = count)) +
    geom_histogram(fill = "#8888ff", color = "#4020dd") +
    ggtitle("Users' rating histogram") +
    xlab("Rating count") +
    ylab("Number of users") +
    scale_y_continuous(labels = comma) +
    scale_x_log10(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))
  
  # User x Movie matrix construction
  limit <- 60
  user_movie_matrix <- working_set %>% 
    filter(userId %in% sample(unique(working_set$userId), limit)) %>%
    select(userId, movieId, rating) %>%
    mutate(rating = 1) %>%
    spread(movieId, rating) %>% 
    select(sample(ncol(.), limit)) %>% 
    as.matrix() %>% 
    t(.) # This function transposes the matrix

  # Matrix plot
  user_movie_matrix %>% 
    image(1:limit, 1:limit,., xlab = "Movies", ylab = "Users") +
    abline(h = 0:limit + 0.5, v = 0:limit + 0.5, col = "grey") +
    title(main = list("User x Movie matrix", cex = 1, font = 2))

##### Genres Exploration #####

  # Genres count
  working_set %>% 
    group_by(genres) %>% 
    summarize(count = n()) %>%
    slice_head(n = 8)
  
  # Individual genres count
  genres <- c("Action", "Adventure", "Animation", 
              "Children", "Comedy", "Crime", 
              "Documentary", "Drama", "Fantasy", 
              "Film-Noir", "Horror", "Musical", 
              "Mystery", "Romance", "Sci-Fi", 
              "Thriller", "War", "Western")

  genres_df <- data.frame(
    Genres = genres,
    Count = sapply(genres, function(x) {
      sum(str_detect(working_set$genres, x))
      })
  )

  print(genres_df)
  
  # Genre popularity plot
  genres_df %>%
    ggplot(aes(x = Count, y = Genres)) +
    ggtitle("Genre Popularity") +
    geom_bar(stat = "identity", width = 0.6, fill = "#8888ff") +
    xlab("Number of ratings") +
    ylab("Genres") +
    scale_x_continuous(labels = comma) +
    theme_economist() +
    theme(plot.title = element_text(vjust = 3.5),
          axis.title.x = element_text(vjust = -5, face = "bold"),
          axis.title.y = element_text(vjust = 10, face = "bold"),
          axis.text.x = element_text(vjust = 1, hjust = 1, angle = 0),
          axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 12),
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

  # Average rating for each genre
  genres_df_2 <- data.frame(
    Genres = genres,
    Rating = sapply(genres, function(x) {
      mean(working_set[str_detect(working_set$genres, x)]$rating)
    })
  )

  print(genres_df_2)

  # Genre rating summary
  summary(genres_df_2)

  # Genre rating plot
  genres_df_2 %>%
    ggplot(aes(x = Rating, y = Genres)) +
    ggtitle("Genre Average Rating") +
    geom_bar(stat = "identity", width = 0.6, fill = "#8888ff") +
    xlab("Average ratings") +
    ylab("Genres") +
    scale_x_continuous(labels = comma, limits = c(0.0, 5.0)) +
    theme_economist() +
    theme(plot.title = element_text(vjust = 3.5),
          axis.title.x = element_text(vjust = -5, face = "bold"),
          axis.title.y = element_text(vjust = 10, face = "bold"),
          axis.text.x = element_text(vjust = 1, hjust = 1, angle = 0),
          axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 12),
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

##### Preliminary Questions

  # How many rows and columns are there in the training dataset?
  dim(working_set)[1] # Rows
  dim(working_set)[2] # Columns

  # How many zeros were given as ratings in the training dataset?
  sum(working_set$rating == 0.0)

  # How many threes were given as ratings in the training dataset?
  sum(working_set$rating == 3.0)

  # How many different movies are in the training dataset?
  dim(as.data.frame(table(working_set$movieId)))[1]

  # How many different users are in the training dataset?
  dim(as.data.frame(table(working_set$userId)))[1]

  # How many movie ratings belong to the drama, comedy, thriller and romance genres (respectively) in the working_set dataset?
  genres_quiz <- c("Drama", "Comedy", "Thriller", "Romance")
  sapply(genres_quiz, function(x) {
    sum(str_detect(working_set$genres, x))
  })

  # Which of the following movies ("Forrest Gump", "Jurassic Park", "Pulp Fiction", "Shawshank Redemption" and "Speed 2: Cruise Control") has the greatest number of ratings?
  ratings_quiz = c("Forrest Gump", 
                  "Jurassic Park \\(1993", 
                  "Pulp Fiction", 
                  "Shawshank Redemption", 
                  "Speed 2: Cruise Control")
  sapply(ratings_quiz, function(x) {
    sum(str_detect(working_set$title, x))
  })

  # What are the five most given ratings in order from most to least?
  as.data.frame(table(working_set$rating)) %>% arrange(desc(Freq))

##### Train-test split #####

  # Train-test split using R built-in functions

    # Approach 1: training index

    set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
    train_index <- sample(1:nrow(movielens), 0.9*nrow(movielens))
    train_set <- movielens[train_index,]
    temp_test_set <- movielens[-train_index,]

    tibble(Dataset = c("movielens", "train_set", "temp_test_set"),
          "Number of ratings" = c(nrow(movielens), nrow(train_set), nrow(temp_test_set)))

    # Approach 2: testing index

    set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
    test_index <- sample(1:nrow(movielens), 0.1*nrow(movielens))
    train_set <- movielens[-test_index,]
    temp_test_set <- movielens[test_index,]

    tibble(Dataset = c("movielens", "train_set", "temp_test_set"),
          "Number of ratings" = c(nrow(movielens), nrow(train_set), nrow(temp_test_set)))
  
  # Train-test split using createDataPartition

    # Approach 1: training index

    set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
    train_index <- createDataPartition(movielens$rating, times = 1, p = 0.9, list = FALSE)
    train_set <- movielens[train_index,]
    temp_test_set <- movielens[-train_index,]

    tibble(Dataset = c("movielens", "train_set", "temp_test_set"),
          "Number of ratings" = c(nrow(movielens), nrow(train_set), nrow(temp_test_set)))

    # Approach 2: testing index

    set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
    test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
    train_set <- movielens[-test_index,]
    temp_test_set <- movielens[test_index,]

    tibble(Dataset = c("movielens", "train_set", "temp_test_set"),
          "Number of ratings" = c(nrow(movielens), nrow(train_set), nrow(temp_test_set)))
  
  # Make sure userId and movieId in the testing set are also in the training set set
  test_set <- temp_test_set %>% 
        semi_join(train_set, by = "movieId") %>%
        semi_join(train_set, by = "userId")

  # Add rows removed from the testing set back into the training set set
  removed <- anti_join(temp_test_set, test_set)
  train_set <- rbind(train_set, removed)

##### Random guessing #####

  # Random guessing model and predictions
  rating_range <- seq(0.5, 5, 0.5)
  guess_right <- function(x, y) {
    mean(y == x)
  }

  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
  simulation <- replicate(10000, {
    i <- sample(train_set$rating, 1000, replace = TRUE)
    sapply(rating_range, guess_right, i)
  })

  guess_prob <- c()
  for(i in 1:nrow(simulation)) {
    guess_prob <- append(guess_prob, mean(simulation[i,]))
  }

  y_hat_random <- sample(rating_range, 
                        size = nrow(validation), 
                        replace = TRUE, 
                        prob = guess_prob)
  
  # Evaluation tibble construction
  evaluation <- tibble(Model = c("Cinematch", "The Netflix Prize", "Random guessing"),
                       MAE = c(NA, NA, MAE(validation$rating, y_hat_random)),
                       MSE = c(NA, NA, mse(validation$rating, y_hat_random)),
                       RMSE = c(0.9525, 0.85725, RMSE(validation$rating, y_hat_random)))
  print(evaluation)

##### Linear model #####
  
  # Mean baseline model construction
  mu <- mean(train_set$rating)
  y_hat_mean <- rep(mu, nrow(validation))

  evaluation <- bind_rows(evaluation, tibble(Model = "Linear model (mean baseline)",
                                             RMSE = RMSE(validation$rating, y_hat_mean),
                                             MSE  = mse(validation$rating, y_hat_mean),
                                             MAE  = MAE(validation$rating, y_hat_mean)))
  print(evaluation)
  
  # Bias per movie table
  b_i <- train_set %>%
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu),
              b_i_isolated = mean(rating))
  b_i %>% slice_head(n = 10)
  
  # Isolated movie bias plot
  b_i_isolated_plot <- b_i %>%
    ggplot(aes(x = b_i_isolated)) + 
    geom_histogram(bins = 20, fill = "#8888ff", color = "#4020dd") +
    ggtitle("Movie Bias (isolated)") +
    xlab("Bias value") +
    ylab("Count") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))
  
  # Adjusted movie bias plot
  b_i_plot <- b_i %>%
    ggplot(aes(x = b_i)) + 
    geom_histogram(bins = 20, fill = "#8888ff", color = "#4020dd") +
    ggtitle("Movie Bias (adjusted)") +
    xlab("Bias value") +
    ylab("Count") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

  # Both b_i plots are combined with plot_grid()
  plot_grid(b_i_isolated_plot, b_i_plot, labels = "AUTO", nrow = 2)
  
  # Linear model construction (mean + movie bias)
  y_hat_b_i <- mu + validation %>%
    left_join(b_i, by = "movieId") %>%
    .$b_i

  evaluation <- bind_rows(evaluation,
                          tibble(Model = "Linear model (mean + movie bias)",
                                 RMSE = RMSE(validation$rating, y_hat_b_i),
                                 MSE  = mse(validation$rating, y_hat_b_i),
                                 MAE  = MAE(validation$rating, y_hat_b_i)))
  print(evaluation)
  
  # Bias per user
  b_u <- train_set %>%
    left_join(b_i, by = 'movieId') %>%
    group_by(userId) %>% 
    summarize(b_u = mean(rating - mu - b_i),
              b_u_isolated = mean(rating))
  b_u %>% slice_head(n = 10)
  
  # Isolated user bias plot
  b_u_isolated_plot <- b_u %>%
    ggplot(aes(x = b_u_isolated)) + 
    geom_histogram(bins = 20, fill = "#8888ff", color = "#4020dd") +
    ggtitle("User Bias (isolated)") +
    xlab("Bias value") +
    ylab("Count") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

  # Adjusted user bias plot
  b_u_plot <- b_u %>%
    ggplot(aes(x = b_u)) + 
    geom_histogram(bins = 20, fill = "#8888ff", color = "#4020dd") +
    ggtitle("User Bias (adjusted)") +
    xlab("Bias value") +
    ylab("Count") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

  # Both b_u plots are combined with plot_grid()
  plot_grid(b_u_isolated_plot, b_u_plot, labels = "AUTO", nrow = 2)
  
  # Linear model construction (mean + movie bias + user bias)
  y_hat_b_u <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(y_hat = mu + b_i + b_u) %>%
    .$y_hat
  
  evaluation <- bind_rows(evaluation, 
                          tibble(Model = "Linear model (mean + movie and user bias)",
                                 RMSE = RMSE(validation$rating, y_hat_b_u),
                                 MSE  = mse(validation$rating, y_hat_b_u),
                                 MAE  = MAE(validation$rating, y_hat_b_u)))
  print(evaluation)
  
  # Top 10 movie recommendation by the linear model
  top10_prediction_linear <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(y_hat = mu + b_i + b_u) %>%
    arrange(desc(y_hat)) %>%
    select(title) %>%
    unique() %>%
    slice_head(n = 10)
  top10_prediction_linear_df <- data.frame(Title = top10_prediction_linear,
                                          Rating = rep(NA, 10), 
                                          Count = rep(NA, 10))

  for (i in 1:10) {
    indexes <- which(test_set$title == as.character(top10_prediction_linear[i]))
    top10_prediction_linear_df$Rating[i] <- mean(test_set$rating[indexes])
    top10_prediction_linear_df$Count[i] <- sum(
      test_set$title == as.character(top10_prediction_linear[i])
    )
  }
  print(top10_prediction_linear_df)
  
  # Worst 10 movie recommendation by the linear model
  worst10_prediction_linear <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(y_hat = mu + b_i + b_u) %>%
    arrange(b_i) %>%
    select(title) %>%
    unique() %>%
    slice_head(n = 10)
  worst10_prediction_linear_df <- data.frame(Title = worst10_prediction_linear,
                                            Rating = rep(NA, 10),
                                            Count = rep(NA, 10))

  for (i in 1:10) {
    indexes <- which(test_set$title == as.character(worst10_prediction_linear[i]))
    worst10_prediction_linear_df$Rating[i] <- mean(test_set$rating[indexes])
    worst10_prediction_linear_df$Count[i] <- sum(
      test_set$title == as.character(worst10_prediction_linear[i])
    )
  }
  print(worst10_prediction_linear_df)

##### Regularization #####

  # Regularization function
  regularization <- function(lambda, train_set, test_set){
    mu <- mean(train_set$rating)

    b_i <- train_set %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu) / (n() + lambda))

    b_u <- train_set %>% 
      left_join(b_i, by="movieId") %>%
      filter(!is.na(b_i)) %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))

    predicted_ratings <- test_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      filter(!is.na(b_i), !is.na(b_u)) %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    
    return(RMSE(predicted_ratings, test_set$rating))
  }
  
  # The regularization function at play
  lambdas <- seq(0, 10, 0.25)
  lambdas_rmse <- sapply(lambdas,
                        regularization, 
                        train_set = train_set, 
                        test_set = test_set)
  lambdas_tibble <- tibble(Lambda = lambdas, RMSE = lambdas_rmse)
  print(lambdas_tibble)
  
  # Lambda's effect on RMSE plot
  lambdas_tibble %>%
    ggplot(aes(x = Lambda, y = RMSE)) +
    geom_point() +
    ggtitle("Lambda's effect on RMSE") +
    xlab("Lambda") +
    ylab("RMSE") +
    scale_y_continuous(n.breaks = 6, labels = comma) +
    scale_x_continuous(n.breaks = 10) +
    theme_economist() +
    theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
          axis.title.y = element_text(vjust = 10, face = "bold"), 
          plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))
  
  # Regularized linear model construction
  lambda <- lambdas[which.min(lambdas_rmse)]

  mu <- mean(train_set$rating)
  
  b_i_regularized <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))

  b_u_regularized <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

  y_hat_regularized <- validation %>% 
    left_join(b_i_regularized, by = "movieId") %>%
    left_join(b_u_regularized, by = "userId") %>%
    mutate(prediction = mu + b_i + b_u) %>%
    pull(prediction)

  evaluation <- bind_rows(evaluation,
                          tibble(Model = "Linear model with regularized bias",
                                MAE  = MAE(validation$rating, y_hat_regularized),
                                MSE  = mse(validation$rating, y_hat_regularized),
                                RMSE = RMSE(validation$rating, y_hat_regularized)))
  print(evaluation)
  
  # Top 10 movie recommendation by the regularized linear model
  top10_prediction_regularized <- test_set %>%
    left_join(b_i_regularized, by = "movieId") %>%
    left_join(b_u_regularized, by = "userId") %>%
    mutate(y_hat = mu + b_i + b_u) %>%
    arrange(desc(y_hat)) %>%
    select(title) %>%
    unique() %>%
    slice_head(n = 10)
  top10_prediction_regularized_df <- data.frame(Title = top10_prediction_regularized,
                                                Rating = rep(NA, 10),
                                                Count = rep(NA, 10))
  
  for (i in 1:10) {
    indexes <- which(test_set$title == as.character(top10_prediction_regularized[i]))
    top10_prediction_regularized_df$Rating[i] <- mean(test_set$rating[indexes])
    top10_prediction_regularized_df$Count[i] <- sum(
      test_set$title == as.character(top10_prediction_regularized[i])
    )
  }
  print(top10_prediction_regularized_df)
  
  # Worst 10 movie recommendation by the regularized linear model
  worst10_prediction_regularized <- test_set %>%
    left_join(b_i_regularized, by = "movieId") %>%
    left_join(b_u_regularized, by = "userId") %>%
    mutate(y_hat = mu + b_i + b_u) %>%
    arrange(y_hat) %>%
    select(title) %>%
    unique() %>%
    slice_head(n = 10)
  worst10_prediction_regularized_df <- data.frame(Title = worst10_prediction_regularized,
                                                  Rating = rep(NA, 10),
                                                  Count = rep(NA, 10))
  
  for (i in 1:10) {
    indexes <- which(test_set$title == as.character(worst10_prediction_regularized[i]))
    worst10_prediction_regularized_df$Rating[i] <- mean(test_set$rating[indexes])
    worst10_prediction_regularized_df$Count[i] <- sum(
      test_set$title == as.character(worst10_prediction_regularized[i])
    )
  }
  print(worst10_prediction_regularized_df)

##### Matrix Factorization #####

  # 1. The training and testing sets need to be converted into recosystem input format
  set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
  train_recosystem <- with(train_set, data_memory(user_index = userId, 
                                                  item_index = movieId,
                                                  rating     = rating))
  test_recosystem <- with(test_set, data_memory(user_index = userId, 
                                                item_index = movieId, 
                                                rating     = rating))

  # 2. The model object is created
  recommendation_system <- Reco()

  # 3. The model is tuned
  tuning <- recommendation_system$tune(train_recosystem, opts = list(dim = c(10, 20, 30),
                                                                    lrate = c(0.1, 0.2),
                                                                    nthread  = 4,
                                                                    niter = 10))

  # 4. The model is trained
  recommendation_system$train(train_recosystem, opts = c(tuning$min,
                                                        nthread = 4,
                                                        niter = 20))

  # 5. A prediction is made
  y_hat_MF <-  recommendation_system$predict(test_recosystem, out_memory())
  
  # The model's RMSE is computed and added to the evaluation table
  evaluation <- bind_rows(evaluation,
                          tibble(Model = "Matrix factorization",
                                MAE  = MAE(validation$rating, y_hat_MF),
                                MSE  = mse(validation$rating, y_hat_MF),
                                RMSE = RMSE(validation$rating, y_hat_MF)))
  print(evaluation)
  
  # Top 10 movie recommendation by the matrix factorization model
  top10_prediction_MF <- tibble(title = test_set$title, y_hat = y_hat_MF) %>%
    arrange(desc(y_hat)) %>%
    select(title) %>%
    unique() %>%
    slice_head(n = 10)
  top10_prediction_MF_df <- data.frame(Title = top10_prediction_MF,
                                      Rating = rep(NA, 10),
                                      Count = rep(NA, 10))

  for (i in 1:10) {
    indexes <- which(test_set$title == as.character(top10_prediction_MF[i,]))
    top10_prediction_MF_df$Rating[i] <- mean(test_set$rating[indexes])
    top10_prediction_MF_df$Count[i] <- sum(
      test_set$title == as.character(top10_prediction_MF[i,])
    )
  }
  print(top10_prediction_MF_df)
  
  # Worst 10 movie recommendation by the matrix factorization model
  worst10_prediction_MF <- tibble(title = test_set$title, y_hat = y_hat_MF) %>%
    arrange(y_hat) %>%
    select(title) %>%
    unique() %>%
    slice_head(n = 10)
  worst10_prediction_MF_df <- data.frame(Title = worst10_prediction_MF,
                                        Rating = rep(NA, 10),
                                        Count = rep(NA, 10))

  for (i in 1:10) {
    indexes <- which(test_set$title == as.character(worst10_prediction_MF[i,]))
    worst10_prediction_MF_df$Rating[i] <- mean(test_set$rating[indexes])
    worst10_prediction_MF_df$Count[i] <- sum(
      test_set$title == as.character(worst10_prediction_MF[i,])
    )
  }
  print(worst10_prediction_MF_df)