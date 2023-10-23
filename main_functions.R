# Remove all objects from the global environment
rm(list = ls())

# List of packages to install and load
packages_to_install <- c("dplyr", "ggplot2", "RSQLite", "plotly",
                         "keras","DALEX", "modelStudio", "r2d3", "fastshap")

# Install packages if they are not already installed
for (pkg_name in packages_to_install) {
  if (!require(pkg_name, character.only = TRUE)) {
    # If loading fails, attempt to install the package
    install.packages(pkg_name, dependencies = TRUE)
    library(pkg_name, character.only = TRUE)
    }
}


#load data

load_data <- function(id_number=1, lags=0, columns_remove='', scaling='s') {
  # Replace 'your_database.db' with the path to your SQLite3 database file
  con <- dbConnect(SQLite(), dbname = 'Area1_Meters.db')
  
  table_names <- dbListTables(con)
  
  # Execute the query and store the result in a data frame
  mydata <- dbGetQuery(con, paste("SELECT * FROM", shQuote(table_names[id_number], type = "sh")))
  
  while (nrow(mydata)==0) {
    mydata <- dbGetQuery(con, paste("SELECT * FROM", shQuote(table_names[random_integer <- sample(1:length(table_names), 1)], type = "sh")))
  }
  
    mydata <- mydata[, !names(mydata)==columns_remove]
    

  if (lags>0) {
    for (order in 1:lags) {
      lag_column_name <- paste("VAL_Lag", order, sep = "_")
      
      mydata <- mydata %>%
        mutate({{ lag_column_name }} := lag(VAL, n = order))
    }
  }
  
  # Remove rows with NA values using na.omit()
  mydata <- na.omit(mydata)
  
  train_data <- subset(mydata, year=='2021')
  test_data <- subset(mydata, year=='2022')
  
  train_month <- train_data$READTS
  test_month <-  test_data$READTS
  
  train_data <- train_data[, !names(train_data) %in% c('READTS', 'year')]
  test_data <- test_data[, !names(test_data) %in% c('READTS', 'year')]
  
  
  if (scaling == "s") { # Standardization
    # Separate features and target
    train_x <- train_data[, !names(train_data) == 'VAL']
    train_y <- train_data[, 'VAL']
    
    test_x <- test_data[, !names(test_data) == 'VAL']
    test_y <- test_data[, 'VAL']
    
    # Standardize data
    mean <- apply(train_x, 2, mean)
    std <- apply(train_x, 2, sd)
    
    train_x <- scale(train_x, center = mean, scale = std)
    test_x <- scale(test_x, center = mean, scale = std)
    
    train_data <- cbind.data.frame('VAL' = train_y, train_x)
    test_data <- cbind.data.frame('VAL' = test_y, test_x)
  } else if (scaling == "n") { # Normalization
    # Separate features and target
    train_x <- train_data[, !names(train_data) == 'VAL']
    train_y <- train_data[, 'VAL']
    
    test_x <- test_data[, !names(test_data) == 'VAL']
    test_y <- test_data[, 'VAL']
    
    # Normalize data
    min_vals <- apply(train_x, 2, min)
    max_vals <- apply(train_x, 2, max)
    
    train_x <- scale(train_x, center = min_vals, scale = max_vals - min_vals)
    test_x <- scale(test_x, center = min_vals, scale = max_vals - min_vals)
    
    train_data <- cbind.data.frame('VAL' = train_y, train_x)
    test_data <- cbind.data.frame('VAL' = test_y, test_x)
  }
  
  train_x <- as.matrix(train_data[, !names(train_data)=='VAL'])
  train_y <- train_data[, 'VAL']
  
  test_x <- as.matrix(test_data[, !names(test_data)=='VAL'])
  test_y <- test_data[, 'VAL']
  
  dbDisconnect(con)
  
  #result <- list('train'=train_data, 'test'=test_data)
  result <- list("train_x"=train_x, "train_y"=train_y, "test_x"=test_x, "test_y"=test_y, "train_time"=train_month, "test_time"=test_month)
  
  return(result)
}

compare_plot <- function(actual, pred) {
  pred_df <- cbind.data.frame(x=1:length(actual), actual = actual, prediction = pred)
  
  mse <- mean((pred - actual)^2)
  
  
  # Create a line plot with different colors using plotly
  plot <- plot_ly(pred_df, x = ~x) %>%
    add_lines(y = ~actual, name = "Actual", line = list(color = "blue")) %>%
    add_lines(y = ~pred, name = "Predicted", line = list(color = "red")) %>%
    layout(
      xaxis = list(title = "Month"),
      yaxis = list(title = "VAL"),
      title = paste("Mean Squared Error on Test Set:", round(mse, 2))
    )
  
  # Display the interactive plot
  plot  
}

