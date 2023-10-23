# Assuming your functions are in another file called 'my_functions.R'
source("main_functions.R")

#install.packages("ranger")
library(ranger)

mydata <- load_data(id_number=1, lags=1, columns_remove='', scaling='s')

# Train the model
model <- ranger(train_y ~ ., data = data.frame(train_y=mydata$train_y, mydata$train_x), num.trees = 100)

# Predict on the test set
predictions_ranger <- predict(model, mydata$test_x)$predictions


compare_plot(actual = mydata$test_y, pred = predictions_ranger)


# Calculate Mean Squared Error
mse_ranger <- mean((predictions_ranger - test_y)^2)
print(paste("Mean Squared Error on Test Set using ranger:", mse_ranger))


pred <- cbind.data.frame(x=1:length(predictions_ranger), real = test_y, prediction = predictions_ranger)

# Create a line plot with different colors using plotly
plot <- plot_ly(pred, x = ~x) %>%
  add_lines(y = ~real, name = "Line 1", line = list(color = "blue")) %>%
  add_lines(y = ~prediction, name = "Line 2", line = list(color = "red")) %>%
  layout(
    xaxis = list(title = "X-Axis"),
    yaxis = list(title = "Y-Axis"),
    title = "Two Lines with Different Colors"
  )

# Display the interactive plot
plot



predict_function <- function(model, newdata) {
  predict(model, newdata)$predictions
}

explainer <- DALEX::explain(
  model = model,
  data = mydata$test_x[1:5,],
  y = mydata$test_y[1:5],
  predict_function = predict_function,
  verbose = FALSE
)

ms_ranger <- modelStudio(explainer, new_observation = data.frame(mydata$test_x)[50:55, ], new_observation_y = mydata$test_y[50:55])
r2d3::save_d3_html(ms_ranger, file = "./modelStudio_output_ranger.html")
ms_ranger

