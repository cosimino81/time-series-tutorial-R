
# Creation of the dataset
set.seed(54)
myts <- ts(c(rnorm(50, 34, 10), rnorm(67, 7, 1), runif(23, 3, 14)))

###---- 1) PLOT THE DATASET AND: ----
#         - explain the statisticail trait of it
#         - identify the problems and make a plan to fix them
plot(myts)

# As we can see the dataset has not constant mean and not constant variance.
# It's divided essentially into two parts (even three, the third part start at t=120)


###--- 2) SET UP THREE FORECAST MODELS WITH 10 STEPS INTO THE FUTURE ----
#       - USE:Naive, mean and drift methods
library(forecast)

mean_m <- meanf(myts,h = 10)
naive_m <- naive(myts, h = 10)
drift_m <- rwf(myts, h = 10, drift = T)

###--- 3) GET THE PLOT WITH THE THREE FORECASTS OF THE MODEL ----
#       - Add a legend
#       - Reassemble the plot of the course
#
# If we analyse the forecasts of the methods it seems that the naive approach wins on the others.
# In fact it projects the last observations to the future
plot(mean_m, plot.conf = F, main = "", bty = "l") 
lines(naive_m$mean, col = 123, lwd =2)
lines(drift_m$mean, col =22, lwd =2) 
legend("bottomleft", lty = 1,
       col = c(4,123,22), 
       bty = "n", # Delete the border of the legend
       cex = 0.75, # Minimise the size of the legend
       legend = c("Mean method", "Naive method", "Drift method"))

###--- 4) WHICH METHOD LOOKS MORE PROMISING? ----
#       - Calculate the accuracy
#
# In order to calculate the accuracy of our models we have to fit it on the 
# training set and than to calculate the accuracy on the test set.
# The training set is usually the 80% of all the observations,
# the test set is the 20%

length(myts)
# training size
(140/100)*80 
# test size
(140/100)*20

## -Training and test
training_set <- window(myts, start= 1, end = 112)
test_set <- window(myts, start = 113, end =140)

## -Fitting the model
mean_fitted <- meanf(training_set, h = 28) # h=28 are the points to predict
naive_fitted <- naive(training_set, h = 28)
drift_fitted <- rwf(training_set, h = 28, drift = T)

## -Accuracy
# As we have seen into the plot, the naive method is the one who gives us
# the lowest values for the errors
accuracy(mean_fitted, test_set)
accuracy(naive_fitted, test_set)
accuracy(drift_fitted, test_set)


###--- 5) GET THE ERROR MESURE AND COMPARE THEM ----
#       - Do the results match the visual impression of the plot?
#       - If not why?

# (Answer) From a first sight we can see that the mean is close to zero
plot(naive_m$residuals)

###--- 6) BASED ON THE ERROR MESURE: ----
#         - Check all relevant statistical traits of the best method:
#         - Mean of zero
#         - Standard distribution of the residuals
#         - No autocorrelation in the residuals
#         - Costant variance

## -The mean is close to zero
# [2:140] because the first element is NA
mean(naive_m$residuals[2:140])

# The residual doesn't seems normal distributed
hist(naive_m$residuals)


## -The Shapiro-Wilk test 
# It's a test used to check if a variable is normal distributed.
# A p-value < 0.05 refuse the null hypoesis, it means that the 
# residuals are not normal distributed.
shapiro.test(naive_m$residuals)

## -Residual autocorrelation
# We have four bars crossing the threshold (except the first one)
# It means that the residuals are autocorrelated.
acf(naive_m$residuals[2:140])

## -Variance
plot(naive_m$residuals)

# So far we have got negative indicators that the model we have used (naive) is not good.
# Even thought the mean is close to zero they are not normal distributed they are autocorrelated,
# and the variance is not constant across the serie.
# Anyway, from the three we have tested it results the one who fits better.

## Additional checking of the residuals
# The Ljung-Box test
checkresiduals(naive_m)



###--- 7) EXAMIN THE RESULTS: ARE THERE ANY FIXES NEEDED? ----
#       - What is the easiest tool to improve the model?


###--- 8) PERFORME THE WHOLE ANALYSIS WITH THE LOG TRANSFORMATION ON THE DATA ----
#       - Check if the assumption are now met
mylog_ts <- log(myts)

## -As we can see the log of the original serie has a smaller variance (1.5 - 4.0)
plot(mylog_ts)
plot(myts)


## -Fitting the 3 mothods
log_mean_m <- meanf(mylog_ts, h = 10)
log_naive_m <- naive(mylog_ts, h = 10)
log_drift_m <- rwf(mylog_ts, drift = T)

## -Plotting the mean of the three methods
# The pattern shown is similar as before
plot(log_mean_m, plot.conf =F, main = "Log of the serie")
lines(log_naive_m$mean, col =123)
lines(log_drift_m$mean, col =22)
legend("bottomleft", lty = 1,
       col = c(4,123,22), 
       bty = "n", # Delete the border of the legend
       cex = 0.75, # Minimise the size of the legend
       legend = c("Mean method", "Naive method", "Drift method"))



## -Testing the accuracy
# Creation of the training set and test set
log_training <- window(mylog_ts, start = 1, end = 112)
log_test <- window(mylog_ts, start = 113, end =140)


## -Fitting the models on the training set
log_mean_train <- meanf(log_training, h = 28)
log_naive_train <- naive(log_training, h = 28)
log_drift_train <- rwf(log_training, drift = T)


## -Accuracy
# The naive method seems the most accurate
accuracy(log_mean_train, log_test)
accuracy(log_naive_train, log_test)
accuracy(log_drift_train, log_test)


## -Checking the statistics traits of the residuals
# Plotting the residuals
plot(log_naive_m$residuals)

## -Calculate the mean
# very close to zero
mean(log_naive_m$residuals[2:140])


## -Histogram of the residuals
# It seems that they are not normal distributed
hist(log_naive_m$residuals)


## -Shapiro Test
# The p-value < 0.05 so it means that the residuals are normal distributed 
shapiro.test(log_naive_m$residuals)


## -Autocorrelation test
# Here we can see a big emprovement, there is only a single line 
# that exceed the threshold (except the first one)
acf(log_naive_m$residuals[2:140])





