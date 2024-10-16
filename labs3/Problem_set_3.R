## Problem set 3. Week 3

# Exercise 3.1: Fit the Frequency Model
# In this exercise, you will perform the analysis corresponding to the response duration by word frequency studied during the lecture. Load in the dataset 'ELP_frequency.csv'.
#  Use mutate() to apply the log10() function to the frequency column. Fit a model in which response durations are modeled as a function of log frequencies.
# Create a plot for the relationship between these two variables.


# HERE WE IMPORTED THE TIDYVERSE PACKAGE WHICH CONTAINS USEFUL LIBRARIES SUCH AS DPLYR, GGPLOT2 ND MANY MORE
library(tidyverse)

# HERE WE READ THE USEFUL FILE 'ELP_FREQUENCY.CSV', AND WE USED THE VIEW() FUNCTION TO PREVIEW THE DATA TO FAMILIARZE OURSELVES WITH THE COLUMN NAMES
data <- read.csv("ELP_frequency.csv")
View(data)


# WE CREATED A DATA VARIABLE, THEN WE CREATED A COLUMN CALLED FREQUENCY_LOG,
# WHICH IS SIMPLY THE LOG CONVERSION OF THE COLUMN FREQ, WE USED THE MUTATE() FUNCTION
# TO CREATE A NEW COLUMN INTO THE DATA DATABASE.

data <- data %>%
    mutate(Frequency_log = log10(Freq))


# I PRINTED THE DATAFRAME TO VIEW THE NEW COLUMN FREQUENCY_LOG
data


# HERE I FIT A MODEL IN WHICH RESPONSE DURATIONS ARE MODELED AS A FUNCTION OF LOG FREQUENCIES (WHHICH WAS JUST CREATED ABOVE).
# WE SAVED THE MODEL TO A VARIABLE NAME CALLED 'MODEL'
model <- lm(data$RT ~ data$Frequency_log)
model


# HERE WE PLOTTED A GRAPH, WITH THE X AXIS BEING A FREQUENCY LOG and Y AXIS AS A RESPONSE TIME
# SPECIFIED WHAT TYPEN OF GRAPH, BEING GEOM_POINT
# WE SPECIFIED A CONFIDENCE INTERVAL, USING THE GEOM_SMOOTH FUNCTION
# SPECIFIED THE AXIS LABELELLED
ggplot(data, aes(x = Frequency_log, y = RT)) +
    geom_point() +
    geom_smooth(method = lm, color = "blue") +
    labs(
        title = "Response Duation versus Log Frequency",
        x = "Log Frequency", y = "Response Duration"
    )


# Exersise 3.2. Additional exercise: can you add a horizontal line
# showing the mean response duration using geom_hline() and the yintercept aesthetic?

# HERE I FOUND THE MEAN OF THE RESPONSE TIME USING THE mean() FUNCTION. AND SAVED IT
# INTO A VARIABLE NAME CALLED 'mean'
mean <- mean(data$RT)



# THIS PLOT WAS USED TO ADD A HORIZONTAL LINE TO THE PLOT, WHICH IS THE MEAN OF THE RESPONSE
# DURATION, AND WE USED THE 'yintercept' AS AN ARGUMENT IN THE 'geom_hline' FUNCTION TO
# SPECIFY THE 'mean'
ggplot(data, aes(x = Frequency_log, y = RT)) +
    geom_point() +
    geom_smooth(method = lm, color = "blue") +
    geom_hline(yintercept = mean, linetype = "dashed", color = "red")
labs(
    title = "Response Duation versus Log Frequency",
    x = "Log Frequency", y = "Response Duration"
)


# Exercise 3.3: Calculating R2 by Hand
# Run the following lines in R (this requires that you still have the random x-values
# and random y-values generated in the lecture).
# Try to make sense of each command.
# Compare the resulting number to the R2 value reported by summary(xmdl) or glance(xmdl).
# Use the lecture notes.


# HERE WE CREATED THE RANDOM VARIABLES AGAIN
# TO FOLLOW THE CLASS
# WE USED THE 'rnorm'
x <- rnorm(50)
# WE PRINTED THE FIRST 6 INSTANCES
head(x)
# HERE WE HARD CODED THE LINEAR REGRESSION MODEL y = mx + c
y <- 10 + 3 * x
y

# HERE WE PLOTTED THE FITTED VALUES
plot(x, y, pch = 19)

# THE LAW OF LINEAR REGRESSION IS THAT THE RESIDUALS HAS TO BE NORMAL DISTRIBUTED AROUND THE
# FITTED MODEL (mean)
# HENCE, WE CREATED CREATED RANDOM RESIDUALS BELOW
error <- rnorm(50)


# WE ADDED THE ERRORS TO THE MODEL CREATED ABOVE
y <- 10 + 3 * x + error

plot(x, y, pch = 19)


# HERE WE CREATED A VARIABLE 'xmdl', TO SAVED THE LINEAR MODEL OF Y AS A FUNCTION OF X
xmdl <- lm(y ~ x)
xmdl

# WE PRINTED THE VALUES OF RESIDUALS, FITTED VALUES (PREDICTED VALUES BY THE MODEL), THE SUMMARY
# THE COEEFICIENT, AND WE TRIED TO CREATED A NEW TIBBLE (NEW X VALUES, CALLED XVALS).
# WE USED THE PREDICT() FUNCTION TO PREDICT NEW VALUES FOR Y.
head(fitted(xmdl))
head(residuals(xmdl))
summary(xmdl)
coef(xmdl)
coef(xmdl)[2]
?seq()
xvals <- seq(from = -3, to = 3, by = 0.1)
mypreds <- tibble(x = xvals)
mypreds$fit <- predict(xmdl, newdata = mypreds)
mypreds




# HERE, WE PERFORM THE SAME MODELS
xmdl <- lm(y ~ x)
# HERE, WE CREATED THE BASELINE MODEL, A NULL MODEL
xmdl_null <- lm(y ~ 1)
xmdl_null

# HERE WE USED THE RESIDUALS() FUNCTION TO PRINT THE RESIDUALS
res <- residuals(xmdl)

# HERE, WE PRINTED THE RESIDUALS() FUNCTION TO PRINT THE ERRORS OF THE NULL MODEL
res_null <- residuals(xmdl_null)

# HERE, WE SUMMED UP ALL THE RESIDUALS IN THE OBSERVATION FROM THE FITTED VALUES,
# AND WE SQUARED THE SUM, GIVING THE SUM OF SQUARED ERROR
sum(res^2)
# HERE, WE SUMMED ALL THE ERRORS OR RESIDUALS FROM THE BASE MODEL, THE NULL MODELS
# WE THEN SQUARED THE SUM OF THE SQUARED RESIDUALS.
sum(res_null^2)

# HERE WE PERFORMED A MATHEMATICAL EQUATION, 1 - SSE/ SSE(null). WHICH SHOULD IN THEORY
# GIVE US THE R2 SQUARED, SAME AS THE THAT COMPUTED AND PRINTED IN THE SUMMARY() OF THE MODEL
1 - (sum(res^2) / sum(res_null^2))


# HERE WE PRINTED THE RESULT OF THE HARD CODED FUNCTION
# 0.8906501
#
sumamry <- summary(xmdl)
sumamry$r.squared
# Multiple R-squared:  0.8906501

# THIS GIVES US THE SAME ANSWERS MEANING.
