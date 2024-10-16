# Problem set 2.


library(tidyverse)
library(magrittr)


# 2.1 Please, take your time to review the tidyverse style guide:
# http://style.tidyverse.org/


# 2.2 Now, have a look at the RStudio keyboard shortcut list:
#  https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts


# 2.3 Cool, right? Now, think about which shortcuts you want to use in the future in your practice.


# 2.4 Let's Subset a data frame with Tidyverse Function
# This exercise uses the nettle data frame to explore different ways of indexing using filter() and select(). First, load in the nettle data:
nettle <- read.csv("nettle_1999_climate.csv")
head(nettle) # display first 6 rows
# Next, attempt to understand what the following commands do. Then execute them in R and see whether the output matches your expectations.
filter(nettle, Country == "Benin")
filter(nettle, Country %in% c("Benin", "Zaire"))
select(nettle, Langs)
filter(nettle, Country == "Benin") %>% select(Langs)
filter(nettle, Country == "Benin") %>%
  select(Population:MGS)
filter(nettle, Langs > 200)
filter(nettle, Langs > 200, Population < median(Population))


# 2.5 Exercise: Creating a Pipeline
# Execute the following code in R (you may omit the comments for now) and then read the explanation below.
# Reduce the nettle tibble to small countries:
smallcountries <- filter(nettle, Population < 4)
# Create categorical MGS variable:
nettle_MGS <- mutate(smallcountries,
  MGS_cat = ifelse(MGS < 6, "dry", "fertile")
)
# Group tibble for later summarizing:
nettle_MGS_grouped <- group_by(nettle_MGS, MGS_cat)
# Compute language counts for categorical MGS variable:
summarize(nettle_MGS_grouped, LangSum = sum(Langs))

# The previous code reduces the nettle tibble to small countries (Population < 4).
# The resulting tibble, smallcountries, is changed using the ifelse() function.
# In this case, the function splits the dataset into countries with high and low ecological risk, using six months as a threshold.
# The ifelse() function spits out 'dry' when MGS < 6 is TRUE and 'fertile' when MGS < 6 is FALSE.
# Then, the resulting tibble is grouped by this categorical ecological risk measure.
# As a result of the grouping, the subsequently executed summarize() function knows that summary statistics should be computed based on this grouping variable.
# This code is quite cumbersome! In particular, there are many intervening tibbles
# (smallcountries, nettle_MGS, and nettle_MGS_grouped) that might not be used anywhere else in the analysis.
# For example, the grouping is only necessary so that the summarize() function knows what groups to perform summary statistics for.
# These tibbles are fairly dispensable.
# Can you condense all of these steps into a single pipeline where the nettle tibble is first piped to filter(),
# then to mutate(), then to group_by(), and finally to summarize()?


# MY COMMENT
# HERE WE WERE ASKED TO ELIMINATE THE REDUNDANT VARIABLE NAMES.
# SO, WE EMPLOYED THE USE OF PIPING %>%
# WE FIRSTLY TOOK THE VARIABLE NAME HOLDING THE DATA, AND WE PIPED IT INTO A FILTER FUNCTION
# THE FILTER FUNCTION, WHICH IS USED TO FILTER THE ROWS BASED ON A CERTAIN CONDITION
# IN THIS CASE, THE CONDITION IS POPULATION LESS THAN 4.
# THEN, WE TOOK THIS 'FILTERED' DATA AND PIPED IT INTO ANOTHER FUNCTION 'MUTATE'
# MUTATE ALTER THE TIBBLE TO CREATE A NEW COLUMN, AND IT TAKES A CONDITION,
# THE CONDITION IN THIS CASE BEING, IF MGS IS LESS THAN 6, LABEL 'DRY', ELSE LABEL 'FERTILE'
# THEN WE TOOK THE OUTCOME OF THIS AND PIPED IT INTO A FUNCTION CALLED GROUPBY(),
# THE GROUPBY() TAKES AN ARGUMENT WITH WHICH TO GROUP THE TIBBLE BY in THIS CASE MGS_cat
# DRY and FERTILE
# FINALLY, WE PIPED THIS INTO A FUNCTION 'SUMMARIZE()', WHICH GAVE US, THE SUM OF LANGS (LANGUAGE)
# IN BOTH CATEORY 'DRY' and "FERTILE", BEING 447 and 1717 RESPECTIVELY.


View(nettle)

nettle %>%
  filter(Population < 4) %>%
  mutate(MGS_cat = ifelse(MGS < 6, "dry", "fertile")) %>%
  group_by(MGS_cat) %>%
  summarize(LangSum = sum(Langs))


# 2.6. Plotting a Histogram of the Emotional Valence Ratings:
# With the Warriner et al. (2013) data,
# create a ggplot2 histogram and plot the mean as a vertical line into the plot using geom_vline() and the xintercept aesthetic.
# Can you additionally add vertical dashed lines to indicate where 68% and 95% of the data lie?
# (Ignore any warning messages about binwidth that may arise).


# WE FIRSTLY IMPORTED THE TIDYVERSE AND MAGITR
# WE READ AND SAVED THE FILE INTO THE VARIABLE EMOTION
# WE COMPUTED THE MEAN AND SD using the FUNCTION mean() and sd()
# THE WE USED THE GGPLOT() library IN THE TIDYVERSE TO PLOT THE GRAPH
# TO COMPUTE THE 68% and 95% LEVEL, WE COMPUTED mean + sd and mean + 2*sd
# WE USED THE geom_vline TO PUT THE LINES ON THE PLOTS




qnorm(0.68)



emotion <- read.csv("warriner_2013_emotional_valence.csv")
View(emotion)


mean <- mean(emotion$Val)
sd <- sd(emotion$Val)



hist_plot <- ggplot(emotion, aes(x = Val)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue") +
  geom_vline(aes(xintercept = mean), color = "red", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = mean - sd), color = "blue", linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean + sd), color = "blue", linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean - 2 * sd), color = "purple", linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean + 2 * sd), color = "purple", linetype = "dashed", size = 0.8) +
  labs(
    title = "Histogram with Mean and Standard Deviation Intervals",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()

# 2.7 Plotting Density Graphs:
# In the plot you created in the last exercise, exchange geom_histogram() with geom_density(), which produces a kernel density graph.
# This is a plot that won't be covered in this book, but by looking at it you may be able to figure out that it is essentially a smoothed version of a histogram.
#  There are many other geoms to explore. Check out the vast ecosystem of online tutorials for different types of ggplot2 functions.


# HERE TO CHANGE THE PLOT TO A DENSITY PLOT
# WE CHANGED THE 'geom_histogram' IN THE PREVIOUS PLOT TO 'geom_density'


density_plot <- ggplot(emotion, aes(x = Val)) +
  geom_density(binwidth = 0.5, color = "black", fill = "lightblue") +
  geom_vline(aes(xintercept = mean), color = "red", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = mean - sd), color = "blue", linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean + sd), color = "blue", linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean - 2 * sd), color = "purple", linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean + 2 * sd), color = "purple", linetype = "dashed", size = 0.8) +
  labs(
    title = "Histogram with Mean and Standard Deviation Intervals",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()

# Also add the code to save as an PNG file
ggsave("emotional_valence_density_plot.png", plot = density_plot, width = 8, height = 5)
ggsave("emotional_valence_histogram_plot.png", plot = hist_plot, width = 8, height = 5)


# HERE WE HAD SAVED EACH PLOTS AS HIST_PLOT AND DENSITY PLOT AND SAVE THE PLOT AS A PNG
# SO WE CAN ADDRESS EACH VARIABLE.
