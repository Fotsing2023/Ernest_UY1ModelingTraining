rm(list = ls())
library(tidyverse)
library(ggplot2)
# this code sets one theme for all ggplots made in the session. This is the black and white theme, and the 12 refers to the relative size of the text in the plot. We will see examples of how to modify your plot theme manually later, but this is a handy way to change the basic default plot theme within a script.
theme_set(theme_bw(12))
#Data Import and Review
tempData <- read.csv("./temperature.csv")
dim(tempData)
## [1] 6039 29
str(tempData)
summary(tempData)

#Prep and Explore Data
Here we need to subset the data to include only stations in the US. Remember that filter is the
tidyverse function that keeps rows in your dataset that meet a provided logical statement. These logical
statements can include reference to any columns in your dataset.
# Let's check how all the country names are recorded in the data frame, so we can see exactly how the US is written in the dataset.

unique(tempData$CNTRY_NAME)
temp.us <- filter(tempData,
                  CNTRY_NAME == "United States")
dim(temp.us) # 1545 stations in USA
## [1] 1545 29
Let’s now explore the data visually, starting with some histograms. Here we have only one predictor, so
we don’t need to worry about correlation/collinearity issues.

par(mfrow = c(1,2))
hist(tempData$ELEV)
hist(tempData$YEAR)
We can see that we have some of those pesky -999.00 values in the USA data. We can use filter ,
just as above, to remove rows of data that have this value. Here I am overwriting the filtered data onto
my existing object

temp.us <- filter(temp.us,
                  ELEV != -999.0)
dim(temp.us) #looks like we only lost 9 stations due to missing elevation data

Let’s now get an idea of how our predictor variable ( ELEV ) relates to our response variable ( YEAR ). Be
sure you are comfortable with the ggplot commands here, as things will start to get more complicated
with plotting our data and our response curves. 

baseUS <- ggplot(data = temp.us,
                 mapping = aes(x = ELEV, y = YEAR)) +
  geom_point(shape = 21,
             fill = "tomato",
             size = 3,
             alpha = 0.25) +
  labs(x = "Elevation [m]",
       y = "Yearly average temperature [C]",
       title = "USA Stations")
baseUS

We can see then that the relationship or slope should be negative. Plotting like this will also give you a
sense of whether there are any unexpected or strange data points.
Fit a Linear Model

lmUsElev <- lm(YEAR ~ ELEV,
               data = temp.us)
summary(lmUsElev)
print(lmUsElev)

Results here indicate, as we saw in the plot, that temperature decreases with elevation, and the effect of
elevation is significant, that is, the slope is different from zero.

A1. The regression coefficient for elevation is -0.003.
A2. For further interpretation, we can say that for every 100m increase in elevation, we predict a
decrease in 0.3 (100 * 0.003) degrees Celsius in mean annual temperature.

A3. Based on the estimated R-squared value, we can say that about 13% of the variation in mean
annual temperature is explained by elevation.

Plot the Linear Model
Here we can actually use our plot from above and add to it, as I do below. You can also just start from
scratch if you prefer, but this is one of the advantages of saving your plot as an object.
baseUS +

geom_abline(intercept = coef(lmUsElev)[1],
              slope = coef(lmUsElev)[2])

Predict at New Location
We can now use this resulting model to predict the annual temperature in Front Royal, VA, which sits at
about 170 meters asl. We can do this in a few ways in R, but here I first do it manually, pulling the
regression coefficients from my model result.

coef(lmUsElev)[1] + coef(lmUsElev)[2] * 170

## (Intercept)
## 12.35319
I can also use the predict function, first setting up a basic data frame, with all the values I want to predict
for. Here there is only one value I need, 170. Note that the column name here has to be exactly the same
as the column in the original data.

newdf <- data.frame(ELEV = 170)
predict(lmUsElev, newdata = newdf)
## 1
## 12.35319
A4. So, we would predict, based on a model with only elevation, that the mean annual temperature for
Front Royal, Virginia would be 12.35 degrees Celsius.
