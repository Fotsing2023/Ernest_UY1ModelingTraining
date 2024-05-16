
##############################Picth---------------------
pitch <- c(233,204,242,130,112,142)
sex <-c(rep("female",3),rep("male",3))
#Create a dataframe
my.df1 = data.frame(sex,pitch)
#Run the linear model
xmdl = lm(pitch ~ sex, my.df1)
summary(xmdl)

# create age
age<- c(15, 20, 30, 56, 12, 45)

my.df2 = data.frame(my.df1,age)

xmdl2<- lm(pitch ~ sex + age, data=my.df2) #Multiple linear regression: model one response variable as a function of multiple predictor variables
summary(xmdl2)

#-------------------------LM------------------------
data(trees)
 str(trees)
 head(trees)
 summary(trees)
 
 lm1 <- lm(Volume ~ Girth,
           data = trees)
 plot(Volume ~ Girth, data = trees)
 abline(lm1, col ="orange",
        lwd = 2)

 coefficients(lm1) 
 
 #------------------------
 
 data(mtcars)
 input <- mtcars[,c('mpg','cyl')]
 print(head(input))
 # Give the chart file a name.
 png(file = "boxplot.png")
 # Plot the chart.
 boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
         ylab = "Miles Per Gallon", main = "Mileage Data")
 # Save the file.
 dev.off()
 
 # Give the chart file a name.
 png(file = "boxplot_with_notch.png")
 # Plot the chart.
 boxplot(mpg ~ cyl, data = mtcars,
         xlab = "Number of Cylinders",
         ylab = "Miles Per Gallon",
         main = "Mileage Data",
         notch = TRUE,
         varwidth = TRUE,
         col = c("green","yellow","purple"),
         names = c("High","Medium","Low")
 )
 # Save the file.
 dev.off()
 
  # chisqauretest
 library("MASS")
 print(str(Cars93))
 # Create a data frame from the main data set.
 car.data <- data.frame(Cars93$AirBags, Cars93$Type)
 # Create a table with the needed variables.
 car.data = table(Cars93$AirBags, Cars93$Type)
 print(car.data)
 # Perform the Chi-Square test.
 print(chisq.test(car.data))
 
 #------The result shows the p-value of less than 0.05 which indicates a strong correlation.
 #install.packages("party") # for decision trees 
 