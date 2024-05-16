
##############################Picth---------------------
pitch <- c(233,204,242,130,112,142)
sex <-c(rep("female",3),rep("male",3))
levels(sex)
str(my.df1)
mydf1$sex <- as.factor(sex)

age <- c(20,14,15,30,100,90)
tail <- c(167,178,200,115,199,156)
long <-  c(16,17,20,11,19,39)

rm(my.df1)

#Create a dataframe
my.df1 = data.frame(sex,pitch, age, tail, long)

xmdl = lm(pitch ~ sex, my.df1)


corrM <- my.df1 %>% 
  dplyr::select(pitch, tail, long, age) %>% cor()

str(my.df1)

corrplot(corrM, 
         method = "number", 
         type = "lower", 
         bg = "white", 
         diag = F, 
         tl.col = "black") # intaller corrplot

corrplot(corrM, method = "ellipse", # this is just another method for good visibility
         type = "upper", 
         diag = F)

#Run the linear model

xmdl = lm(tail ~ sex + age + pitch + long, my.df1)

xmdl2<- lm(tail ~ pitch + sex + long, my.df1)

xmdl3<- lm(tail ~ pitch + sex + age, my.df1)


AIC(xmdl)
AIC(xmdl2)
AIC(xmdl3)

          
summary(xmdl)

install.packages((AICcmodavg))

# create age
age<- c(15, 20, 30, 56, 12, 45)

my.df2 = data.frame(my.df1,age)

xmdl2<- lm(pitch ~ sex + age, data=my.df2) #Multiple linear regression: model one response variable as a function of multiple predictor variables
summary(xmdl2)
plot(xmdl)
abline

#-------------------------LM------------------------
data(trees)

trees<- trees
 str(trees)
 head(trees)
 summary(trees)
 
 lm1 <- lm(Volume ~ Girth,
           data = trees)
 summary(lm1)
 
 plot(Volume ~ Girth, data = trees)
 
 abline(lm1, col ="orange",
        lwd = 2)

 coefficients(lm1) 
 
 plot(residuals(lm1))
 abline(h=0, lty=5, col ="red")
 plot(dffits(lm1))
 plot(dfbeta(lm1)[,1]) #etc. (needs to be done for each column/estimate, separately)
 plot(cooks.distance(lm1))
 plot(as.vector(influence(lm1)$hat))
 
 # checks of assumptions
 hist(residuals(lm1)) 
 qqnorm(residuals(lm1))
 qqline(residuals(lm1))
 
 
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
 