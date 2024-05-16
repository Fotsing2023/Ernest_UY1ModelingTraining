install.packages("raster")
library(raster)

wrd<- getwd()

# Define the URL of the Excel file
url <- "https://github.com/Fotsing2023/Ernest_UY1ModelingTraining/raw/main/example.xlsx"

# Define the file path where you want to save the Excel file
file_path <- "example.xlsx"

# Download the Excel file
download.file(url, file_path, mode = "wb")


T <- matrix(1:50, nrow = 10, ncol=5)
length(T)
str(T)
names(T)
colnames(T) <- c("oui", "non", "jamais", "hasard", "bonjour")                   
t <- matrix(50:99, nrow = 10, ncol=5)
colnames(t) <- c("moi", "toi", "lui", "nous", "vous")
R<- cbind(T, t)                   
mean(R)
list(R)
class(R)
lapply()
lapply(R, mean)

?lapply
require(stats); require(graphics)

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))# The list x contains three elements, each with its own name (a, beta, logic).
# compute the list mean for each list element. seq of -3 to 3 and expo of each elements
lapply(x, mean)# lapply() function to apply the mean() function to each element of the list x
# median and quartiles for each list element

lapply(x, quantile, probs = 1:3/4) #The quantile() function is applied to each element of x, with the probs argument specifying the quantiles to compute.
#In this case, probs = 1:3/4 computes the quartiles (25th, 50th, and 75th percentiles) for each element.

sapply(x, quantile) # sapply() attempts to simplify the result by converting it to a matrix or vector if possible.

i39 <- sapply(3:9, seq) # list of vectors
sapply(i39, fivenum)
vapply(i39, fivenum, c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

# how to write function in R 
Function.name <- function(parameters){
  function body
}
# example

circumference <- function(r){
  2*pi*r
}
print(circumference(2))

# set r as a default
circumference <- function(r=1){
  2*pi*r
}
print(circumference())
print(circumference(2))

# sum of 2 number

sum_two_nums <- function(x, y){
  x + y
}
print(sum_two_nums(1, 2))

# sumary statistics
mean_median <- function(vector){
  mean <- mean(vector)
  median <- median(vector)
  return(c(mean, median))
}
print(mean_median(c(1, 1, 1, 2, 3)))

mean_median(R)


# calcuéate calories
calculate_calories_women <- function(weight, height, age){
  (10 * weight) + (6.25 * height) - (5 * age) - 161
}

print(calculate_calories_women(age=30, 60, 165))

# Using Functions Inside Other Functions

radius_from_diameter <- function(d){
  d/2
}

circumference <- function(r){
  2*pi*r
}

print(circumference(radius_from_diameter(4)))

# in the above piece of code, we created two simple functions first: for calculating the radius of a circle given its diameter and for calculating the circumference of a circle given its radius. Since originally we knew only the diameter of a circle (equal to 4), we called the radius_from_diameter function inside the circumference function to calculate first the radius from the provided value of diameter and then calculate the circumference of the circle. While this approach can be useful in many cases, we should be careful with it and avoid passing too many functions as arguments to other functions since it can affect the code readability.
# Install and load the readxl package

install.packages("readxl")
library(readxl)
# Define the file path where you want to save the Excel file
file_path <- "example.xlsx"

# Write the data frame to an Excel file
write.xlsx(as.data.frame(R), file_path)



#---------------------------how to dowload from a website------
wrd <- getwd()

# Install and load required packages
install.packages(c("openxlsx", "readxl"))
library(openxlsx)
library(readxl)

# Define the URL of the Excel file
url <- "https://github.com/Fotsing2023/Ernest_UY1ModelingTraining/raw/main/example.xlsx"

# Define the file path where you want to save the Excel file
file_path <- "example.xlsx"

# Download the Excel file
download.file(url, file_path, mode = "wb")

# Read the Excel file using openxlsx package
df_openxlsx <- read.xlsx(file_path)

# how to read

# Define the file path of the Excel file
file_path <- "example.xlsx"

# Read the Excel file
df <- read_excel(file_path)

# Define the file path of the CSV file
file_path <- "example.csv"

# Read the CSV file
df <- read.csv(file_path)
mydata <- read.csv ("data.csv", header = TRUE)




