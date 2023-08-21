# We start by clearing work space and console
rm(list=ls())
cat("\014")
library(ggplot2)


#### Question 1: Create the vector of the pattern 1,2,...,10,1,2,...10,......,121,122,...,130,121,122,...,130
# method 1: decompose matrix into two parts
# (1) repeat the vector 1:10 13*2 = 26 times
# (2) repeat the sequence (0, 10, 20, ..., 120), each 20 times, add to the previous sequence
rep(1:10, 26) + rep( seq(from=0, to=120, by=10), each=20)

# method 2: create the left matrix, use cbind() to join the right matrix, and convert the matrix to vector
m <- matrix(1:130, nrow=13, ncol=10, byrow = TRUE)
c( t(cbind(m, m)) )


#### Question 2: Create a 5 by 5 matrix where the diagonal elements have value 10 and the rest of the elements have values 5.
# method 1: decompose into two matrices, and sum over two matrices
diag(5, 5,5) + matrix(rep(5, 25), 5, 5)

# method 2: create a vector of 30 elements and put the first 25 into the matrix
matrix(rep(c(10,5,5,5,5,5), 5), 5,5)

# method 3: create a sequence of 5 for 25 times, replace the diagonal position with 10 
a <- rep(5,25)
a[seq(1,25,6)] <- 10
matrix(a, 5, 5)


#### Question 3: Data Exploration and Visualization 
# Q3.1 Read the CSV file into RStudio and convert the variable FuelType to factor type.
# replace ../ with your local file directory
auto <- read.csv("../auto.csv", stringsAsFactors = FALSE)
# or you can load the data as follows:
# this will read the csv file into a "tibble", you will need to covert it into a data.frame
#library(readr)
#auto <- read_csv("../auto.csv")
#auto <- data.frame(auto)
auto$FuelType <- as.factor(auto$FuelType)

# Q3.2 What is the average price of vehicles whose fuel type is "Diesel" and whose age is less than (or equal to) a year?
mean( auto[(auto$FuelType=='Diesel') & (auto$Age<=12), 'Price'] ) # 26396.11

# Q3.3 "akm": the average kilometers driven per month 
auto$akm <- auto$KM/auto$Age
aggdf <- aggregate(akm ~ FuelType, data = auto, mean)
# Bar Chart with X as FuelType and Y as akm
g <- ggplot(aggdf, aes(FuelType, akm))
g + geom_bar(stat='identity') + geom_text(aes(label=round(akm, digits=0.2)), vjust=-0.5)
# same as before with user-defined colors  
g + geom_bar(stat='identity', width=0.5, fill='lightblue', color='darkblue') + geom_text(aes(label=round(akm, digits=0.2)), vjust=-0.5)

# Q3.4 Scatter plot of Age and Price, then fit a smooth curve
g <- ggplot(auto, aes(Age, Price))
g + geom_point() + geom_smooth(method=loess)

# Q3.5 Box Plot of Price for each FuelType, highlight outlier in red
g <- ggplot(auto, aes(FuelType, Price)) 
g + geom_boxplot() + geom_boxplot(outlier.color = "red")

 



