library(datasets)
data(iris)
?iris

# mean of 'Sepal.Length' for the species virginica
virginica <- subset(iris, Species =="virginica")

# Continuing with the 'iris' dataset from the previous Question, what R code returns a vector 
# of the means of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
apply(iris[, 1:4], 2, mean)
#, 1:4 designates columns 1:4, "2" indicates that the functions should be applied to columns

library(datasets)
data(mtcars)

with(mtcars, tapply(mpg, cyl, mean))

tapply(mtcars$mpg, mtcars$cyl, mean)

sapply(split(mtcars$mpg, mtcars$cyl), mean)

tapply(mtcars$hp, mtcars$cyl, mean)


install.packages("swirl")
packageVersion("swirl")
library(swirl)
install_from_swirl("R Programming")
swirl(resume)