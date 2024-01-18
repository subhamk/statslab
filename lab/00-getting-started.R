# R script for '00-intro'
# Note: 
# Commented lines begin with # symbol
# Any changes you make here will be lost once you exit

# Add two numbers
1 + 2

# Subtract numbers
5 - 3 

# Simple mathematical operations
(1 + 2) /  (5 - 3)

# assigning values to an object
x <- 12             # assign a value of 12 to the variable x
y <- 5              # assign a value of 5 to the variable y
product_xy <- x * y # product of x and y
product_xy          # display answer

# A simple function to calculate the product of x and y
product <- function(x, y) {
  # function to calculate the product of x and y
  result <- x * y
  print(paste(x, "multiplied by", y, "is", result))
}
product(5, 4)           # calling the function product
product(x = 5, y = 4)   # alternatively, use named arguments



