# IS607 Week 2 Quiz Student: Neil Acampa
# 1) Create a vector that contains 20 numbers

# 2) Convert the vector in question 1 to a charactor vector

# 3) Convert vector A into a vector of factors
#    C <- factor(A)

# 4) Show how many levels this vector has
#    levels(c) 

# 5) Take vector (A) and perform 3x^2 - 4x + 1 
#    F <- 3 * (A * A) - 4 + 1

# 6) Implement ordinary least squares B = inverse(xTx) * xTy

# 7) Create a named list

# 8) Create a dataframe with 4 columns - character, factor - 3 levels, numeric,date
#    and with 10 observations

# 9) Illustrate how to add a row with a value for the factor column that does not exist

# 10) Show how to read a file temperatures.csv from the current working directory

# 11) Show how to read in a tsv file (measurements.txt) from a directory other than the
#     current one on the local machine

# 12) Show code to read a delimited file with a "|" seperator from web site

# 13) Write a loop to calculate 12 factorial

# 14) Use a loop to calculate the final balance rounded to the nearest cent
#     in an account that earns 3.24% componded monthly after 6 years with a starting  
#     balance of 1500

# 15) Create a numeric vector and calculate the sum of every third element

# 16) Use a for loop next loop to calculate Sum(i=1 to 10) x^i x init at 2

# 17) Use a while to calculate Sum(i=1 to 10) x^i x init at 2

# 18) Solve above without a loop

# 19) Show how to create a numeric vector from 20 to 50 by 5

# 20) Show how to create a character vector of length 10 with the same word "example", 10 times

# 21) Show how to take a trio of numbers a,b, and c and implement the quadratic equation

# 1). Create a vector that contains 20 numbers

A <- c(1,3,5,7,2,4,5,2,8,7,1,2,9,20,0,10,30,18,4,1)
A

# 2). Convert the vector in question 1 to a charactor vector

B <- as.character(A)
B
# Validate character vector
is.character(B)


# 3). Convert vector A into a vector of factors

C <- factor(A)
C

# 4). Show how many levels this vector has
levels(C)

# 5). Take vector (A) and perform 3x^2 - 4x + 1 

F <- 3 * (A * A) - 4 * A + 1
F
sum(F)

# 6) Implement ordinary least squares B = inverse(xTx) * xTy


# y vector
y <- c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1)

# Format the x matrix for columns X,M and Z

X <- c(1,1,1,1,1,1,1,1)
M <- c(5,4,6,2,3,2,7,8)
Z <- c(8,9,4,7,4,9,6,4)
x <- cbind(X,M,Z)

# xT = x transpose
xT = t(x)
xTx = xT %*% x
xTx

# x Transpose y
xTy = xT %*% y
xTy

# xTx inverse
xTxinv = solve(xTx)
xTxinv

# Ordinary Least Squares solution  
b = xTxinv %*% xTy

b

# 7).  Create a named list

l = list(f1 = c(1,2,3), f2= c(4,5,6))
l

#8). Create a dataframe with 4 columns: character, factor - 3 levels, numeric,date
#    and with 10 observations

d1 <- c("a","b","c","d","e","f","g","h","i","j")
d2 <- c(1,2,1,1,7,1,2,2,7,7)
d2f<- factor(d2)
d3 <- c(10,20,30,40,50,60,70,80,90,100)

startdt <- as.Date("2014/1/1")
enddt  <- as.Date("2014/1/10")
d4 <- seq(startdt,enddt,"day")

df <- data.frame(d1,d2f,d3,d4)
df

# 9). Illustrate how to add a row with a value for the factor column that does not exist
#     Sort and Search the levels in the factor column, if the new value is not in the 
#     levels add that level to the factor variable,  then add that element to the row of factors
#     along with the 3 other columns
found = 0
m = factor(df[,2])
temp = levels(m)
temp = sort(as.numeric(temp))
temp
L = length(temp)

# search the sorted Levels for arbitrary value n (20
# if value is not found, add a new row

n = 21
for (j in 1:L) {
  if (n == temp[j]) {
    found = 1
  }
}
if (found == 0) {
  n = as.character(n)
  df[,2] = factor(df[,2], levels = c(levels(m),n))
  newrw <- c("a",n,110,"2014/1/16")
  df = rbind(df,newrw)
}
df
levels(df[,2])





# 10). Show how to read a file temperatures.csv from the current working directory

cwd   = getwd()
cwdfle= paste(cwd,"/temperatures.csv",sep = "")
cfile = read.csv(file=cwdfle, head=TRUE)
cfile

# 11). Show how to read in a tsv file (measurements.txt) from a directory other than the
#      current one on the local machine
getwd()
cwd1 = setwd("c:/Users/Nacampa")
cwd1fle = paste(cwd1,"/measurements.txt", sep = "")
cwd1fle
cfile1 = read.table(file=cwd1fle, sep = "\t")
cfile1

# 12). Show code to read a delimited file with a "|" seperator from web site

#url.show("http://lib.stat.cmu.edu/datasets/csb")



# 13). Write a loop to calculate 12 factorial

n = 12
result = 1
for (f in n:1) {
  if (f != 1) {
    result=result*f;
  }
}
result

# 14). Use a loop to calculate the final balance rounded to the nearest cent
#      in an account that earns 3.24% componded monthly after 6 years with a starting  
#      balance of 1500

sbalance = 1500
monthlyint   = .0324
totalbal   = sbalance
for (i in 1:6) {
  temp = (totalbal*monthlyint)
  totalbal = totalbal + temp
}
totalbal

#15). Create a numeric vector and calculate the sum of every third element


A <- c(1,3,5,7,2,4,5,2,8,7,1,2,9,20,0,10,30,18,4,1)
sum=0
L = length(A)
for (i in 1:L) {
  if ((i %% 3) == 0) {
    sum = sum + A[i]
  }
}
sum

#16). Use a for next loop to calculate Sum(i=1 to 10) x^i x init at 2

x = 2
sumv <- numeric(length = 10)
for (i in 1:10) {
  sumv[i] = (x^i)
}
sumv
sum(sumv)

# 17). Use a while to calculate Sum(i=1 to 10) x^i x init at 2

i = 0
x = 2
sumv1 <- numeric(length = 10)
while (i < 10) {
  i=i+1
  sumv1[i] = (x^i)
}
sumv1
sum(sumv1)

#18). Solve above without a loop

mvec <- seq(1,10,1)
svec <- c(2,2,2,2,2,2,2,2,2,2)
# multiple svec by powers of mvec
sumv2 = svec ^ mvec
sumv2
sum(sumv2)


#19). Show how to create a numeric vector from 20 to 50 by 5

nvec <- seq(20,50,5)
nvec

#20). Show how to create a character vector of length 10 with the same word "example", 10 times

b <- character(10)
for (x in 1:10) {
  b[x] <- c("example")
}
b



# 21). Show how to take a trio of numbers a,b, and c and implement the quadratic equation
#      b +/- sqrt(b^2 -4ac) / 2(a)


a = 1
b = 4
c = 3
temp = b^2 - 4 *(a*c)
temp = sqrt(temp)
sol1 = (-b + temp) / 2*a
sol2 = (-b - temp) / 2*a

a = 1
b = -5
c = -6
temp = b^2 - 4 * (a*c)
temp = sqrt(temp)
sol1 = (-b + temp) / 2*a
sol2 = (-b - temp) / 2*a
sol1
sol2
