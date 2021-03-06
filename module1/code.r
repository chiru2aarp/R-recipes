#***R Basics***
#**************
## CTRL +L  will clear the console...
# Expressions
x = 1
"hi";7+13;exp(10)
x
#we have used ; to separate the variables in an expression

#Objects:

x = c(1:5) # Numeric Vector
y ="I am Home" # Character Vector
z = list(c(1:5),"I am Home") # List
fun = function(x,y){x+y} # a simple function
fun(3,7) # using the function to get an output
# can remove morthan one object, if not found warning....
remove(x, y, z)

# special Values :
## NA / MIssing values
z = c( 1,2,3, NA,5,NA) # NA in R is missing Data 
complete.cases(z) # function to detect NA, False will return on NA
is.na(z) # function to detect NA, TRUE will be return on NA
##NaN
0/0
m = c(2/3,3/3,0/0)
m
is.finite(m)
is.infinite(m)
is.nan(m)
## infinite
k = 1/0
is.infinite(k)

#Matrices
mat = matrix(c(1,2,3,4,5,6,7,8,9,10),nrow = 2, ncol = 5)
mat
t(mat) # transpose a matrix
d = diag(3) # generate an identitity matrix
zro = matrix(rep(0,6),ncol = 2,nrow = 3 )# generate a matrix of Zeros
zro
#########################
### Matrices ############
#########################

mat = matrix(c(1:10),nrow = 2, ncol = 5)
mat
mat[2,3]
mat = mat[,-2] #removes second column from matrix
mat
x = c(1:3)
y = c(4:6)
z = x+y # adds two matrices
z
z = x*y # element wise multiplication
z
z = x%*%y# matrix Multiplication
z
bind = cbind(x,y)
z = 3*bind # scalar Multiplication
z
c = seq(1,10,by = 2) # generate a sequence of values
c
rep(1,3) # repeating values
ncol(mat) # returns the number of columns in a matrix
nrow(mat) # returns the number of rows in a matrix
is.matrix(mat) 
y = 3
mat = mat*y # scalar Multiplication
k = matrix((1:4),2,2)
l = matrix((5:10),2,3)
dim(k) # returns the dimension of a matrix
dim(l)
k%*%l # matrix multiplication
l%*%k ##  this will generate  an Error in l %*% k : non-conformable arguments
k = matrix(rnorm(25),5,5) # generates a 5x5 matrix by generating 25 random numbers.
det(k) # find the determinant
solve(k) # find the inverse

#######################
# data frames #########
#######################

k = matrix((1:4),2,2)
l = c("tom", "mary")
data = data.frame(k,l)
data
data(mtcars)
head(mtcars)
head(mtcars, 2)
tail(mtcars)
summary(mtcars)
dim(mtcars)
rownames(mtcars)
colnames(mtcars)
######################
sales = c(200,300,300,300,100,300,260,250,300)
data = matrix(sales,3,3)
rownames(data)= c("March" , "April", "May")
colnames(data) = c(2004,2005,2006)
barplot(data, main="sales",
xlab="Months", col=c("darkblue","red", "yellow"), beside=TRUE, legend = rownames(data))
######################
series = c(1:10)
series1 = c(11:20)
mean(series) # mean
sd(series) # standard deviation
cov(series,series1) # covariance
cor(series,series1) # correlation of a matrix
summary(series)
#####################
###Data Frames - 1###
#####################
data = data.frame(x = c(1:4), y = c("tom","jerry","luke","brian"))
data
mat = matrix(c(1:10), nrow = 2, ncol = 5)
data.frame(mat)

##########################
##Editing a data frame ###
##########################
data = data.frame(x = c(1:4), y = c("tom","jerry","luke","brian"))
data$age = c(2,2,3,5)
data
data = data[c(2,1)]# will reorder the columns, 2 and 1 are col numbers. 3 omitted
data
names(data)# to view the names of our data set
data = data.frame(x = c(1:4), y = c("tom","jerry","luke","brian"))
colnames(data) = c("Number","Names") # to rename columns in a data frame
data
#*************************
#***Writting a function***
#*************************
add = function (x,y){
  x+y
}
#******************************
#***If Else statements*********
#******************************
if(x>3){
  print("greater value")
}else {
  print("lesser value")
}
#******************************
#***Basic Loops****************
#******************************
x = c(1:10)
y = c(1:10)
#z = c()
for(i in 1:10){
z[i] = x[i]*y[i]
}
#**************************************
#****Nesting loops and If statements***
#**************************************
mat= matrix(1:25, 5,5)

for (i in 1:5){
  for (j in 1:5){
    if (i ==j){
      print(mat[i,j])
    }
   }
}
#******************************
#*** Apply*********************
#******************************
mat= matrix(1:25, 5,5)
apply(mat,1,sd) # 1 will do row wise computation
apply(mat,2,sd)# 2 will do columnwise computation

#******************************
#*** Lapply*********************
#******************************
j = list(x = 1:4, b = rnorm(100,1,2))
lapply(j,mean)
#******************************
#***Tapply*********************
#******************************

tapply(mtcars$mpg,mtcars$gear,mean)

#******************************
#***Using Par to edit a plot***
#******************************
x=c(1:10)
y=c(1:10)
par(bg = "#646989", las = 1, col.lab = "black", col.axis = "white",bty = "n",cex.axis = 0.9,cex.lab= 1.5)
plot(x,y, pch = 20, xlab = "fake x data", ylab = "fake y data")

#******************************
#***editing labels on a plot***
#******************************
x =c(1:10)
y=c(1:10)
plot(x,y, xlab = "x axis", ylab = "y axis", cex.lab = 3,col.lab = "red")
plot(x,y, xlab = "x axis", ylab = "y axis", cex.lab = 3,col.lab = "red", main = "some data", cex.main=1.5, col.main = "red")

#*******************************
#***saving a plot***************
#*******************************
png("TEST.png", width = 300, height = 600)
plot(x,y, xlab = "x axis", ylab = "y axis", cex.lab = 3,col.lab = "red", main = "some data", cex.main=1.5, col.main = "red")
dev.off()

