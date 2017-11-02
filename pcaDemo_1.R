setwd("C:/Users/cuppu/Downloads/GL/Adv_stat/practice examples")
## US crime data
data = read.csv("USCrimeData.csv", stringsAsFactors = F)
## Description of the data
# This data set contains statistics, in arrests per 100,000 residents for assault, crime1, and crime2 
# in each of the 50 US states in 1973. Also given is the percent of the population living in urban areas.

# This data set contains 50 observations on 4 variables.
# Crime1: 	 numeric	 Crime1 arrests (per 100,000)
# Assault:	 numeric	 Assault arrests (per 100,000)
# UrbanPop	 numeric	 Percent urban population
# Crime2	 numeric	 Crime2 arrests (per 100,000)

# Row names contain the 50 US states in alphabetical order
#data <- USCrimeData
row.names(data) = data$X
states = data$X
head(data)
#            Crime1 Assault UrbanPop Crime2
#Alabama      13.2     236       58   21.2
#Alaska       10.0     263       48   44.5
#Arizona       8.1     294       80   31.0
#Arkansas      8.8     190       50   19.5
#California    9.0     276       91   40.6
#Colorado      7.9     204       78   38.7

data = data[,-1]
# The columns of the data set contain the four variables

names(data)
# [1] "Crime1"   "Assault"  "UrbanPop" "Crime2"

# Examine the data
# Note that the variables have vastly different means

apply(data, 2, mean)
#  Crime1  Assault UrbanPop   Crime2 
# 7.788  170.760   65.540   21.232 

# Observe that apply() function allows us to apply a function -  in this case, the mean() - to each row
# or column of the data set. 
# The second input denotes whether we wish to compute the mean of the rows, 1, or the columns, 2
# We see that there are on average three times as many Crime2s as Crime1s and more than eight times
# as many Assaults as Crime2s. 
# We can also examine the variances of the four variables using the apply() function

apply(data, 2, var)
#  Crime1    Assault   UrbanPop     Crime2 
# 18.97047 6945.16571  209.51878   87.72916

# The variables also have different variances:
# The UrbanPop variable measures the percentage of the population in each state living in an urban
# area, which is not a comparable number to the number of Crime2 in each state per 100,000 individuals
# If we failed to scale the variable the variables before performing PCA, then most of the principal
# components would be driven by the Assault variable, since it has by far the largest mean and variance.
#####!!!! Thus, it is important to standardize the variables to have mean zero and standard derivation one before
# performing PCA
# Perform PCA using the prcomp() function, which is one of the several functions in R that perform PCA

pr.out = prcomp(data, scale = T)
# By default, the prcomp() function centers the variables to have mean zero.
# By using the option scale = TRUE, we scale the variables to have standard deviation one
# The output from prcomp() contains a number of useful quantities

names(pr.out)
#[1] "sdev"     "rotation" "center"   "scale"    "x"

# The center and scale components correspond to the means and standard deviations of the variables
# that were used for scaling prior to implementing PCA
pr.out$center
# Crime1  Assault UrbanPop   Crime2 
# 7.788  170.760   65.540   21.232 
pr.out$scale
# Crime1   Assault  UrbanPop    Crime2 
# 4.355510 83.337661 14.474763  9.366385 

# The rotation matrix provides the principal component loadings; each column of pr.out$rotation
# contains the corresponding principal component loading vector

# This function names it the rotation matrix, because when we matrix-multiply the X matrix by
# pr.out$rotation , it gives us the coordinates of the data in the rotated coordinate system.
# These coordinates are the principal component scores

pr.out$rotation
#              PC1        PC2        PC3         PC4
# Crime1   -0.5358995  0.4181809 -0.3412327  0.64922780
# Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
# UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
# Crime2   -0.5434321 -0.1673186  0.8177779  0.08902432

# We see that there are four distinct principal components
# This is to be expected because there are in general min(n-1, p) informative principal components
# in a data set with n observations and p variables

# Using the prcomp() function, we do not need to explicitly multiply the data by the principal component
# loading vectors in order to obtain the principal component score vectors 
# Rather the 50X4 matrix X has as its columns the principal component score vectors
# That is, the kth column is the kth principal component score vector. 

dim(pr.out$x)
# [1] 50  4
pr.out$x

apply(data, 2, sd)
#We can plot the first two principal components as follows:
biplot(pr.out, scale = 0)


# The scale = 0 argument to biplot() ensures that the arrows are scaled to represent the loadings;
# other values for scale give slightly different biplots with different interpretations

# Recall that the principal components are only unique up to a sign change
pr.out$rotation = -pr.out$rotation
pr.out$x = - pr.out$x 
biplot(pr.out, scale = 0 )
biplot(pr.out, scale = 0, main = "Scaled")

# The prcomp() function also outputs the standard deviation of each principal component
# For instance, for the present data set, we can access these standard deviations as follows:
pr.out$sdev
# [1] 1.5748783 0.9948694 0.5971291 0.4164494
sd(pr.out$x[,1])
# [1] 1.574878
sd(pr.out$x[,2])
# [1] 0.9948694
sd(pr.out$x[,3])
# [1] 0.5971291
sd(pr.out$x[,4])
# [1] 0.4164494

# The variance explained by each principal component is obtained by squaring these:
pr.var = pr.out$sdev^2
pr.var
# [1] 2.4802416 0.9897652 0.3565632 0.1734301

# To compute the proportion of variance explained by each principal component, we simply divide 
# the variance explained by each principal component by the total variance explained by all four
# principal components

pve = pr.var/sum(pr.var)
pve
# [1] 0.62006039 0.24744129 0.08914080 0.04335752
pve*100
# [1] 62.006039 24.744129  8.914080  4.335752

# We see that the first principal component explains 62.0% of the variance in the data
# Second principal component explains 24.7% of the variance
# Third principal component explains 8.9% of the variance
# Fourth explains 4.34%

# We can plot the PVE explained by each component, as well as the cumulative PVE, as follows:
plot(pve, 
     xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

plot(cumsum(pve), 
     xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")


### standardize the data (9, 200, 50, 15) will be with mean's(7, 64, 24, 21) and sd's(.7, 83, 14, 9.3)
# will lead to Xstd(0.2, 2, 3, -0.6) approximately... 
## and xstd =  1 * 4 .. and pc1 = 4 * 1 => xstd * pca1 = 1*1

## here its like Xstd * pc1 + xstd * pc2 + xstd * pc3.... each will give one out put xz1, xz2, xz3....


