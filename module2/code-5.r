# ***********************
# ***Scatter plot 3D*****
# ***********************
#install.packages("plot3D")
library(plot3D)
inc = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/5/income2.csv")
scatter3D(x = inc$Education, y = inc$Income, z =inc$Seniority, colvar = inc$Income,
	pch = 16, cex = 1.5, xlab = "Education", ylab = "Income",
	zlab = "Seniority", theta = 50, d = 2,clab = c("Income"),
	colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75, dist = -.08, side.clab = 3)
	,main = "Reltionship Between Income , Education and Seniority")
# **********************
# ***Theres More *******
# **********************
#install.packages("rgl")
library(rgl)
inc = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/5/income2.csv") 
lmin = lm(inc$Income~inc$Education+inc$Seniority) # computes the regression parameters
est = coef(lmin)
a = est["inc$Education"] # extracts the slope coefficient of Education
b = est["inc$Seniority"] # extracts the slope coefficient of Seniority
c=-1
d= est["(Intercept)"]
plot3d(inc$Education,inc$Seniority,inc$Income, type = "s", col = "blue", xlab = "Education",ylab = "Income",zlab = "Seniority",box = FALSE)
planes3d(a,b,c,d, alpha = 0.5, col = "red")

# **********************
# ***Text in 3D *******
# **********************
##Text 3d #####
#install.packages("plot3D")
library(plot3D)
inc= read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/5/income2.csv")
row.names(inc)= inc$names
text3D(x = inc$Education, y = inc$Income, z =inc$Seniority, colvar = inc$Income,labels= row.names(inc),
	pch = 16, cex = 0.8, xlab = "Education", ylab = "Income",
	zlab = "Seniority", theta = 60, d = 2,clab = c("Income"),
	colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75, dist = -.08, side.clab = 3)
	,bty = "g")
# **********************
# ***Theres More *******
# **********************
##text 3D with Gender differences####

#install.packages("plot3D")
library(plot3D)
inc= read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/5/income2.csv")
row.names(inc)= inc$names
text3D(x = inc$Education, y = inc$Income, z =inc$Seniority, colvar = inc$gender,col = c("red","black"),labels= row.names(inc),
pch = 16, cex = 0.8,xlab = "Education", ylab = "Income",
zlab = "Seniority", theta = 60, d = 2,clab = c("Income"), bty = "g", colkey = FALSE)
legend("topright", fill = c("red", "black"), legend= c("Female","Male"), bty = "n")

### the following code is an alternative way of generating some basic Scatter plot in 3D####
#install.packages("scatterplot3d")
library(scatterplot3d)
inc = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/5/income2.csv", 
               sep = ",", header = TRUE)
scatterplot3d(inc$Education,inc$Seniority,inc$Income, col.axis ="blue",pch = 20,color = "red",angle = 55, xlab = "Education", ylab ="Seniority", zlab = "Income")
## To insert lines connecting the dots ###
scatterplot3d(inc$Education,inc$Seniority,inc$Income, col.axis ="blue",pch = 20,color = "red",angle = 55, xlab = "Education", ylab ="Seniority",zlab = "Income", type= "h", box = FALSE)
## To insert a plane ##
regs = scatterplot3d(inc$Education,inc$Seniority,inc$Income, col.axis ="blue",pch = 20,color = "red",angle = 55, xlab = "Education", ylab ="Seniority", zlab = "Income", box = FALSE)
lmin = lm(inc$Income~inc$Education+inc$Seniority)
regs$plane3d(lmin,lty.box = "solid")
# ************************************
# ***3D Pie Charts********************
# ************************************
#install.packages("plotrix")
library(plotrix)
data = c(179718,41370,41914,44280)
pct = (data/sum(data))*100
pct = round(pct,2)
labels = c("Army", "Navy", "Air Force","Marines")
labels = paste(labels,pct, "%")
col = c("purple", "violetred1", "green3","red", "cyan")
p = pie3D(pct,labels = NA, labelcex= 1, explode = 0, main = "Traumatic Brain Injury 2000-2014(Q2)")
pie3D.labels(p,labels=labels, labelcex = 0.9)
# ************************************
# ***3D Histogram********************
# ************************************
#install.packages("plot3D")
library(plot3D)
x = y = seq(-4,4,by =0.5 )
f = function(x,y){z = (25-(x^2-y^2))}
z = outer(x,y,f)
hist3D(z = z, x = x, y = y, border = "black", image = TRUE)
# ************************************
# *** 3D Ribbon  ********************
# ************************************
x = y = seq(-4,4,by =0.5 )
f = function(x,y){z = (25-(x^2-y^2))}
z = outer(x,y,f)
ribbon3D(z = z, x = x, y = y, border = "black", image = TRUE)
# ************************************
# *** 3D Contour     *****************
# ************************************
#install.packages("plot3D")
dev.off()
library(plot3D)
par(mar=c(2,1,2,1))
l = layout(matrix(c(1,2,3,4),2,2,byrow = TRUE))
x=y= seq(-2,2,by = .2)
fun = function(x,y){z=x*exp(-x^2-y^2)}
z = outer(x,y,fun)
r = 1:nrow(z)
p = 1:ncol(z)
contour3D(x=x,y=y,z=z, colvar=z, bty="b2",dDepth =1, theta=60, nlevels = 20, colkey = FALSE)
contour3D(x=r,y=p,z=5, colvar=z, bty="b2",dDepth =1, theta=60, nlevels = 20, colkey = FALSE)
contour3D(x=5,y=p,z=r, colvar=z, bty="b2",dDepth =1, theta=60, nlevels = 20, colkey = FALSE)
contour3D(y=z, colvar=z, bty="b2",dDepth =1, theta=60, nlevels = 20, colkey = FALSE)
# ***********************************
# ** Contour and Surface************
# ***********************************
library(plot3D)
x = y = seq(-3,3, length.out = 10)
f = function(x,y){ z= (y^2-x^2)}
m = outer(x,y,f)
image2D(m)
image2D(m, contour = TRUE)
persp3D(z = m, contour = TRUE)
persp3D(z = volcano, contour = TRUE)
# **********************************
# Interactive Contour plot**********
# **********************************
#install.packages("rgl")
library(rgl)
c = terrain.colors(5)
persp3d(z = volcano, contour = TRUE, col = c)

# **********************************
# ** Surface plot and Animation******
# **********************************
#install.packages(c("animation"))
library(plot3D)
library(animation)
library(plot3D)
x = y = seq(0,2*pi, length.out = 100)
z = mesh(x,y)
u = z$x
v = z$y
m= (sin(u)*sin(2*v)/2)
n = (sin(2*u)*cos(v)*cos(v))
o = (cos(2*u)*cos(v)*cos(v))
par(mfrow=c(2,2))
surf3D(m, n,o, colvar = o, border = "black",colkey = FALSE, box = TRUE)
surf3D(m, n,o, colvar = o, border = "black",colkey = TRUE, box = TRUE,theta = 60)
surf3D(m, n,o, colvar = o, border = "black",colkey = TRUE, box = TRUE,theta = 100)
# ******************************
# ** Animating a Surface plot ***
# *******************************
#install.packages(c("plot3D","animation"))
library(plot3D)
library(animation)
saveHTML({
for (i in 1:100 ){
x = y = seq(0,2*pi, length.out = 100)
z = mesh(x,y)
u = z$x
v = z$y
m= (sin(u)*sin(2*v)/2)
n = (sin(2*u)*cos(v)*cos(v))
o = (cos(2*u)*cos(v)*cos(v))
surf3D(m, n,o, colvar = o, border = "black",colkey = FALSE, theta = i, box = TRUE)
}
},interval = 0.1, ani.width = 500, ani.height = 1000) 

