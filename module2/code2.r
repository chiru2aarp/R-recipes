# **********************************
# *** Recipe 1 : Simple Dendogram***
# **********************************
set.seed(5)
x = rnorm(10)
y = rnorm(10)
z = seq(1,10, by = 1)
mtx = data.frame(cbind(x,y))
d = dist(mtx, method = "euclidean")
#ave is short abbrevation for average... in r it works..
clust = hclust(d, "ave")
par(mfrow = c(1,2))
plot(mtx$x,mtx$y, type = "n", xlab = "x", ylab = "y") # generates the plot on the left side
text(mtx$x,mtx$y, labels = z) # applies labels to plot on the left
plot(clust, main = "Dendrogram") # generates plot on the right 
# **********************************
# *** Recipe 1 : Theres More********
# **********************************
data = data.frame(USArrests[,1:3])
dt = dist(data, method = "euclidean")
#default is complete
clust = hclust(dt)
plot(data$Murder, data$Assault) # generates a scatter plot
text(USArrests$Murder,USArrests$Assault, labels = row.names(USArrests), cex = 0.6) # applies labels to a scatter plot.
plot(clust) # generates a dendogram plot
# **************************************************
# *** Recipe # 2 :Colors and Labels in  Dendogram***
# **************************************************

#install.packages("dendroextras")
library(dendroextras)
data(USArrests)
par(mar = c(2,10,2,10), cex = 0.6)
clst1=colour_clusters(hclust(dist(USArrests), "ave"),5,groupLabels=as.roman)
plot(clst1, main = "Dendrogram with 5 clusters", horiz = TRUE)

## to change the view from rectangle to Triangle##
plot(clst1, main = "Dendrogram with 5 clusters", horiz = TRUE, type = "triangle")
# **************************************************
# ** Neat Map implementation************************
# **************************************************
#install.packages("NeatMap")
library(NeatMap)
clust = hclust(dist(USArrests), method = "complete")
pos<-nMDS(USArrests,metric="euclidean")
draw.dendrogram3d(clust,pos$x,labels=rownames(USArrests),label.size=0.75)
# **************************************************
# **Recipe# 3:Heat Map *****************************
# **************************************************
#install.packages("pheatmap")
library(pheatmap)
irq = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/2/data/iraqbdc.csv", 
               header = TRUE, sep =",")
row.names(irq)=irq$years
irq = data.matrix(irq)
irq = data.matrix(irq[,2:13])
pheatmap(irq, cluster_row= FALSE, cluster_col = FALSE, main ="Iraq Body Count")

### to change the line type####
pheatmap(irq, cluster_row= FALSE, cluster_col = FALSE, main ="Iraq Body Count", lty = 3, lwd = 3, lineend = "square")
# **************************************************
# **Theres More :Heat Map***************************
# **************************************************
irq = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/2/data/iraqbdc.csv", 
               header = TRUE, sep =",")
row.names(irq)=irq$years
irq = data.matrix(irq)
irq = data.matrix(irq[,2:13])
heatmap(irq, Rowv = NA, Colv = NA, main = "Iraq Body Count Heat Map", xlab = " Body Count per month", ylab = "Years")
# **************************************************
# **Recipe# 4: customizing Heat Maps****************
# **************************************************
#install.packges(c("pheatmap","RColorBrewer"))
library(RColorBrewer)
library(pheatmap)
irq = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/2/data/iraqbdc.csv", header = TRUE, sep =",")
row.names(irq) = irq$years
irq = data.matrix(irq)
irq = data.matrix(irq[,2:13])
heatcolor = brewer.pal(7,"Greens")
pheatmap(irq, cluster_row= FALSE, cluster_col = FALSE, display_numbers = TRUE, color = heatcolor, main = "Iraq Body Count", fontsize_number = 10)
## Implementing the above code in R basic Package##
heatmap(irq, Rowv = NA, Colv = NA, main = "Iraq Body Count Heat Map", xlab = " Body Count per month", ylab = "Years", col = heatcolor)
# **************************************************
# **Recipe# 4: Heat Maps and Dendrograms************
# **************************************************
data = as.matrix(scale(USArrests))
clst = hclust(dist(data))
pheatmap(data)
### to plot just the dendrogram ##
plot(clst)

## to generate PCA chart #######
pr.out = prcomp(USArrests, scale = TRUE)
biplot (pr.out , scale =0, col =c("red","blue"), cex = c(0.8,1), xlab = "First Principal Component", ylab = "Second Principal Component")
abline(h = 0, lty =2)
abline(v = 0, lty =2)


#outmat <- pr.out$x

#pc <- princomp(iris[,1:4], cor=TRUE, scores=TRUE)
#pc

# 3d
library(rgl)
plot3d(pr.out$x)
#iris$Species

text3d(pr.out$x[,1:3],texts=rownames(USArrests), col="red")
#head(USArrests)
text3d(pr.out$rotation[,1:3], texts=rownames(pr.out$rotation), col="Blue")
coords <- NULL
for (i in 1:nrow(pr.out$rotation)) {
  coords <- rbind(coords, rbind(c(0,0,0),pr.out$rotation[i,1:3]))
}
lines3d(coords, col="red", lwd=4)

# open3d()
# x <- sort(rnorm(1000))
# y <- rnorm(1000)
# z <- rnorm(1000) + atan2(x, y)
# plot3d(x, y, z, col = rainbow(1000))

# **************************************************
# **Recipe# 5: An interactive Dendrograms***********
# **************************************************
install.packages("NeatMap")
library(NeatMap)
irq = read.csv("iraqbdc.csv", header = TRUE, sep =",")
irq$years= row.names(irq)
irq = data.matrix(irq)
make.profileplot3d(irq,row.method="PCA",column.method="average.linkage", col = c("red","green","blue"), point.size = 10, labels = row.names(irq))
make.stereo.profileplot3d(irq,row.method="PCA",column.method="average.linkage", labels = row.names(irq), label.size = 1)
# **************************************************
# **Recipe# 6: Tree Maps ***************************
# **************************************************
#install.packages("googleVis")
library(googleVis)
shk = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/2/data/shrink.csv", 
               header = TRUE, sep =",")
shk$Parent[shk$CountryName == "World"]= NA
tree = gvisTreeMap(shk, idvar = "CountryName", parentvar = "Parent", 
                   sizevar = "X2009", colorvar = "Change", 
                   options= list(width = 900, height = 500,showScale = TRUE, 
                                 maxColor = "#009933", minColor ="#CC0000", 
                                 title = "change in GDP per capita, PPP (constant 2011 $)", fontColor = "black"))
plot(tree)
# **************************************************
# **Recipe# 6: Tree Maps with drill down effect******
# **************************************************
shk = read.csv("~/../Downloads/Analytics/Books/R Recipes/module 2/2/data/shrink1.csv", 
               header = TRUE, sep =",")
tree = gvisTreeMap(shk, idvar = "CountryName", parentvar = "Parent",sizevar = "X2009", colorvar = "Change")
plot(tree)
