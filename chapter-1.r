#Chapter 1
#==========
#
#Recipe: Read data from csv files
#----------------------------------

auto  <- read.csv("auto-mpg.csv", header = TRUE, sep = ",")

names(auto)

read.csv("<file name>", sep=";", dec=",")

auto  <- read.csv("auto-mpg-noheader.csv", header=FALSE)

head(auto,2)

auto  <- read.csv("auto-mpg-noheader.csv")

head(auto,2)
data(mtcars)
head(mtcars)
remove(data)
auto <- read.csv("auto-mpg-noheader.csv", 
      header=FALSE, col.names = 
      c("No", "mpg", "cyl", "dis","hp", 
      "wt", "acc", "year", "car_name"))

head(auto,2)

auto  <- read.csv("auto-mpg.csv", na.strings="")

auto <- read.csv("auto-mpg.csv",stringsAsFactors=FALSE)

# The following line of code will not work until the web site of the book is set up with the appropriate resource
housing <- read.csv(
"http://www.exploredata.net/ftp/WHO.csv")
#      
#Recipe: Reading XML data 
#-----------------------------------------------
install.packages("XML")      
library(XML)
url <- "http://www.w3schools.com/xml/cd_catalog.xml"
xmldoc <- xmlParse(url)
rootNode <- xmlRoot(xmldoc)
rootNode[1]
data <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
cd.catalog <- data.frame(t(data),row.names=NULL)
cd.catalog[1:2]

url <- "http://en.wikipedia.org/wiki/World_population"
tables <- readHTMLTable(url)
world.pop <- tables[[4]]

table <- readHTMLTable(url,which=4)

#Reading JSON Data
#--------------------------------------
install.packages("jsonlite") 
library(jsonlite) 
dat <- fromJSON("students.json")

dat <- fromJSON("student-courses.json")

url <-     "http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json"
jsonDoc <- fromJSON(url)
dat <- jsonDoc$list$resources$resource$fields
dat[1:2,]
dat.1[1:3,]
dat.2[,c(1,2,4:5)]

c(1,2,4:5)

#Recipe: Read data from fixed width formatted files
#--------------------------------------------------

student  <- read.fwf("student-fwf.txt", 
      widths=c(4,15,20,15,4), 
      col.names=c("id","name","email","major","year"))

student  <- read.fwf("student-fwf-header.txt", 
   widths=c(4,15,20,15,4), header=TRUE, sep="\t", skip=2)

student  <- read.fwf("student-fwf.txt", 
   widths=c(4,16,-20,15,4), col.names= 
   c("id","name","major","year"))

#Recipe: Read data from R data and R libraries
#----------------------------------------------
customer <- c("John", "Peter", "Jane")
orderdate <- as.Date(c('2014-10-1','2014-1-2','2014-7-6'))
orderamount <- c(280, 100.50, 40.25)
order <- data.frame(customer,orderdate,orderamount)
names <- c("John", "Joan")
save(order, names, file="test.Rdata")
saveRDS(order, file="order.rds")
remove(order)

load("order.Rdata")

data(iris)
data(list(cars,iris))

save.image(file = "all.RData")

odd <- c(1,3,5,7)
even <- c(2,4,6,8)
save(list=c("odd","even"),file="OddEven.Rdata")

attach("order.Rdata")

data()

#Recipe: Read data from relational databases
#----------------------------------------------
customer <- c("John", "Peter", "Jane")
orddt <- as.Date(c('2014-10-1','2014-1-2','2014-7-6'))
ordamt <- c(280, 100.50, 40.25)
order <- data.frame(customer,orddt,ordamt)

library(RODBC)
con <- odbcConnect("order_dsn", uid="user", pwd="pwd")
sqlSave(con,order,"orders",append=FALSE)
custData <- sqlQuery(con, "select * from orders")
close(con)

library(RMySQL)
con <- dbConnect("MySQL", dbname="Customer",
         host="127.0.0.1", port=8889, username="root", 
         password="root")
dbWriteTable(con,"orders", order)
dbReadTable(con,"Orders")
dbGetQuery(con,"select * from orders")

rs <- dbSendQuery(con, "select * from orders")
while(!dbHasCompleted(rs)) {
     fetch(rs,n=2)
}
dbClearResult(rs)
dbDisconnect(con)
dbListConnections(dbDriver("MySQL"))

library(RJDBC)
driver <- JDBC("com.mysql.jdbc.Driver", 
     classpath=
     "/etc/jdbc/mysql-connector-java-5.1.34-bin.jar", "`")
con <- dbConnect(driver,"jdbc:mysql://host:port/Customer"
     ,"username","password")
# The remaining operations are identical to RMySQL

fetch(rs,n=-1) 

dbSendQuery(con, statement=paste(
        "select ordernumber, orderdate, customername",
        "from orders o, customers c",
        "where o.customer = c.customer",
        "and c.state = 'NJ'",
        "ORDER BY ordernumber"))


#Recipe: Read data from mongoDB
#------------------------------
#use customer
db.orders.save(
  {customername:"John"; orderdate:ISODate("2014-11-01"); orderamount:1000})
db.orders.find()
db.save 

install.packages("rmongodb")
library(rmongodb)
mongo <- mongo.create()
mongo.create(host = "127.0.0.1", db = "customer")
mongo.is.connected(mongo)
coll<- mongo.get.database.collections(mongo,"customer")
json <- "{\"orderamount\":{\"$lte\":25000}, 
      \"orderamount\":{\"$gte\":1000}}"
dat <- mongo.find.all(mongo,coll,json)

library(jsonlite)
json <- "{\"orderamount\":{\"$lte\":25000}, 
      \"orderamount\":{\"$gte\":1500}}"
validate(json)

#Recipe: Remove cases with missing values (NA)
#---------------------------------------------

dat <- read.csv("missing-data.csv", na.strings="")
setwd("C:/Users/CHIRANJEEVI/Downloads/Books/Analytics_DS/R/R Recipes/module 1/3")
dat.cleaned <- na.omit(dat)

is.na(dat[4,2])

is.na(dat$Income)

dat.income.cleaned <- dat[!is.na(dat$Income), ]

nrow(dat.income.cleaned)

dat.income.cleaned <- dat[!is.na(dat$Income), ]

complete.cases(dat)

dat.cleaned <- dat[complete.cases(dat), ]
nrow(dat.cleaned)

dat$Income[dat$Income==0] <- NA

mean(dat$Income)

mean(dat$Income, na.rm = TRUE)

#Recipe: Replace missing values with the mean
#--------------------------------------------

dat <- read.csv("missing-data.csv", na.strings = "")
dat$Income.imp.mean <- ifelse(is.na(dat$Income), 
       mean(dat$Income, na.rm=TRUE), dat$Income)

rand.impute <- function(a) {
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

random.impute.data.frame <- function(dat, cols) {
  nms <- names(dat)
  for(col in cols) {
    name <- paste(nms[col],".imputed", sep = "")
    dat[name] <- rand.impute(dat[,col]) 
  }
  dat
}


dat <- read.csv("missing-data.csv", na.strings="")
random.impute.data.frame(dat, c(1,2))

#Recipe: Remove duplicate cases
#--------------------------------

salary <- c(20000, 30000, 25000, 40000, 30000, 34000, 30000)
family.size <- c(4,3,2,2,3,4,3)

car <- c("Luxury", "Compact", "Midsize", "Luxury",
       "Compact", "Compact", "Compact")
prospect <- data.frame(salary, family.size, car)
prospect.cleaned <- unique(prospect)
nrow(prospect)
nrow(prospect.cleaned)

duplicated(prospect)

prospect[duplicated(prospect), ]

#Recipe: Change the scale of a variable to [0,1]
#-----------------------------------------------
install.packages("scales")
library(scales)
students <- read.csv("data-conversion.csv")

students$Income.rescaled <- rescale(students$Income)

rescale(students$Income)

(students$Income - min(students$Income)) / (max(students$Income) - min(students$Income))

rescale(students$Income, to = c(1, 100))

rescale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".rescaled", sep = "")
    dat[name] <- rescale(dat[,col]) 
  }
  cat(paste("Rescaled ", length(column.nos), 
        " variable(s)\n"))
  dat
}

rescale.many(students, c(1,4))

#Recipe: Normalize or standardize data in a data frame
#------------------------------------------------------

housing <- read.csv("BostonHousing.csv")

housing.z <- scale(housing)

scale.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".z", sep = "")
    dat[name] <- scale(dat[,col]) 
  }
  cat(paste("Scaled ", length(column.nos), " variable(s)\n"))
  dat
}

housing <- read.csv("BostonHousing.csv")
housing <- scale.many(housing, c(1,3,5:7))

names(housing)

students <- read.csv("data-conversion.csv")

b <- c(-Inf, 10000, 31000, Inf)
n <- c("Low", "Medium", "High")
students$Income.cat <- cut(students$Income, breaks = b, labels = n)
students

students$Income.cat2 <- 
  cut(students$Income, breaks = 4, labels = c("Level1", "Level2", "Level3", "Level4"))


#Recipe: Create dummies for categorical variables
#-------------------------------------------------
install.packages("dummies")
library(dummies)
students <- read.csv("data-conversion.csv")
students.new <- dummy.data.frame(students, sep = ".")
names(students.new)


dummy(students$State, sep = ".")

students.new1 <- dummy.data.frame(students, names = c("State", "Gender") , sep = ".")
s