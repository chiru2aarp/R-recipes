#install.packages(c("wordcloud"))
library(wordcloud)
library(RColorBrewer)
library(tm)
pal = brewer.pal(6,"RdGy")
wordcloud("I also want to thank all the members of Congress and myadministration who are here today for the wonderful work that they do. I want to thank Mayor Gray and everyone here at THEARC for having me….", min.freq = 1, scale=c(2,0.5), random.color = TRUE, color = pal)
wordcloud(c("inequality","law","policy","unemploy","job","Economy","Democracy",
            "Republicans","challenge","congress","America","growth"),
          freq=c(26,9,2,7,30,26,1,4,3,9,57,9), min.freq = 0,random.color = TRUE, col = pal)

file = readLines("~/../Downloads/Analytics/Books/R Recipes/module 2/8/data/3lpigs.txt")
doc = Corpus(VectorSource(file))
doc= tm_map(doc, tolower)
doc= tm_map(doc, removePunctuation)
doc= tm_map(doc, removeNumbers)
stopwords("english")
doc= tm_map(doc, removeWords,stopwords("english"))
doc= tm_map(doc, removeWords,c("applause","must", "will","know"))
wordcloud(as.character(doc), scale= c(2,0.5))

windowsFonts(JP1 = windowsFont("MS Mincho"))
par(family = "JP1")
wordcloud(doc, scale= c(2,0.5))

files = DirSource("~/../Downloads/Analytics/Books/R Recipes/module 2/8/data/speech/")
data = Corpus(files)

data= tm_map(data, content_transformer(tolower))
data=tm_map(data, removePunctuation)
data=tm_map(data, removeNumbers)
data=tm_map(data, removeWords, stopwords("english"))
data=tm_map(data,removeWords,c("applause","Applause","APPLAUSE","And","But","will","must"))

data = TermDocumentMatrix(data)
data = as.matrix(data)

colnames(data)= c("3lpigs","3students","13chil")

comparison.cloud(data,max.words = 250, title.size = 2,colors = brewer.pal(3,"Set1"))




#######################
#######################

data(crude)

data= tm_map(crude, content_transformer(tolower))
data=tm_map(data, removePunctuation)
data=tm_map(data, removeNumbers)
data=tm_map(data, removeWords, stopwords("english"))
data = TermDocumentMatrix(data)

findFreqTerms(data, 14)
findAssocs(data, c("oil","crude"), c(0.56))

data = as.matrix(data)
crf = cor(data)
corrplot(crf, method = c("ellipse"),type = "lower",cl.lim = c(-0.1,1))
