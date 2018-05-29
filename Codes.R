#Libraries needed for this project
library(e1071)
library(gmodels)
library(class)
library(rpart)
library(caret)
library(ggplot2)
library(corrplot)
library(readr)
library(stringr)
library(xts)
library(lubridate)
library(forecast)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Data processing ---------------------------------------------------------
USvideos <- read_csv("Project/Datasets/USvideos.csv")
#We create a vector with the categories in string
strcategories <- c("Film & Animation", "Vehicles", "Others", "", "", "", "", "", "", "Music", "", "", "", "", "Animals", "", "Sports", "Short Movies", "Travelling", "Gaming", "Videoblogging", "People & Blogs", "Comedy", "Entertatiment", "News & Politics", "Howto & Style", "Education", "Science and Tech", "Non-Profits", "Movies", "Animation/Anime", "Action", "Classics", "Comedy", "Documentary", "Drama", "Family", "Foreign", "Horor", "SCI-FI", "Thriller", "Shorts", "Shows", "Trailers") 
#We create a copy of our initial DF to change things
df1 <- cbind(USvideos)

df1[["Categories"]]<-df1[["category_id"]]

df1 <- df1[match(unique(df1$video_id), df1$video_id),]
catid <- USvideos[match(unique(USvideos$category_id), USvideos$category_id),]$category_id
catid <-catid[order(catid)]

#Vector to count the videos from
c <- vector(mode="numeric", length=44)

for(row in 1:nrow(df1)){
  c[as.numeric(df1[row, "Categories"])] = c[as.numeric(df1[row, "Categories"])] +1
  if(df1[row, "Categories"] == "1")
    df1[row, "Categories"] <- "Film and Animation"
  else if(df1[row, "Categories"] == "2")
    df1[row, "Categories"] <- "Vehicles"
  else if(df1[row, "Categories"] == "10")
    df1[row, "Categories"] <- "Music"
  else if(df1[row, "Categories"] == "15")
    df1[row, "Categories"] <- "Animals"
  else if(df1[row, "Categories"] == "17")
    df1[row, "Categories"] <- "Sports"
  else if(df1[row, "Categories"] == "18")
    df1[row, "Categories"] <- "Short Movies"
  else if(df1[row, "Categories"] == "19")
    df1[row, "Categories"] == "Travel and Events"
  else if(df1[row, "Categories"] == "20")
    df1[row, "Categories"] <- "Gaming"
  else if(df1[row, "Categories"] == "21")
    df1[row, "Categories"] <- "VideoBlogging"
  else if(df1[row, "Categories"] == "22")
    df1[row, "Categories"] <- "People and Blogs"
  else if(df1[row, "Categories"] == "23")
    df1[row, "Categories"] <- "Comedy"
  else if(df1[row, "Categories"] == "24")
    df1[row, "Categories"] <-"Entertaiment"
  else if(df1[row, "Categories"] == "25")
    df1[row, "Categories"] <- "News and Politics"
  else if(df1[row, "Categories"] == "26")
    df1[row, "Categories"] <- "How to and Style"
  else if(df1[row, "Categories"] == "27")
    df1[row, "Categories"] <- "Education"
  else if(df1[row, "Categories"] == "28")
    df1[row, "Categories"] <- "Science and Tech"
  else if(df1[row, "Categories"] == "29")
    df1[row, "Categories"] <- "Non Profit and Activism"
  else if(df1[row, "Categories"] == "30")
    df1[row, "Categories"] <- "Movies"
  else if(df1[row, "Categories"] == "31")
    df1[row, "Categories"] <- "Anime/Animation"
  else if(df1[row, "Categories"] == "32")
    df1[row, "Categories"] <- "Action/Adventure"
  else if(df1[row, "Categories"] == "33")
    df1[row, "Categories"] <- "Classics"
  else if(df1[row, "Categories"] == "34")
    df1[row, "Categories"] <- "Comedy"
  else if(df1[row, "Categories"] == "35")
    df1[row, "Categories"] <- "Documentary"
  else if(df1[row, "Categories"] == "36")
    df1[row, "Categories"] <- "Drama"
  else if(df1[row, "Categories"] == "37")
    df1[row, "Categories"] <- "Family"
  else if(df1[row, "Categories"] == "38")
    df1[row, "Categories"] <- "Foreign"
  else if(df1[row, "Categories"] == "39")
    df1[row, "Categories"] <- "Horror"
  else if(df1[row, "Categories"] == "40")
    df1[row, "Categories"] <- "Sci-Fi"
  else if(df1[row, "Categories"] == "41")
    df1[row, "Categories"] <- "Thriller"
  else if(df1[row, "Categories"] == "42")
    df1[row, "Categories"] <- "Shorts"
  else if(df1[row, "Categories"] == "43")
    df1[row, "Categories"] <- "Shows"
  else 
    df1[row, "Categories"] <- "Trailers"
}

# Statistics of videos ---------------------------------------------------------

#Calculation of percentages of each category
pct <- round((c/sum(c))*100)

#We obtain the position of our categories with at least 1 video and at least 5%
positions <- character()
for (element in 1:length(pct)){
  if (pct[element] > 5){
    positions <- c(positions, strcategories[element])
  }
}
pct <- pct[pct>5]

#Addition as last element of sum of the ones with less than 5% and give and id of 3
pct <- c(pct, 100-sum(pct))
positions <- c(positions, "Other")

# Plot of the percentage distribution ---------------------------------------------------------

barplot(pct, names.arg = positions, las = 1, horiz = TRUE, xlim = c(0, 25), main = "Percentage distribution", cex.names = 0.45)
pie(pct, label = positions, radius = 0.9, main = "Pie Categories Distribution")

# Calculations for representations ---------------------------------------------------------

#Calculation of  days to be trendy
initialdate = ymd(substr(df1$publish_time, start = 1,stop = 10))
trendingdate = ydm(df1$trending_date)
df1[["DaysToTrend"]] <- trendingdate -initialdate
df1$DaysToTrend <- as.numeric(df1$DaysToTrend)

df1 = subset(df1, df1$DaysToTrend<20) #-> Videos with relevant points

#Calculation of average day per category to be trendy
avgDayCat <- numeric()

for(element in 1:length(strcategories)){
  avgDayCat <- c(avgDayCat, sum(df1$DaysToTrend[which(df1$category_id == element)])/sum(df1$category_id == element))
}

avgDayCat[is.na(avgDayCat)] <- 0
dayPosition <- character()

for(element in 1:length(strcategories)){
  if(avgDayCat[element] != 0)
    dayPosition <- c(dayPosition, strcategories[element])
}
avgDayCat <- avgDayCat[avgDayCat>0]

#Days to trend plot
barplot(avgDayCat, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 4), main = "Days To Trend", cex.names = 0.45)

#Average days in trend per video category adding to the whole dataset with repeated values daystotrend column
initialdate = ymd(substr(USvideos$publish_time, start = 1,stop = 10))
trendingdate = ydm(USvideos$trending_date)
USvideos[["DaysToTrend"]] <- trendingdate -initialdate
USvideos$DaysToTrend <- as.numeric(USvideos$DaysToTrend)


video_id = df1$video_id
daystrending <- numeric()
likesIncrease <- numeric()
dislikesIncrease <- numeric()
commentIncrease <- numeric()
viewIncrease <- numeric()

pos <- 0

#We calculate how many days a video taking the highest daytotrend-lowestdaystotrend and increases since they become trend
for(position in 1:length(df1$video_id)){
  aux <- subset(USvideos, USvideos$video_id == video_id[position])
  if(length(aux$DaysToTrend) > 1){
    dt <- (aux$DaysToTrend[length(aux$DaysToTrend)] - aux$DaysToTrend[1])[1]
    il <- (aux$likes[length(aux$likes)] - aux$likes[1])[1]
    ic <- (aux$comment_count[length(aux$comment_count)] - aux$comment_count[1])[1]
    id <- (aux$dislikes[length(aux$dislikes)] - aux$dislikes[1])[1]
    iv <- (aux$views[length(aux$views)] - aux$views[1])[1]
  }else{
    dt <- 1
    il <- 0
    ic <- 0
    id <- 0
    iv <- 0
  }
  daystrending <- c(daystrending, dt)
  likesIncrease <- c(likesIncrease, il)
  dislikesIncrease <- c(dislikesIncrease, id)
  commentIncrease <- c(commentIncrease, ic)
  viewIncrease <- c(viewIncrease, iv)
  
}

df1[["DaysTrending"]] <- daystrending
df1[["LikeIncrease"]] <- likesIncrease
df1[["DislikeIncrease"]] <- dislikesIncrease
df1[["CommentIncrease"]] <- commentIncrease
df1[["ViewIncrease"]] <- viewIncrease

daysTrendingAvg <- numeric()
likesCategory <- numeric()
dislikesCategory <- numeric()
commentsCategory <- numeric()
viewsCategory <- numeric()
viAvg <- numeric()
liAvg <- numeric()
ciAvg <- numeric()
diAvg <- numeric()

for(element in 1:length(strcategories)){
  daysTrendingAvg <- c(daysTrendingAvg, sum(df1$DaysTrending[which(df1$category_id == element)])/sum(df1$category_id == element))
  likesCategory <- c(likesCategory, sum(df1$likes[which(df1$category_id == element)])/sum(df1$category_id == element))
  dislikesCategory <- c(dislikesCategory, sum(df1$dislikes[which(df1$category_id == element)])/sum(df1$category_id == element))
  commentsCategory <- c(commentsCategory, sum(df1$comment_count[which(df1$category_id == element)])/sum(df1$category_id == element))
  viewsCategory <- c(viewsCategory, sum(df1$views[which(df1$category_id == element)])/sum(df1$category_id == element))
  viAvg <- c(viAvg, sum(df1$ViewIncrease[which(df1$category_id == element)])/sum(df1$category_id == element))
  liAvg <- c(liAvg, sum(df1$LikeIncrease[which(df1$category_id == element)])/sum(df1$category_id == element))
  ciAvg <- c(ciAvg, sum(df1$CommentIncrease[which(df1$category_id == element)])/sum(df1$category_id == element))
  diAvg <- c(diAvg, sum(df1$DislikeIncrease[which(df1$category_id == element)])/sum(df1$category_id == element))
}

daysTrendingAvg[is.na(daysTrendingAvg)] <- 0
likesCategory[is.na(likesCategory)] <- 0
dislikesCategory[is.na(dislikesCategory)] <- 0
commentsCategory[is.na(commentsCategory)] <- 0
viewsCategory[is.na(viewsCategory)] <- 0
viAvg[is.na(viAvg)] <- 0
ciAvg[is.na(ciAvg)] <- 0
diAvg[is.na(diAvg)] <- 0
liAvg[is.na(liAvg)] <- 0


daysTrendingAvg <- daysTrendingAvg[daysTrendingAvg>0]
likesCategory <- likesCategory[likesCategory>0]
dislikesCategory <- dislikesCategory[dislikesCategory>0]
commentsCategory <- commentsCategory[commentsCategory>0]
viewsCategory <- viewsCategory[viewsCategory>0]
viAvg <- viAvg[viAvg>0]
ciAvg <- ciAvg[ciAvg>0]
diAvg <- diAvg[diAvg>0]
liAvg <- liAvg[liAvg>0]

# Plots of average behaviors per category ---------------------------------------------------------
#Plot of average days in trend per category
barplot(daysTrendingAvg, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 5.5), main = "Days To Trend", cex.names = 0.45)
barplot(viewsCategory, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 1200000), main = "Views", cex.names = 0.45)
barplot(likesCategory, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 120000), main = "Likes", cex.names = 0.45)
barplot(dislikesCategory, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 17000), main = "Dislikes", cex.names = 0.45)
barplot(commentsCategory, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 35000), main = "Comments", cex.names = 0.45)

# Plots of different increments ---------------------------------------------------------
barplot(viAvg, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 3000000), main = "Views Increase", cex.names = 0.45)
barplot(ciAvg, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 15000), main = "Comments Increase", cex.names = 0.45)
barplot(liAvg, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 90000), main = "Likes Increase", cex.names = 0.45)
barplot(diAvg, names.arg = dayPosition, las = 1, horiz = TRUE, xlim = c(0, 7000), main = "Dislikes Increase", cex.names = 0.45)


#Modelling

# Time-series analysis ---------------------------------------------------------
dates <- USvideos[match(unique(USvideos$trending_date), USvideos$trending_date),]$trending_date
catid <- USvideos[match(unique(USvideos$category_id), USvideos$category_id),]$category_id
catid <-catid[order(catid)]

for(i in 1:length(catid)){
  catWithViews <- strcategories[catid]
}

#Dataframe with dates and columns are the categories with views
dfcat <- data.frame(matrix(ncol = length(catWithViews), nrow = length(dates)))
rownames(dfcat) <- dates
colnames(dfcat) <- catWithViews
dfcat[is.na(dfcat)]<- 0

for(day in 1:nrow(dfcat)){
  for(category in 1:ncol(dfcat)){
    aux <- subset(USvideos, USvideos$trending_date == dates[day])
    dfcat[day, category] <- sum(aux$views[which(aux$category_id==catid[category])])
  }
}

par(mfrow = c(4,4), mar=c(2.5,2.5,2.5,2.5))
for(col in 1:ncol(dfcat)){
  assign(paste("viewts", col, sep=""),zoo(dfcat[col]/1000000, as.Date(dates, format = "%y.%d.%m")))
  viewts <- zoo(dfcat[col]/1000000, as.Date(dates, format = "%y.%d.%m"))
  plot(viewts, xlab = "Months", ylab = "Millions of views", main = paste("Views(M) for",catWithViews[col]))
}
par(mfrow = c(4,4), mar=c(3,3,3,3))
for(col in 1:ncol(dfcat)){
  acf(coredata(assign(paste("viewts", col, sep=""),zoo(dfcat[col], as.Date(dates, format = "%y.%d.%m")))))
}

for(col in 1: ncol(dfcat)){
  assign(paste("J", col, sep=""),jarque.bera.test(coredata(assign(paste("viewts", col, sep=""),zoo(dfcat[col], as.Date(dates, format = "%y.%d.%m"))))))
}

for(col in 1: ncol(dfcat)){
  assign(paste("L", col, sep=""),Box.test(coredata(assign(paste("viewts", col, sep=""),zoo(dfcat[col], as.Date(dates, format = "%y.%d.%m")))), lag = 20, type = 'Ljung'))
}

for(col in 1:ncol(dfcat)){
  plot(diff(assign(paste("viewtsd", col, sep=""),zoo(dfcat[col]/1000000, as.Date(dates, format = "%y.%d.%m")))),  xlab = "Months", ylab = "Millions of views", main = paste("Views(M) for",catWithViews[col]))
}
par(mfrow = c(4,4), mar=c(3.5,3.5,3.5,3.5))
for(col in 1:ncol(dfcat)){
  acf(coredata(diff(assign(paste("viewtsd", col, sep=""),zoo(dfcat[col], as.Date(dates, format = "%y.%d.%m"))))), lag = 15)
}
for(col in 1:ncol(dfcat)){
  pacf(coredata(diff(assign(paste("viewtsd", col, sep=""),zoo(dfcat[col], as.Date(dates, format = "%y.%d.%m"))))), main = catWithViews[col], lag = 15)
}


for(col in 1:ncol(dfcat)){
  EACF(coredata(diff(assign(paste("viewtsd", col, sep=""),zoo(dfcat[col]/1000000, as.Date(dates, format = "%y.%d.%m"))))))
}
dfd <- data.frame(matrix(ncol = length(catWithViews), nrow = length(dates)-1))
for(col in 1:ncol(dfcat)){
  dfd[col]<- diff(zoo(dfcat[col]/1000000, as.Date(dates, format = "%y.%d.%m")))
  assign(paste("autoarima", col, sep=""),auto.arima(coredata(dfd[col]), max.p = 8, max.q = 8, stationary = FALSE, ic = "aic", stepwise = TRUE))
}


par(mfrow = c(1,1))
plot(forecast(autoarima2, h=30))
plot(forecast(autoarima12, h=30))

par(mfrow = c(1,2))
arima22 <- arima(coredata(dfd[1:100, 1]), order = c(1,0,2),include.mean = T, method = "ML")
par(mfrow = c(1,2))
plot(forecast(arima22, h=30))
plot(dfd[2])
arima11 <- arima(coredata(dfd[1:100, 1]), order = c(1,0,2),include.mean = T, method = "ML")
par(mfrow = c(1,2))
plot(forecast(arima11, h=30), main = "ARMA model")
plot(dfd[2], main = "Evolution of vehicles")


# Linear analysis ---------------------------------------------------------

par(mfrow = c(1,1))
lm = lm((ViewIncrease)^(1/3) ~ CommentIncrease + LikeIncrease + dislikesIncrease + DaysTrending, data=df1)
dflm <- subset(df1,cooks.distance(lm)<(4/length(df1$video_id)))
lm = lm((ViewIncrease)^(1/3) ~ CommentIncrease + LikeIncrease + DaysToTrend, data=df1)
plot(lm)

# Classification analysis ---------------------------------------------------------

#Analyze correlation
df3 <- df1[c('views','likes','dislikes','comment_count','DaysToTrend','DaysTrending','LikeIncrease','DislikeIncrease','CommentIncrease','ViewIncrease','category_id')]
df3 <- sapply(df3, as.numeric )
res <- cor(df3)
corrplot(res, type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
#Draw clusters
df1$category_id <-as.factor(df1$category_id)
ggplot(df1, aes(views,likes, color = df1$category_id)) + geom_point()

# Data preprocessing ---------------------------------------------------------
#Subdataset with the numerical variables with more influence and unique video ID
df2 <- df1[c('likes','views')] 
ind <- sample(2, nrow(df2), replace=TRUE, prob=c(0.67, 0.33))
vid_train <- df2[ind==1,]
vid_test <- df2[ind==2,]

#Removal of the class labels for modelling
train_lab <- factor(df1[ind==1,5])
test_lab <- factor(df1[ind==2,5])

#Normalize the data
fnormalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
vid_norm_tr <-as.data.frame(fnormalize(vid_train))
vid_norm_ts <-as.data.frame(fnormalize(vid_test))

# Naive Bayes ---------------------------------------------------------

#Naive Bayes model with cross-fold validation 
nb2 =train(vid_norm_tr,train_lab,'nb',trControl=trainControl(method='cv',number=10))
nb2_pred<-predict(nb2$finalModel,vid_norm_ts)

#Naive Bayes model without cross-fold validation
vid_train <- sapply(vid_train, as.numeric )
vid_test <- sapply(vid_test, as.numeric )
nb = naiveBayes(vid_norm_tr,train_lab,laplace=1)
nb_pred = predict(nb,vid_norm_ts)

#Accuracy for Naive Bayes with cross-fold validation
100*sum(nb2_pred$class == test_lab)/length(test_lab)
#Accuracy for Naive Bayes without cross-fold validation
100*sum(nb_pred == test_lab)/length(test_lab)

#Per-class precision for Naive Bayes classifier
cm = as.matrix(table(test_lab,nb_pred))
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 

# K-Nearest Neighbors ---------------------------------------------------------

#Elbow method for k optimal value
wss <- (nrow(dfcat)-1)*sum(apply(dfcat,2,var))
for (i in 2:30) {
  wss[i] <- sum(kmeans(dfcat,centers=i)$withinss)
}
plot(1:30, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#Best result obtained for KNN model without cross-fold validation
knn <- knn(train=vid_norm_tr, test=vid_norm_ts, cl=train_lab, k=27)

#Cross-fold validation with KNN
knn2 =train(vid_norm_tr,train_lab,'knn',trControl=trainControl(method='cv',number=10))
knn2_pred <- predict(knn2,vid_norm_ts)

#Accuracy for KNN model without cross-fold validation
100*sum(knn == test_lab)/length(test_lab)
#Accuracy for KNN model with cross-fold validation
100*sum(knn2_pred == test_lab)/length(test_lab)

#Per-class precision for KNN classifier
cm = as.matrix(table(test_lab,knn2))
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 

# Decision tree ---------------------------------------------------------
vid_norm_tr[["category_id"]] <- train_lab
tree<-rpart(category_id~views+likes,data=vid_norm_tr)
tree_pred<-predict(tree,vid_norm_ts,type="class")

#Accuracy for decision tree model
mtab<-table(tree_pred,test_lab)
confusionMatrix(mtab)

# Word cloud for future classification using dictionaries ---------------------------------------------------------

# Word cloud for titles in USvideos
text <- paste(df1$title, sep=" ")
# Load the data as a corpus
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove our own stopwords as a character vector
docs <- tm_map(docs, removeWords, c("the", "and","how","not","with")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.20, colors=brewer.pal(8, "Dark2"))