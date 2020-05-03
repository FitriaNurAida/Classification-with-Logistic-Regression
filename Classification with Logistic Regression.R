# Importing Data #
df <- read.csv("E:/My Dictionary/Using R/Data/House_Price_2.csv")
View(df)
str(df)

############################## Preprocessing Data ############################################
summary(df)
boxplot(df$n_hot_rooms) #vaiable n_hot_rooms has outliers data
pairs(~Sold+rainfall, data = df) #there is an outlier
barplot(table(df$airport))
barplot(table(df$bus_ter)) #this variable is useless because it contains only 1 category

# Outlier Treatment #
summary(df$n_hot_rooms)
quantile(df$n_hot_rooms, 0.99) #P99
uv = 3*quantile(df$n_hot_rooms, 0.99) ; uv #3*P99
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv #replace all values above 3*P99 with 3*P99, it is used for higher outlier
summary(df$n_hot_rooms)

summary(df$rainfall)
lv = 0.3*quantile(df$rainfall, 0.01) ; lv 
df$rainfall[df$rainfall<lv] <-lv ##replace all values above 0.3*P1 with 0.3*P1, it is used for lower outlier
summary(df$rainfall)

# Handling Missing Value #
mean(df$n_hos_beds)
mean(df$n_hos_beds,na.rm = TRUE) #without NA value
which(is.na(df$n_hos_beds)) #Mencari letak NA
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean(df$n_hos_beds,na.rm = TRUE) #Imputing mean
summary(df$n_hos_beds)
which(is.na(df$n_hos_beds))

# Variable Transformation #
df$avg_dist = (df$dist1+df$dist2+df$dist3+df$dist4)/4 #dimension reduction
View(df)
df2 <- df[,-6:-9] #because we have had new variable svg_dist then we remove variable dist
df <- df2
rm(df2)
df <-df[,-13] #because variabel 13 has 1 category so it is useless

# Dummy Variable #
install.packages("dummies")
library(dummies)
df <- dummy.data.frame(df)
View(df)
df <- df[,-8] #if the variable has 2 categories, there will be 1 dummy variable, because -> the number of dummy variable = the number of categories - 1
df <- df[,-13] #if the variable has 2 categories, there will be 1 dummy variable, because -> the number of dummy variable = the number of categories - 1

################### Logistic Regression with Single Predictor #########################
install.packages("MASS")
library(MASS)
glm.fit <- glm(Sold~price, data=df, family = binomial)
summary(glm.fit)
#variable Sold is significantly affected by variable price

################### Logistic Regression with Multiple Predictor #########################
glm.fit <- glm(Sold~., data=df, family = binomial)
summary(glm.fit)
# significant variable : price, air_equal, room_num, teachers, poor_prop, n_hot_beds
glm.prob <- predict(glm.fit, type="response")
glm.prob[1:10] #to top 10 result
glm.pred<-rep(0,506) #506 is the total number of data, means that I classify all responses to 0
glm.pred[glm.prob>0.5] <- 1 #then I classify the responses which has probablity greater than 0.5 as 1

# Confusion Matrix #
glm.pred[1:10]
cm <- table(glm.pred, df$Sold) #Confusion Matrix
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm); Accuracy #akurasi = 68,38%
