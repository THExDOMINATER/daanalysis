install.packages("ggplot2")
library(ggplot2)

#SCATTER PLOT
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
# Change the point size, and shape
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

#barplot
ggplot(mtcars)+
  barplot(height=BOD$demand)

#histogram
ggplot(mtcars, aes(x=wt)) + geom_histogram()

#boxplot and dot plot
ggplot(mtcars,aes(x=wt,y=mpg))+geom_boxplot()+geom_dotplot(binaxis="y",stackdir = "center")

#LOGISTIC REGRESSION
#The Logistic Regression is a regression model in which the response variable (dependent variable) has categorical values such as True/False or 0/

input <- mtcars[,c("am","cyl","hp","wt")]

print(head(input))

input <- mtcars[,c("am","cyl","hp","wt")]

am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = binomial)

print(summary(am.data))

#LINEAR REGRESSION

x<- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y<- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
relation <- lm(y~x)
print(relation)
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
relation <- lm(y~x)
print(summary(relation))
a <- data.frame(x = 170)
result <-  predict(relation,a)
print(result)
png(file = "linearregression.png")
plot(y,x,col = "blue",main = "Height & Weight Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")
dev.off()

#MULTIPLE REGRESSION
input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))
model <- lm(mpg~disp+hp+wt, data = input)
print(model)
cat("# # # # The Coefficient Values # # # ","\n")
a <- coef(model)[1]
print(a)
Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]
print(Xdisp)
print(Xhp)
print(Xwt)
x1<-221
x2<-102
x3<-2.91
Y = 37.15+(-0.000937)*x1+(-0.0311)*x2+(-3.8008)*x3
print(Y)




# DECISION TREE


library(caret)
library(rpart.plot)
irisdata<-datasets::iris
head(iris)
table(iris$Species)
summary(irisdata)
str(irisdata)
set.seed(3033)
intrain <- createDataPartition(y = irisdata$Species, p= 0.7, list = FALSE)
training <- irisdata[intrain,]
testing <- irisdata[-intrain,]
dim(training);dim(testing)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit_info <- train(Species ~., data = training, method = "rpart", parms = list(split = "information"), trControl=trctrl, tuneLength = 10)
prp(dtree_fit_info$finalModel, box.palette="Reds", tweak=1.2)
test_pred_info<-predict(dtree_fit_info,newdata = testing)
confusionMatrix(test_pred_info,testing$Species)
set.seed(3333)
dtree_fit_gini <- train(Species ~., data = training, method = "rpart", parms = list(split = "gini"), trControl=trctrl, tuneLength = 10)
prp(dtree_fit_gini$finalModel,box.palette = "Blues", tweak = 1.2)

test_pred_gini<-predict(dtree_fit_gini,newdata = testing)
confusionMatrix(test_pred_gini,testing$Species)


#KMEANS

iris
head(iris)
iris.labels <- iris$Species 
table(iris.labels)
iris_data <- iris[1:4]
iris_data
iris_data_scale <- scale(iris_data)
iris_data_scale
iris_data<- dist(iris_data_scale)
library (factoextra)
fviz_nbclust(iris_data_scale, kmeans, method ="wss") +
  labs(subtitle ="Elbow Method")
km.out <-kmeans(iris_data_scale, centers =3, nstart =100)
print(km.out)
km.clusters <-km.out$cluster
rownames(iris_data_scale)<-paste(iris$Species, 1:dim(iris)[1],seq="_")
fviz_cluster(list(data=iris_data_scale,cluster =km.clusters))

#dataframes

Employees = data.frame(Name=c("Anastasia S","Dima R","Katherine S", "JAMES A","LAURA MARTIN"),
                       Gender=c("M","M","F","F","M"),
                       Age=c(23,22,25,26,32),
                       Designation=c("Clerk","Manager","Exective","CEO","ASSISTANT"),
                       SSN=c("123-34-2346","123-44-779","556-24-433","123-98-987","679-77-576")
)
print("Details of the employees:")                      
print(Employees)
# Employees[c(3,5),c(2,3)]
table(Employees$Gender)
install.package('dplyr')
library(dplyr)
Employees %>%
  group_by(Age) %>%
  summarize(Freq=n())
hist(Employees$Age)

#palindrome

{
  n = as.integer(readline(prompt = "Enter a number :"))
  
  rev = 0
  num = n
  
  while (n > 0) {
    r = n %% 10
    rev = rev * 10 + r
    n = n %/% 10
  }
  
  if (rev == num)
  {
    print(paste("Number is palindrome :", rev))
  }
  else{
    print(paste("Number is not palindrome :", rev))
  }
}

#factorial

answer1 <- factorial(c(0, 1, 2, 3, 4)) 
  
print(answer1) 
