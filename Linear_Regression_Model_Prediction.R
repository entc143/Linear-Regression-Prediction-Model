library(MASS)
data("Boston")
View(Boston)

#### We will split the database into Training & Testing data set ####
#set.seed(2)
library(caTools)
splilt <- sample.split(Boston$medv,SplitRatio = 0.7)
traning_data <- subset(Boston,split=="TRUE")
testing_data <- subset(Boston,split=="FALSE")
###### Corr-relation  #####
cr <- cor(Boston)
######### Creating Scatter Plot Matrix ########
attach(Boston)
library(lattice)
splom(~Boston[c(1:6,14)],groups = NULL,data = Boston,axis.line.tck =0,axis.text.alpha = 0)
splom(~Boston[c(7:14)],groups = NULL,data = Boston,axis.line.tck =0,axis.text.alpha = 0)
##Studying rm and medv  ####
plot(rm,medv)
abline(lm(medv~rm),col="red") ## REgression fit Line

#### We can use corplot to visualize ####
library(corrplot)
corrplot(cr,type = "lower")
corrplot(cr,method = "number")


#### To exclude medv(output) ####
Boston_a = subset(Boston,select = -c(medv))
numericData = Boston_a[sapply(Boston_a,is.numeric)]
descrcor = cor(numericData)

#### To find Multi-collinearity ####
install.packages("car")
library(car)
model <- lm(medv~.,data = traning_data)
vif(model) ## if correlation is 1 then it is not multivariate data


#### Model with all the database ####
model = lm(medv~.,data = traning_data)
# Or 
model = lm(medv~rm+rad+nox+dis,data = traning_data)

#### Prediction Model ####
predits <- predict(model,testing_data)
predits

#### To show the difference ####
plot(testing_data$medv,type="l",lty=1.8,col = "green")
lines(predics,type = "i",col = "blue")

#### To predict with new dataset ####
predicts = predict(model,sample_data)
predicts




#### For description of Model ####
summary(model)