## Reading the data set into R #####

carmileage <- read.csv("carMPG.csv")

### Checking the structure of car mileage

str(carmileage)

############ Data preparation #########################

##### Removing duplicate Rows

length(which(duplicated(carmileage)))

### There are no duplicate rows in the data.

## Variables Formatting 

### Upon checking the structure of the data set we now look a each variable and decide
### the type of it.

## 1. MPG ("numeric" since its a continuous variable)

class(carmileage$MPG)

unique(carmileage$MPG)

## 2. Cylinders (Integer since multi valued discrete variable)

class(carmileage$Cylinders)

unique(carmileage$Cylinders)


## 3. Displacement ("numeric" since it is a continuous variable)

class(carmileage$Displacement)

unique(carmileage$Displacement)


### 4.Horsepower ("numeric since it is a continuous variable")

class(carmileage$Horsepower)

unique(carmileage$Horsepower)

carmileage$Horsepower <- as.numeric(levels(carmileage$Horsepower))[carmileage$Horsepower]

### 5.Weight ("Numeric since it is a continuous variable")

class(carmileage$Weight)

unique(carmileage$Weight)

carmileage$Weight <- as.numeric(carmileage$Weight)

## 6.Acceleration ("numeric since it is continuous variable")

class(carmileage$Acceleration)

unique(carmileage$Acceleration)

## 7.Model year(" integer since it is multivalued discrete variable")

class(carmileage$Model_year)

unique(carmileage$Model_year)


## 8. Origin (" integer since it is a multivalued discrete variable")

class(carmileage$Origin)

unique(carmileage$Origin)

### 9. Car name


class(carmileage$Car_Name)

unique(carmileage$Car_Name)



##### Data Cleaning : Missing value and outlier treatment for all variables ##### 

## 1.MPG

class(carmileage$MPG)

which(is.na(carmileage$MPG))

which(is.null(carmileage$MPG))

unique(carmileage$MPG)

boxplot(data = carmileage, x = carmileage$MPG, na.action = NULL)

quantile(carmileage$MPG, probs = seq(0,1,0.025))


### 2. Cylinders

class(carmileage$Cylinders)

which(is.na(carmileage$Cylinders))

which(is.null(carmileage$Cylinders))

unique(carmileage$Cylinders)


### 3.Displacement


class(carmileage$Displacement)

which(is.na(carmileage$Displacement))

which(is.null(carmileage$Displacement))

unique(carmileage$Displacement)

boxplot(data = carmileage, x = carmileage$Displacement, na.action = NULL)

## As seen in the box plot there are no outlier values.

###### 4. Horsepower

class(carmileage$Horsepower)

which(is.na(carmileage$Horsepower))

## we replace the NA values with mean.

carmileage$Horsepower[which(is.na(carmileage$Horsepower))] <- mean(carmileage$Horsepower,na.rm = TRUE)

which(is.na(carmileage$Horsepower))

which(is.null(carmileage$Horsepower))

unique(carmileage$Horsepower)

boxplot(data = carmileage, x = carmileage$Horsepower)

## we could see that there are outliers in the Horsepower variable so we remove them
## using the capping and flooring method.

plot(carmileage$Horsepower)

quantile(carmileage$Horsepower, probs = seq(0,1,0.005), na.rm = TRUE)

carmileage$Horsepower[carmileage$Horsepower > 170.0000] <- 170.0000

carmileage$Horsepower[carmileage$Horsepower < 58.000] <- 58.000

boxplot(data = carmileage, x = carmileage$Horsepower)

## now we dont see any outlier values in the Horsepower variable


### 5.	Weight

class(carmileage$Weight)

which(is.na(carmileage$Weight))

which(is.null(carmileage$Weight))

unique(carmileage$Weight)

boxplot(data = carmileage, x = carmileage$Weight)

## we could not see that there are outliers in the Weight variable.
## we check the quantile to check for any exterme values and perform flooring 
## and capping on Weight

quantile(carmileage$Weight, probs = seq(0,1,0.005), na.rm = TRUE)

carmileage$Weight[carmileage$Weight > 4515.560] <- 4515.560

carmileage$Weight[carmileage$Weight < 1753.410] <- 1753.410

#### 6. Acceleration

class(carmileage$Acceleration)

which(is.na(carmileage$Acceleration))

which(is.null(carmileage$Acceleration))

unique(carmileage$Acceleration)

boxplot(data = carmileage, x = carmileage$Acceleration)

## we could see that there are outliers in the Acceleration variable so we remove them
## using the capping and flooring method.

quantile(carmileage$Acceleration, probs = seq(0,1,0.005), na.rm = TRUE)

carmileage$Acceleration[carmileage$Acceleration > 19.6540] <-19.6540

carmileage$Acceleration[carmileage$Acceleration < 9.4850] <- 9.4850

boxplot(data = carmileage, x = carmileage$Acceleration)

## now we dont see any outlier values in the Acceleration variable

###### 7.Model year

class(carmileage$Model_year)

which(is.na(carmileage$Model_year))

summary(carmileage$Model_year)

#######  8.Origin

class(carmileage$Origin)

which(is.na(carmileage$Origin))

summary(carmileage$Origin)

####### 9.Car_Name

class(carmileage$Car_Name)

which(is.na(carmileage$Car_Name))

summary(carmileage$Car_Name)

###################   Variables Transformation #########################

## we create a car company variable to get the names of car companies of each 
## car name in order to reduce the numberofcar names

carmileage$Car_Company <- gsub( " .*$", "", carmileage$Car_Name)

#### reducing the number of car company variables by correcting the company names
###

unique(carmileage$Car_Company)

########## by checking all the unique values of car company names we find below 

## "chevrolet" and "chevy" has been wrongly spelled as "chevroelt"so we use gsub to correct it.

carmileage$Car_Company <- gsub("chevroelt","chevrolet", carmileage$Car_Company)

carmileage$Car_Company <- gsub("chevy","chevrolet", carmileage$Car_Company)

unique(carmileage$Car_Company)

## "toyota" has been wrongly spelled as "toyouta" so we use gsub to correct it.

carmileage$Car_Company <- gsub("toyouta", "toyota", carmileage$Car_Company)

## "volkswagen" has been wrongly spelled as "vokswagen" and "vw" so we use gsub to correct it.

carmileage$Car_Company <- gsub("vokswagen", "volkswagen", carmileage$Car_Company)

carmileage$Car_Company <- gsub("vw", "volkswagen", carmileage$Car_Company)

## "mercedes-benz" has been wrongly spelled as "mercedes" so we use gsub to correct it.

carmileage$Car_Company <- gsub("mercedes", "mercedes-benz", carmileage$Car_Company)

## Upon replacing mercedes  with "mercedes-benz" we find that "mercedes-benz" has 
## changed as "mercedes-benz-benz" so now we replace "mercedes-benz-benz" 
## with "mercedes-benz"

carmileage$Car_Company <- gsub( "mercedes-benz-benz", "mercedes-benz", carmileage$Car_Company)

## MAZDA has been wrongly spelled as maxda so we replace them.

carmileage$Car_Company <- gsub( "maxda", "mazda", carmileage$Car_Company)

## Now we use Model.matrix to convert the Categorical variable Car_Company
## into a numeric variables with 1 or 0 for all the factor levels

Carcomp_num <- data.frame(model.matrix(~Car_Company, data = carmileage))[,-1]


## we convert the Discrete variable cyinders into Numeric 

binsc<- data.frame(cut(carmileage$Cylinders, breaks = 3))

colnames(binsc)[] <- "cylinders"

binsc1 <- data.frame(model.matrix(~cylinders, data = binsc))

binsc1 <- data.frame(model.matrix(~cylinders, data = binsc))[-1]

## we conver the Discrete Variable Origin into numeric 

Origin_num <- data.frame(model.matrix(~Origin, data = carmileage))[-1]

## we reduce the number of variables by reducing the year of model 

bins<- data.frame(cut(carmileage$Model_year, breaks = 4))

colnames(bins)[] <- "Year"

bins1 <- data.frame(model.matrix(~Year, data = bins))

bins1 <- data.frame(model.matrix(~Year, data = bins))[-1]

colnames(bins1)[] <- c("Model Year:2006-2009","Model Year:2009-2012","Model Year:2012-2015")

## we bind the Numeric variables created from categorical variables and remove the 
## categorical variables to proceed with model creation.

carmileage <- carmileage[,-c(2,7:10)]

### Now we add the Dummy variables created to the carmileage data frame

carmileage <- cbind(carmileage, Carcomp_num,Origin_num,bins1,binsc1)

## diving into training and test data sets 

set.seed(100)

indices= sample(1:nrow(carmileage), 0.7*nrow(carmileage))

train = carmileage[indices,]
test = carmileage[-indices,]

####### Model building #######

model_1 <- lm(MPG~.,data=train)
summary(model_1)

library("MASS")
library("car")

# Apply the stepwise approach

step <- stepAIC(model_1, direction="both")

step

## model 2 

model_2 <- lm(formula = MPG ~ Horsepower + Weight + Acceleration + Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + Car_Companymercury + 
                Car_Companynissan + Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_2)

vif(model_2)

## we remove Acceleration as it has vif >2 and high P value compared to other variables
## which have higher VIF so since it is not so significant we remove it.

model_3 <- lm(formula = MPG ~ Horsepower + Weight +Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + Car_Companymercury + 
                Car_Companynissan + Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_3)

vif(model_3)


## we remove Horse power as it has some what larger p value compared to weight.
## we also check the correlation between Horsepower and Weight as they seem to be 
## Collinear

cor(carmileage$Horsepower, carmileage$Weight)

## [1] 0.8831895

## They seem to be be highly collinear so we remove Horsepower as it has high p value 
## compared to weight.

model_4 <- lm(formula = MPG ~ Weight +Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + Car_Companymercury + 
                Car_Companynissan + Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_4)

vif(model_4)


## now upon checking the VIF values of all variables we find that they are as per
## our business rule (less than 2) but since our model contains more variables than
## needed ( 5 as per our rule) we use P value to reduce them.

summary(model_4)

## we remove Car_Companynissan


model_4 <- lm(formula = MPG ~ Weight +Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + Car_Companymercury + 
                Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_4)

## we remove Car_Companymercury

model_5 <- lm(formula = MPG ~ Weight +Car_Companydatsun + 
                Car_Companyford + Car_Companyhonda + 
                Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_5)

## we remove Car_Companyford

model_6 <- lm(formula = MPG ~ Weight +Car_Companydatsun +Car_Companyhonda + 
                Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota + Car_Companytriumph + Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_6)

## we remove Car_Companytriumph


model_7 <- lm(formula = MPG ~ Weight +Car_Companydatsun +Car_Companyhonda + 
                Car_Companyplymouth + Car_Companypontiac + 
                Car_Companytoyota +Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_7)


## we remove Car_Companytoyota


model_8 <- lm(formula = MPG ~ Weight +Car_Companydatsun +Car_Companyhonda + 
                Car_Companyplymouth + Car_Companypontiac + Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_8)


## we remove Car_Companyhonda


model_9 <- lm(formula = MPG ~ Weight +Car_Companydatsun +
                Car_Companyplymouth + Car_Companypontiac + Car_Companyvolkswagen + 
                `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                cylinders.4.67.6.33., data = train)

summary(model_9)

## we remove Car_Companypontiac

model_10 <- lm(formula = MPG ~ Weight +Car_Companydatsun +
                 Car_Companyplymouth + Car_Companyvolkswagen + 
                 `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                 cylinders.4.67.6.33., data = train)

summary(model_10)

## we remove Car_Companyplymouth

model_11 <- lm(formula = MPG ~ Weight +Car_Companydatsun + Car_Companyvolkswagen + 
                 `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                 cylinders.4.67.6.33., data = train)

summary(model_11)

## we remove Car_Companydatsun

model_12 <- lm(formula = MPG ~ Weight + Car_Companyvolkswagen + 
                 `Model Year:2006-2009` + `Model Year:2009-2012` + `Model Year:2012-2015` + 
                 cylinders.4.67.6.33., data = train)

summary(model_12)

## we remove Car_Companyvolkswagen


model_13 <- lm(formula = MPG ~ Weight +
                 `Model Year:2006-2009` + `Model Year:2009-2012` + 
                 `Model Year:2012-2015` + cylinders.4.67.6.33., data = train)

summary(model_13)

vif(model_13)

## Model Evaluation and Testing

## now we predict using test data set

Predict_1 <- predict(model_13,test)

test$test_MPG <- Predict_1

cor(test$MPG,test$test_MPG)

cor(test$MPG,test$test_MPG)^2

##  0.8363546

## Model acceptance or Rejection :::  Model can be Accepted

## R-squared (correlation between actual and predicted values of test dataset) for 
## the model has good predictive ability (greater than 80%) because : 


## The model does not contain more than 5 variables.
## The model is highly predictive in nature i.e it shows 83% (R squared) of accuracy.
## The model gives high accuracy (test R-squared = 83% ) when tested it on the test dataset.

