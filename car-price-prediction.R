## Import Data

auto<- read.csv('Automobile_data.csv',header = TRUE,stringsAsFactors = FALSE)
head(auto)
str(auto)
summary(auto)
View(auto)
## Loading Libraries
library(ggplot2)
library(dplyr)

## Data Pre-Processing and Exploratory Data Analysis
## Lets take a look at each of the variables

# symboling variable

unique(auto$symboling)

ggplot(data = auto,aes(x = symboling))+
  geom_bar()+
  xlab('Symboling')+
  ylab('Vehicle Count')+
  ggtitle('Symboling vs Vehicle Count')

# data distribution for symboling variable is fairly normal

##NORMALIZED LOSSES VARIABLE
# unique(auto$normalized.losses)
# 
# ggplot(data = auto,aes(x = factor(normalized.losses)))+
#   geom_histogram()+
#   xlab('Normalized Losses')+
#   ylab('Count')+
#   ggtitle("Normalized Losses vs Count")

## MAKE VARIABLE

unique(auto$make)

ggplot(data = auto,aes(x = factor(make)))+
  geom_bar()+
  xlab('Make')+
  ylab('Vehicle Count')+
  ggtitle('Make vs Vehicle Count')

# We can see that the ditribution is not normal but multi-modal, most bought car is toyota

## FUEL TYPE VARIABLE
unique(auto$fuel.type)

ggplot(data = auto,aes(x = factor(fuel.type)))+
  geom_bar()+
  xlab('Fuel Type')+
  ylab('Vehicle Count')+
  ggtitle('Fuel Type vs Vehicle Count')

## We can see that high majority of vehicles use gas

## ASPIRATION VARIABLE

unique(auto$aspiration)

ggplot(data = auto,aes(x = factor(aspiration)))+
  geom_bar()+
  xlab('Aspiration')+
  ylab('Vehicle Count')+
  ggtitle('Aspiration vs Vehicle Count')

## We can see that most of the vehicles are std and not turbocharged


## NUM OF DOORS VARIABLE

ggplot(data = auto,aes(x = factor(num.of.doors)))+
  geom_bar()+
  xlab('Number of Doors')+
  ylab('Vehicle Count')+
  ggtitle('Number of Doors vs Vehicle Count')

## There is not a very huge difference between the number of four and two- door vehicles
## Also there are some missing values

## BODY STYLE VARIABLE

ggplot(data = auto,aes(x = factor(body.style)))+
  geom_bar()+
  xlab('BOdy Style')+
  ylab('Vehicle Count')+
  ggtitle('Body Style vs Vehicle Count')

## We can see that majority of the vehicles are sedans followed by hatchback


## TYPE OF DRIVE

unique(auto$drive.wheels)

ggplot(data = auto,aes(x = factor(drive.wheels)))+
  geom_bar()+
  xlab('Drive Type')+
  ylab('Vehicle Count')+
  ggtitle('Drive Type vs Vehicle Count')

## Most of the vehicles are front wheel and rear wheel drives, minimal number of vehicles are 4wd

## LOCATION OF ENGINE
unique(auto$engine.location)

ggplot(data = auto,aes(x = factor(engine.location)))+
  geom_bar()+
  xlab('Engine Location')+
  ylab('Vehicle Count')+
  ggtitle('Engine Location vs Vehicle Count')

## The number of rear engined vehicles are very miniscule, rest are front engined

## WHEEL BASE VARIABLE

ggplot(data = auto,aes(x = factor(wheel.base)))+
  geom_histogram(stat = "count")+
  xlab('Wheel Base')+
  ylab('Vehicle Count')+
  ggtitle('Wheel Base vs Vehicle Count')

# The graph shows the number of vehicles at the various wheel bases

## LENGTH OF VEHICLE

ggplot(data = auto,aes(x = factor(length)))+
  geom_histogram(stat = "count")+
  xlab('Length')+
  ylab('Vehicle Count')+
  ggtitle('Length vs Vehicle Count')

# The graph shows the number of vehicles and the vehicle length

## WIDTH OF VEHICLE

ggplot(data = auto,aes(x = factor(width)))+
  geom_histogram(stat = "count")+
  xlab('Width')+
  ylab('Vehicle Count')+
  ggtitle('Width vs Vehicle Count')

# The graph shows the number of vehicles and the vehicle width


## HEIGHT OF VEHICLE

ggplot(data = auto,aes(x = factor(height)))+
  geom_histogram(stat = "count")+
  xlab('Height')+
  ylab('Vehicle Count')+
  ggtitle('Height vs Vehicle Count')

# The graph shows the number of vehicles and the vehicle height


## CURB WEIGHT OF VEHICLE

ggplot(data = auto,aes(x = factor(curb.weight)))+
  geom_histogram(stat = "count",binwidth = 20)+
  xlab('Curb Weight')+
  ylab('Vehicle Count')+
  ggtitle('Curb Weight vs Vehicle Count')


## TYPES OF CAMSHAFT VARIABLE

ggplot(data = auto,aes(x = factor(engine.type)))+
  geom_bar()+
  xlab('Camshaft Type')+
  ylab('Vehicle Count')+
  ggtitle('Camshaft Type vs Vehicle Count')

# Majority of the vehicles are having over head camshaft

## NUMBER OF CYLINDERS VARIABLE

ggplot(data = auto,aes(x = factor(num.of.cylinders)))+
  geom_bar()+
  xlab('Number of Cylinders')+
  ylab('Vehicle Count')+
  ggtitle('Number of Cylinders vs Vehicle Count')

# Majority of the vehicles are having 4 cylinders



## ENGINE SIZE VARIABLE

ggplot(data = auto,aes(x = factor(engine.size)))+
  geom_bar(stat = "count")+
  xlab('Engine Size')+
  ylab('Vehicle Count')+
  ggtitle('Engine Size vs Vehicle Count')


## TYPE OF FUEL SYSTEM

ggplot(data = auto,aes(x = factor(fuel.system)))+
  geom_bar()+
  xlab('Fuel System')+
  ylab('Vehicle Count')+
  ggtitle('Fuel System vs Vehicle Count')

# The graph shows the various  types of fuel systems


## COMPRESSION RATIO

ggplot(data = auto,aes(x = factor(compression.ratio)))+
  geom_bar()+
  xlab('Compression Ratio')+
  ylab('Vehicle Count')+
  ggtitle('Compression Ratio vs Vehicle Count')

## The most frequent compression ratio for vehicles is 9


##  HORSEPOWER
ggplot(data = auto,aes(x = horsepower))+
  geom_bar()+
  xlab('HP')+
  ylab('Count')+
  ggtitle('Horsepower vs Vehicle Count')

## 68 is the most frequent horsepower


## PEAK RPM

ggplot(data = auto,aes(x = factor(peak.rpm)))+
  geom_bar()+
  xlab('Peak RPM')+
  ylab('Vehicle Count')+
  ggtitle('PEAK RPM vs Vehicle Count')

# The figure shows the peak RPM of all the vehicles

## CITY MPG
ggplot(data = auto,aes(x = factor(city.mpg)))+
  geom_bar()+
  xlab('City MPG')+
  ylab('Vehicle Count')+
  ggtitle('City MPG vs Vehicle Count')

#


## HIGHWAY MPG

ggplot(data = auto,aes(x = factor(highway.mpg)))+
  geom_bar()+
  xlab('Highway MPG')+
  ylab('Vehicle Count')+
  ggtitle('Highway MPG vs Vehicle Count')


## Converting the numeric data available in string format to numeric data type

auto$normalized.losses<- as.numeric(auto$normalized.losses,na.rm = TRUE)
auto$bore<- as.numeric(auto$bore,na.rm = TRUE)
auto$stroke<- as.numeric(auto$stroke,na.rm = TRUE)
auto$horsepower<- as.numeric(auto$horsepower,na.rm = TRUE)
auto$peak.rpm<- as.numeric(auto$peak.rpm,na.rm = TRUE)
auto$price<- as.numeric(auto$price,na.rm = TRUE)

# Check if the data has been converted
str(auto)

## Converting all the numeric data to numeric datatype
# symboling
# curb.weight
# engine.size
# city.mpg
# highway.mpg

auto$symboling<- as.numeric(auto$symboling,na.rm = TRUE)
auto$curb.weight<- as.numeric(auto$curb.weight,na.rm = TRUE)
auto$engine.size<- as.numeric(auto$engine.size,na.rm = TRUE)
auto$city.mpg<- as.numeric(auto$city.mpg,na.rm = TRUE)
auto$highway.mpg<- as.numeric(auto$highway.mpg,na.rm = TRUE)

# Check if the data has been converted
str(auto)


## Converting all the String data to factor datatype
# make
# fuel.type
# aspiration
# num.of.doors
# body.style
# drive.wheels
# engine.location
# engine.type
# num.of.cylinders
# fuel.system

#Make

# unique(auto$make)
# 
# auto$make = factor(auto$Country,
#                       levels = c('France', 'Spain', 'Germany'),
#                       labels = c(1, 2, 3))

# Fuel Type

unique(auto$fuel.type)
auto$fuel.type = factor(auto$fuel.type,
                        levels = c('gas','diesel'),
                        labels = c(0,1))

# Aspiration

unique(auto$aspiration)

auto$aspiration = factor(auto$aspiration,
                         levels = c('std','turbo'),
                         labels = c(1,2))

# Number of Doors

# unique(auto$num.of.doors)

# Body Style

unique(auto$body.style)

auto$body.style = factor(auto$body.style,
                         levels = c('convertible','hatchback','sedan','wagon','hardtop'),
                         labels = c(0,1,2,3,4))


# Drive Wheels

unique(auto$drive.wheels)

auto$drive.wheels = factor(auto$drive.wheels,
                           levels = c('rwd','fwd','4wd'),
                           labels = c(1,2,3))

# Engine Location

unique(auto$engine.location)

auto$engine.location = factor(auto$engine.location,
                              levels = c('front','rear'),
                              labels = c(0,1))


# engine.type

unique(auto$engine.type)

auto$engine.type = factor(auto$engine.type,
                          levels = c('dohc','ohcv','ohc','l','rotor','ohcf','dohcv'),
                          labels = c(0,1,2,3,4,5,6))

# num.of.cylinders

unique(auto$num.of.cylinders)

auto$num.of.cylinders = factor(auto$num.of.cylinders,
                               levels = c('two','three','four','five','six','eight','twelve'),
                               labels = c(1,2,3,4,5,6,7))

# fuel.system

unique(auto$fuel.system)

auto$fuel.system = factor(auto$fuel.system,
                               levels = c('mpfi','2bbl','mfi','1bbl','spfi','4bbl','idi','spdi'),
                               labels = c(0,1,2,3,4,5,6,7))

# Structure of Data

str(auto)

# summary of data
summary(auto)

# NA values are miniscule, we can omit na's
auto_1<- auto[,c(-2,-3,-6)]
View(auto_1)

auto_2<- na.omit(auto_1)

str(auto_2)

# Checking whether we have any multi-collinearity in the data

# model1<- lm(price~.,data = auto_2)
# summary(model1)
# 
# library(faraway)
# vif(model1)


# Splitting the dataset into the Training set and Test set

install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(auto_2$price, SplitRatio = 0.8)
training_set = subset(auto_2, split == TRUE)
test_set = subset(auto_2, split == FALSE)

# Building a model

model1<- lm(price~.,data = training_set)
model1
summary(model1)

# Eliminating the factors which do not contribute to the model

# symboling,fuel.type1,aspiration2,drive.wheels2,engine.location1,wheel.base,length,height,fuel.system1
# compression.ratio,horsepower,city.mpg,highway.mpg

#body.style1,width,curb.weight,engine.type1,num.of.cylinders,engine.size,bore,stroke,peak.rpm

model2<- lm(price~ body.style + width + curb.weight + engine.type + num.of.cylinders + engine.size + bore + stroke + peak.rpm + fuel.system
            ,data = training_set)
summary(model2)

model3<- lm(price~ body.style + width + curb.weight + engine.type + num.of.cylinders + engine.size + bore + stroke + peak.rpm 
            + fuel.system + horsepower
            ,data = training_set)
summary(model3)


model4<- lm(price~ body.style + width + curb.weight + engine.type + num.of.cylinders + engine.size + bore + stroke + peak.rpm 
            + fuel.system + horsepower + compression.ratio
              ,data = training_set)
summary(model4)

## model3 has the highest accuracy, lets predict the prices using model3

y_pred<- predict(model3,newdata = test_set)
y_pred
View(test_set)

y_pred<- predict(model1,newdata = test_set)
y_pred
View(test_set)






















