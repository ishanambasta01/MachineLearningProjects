#Ishan Ambasta IIIT-Bangalore
library(dplyr)
library(tidyr)
library(MASS)
library(car) # for VIF values
library(ggplot2)

setwd("C:/Users/ADMIN/Downloads")

cars<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)
str(cars)
colnames(cars)<-tolower(colnames(cars))
#Finding NA"s in the data frame
sapply(colnames(cars),function(x) length(which(is.na(cars[,x]))))

#There are 2 categories under the carname column
cars<-separate(cars,carname,c("company","modelname"),sep = "[[:blank:]]",extra = "merge")

#Checking for the mix of titles in the column company
levels(as.factor(cars$company))


#Rectifying title names
cars$company[cars$company=="maxda"] <- "mazda"
cars$company[cars$company=="porcshce"] <- "porsche"
cars$company[cars$company=="toyouta"] <- "toyota"
cars$company[cars$company=="vokswagen"| cars$company=="vw"] <- "volkswagen"
cars$company[cars$company=="Nissan"] <- "nissan"

#setting function to reduce outliers , because outliers effect
# predictions
#http://r-statistics.co/Outlier-Treatment-With-R.html#
outliers_fixed<- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
}

cars$wheelbase <- outliers_fixed(cars$wheelbase)
cars$carlength <- outliers_fixed(cars$carlength)
cars$carwidth <- outliers_fixed(cars$carwidth)
cars$enginesize <- outliers_fixed(cars$enginesize)
cars$stroke <- outliers_fixed(cars$stroke)
cars$compressionratio <- outliers_fixed(cars$compressionratio)
cars$horsepower <- outliers_fixed(cars$horsepower)
cars$peakrpm <- outliers_fixed(cars$peakrpm)
cars$citympg <- outliers_fixed(cars$citympg)
cars$highwaympg <- outliers_fixed(cars$highwaympg) 


#assigning numeric value to number of doors
table(cars$doornumber)
cars$doornumber<-ifelse(cars$doornumber=="two",0,1)
class(cars$doornumber) #should be numeric

sapply(colnames(cars),function(x) length(levels(as.factor(cars[,x]))))

#removing columns with only 1 factor
cars[ ,c("highwaympg","enginesize","stroke","citympg","peakrpm","compressionratio","horsepower","fuelsystem","carlength","carwidth","wheelbase")] <- list(NULL)
cars<-cars[,-(which(colnames(cars)=="modelname"))]
#giving numeric values to categorical variables
levels(as.factor(cars$fueltype))
table(cars$fueltype)
cars$fueltype<-ifelse(cars$fueltype=="gas",1,0)

levels(as.factor(cars$aspiration))
table(cars$aspiration)
cars$aspiration<-ifelse(cars$aspiration=="std",1,0)

levels(as.factor(cars$enginelocation))
cars$enginelocation<-ifelse(cars$enginelocation=="front",1,0)

#reducing number of factors by allocating variables with less frequency in another category
sort(table(cars$enginetype))
cars$enginetype <- ifelse(cars$enginetype %in% c("dohcv","rotor"),"some", cars$enginetype)

sort(table(cars$carbody)) # convertible and hardtop are few in numbers
cars$carbody <- ifelse(cars$carbody %in% c("convertible","hardtop"),"some", cars$carbody)

#saving cars in another variable
cars1<-cars
#########################################Creating dummies#########################################################
dummyofcompany <- model.matrix(~company, data=cars1)
dummyofcompany <- dummyofcompany[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=="company")],dummyofcompany)
View(cars1)


dummyofenginetype <- model.matrix(~enginetype, data=cars1)
dummyofenginetype<-dummyofenginetype[,-1]
cars1<-cbind(cars1[,-which(colnames(cars1)=="enginetype")],dummyofenginetype)

dummyofcarbody <- model.matrix(~carbody, data=cars1)
dummyofcarbody<-dummyofcarbody[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=="carbody")],dummyofcarbody)

dummyofdrivewheel <- model.matrix(~drivewheel, data=cars1)
dummyofdrivewheel<-dummyofdrivewheel[,-1]
cars1<-cbind(cars1[,-which(colnames(cars1)=="drivewheel")],dummyofdrivewheel)

dummyofcylindernumber<-model.matrix(~cylindernumber,data=cars1)
dummyofcylindernumber<-dummyofcylindernumber[,-1]
cars1<-cbind(cars1[,-which(colnames(cars1)=="cylindernumber")],dummyofcylindernumber)

saveoriginal<- cor(cars1)
write.csv(saveoriginal, 'saveoriginal.csv')

#######################Data Modelling########################################################

#setting seed to re-use the random values
set.seed(123)

# sample size
training <- sample(1:nrow(cars1), 0.7 * nrow(cars1))
# creating training and testing data sets
train_unique <- cars1$car_id[training]
test_unique <- cars1$car_id[-training]
cars1 <- cars1[,-which(colnames(cars1)=='car_id')]

train_set<- cars1[training,]
test_set <- cars1[-training,]
# model building: model1 - consisting of all variables

model <- lm(price ~ ., data = train_set)
summary(model)
# R-squared = 0.9461
#Multiple R-squared =0.9624



## Using StepAIC   #
step <- stepAIC(model, direction = 'both')
step

####Model Testing ########
View(train_set)

model1 <- lm(formula = price ~ symboling+fueltype+aspiration+doornumber+enginelocation
             +carheight+curbweight+boreratio+companyaudi+companybmw+companybuick+companychevrolet
             +companydodge+companyhonda+companyisuzu+companyjaguar+companymazda+companymercury+
               companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
               companysaab+companysubaru+companytoyota+companyvolkswagen+companyvolvo+enginetypel+enginetypeohc+
               enginetypeohcf+enginetypeohcv+enginetypesome+carbodysedan+carbodysome+carbodywagon+drivewheelfwd+
               drivewheelrwd+cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumberthree+
               cylindernumbertwelve+cylindernumbertwo,data = train_set)
summary(model1) #0.9461
#Removing enginetypeohcf, p-value= NA and Cylindernumberthree, p-value =NA 




model2<-lm(formula = price ~ symboling+fueltype+aspiration+doornumber+enginelocation
           +carheight+curbweight+boreratio+companyaudi+companybmw+companybuick+companychevrolet
           +companydodge+companyhonda+companyisuzu+companyjaguar+companymazda+companymercury+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysaab+companysubaru+companytoyota+companyvolkswagen+companyvolvo+enginetypel+enginetypeohc+
             enginetypeohcv+enginetypesome+carbodysedan+carbodysome+carbodywagon+drivewheelfwd+
             drivewheelrwd+cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve+cylindernumbertwo,data = train_set)
summary(model2)
#cylindernumbertwo , pvalue >0.05 and companyvolvo , pvalue >0.05

model3<-lm(formula = price ~ symboling+fueltype+aspiration+doornumber+enginelocation
           +carheight+curbweight+boreratio+companyaudi+companybmw+companybuick+companychevrolet
           +companydodge+companyhonda+companyisuzu+companyjaguar+companymazda+companymercury+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysaab+companysubaru+companytoyota+companyvolkswagen+enginetypel+enginetypeohc+
             enginetypeohcv+enginetypesome+carbodysedan+carbodysome+carbodywagon+drivewheelfwd+
             drivewheelrwd+cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model3)#0.9471
#Removing Symboling , p-value >0.05 and doornumber , p-value >0.05

model4<-lm(formula = price ~ fueltype+aspiration+enginelocation
           +carheight+curbweight+boreratio+companyaudi+companybmw+companybuick+companychevrolet
           +companydodge+companyhonda+companyisuzu+companyjaguar+companymazda+companymercury+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysaab+companysubaru+companytoyota+companyvolkswagen+enginetypel+enginetypeohc+
             enginetypeohcv+enginetypesome+carbodysedan+carbodysome+carbodywagon+drivewheelfwd+
             drivewheelrwd+cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model4) #0.9481
#Removing companymercury , p-value>0.05 and enginetypeohc, p-value > 0.05

model5<-lm(formula = price ~ fueltype+aspiration+enginelocation
           +carheight+curbweight+boreratio+companyaudi+companybmw+companybuick+companychevrolet
           +companydodge+companyhonda+companyisuzu+companyjaguar+companymazda+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysaab+companysubaru+companytoyota+companyvolkswagen+enginetypel+
             enginetypeohcv+enginetypesome+carbodysedan+carbodysome+carbodywagon+drivewheelfwd+
             drivewheelrwd+cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model5) #0.9486
#Removing boreratio , p-value >0.05 and carbodysedan ,p-value >0.05

model5<-lm(formula = price ~ fueltype+aspiration+enginelocation
           +carheight+curbweight+companybmw+companybuick+companychevrolet
           +companydodge+companyhonda+companyisuzu+companyjaguar+companymazda+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysaab+companysubaru+companytoyota+companyvolkswagen+enginetypel+
             enginetypeohcv+enginetypesome+carbodysome+carbodywagon+drivewheelfwd+
             drivewheelrwd+cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model5)#0.9494
#carheight , p-value >carheight and companyaudi ,p-value >0.05

model6<-lm(formula = price ~ fueltype+aspiration+enginelocation
           +curbweight+companybmw+companybuick+companychevrolet
           +companydodge+companyhonda+companyisuzu+companyjaguar+companymazda+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysaab+companysubaru+companytoyota+companyvolkswagen+enginetypel+
             enginetypeohcv+enginetypesome+carbodysome+carbodywagon+drivewheelfwd+
             drivewheelrwd+cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model6) #0.9502
# removing drivewheelrwd ,p-value>0.05 and companysaab , p-value >0.05

model7<-lm(formula = price ~ fueltype+aspiration+enginelocation
           +curbweight+companybmw+companybuick+companychevrolet
           +companydodge+companyhonda+companyisuzu+companyjaguar+companymazda+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysubaru+companytoyota+companyvolkswagen+enginetypel+
             enginetypeohcv+enginetypesome+carbodysome+carbodywagon+drivewheelfwd+
             cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model7)#0.9493
#removing companychevrolet,p-value > 0.05 and companyisuzu , p-value>0.05

model8<-lm(formula = price ~ fueltype+aspiration+enginelocation
           +curbweight+companybmw+companybuick
           +companydodge+companyhonda+companyjaguar+companymazda+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysubaru+companytoyota+companyvolkswagen+enginetypel+
             enginetypeohcv+enginetypesome+carbodysome+carbodywagon+drivewheelfwd+
             cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model8) #0.9486
# removing fueltype ,p-value>0.05 and enginetypeohcv,p-value>0.05

model9<-lm(formula = price ~ aspiration+enginelocation
           +curbweight+companybmw+companybuick
           +companydodge+companyhonda+companyjaguar+companymazda+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysubaru+companytoyota+companyvolkswagen+enginetypel+
             enginetypesome+carbodysome+carbodywagon+drivewheelfwd+
             cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model9) #0.9478
# removing companyhonda,p-value>0.05 and drivewheelfwd,p-value >0.05

model10<-lm(formula = price ~ aspiration+enginelocation
           +curbweight+companybmw+companybuick
           +companydodge+companyjaguar+companymazda+
             companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+companyrenault+
             companysubaru+companytoyota+companyvolkswagen+enginetypel+
             enginetypesome+carbodysome+carbodywagon+
             cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model10) #0.9471
# removing companyvolkwagen , p-value>0.05 and companyrenault, p-value>0.05

model11<-lm(formula = price ~ aspiration+enginelocation
            +curbweight+companybmw+companybuick
            +companydodge+companyjaguar+companymazda+
              companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+
              companysubaru+companytoyota+enginetypel+
              enginetypesome+carbodysome+carbodywagon+
              cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model11) #0.9463
# removing companymazda,p-value>0.05 and carbodysome,p-value>0.05

model12<-lm(formula = price ~ aspiration+enginelocation
            +curbweight+companybmw+companybuick
            +companydodge+companyjaguar+
              companymitsubishi+companynissan+companypeugeot+companyplymouth+companyporsche+
              companysubaru+companytoyota+enginetypel+
              enginetypesome+carbodywagon+
              cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model12) #0.9446
# removing companynissan,p-value>0.05 and companydodge,p-value>0.05

model13<-lm(formula = price ~ aspiration+enginelocation
            +curbweight+companybmw+companybuick
            +companyjaguar+
              companymitsubishi+companypeugeot+companyplymouth+companyporsche+
              companysubaru+companytoyota+enginetypel+
              enginetypesome+carbodywagon+
              cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model13) #0.9438
# removing companyplymouth ,p-value>0.05 and companybuick , p-value >0.05

model14<-lm(formula = price ~ aspiration+enginelocation
            +curbweight+ companybmw +companyjaguar+
              companymitsubishi+companypeugeot+companyporsche+
              companysubaru+ companytoyota+enginetypel+
              enginetypesome+carbodywagon+
              cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model14) #0.9421
# removing companysubaru ,p-value >0.05 and aspiration , p-value >0.05

model15<-lm(formula = price ~ enginelocation
            +curbweight+ companybmw +companyjaguar+
              companymitsubishi+companypeugeot+companyporsche+
              companytoyota+enginetypel+
              enginetypesome+carbodywagon+
              cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model15) #0.9402
# removing companymitsubishi ,p-value>0.01

model16<-lm(formula = price ~ enginelocation
            +curbweight+ companybmw +companyjaguar+
              companypeugeot+companyporsche+
              companytoyota+enginetypel+
              enginetypesome+carbodywagon+
              cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model16) #0.9386
# removing companypeugeot,vif =19.02390

model17<-lm(formula = price ~ enginelocation
            +curbweight+ companybmw +companyjaguar+ companyporsche+companytoyota+enginetypel+ enginetypesome+carbodywagon+
              cylindernumberfive+cylindernumberfour+cylindernumbersix+cylindernumbertwelve,data = train_set)
summary(model17) #0.9332

#lm(formula = price ~ enginelocation + curbweight + companybmw + 
#     companyjaguar + companyporsche + companytoyota + enginetypel + 
#     enginetypesome + carbodywagon + cylindernumberfive + cylindernumberfour + 
#     cylindernumbersix + cylindernumbertwelve, data = train_set)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-6378.3 -1355.9    70.1  1137.2  7069.6 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           1.043e+04  2.594e+03   4.020 9.82e-05 ***
#  enginelocation       -1.235e+04  2.159e+03  -5.720 7.06e-08 ***
#  curbweight            1.083e+01  4.877e-01  22.216  < 2e-16 ***
#  companybmw            7.545e+03  9.789e+02   7.708 3.01e-12 ***
#  companyjaguar         4.934e+03  1.713e+03   2.880 0.004661 ** 
#  companyporsche        7.247e+03  1.637e+03   4.426 2.02e-05 ***
#  companytoyota        -1.454e+03  4.947e+02  -2.940 0.003896 ** 
#  enginetypel          -4.305e+03  7.697e+02  -5.593 1.28e-07 ***
#  enginetypesome       -1.055e+04  1.582e+03  -6.671 6.77e-10 ***
# carbodywagon         -1.916e+03  5.571e+02  -3.439 0.000786 ***
#  cylindernumberfive   -1.042e+04  1.342e+03  -7.764 2.22e-12 ***
#  cylindernumberfour   -1.325e+04  1.147e+03 -11.553  < 2e-16 ***
#  cylindernumbersix    -1.317e+04  1.256e+03 -10.487  < 2e-16 ***
#  cylindernumbertwelve -9.811e+03  2.880e+03  -3.407 0.000878 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2115 on 129 degrees of freedom
#Multiple R-squared:  0.9393,	Adjusted R-squared:  0.9332 
#F-statistic: 153.6 on 13 and 129 DF,  p-value: < 2.2e-16


#Prediction
predicted_price <- predict(model17, test_set[,-which(colnames(test_set)=='price')])
test_set$test_price <- predicted_price

r <- cor(test_set$price,test_set$test_price)
rsquared <- cor(test_set$price,test_set$test_price)^2
r
rsquared

# Actual vs Predicted Graph
ggplot(test_set, aes(test_set$price,test_set$test_price)) + geom_point() + stat_smooth(method = lm)

#Factors that help predict the price are :
#  enginelocation
#  curbweight          
#  companybmw         
#  companyjaguar       
#  companyporsche     
#  companytoyota
#  enginetypel        
#  enginetypesome     
#  carbodywagon
#  cylindernumberfive 
#  cylindernumberfour   
#  cylindernumbersix
#  cylindernumbertwelve


#Ishan Ambasta IIIT-Bangalore