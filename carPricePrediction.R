install.packages('MASS')
install.packages('car')

library(MASS)
library(car)


carmodel <- read.csv("Carprice_Assignment.csv")

str(carmodel)


View(carmodel)

################ Data Cleaning ##################


carmodel$companyname <- gsub("\\ .*", "", carmodel$CarName)

unique(carmodel$companyname)


carmodel$companyname <- gsub("vokswagen","volkswagen",carmodel$companyname)
carmodel$companyname <- gsub("maxda","mazda",carmodel$companyname)
carmodel$companyname <- gsub("Nissan","nissan",carmodel$companyname)
carmodel$companyname <- gsub("porcshce","porsche",carmodel$companyname)
carmodel$companyname <- gsub("vw","volkswagen",carmodel$companyname)
carmodel$companyname <- gsub("toyouta","toyota",carmodel$companyname)


unique(carmodel$companyname)

####### checking for NA's ##############

sum(is.na(carmodel))  # No NA Values


###### Checking for duplicates ############
sum(duplicated(carmodel))   # No Duplicate Values


########
unique(carmodel$symboling)
unique(carmodel$fueltype)
unique(carmodel$aspiration)
unique(carmodel$doornumber)
unique(carmodel$carbody)
unique(carmodel$drivewheel)



carmodel$companyname <- toupper(carmodel$companyname)


carmodel$symboling <- as.factor(carmodel$symboling)
carmodel$CarName <- as.factor(carmodel$CarName)
carmodel$carbody <- as.factor(carmodel$carbody)
carmodel$drivewheel <- as.factor(carmodel$drivewheel)
carmodel$enginelocation <- as.factor(carmodel$enginelocation)
carmodel$cylindernumber <- as.factor(carmodel$cylindernumber)
carmodel$doornumber <- as.factor(carmodel$doornumber)
carmodel$fuelsystem <- as.factor(carmodel$fuelsystem)
carmodel$aspiration <- as.factor(carmodel$aspiration)
carmodel$enginetype <- as.factor(carmodel$enginetype)
carmodel$fueltype <- as.factor(carmodel$fueltype)
#carmodel$fuelsystem <- as.factor(carmodel$fuelsystem)


str(carmodel)

##############Data Modelling#############


######### Converting factor variables into 2 numerical values ###########

levels(carmodel$enginelocation) <- c(1,0)
carmodel$enginelocation <- as.numeric(levels(carmodel$enginelocation))[carmodel$enginelocation]

levels(carmodel$doornumber) <- c(1,0)
carmodel$doornumber <- as.numeric(levels(carmodel$doornumber))[carmodel$doornumber]


levels(carmodel$fueltype) <- c(1,0)
carmodel$fueltype <- as.numeric(levels(carmodel$fueltype))[carmodel$fueltype]

levels(carmodel$aspiration) <-c(1,0)
carmodel$aspiration <- as.numeric(levels(carmodel$aspiration))[carmodel$aspiration]


########## Creating Dummy Variables ##################

dummy_drivewheel <- as.data.frame(model.matrix(~drivewheel,data = carmodel))
dummy_drivewheel <- dummy_drivewheel[,-1]
carmodel <- cbind(carmodel[,-which(colnames(carmodel)=='drivewheel')],dummy_drivewheel)

dummy_carbody <- as.data.frame(model.matrix(~carbody,data = carmodel))
dummy_carbody <- dummy_carbody[,-1]
carmodel <- cbind(carmodel[,-which(colnames(carmodel)=='carbody')],dummy_carbody)


dummy_enginetype <-  as.data.frame(model.matrix(~enginetype,data = carmodel))
dummy_enginetype <- dummy_enginetype[,-1]
carmodel <- cbind(carmodel[,-which(colnames(carmodel)=='enginetype')],dummy_enginetype)


dummy_cylindernumber <-  as.data.frame(model.matrix(~cylindernumber,data = carmodel))
dummy_cylindernumber <- dummy_cylindernumber[,-1]
carmodel <- cbind(carmodel[,-which(colnames(carmodel)=='cylindernumber')],dummy_cylindernumber)


dummy_fuelsystem <-  as.data.frame(model.matrix(~fuelsystem,data = carmodel))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
carmodel <- cbind(carmodel[,-which(colnames(carmodel)=='fuelsystem')],dummy_fuelsystem)

dummy_companyname <-  as.data.frame(model.matrix(~companyname,data = carmodel))
dummy_companyname <- dummy_companyname[,-1]
carmodel <- cbind(carmodel[,-which(colnames(carmodel)=='companyname')],dummy_companyname)

dummy_symboling <-  as.data.frame(model.matrix(~symboling,data = carmodel))
dummy_symboling <- dummy_symboling[,-1]
carmodel <- cbind(carmodel[,-which(colnames(carmodel)=='symboling')],dummy_symboling)


carmodel <- carmodel[,-which(colnames(carmodel)=='CarName')]
str(carmodel)

sum(is.na(carmodel))

############ checking for Outliers ##########################

quantile(carmodel$carlength,seq(0,1,0.01))
quantile(carmodel$carwidth,seq(0,1,0.01))
quantile(carmodel$carheight,seq(0,1,0.01))
quantile(carmodel$horsepower,seq(0,1,0.01))
quantile(carmodel$enginesize,seq(0,1,0.01))


####### Not much jump in the outlier values Hence we can proceed furthur

###### Setting seed and converting our model in test and train

set.seed(12345)
trainindices <- sample(1:nrow(carmodel),0.7*nrow(carmodel))
train = carmodel[trainindices,]
test = carmodel[-trainindices,]


model_1 <- lm(price~.,data = train)
summary(model_1)

step <- stepAIC(model_1,direction = "both")
summary(step)
vif(step)
  
#### Removing Fuel System MPFI as insignificant p value
model_2 <-lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
               carwidth + carheight + curbweight + enginesize + boreratio + 
               stroke + compressionratio + peakrpm + citympg + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
               enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumberfive + 
               cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
               companynameCHEVROLET + companynameDODGE + companynameHONDA + 
               companynameISUZU + companynameMAZDA + companynameMERCURY + 
               companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
               companynamePORSCHE + companynameRENAULT + companynameSAAB + 
               companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO + 
               `symboling-1` + symboling0 + symboling1 + symboling2 + symboling3, 
             data = train)
#step2 <- stepAIC(model_2,direction = "both")
summary(model_2)
vif(model_2)

## Remove symboling3 as high p value
model_3 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                companynameISUZU + companynameMAZDA + companynameMERCURY + 
                companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                companynamePORSCHE + companynameRENAULT + companynameSAAB + 
                companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO + 
                `symboling-1` + symboling0 + symboling1 + symboling2, 
              data = train)


summary(model_3)
vif(model_3)

######### removing `symboling-1` variable

model_4 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                companynameISUZU + companynameMAZDA + companynameMERCURY + 
                companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                companynamePORSCHE + companynameRENAULT + companynameSAAB + 
                companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO + 
                symboling0 + symboling1 + symboling2, 
              data = train)

summary(model_4)
vif(model_4)

### Removing symboling2
model_5 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                companynameISUZU + companynameMAZDA + companynameMERCURY + 
                companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                companynamePORSCHE + companynameRENAULT + companynameSAAB + 
                companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO + 
                symboling0 + symboling1, 
              data = train)
summary(model_5)
vif(model_5)

### Removing stroke
model_6 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                compressionratio + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                companynameISUZU + companynameMAZDA + companynameMERCURY + 
                companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                companynamePORSCHE + companynameRENAULT + companynameSAAB + 
                companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO + 
                symboling0 + symboling1, 
              data = train)
summary(model_6)
vif(model_6)

### Removing enginetypeohcv
model_7 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                compressionratio + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                enginetypeohcf  + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                companynameISUZU + companynameMAZDA + companynameMERCURY + 
                companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                companynamePORSCHE + companynameRENAULT + companynameSAAB + 
                companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO + 
                symboling0 + symboling1, 
              data = train)
summary(model_7)
vif(model_7)

### Removing symboling0
model_8 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                compressionratio + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                enginetypeohcf  + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                companynameISUZU + companynameMAZDA + companynameMERCURY + 
                companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                companynamePORSCHE + companynameRENAULT + companynameSAAB + 
                companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO + 
                symboling1, 
              data = train)
summary(model_8)
vif(model_8)

### Removing carheight
model_9 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + curbweight + enginesize + boreratio + 
                compressionratio + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                enginetypeohcf  + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                companynameISUZU + companynameMAZDA + companynameMERCURY + 
                companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                companynamePORSCHE + companynameRENAULT + companynameSAAB + 
                companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO + 
                symboling1, 
              data = train)
summary(model_9)
vif(model_9)

### Removing symboling1
model_10 <-lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + curbweight + enginesize + boreratio + 
                compressionratio + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                enginetypeohcf  + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                companynameISUZU + companynameMAZDA + companynameMERCURY + 
                companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                companynamePORSCHE + companynameRENAULT + companynameSAAB + 
                companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
              data = train)
summary(model_10)
vif(model_10)

### Removing companynamePORSCHE
model_11 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + boreratio + 
                 compressionratio + peakrpm + citympg + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberfive + 
                 cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                 companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                 companynameISUZU + companynameMAZDA + companynameMERCURY + 
                 companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_11)
vif(model_11)

### Removing cylindernumberfive
model_12 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + boreratio + 
                 compressionratio + peakrpm + citympg + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                 companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                 companynameISUZU + companynameMAZDA + companynameMERCURY + 
                 companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_12)
vif(model_12)

### Removing curbweight
model_13 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 compressionratio + peakrpm + citympg + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                 companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                 companynameISUZU + companynameMAZDA + companynameMERCURY + 
                 companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_13)
vif(model_13)

### Removing citympg
model_14 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 compressionratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                 companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                 companynameISUZU + companynameMAZDA + companynameMERCURY + 
                 companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_14)
vif(model_14)

### Removing aspiration
model_15 <- lm(formula = price ~ car_ID + fueltype  + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 compressionratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                 companynameCHEVROLET + companynameDODGE + companynameHONDA + 
                 companynameISUZU + companynameMAZDA + companynameMERCURY + 
                 companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_15)
vif(model_15)

### Removing chevrolet
model_16 <-  lm(formula = price ~ car_ID + fueltype  + enginelocation + 
                  carwidth  + enginesize + boreratio + 
                  compressionratio + peakrpm + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                  enginetypeohcf  + enginetyperotor + cylindernumberthree + fuelsystem2bbl  + companynameBMW + 
                  companynameDODGE + companynameHONDA + 
                  companynameISUZU + companynameMAZDA + companynameMERCURY + 
                  companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                  companynameRENAULT + companynameSAAB + 
                  companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
                data = train)
summary(model_16)
vif(model_16)

### Removing fuelsystem2bbl
model_17 <- lm(formula = price ~ car_ID + fueltype  + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 compressionratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameISUZU + companynameMAZDA + companynameMERCURY + 
                 companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_17)
vif(model_17)


### Removing peakrpm
model_18 <- lm(formula = price ~ car_ID + fueltype  + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 compressionratio  + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameISUZU + companynameMAZDA + companynameMERCURY + 
                 companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_18)
vif(model_18)


### Removing companynameISUZU
model_19 <- lm(formula = price ~ car_ID + fueltype  + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 compressionratio  + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameMAZDA + companynameMERCURY + 
                 companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_19)
vif(model_19)

### Removing companynameMERCURY
model_20 <- lm(formula = price ~ car_ID + fueltype  + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 compressionratio  + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_20)
vif(model_20)

#Removing FuelType as VIF = 46

model_21 <- lm(formula = price ~ car_ID  + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 compressionratio  + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_21)
vif(model_21)


#Removing Compression Ratio

model_22 <- lm(formula = price ~ car_ID  + enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_22)
vif(model_22)

#Removing Car ID

model_23 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN + companynameVOLVO, 
               data = train)
summary(model_23)
vif(model_23)

#Removing companynameVOLVO

model_24 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameSAAB + 
                 companynameTOYOTA + companynameVOLKSWAGEN, 
               data = train)
summary(model_24)
vif(model_24)


#Removing companynameSAAB

model_25 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + boreratio + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameTOYOTA + companynameVOLKSWAGEN, 
               data = train)
summary(model_25)
vif(model_25)


#Removing BOREratio

model_26 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE + companynameHONDA + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameTOYOTA + companynameVOLKSWAGEN, 
               data = train)
summary(model_26)
vif(model_26)

#Removing companynameHONDA

model_27 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameDODGE +  
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameTOYOTA + companynameVOLKSWAGEN, 
               data = train)
summary(model_27)
vif(model_27)


#Removing companynameDODGE

model_28 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + companynamePLYMOUTH + 
                 companynameRENAULT + companynameTOYOTA + companynameVOLKSWAGEN, 
               data = train)
summary(model_28)
vif(model_28)


#Removing companynamePLYMOUTH

model_29 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + 
                 companynameRENAULT + companynameTOYOTA + companynameVOLKSWAGEN, 
               data = train)
summary(model_29)
vif(model_29)

#Removing companynameVOLKSWAGEN

model_30 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetypeohcf  + enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + 
                 companynameRENAULT + companynameTOYOTA ,
               data = train)
summary(model_30)
vif(model_30)


#Removing enginetypeohcf

model_31 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameMAZDA + companynameMITSUBISHI + companynameNISSAN + 
                 companynameRENAULT + companynameTOYOTA ,
               data = train)
summary(model_31)
vif(model_31)

#Removing companynameNISSAN

model_32 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameMAZDA + companynameMITSUBISHI + companynameRENAULT + companynameTOYOTA ,
               data = train)
summary(model_32)
vif(model_32)


#Removing companynameMITSUBISHI

model_33 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameMAZDA + companynameRENAULT + companynameTOYOTA ,
               data = train)
summary(model_33)
vif(model_33)

#Removing companynameMAZDA

model_34 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + enginetypel + 
                 enginetyperotor + cylindernumberthree  + companynameBMW + 
                  companynameRENAULT + companynameTOYOTA ,
               data = train)
summary(model_34)
vif(model_34)


#Removing enginetypel

model_35 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameRENAULT + companynameTOYOTA ,
               data = train)
summary(model_35)
vif(model_35)

#Removing companynameTOYOTA

model_36 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + cylindernumberthree  + companynameBMW + 
                 companynameRENAULT ,
               data = train)
summary(model_36)
vif(model_36)


#Removing companynameRENAULT

model_37 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + cylindernumberthree  + companynameBMW,
               data = train)
summary(model_37)
vif(model_37)

#Removing cylindernumberthree

model_38 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor  + companynameBMW,
               data = train)
summary(model_38)
vif(model_38)


#Removing carbodysedan

model_39 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + carbodywagon + 
                 enginetyperotor  + companynameBMW,
               data = train)
summary(model_39)
vif(model_39)


#Removing carbodywagon

model_40 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + carbodyhatchback + 
                 enginetyperotor  + companynameBMW,
               data = train)
summary(model_40)
vif(model_40)



#Removing carbodywagon

model_41 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize + carbodyhardtop + 
                 enginetyperotor  + companynameBMW,
               data = train)
summary(model_41)
vif(model_41)

#Removing carbodyhardtop

model_42 <- lm(formula = price ~ enginelocation + 
                 carwidth  + enginesize  + 
                 enginetyperotor  + companynameBMW,
               data = train)
summary(model_42)
vif(model_42)

############### Finally the VIF and P values looks good with r square = 0.8977 and adjusted R square = 0.894  #############

##### Lets predict the car price based on the final model

predicted_price <- predict(model_42, test[,-which(colnames(test)=='price')])
test$test_price <- predicted_price

#### Lets check the correlation between predicted and original values

r <- cor(test$price,test$test_price)
r_squared <- cor(test$price,test$test_price)^2
r_squared  #### 0.8942

### i.e. we are able to predict 89.4% variance in our model

residualPlot(model_42)


#################### End of Assignment ##################

