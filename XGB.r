setwd("C:/Workspace/")
traindata <- read.csv("traindata.csv", header = T) #Read Train Data
labeldata <- read.csv("label.csv", header = T) #Read Train Data Labels
testdata <- read.csv("testdata.csv", header = T) #Read Test Data
traindata$status_group <- labeldata$status_group #Add Train Data Labels to Actual Data

#Creating a New attribute called 'years in operation(yip)'
tmp <- format(as.Date(traindata$date_recorded, '%Y-%m-%d'),'%Y') 
tmp <- as.numeric(tmp)
traindata$yip <- tmp - traindata$construction_year
traindata$yip[traindata$yip > 2000] <- 0

#Removing Attributes-based on redundancy and logical reasoning
traindata$num_private <- NULL
traindata$wpt_name <- NULL
traindata$subvillage <- NULL
traindata$region_code <- NULL
traindata$region <- NULL
traindata$ward <- NULL
traindata$recorded_by <- NULL
traindata$scheme_name <- NULL
traindata$permit <- NULL
traindata$extraction_type <- NULL
traindata$extraction_type_class <- NULL
traindata$management_group <- NULL
traindata$quality_group <- NULL
traindata$quantity_group <- NULL
traindata$waterpoint_type_group <- NULL
traindata$source_type <- NULL
# traindata$latitude <- NULL
# traindata$longitude <- NULL
#traindata$funder <- NULL
#traindata$installer <- NULL
traindata$date_recorded <- NULL

#factoring attributes
traindata$basin <- as.factor(traindata$basin)
#traindata$region <- as.factor(traindata$region)
traindata$public_meeting <- as.factor(traindata$public_meeting)
traindata$scheme_management <- as.factor(traindata$scheme_management)
traindata$extraction_type_group <- as.factor(traindata$extraction_type_group)
traindata$management <- as.factor(traindata$management)
traindata$payment <- as.factor(traindata$payment)
traindata$payment_type <- as.factor(traindata$payment_type)
traindata$water_quality <- as.factor(traindata$water_quality)
traindata$waterpoint_type <- as.factor(traindata$waterpoint_type)
traindata$quantity <- as.factor(traindata$quantity)
traindata$source <- as.factor(traindata$source)
traindata$source_class <- as.factor(traindata$source_class)
traindata$status_group <- as.factor(traindata$status_group)

#Removing NAs from attribute
traindata$public_meeting <- factor(traindata$public_meeting, levels = c("FALSE","TRUE",""))
traindata$public_meeting[is.na(traindata$public_meeting)] <- ""

#Creating New attribute called 'YIP' for testdata
tmp1 <- format(as.Date(testdata$date_recorded, '%Y-%m-%d'),'%Y')
tmp1 <- as.numeric(tmp1)

testdata$yip <- tmp1 - testdata$construction_year
testdata$yip[testdata$yip > 2000] <- 0

#Removing redundant and unimportant attributes
testdata$num_private <- NULL
testdata$wpt_name <- NULL
testdata$subvillage <- NULL
testdata$region_code <- NULL
testdata$region <- NULL
testdata$ward <- NULL
testdata$recorded_by <- NULL
testdata$scheme_name <- NULL
testdata$permit <- NULL
testdata$extraction_type <- NULL
testdata$extraction_type_class <- NULL
testdata$management_group <- NULL
testdata$quality_group <- NULL
testdata$quantity_group <- NULL
testdata$waterpoint_type_group <- NULL
testdata$source_type <- NULL
# testdata$latitude <- NULL
# testdata$longitude <- NULL
#testdata$funder <- NULL
#testdata$installer <- NULL
testdata$date_recorded <- NULL

testdata$public_meeting <- casefold(testdata$public_meeting, upper = TRUE)
testdata$public_meeting <- as.factor(testdata$public_meeting)
levels(testdata$scheme_management) <- levels(traindata$scheme_management)

#Diving the training data into Training and Validation Set
labeldata <- traindata$status_group
labeldata <- as.numeric(labeldata)
TL <- labeldata[1:53460] 
VL <- labeldata[53461:59400]
traindata <- traindata[-grep('status_group',colnames(traindata))]
validdata <- traindata[53461:59400,] #validation set-10% of data
traindata <- traindata[1:53460,] #training set-90% of data
alldata <- rbind(traindata, validdata,testdata) #Combining all the data
TL <- TL - 1
VL <- VL - 1


# library(Matrix)
# install.packages("chron", dependencies = T)
# require(xgboost)

#Converting all the data to numeric and factoring levels
cols <- names(alldata)
for(i in cols){
  if(class(alldata[[i]])=="character"){
    levels <- unique(c(alldata[[i]]))
    alldata[[i]] <- as.numeric(factor(D[[i]], levels = levels))
  }
  if(class(alldata[[i]])=="factor"){
    alldata[[i]] <- as.numeric(alldata[[i]])
  }
}

#Subsetting the data
alldata.xgb <- subset(alldata, select = c(-district_code,-id))
alldata.xgb <- as.matrix(as.data.frame(lapply(alldata.xgb,as.numeric)))

#Breaking the dataset again
train.xgb <- alldata.xgb[1:53460,]
valid.xgb <- alldata.xgb[53461:59400,]
test.xgb <- alldata.xgb[59401:74250,]

#Converting the data into DMatrix format
newtrain <- xgb.DMatrix(train.xgb, label = TL)
newvalid <- xgb.DMatrix(valid.xgb, label = VL)
newtest <- xgb.DMatrix(test.xgb)
print("hello")
accuracies <- 0
for(i in 9:9){
  set.seed(i)
  print(i)
  #set.seed(2*i+1)
  #list of parameters for Gradient Boosting
  param.xgb <- list(objective = "multi:softmax",eval_metric = "merror",num_class = 3,
                    booster = "gbtree",eta = 0.2,subsample = 0.7,colsample_bytree = 0.4,
                    max_depth = 14)
  print("hello")
  results <- xgb.cv(params = param.xgb, newtrain, nrounds = 200, nfold = 10,
              early.stop.round = 20,maximize = FALSE, print.every.n = 10)
  #Creating the Gradient Boosting Model
  model.xgb <- xgb.train(data = newtrain, param.xgb, nrounds = 38, watchlist = list(valid = newvalid, train = newtrain),
                         nfold = 10, early.stop.round = 20, print.every.n = 10,
                         maximize = FALSE,save_name = "model.xgb")
  #early.stop.round = 20,
  
  #Testing the model against Validation Set
  pred.valid <- predict(model.xgb, newvalid)
  output.valid <- data.frame(id = validdata$id, status_group = pred.valid)
  output.valid$status_group[output.valid$status_group == 0] = "functional";
  output.valid$status_group[output.valid$status_group == 1] = "functional needs repair";
  output.valid$status_group[output.valid$status_group == 2] = "non functional";
  
  VL[VL == 0] = "functional"
  VL[VL == 1] = "functional needs repair"
  VL[VL == 2] = "non functional"
  print(mean(output.valid$status_group == VL))
  #accuracies[i] <- mean(output.valid$status_group == VL)
}
#Testing the model against test data
pred.test <- predict(model.xgb, newtest)
output.test <- data.frame(id = testdata$id, status_group = pred.test)
output.test$status_group[output.test$status_group == 0] = "functional";
output.test$status_group[output.test$status_group == 1] = "functional needs repair";
output.test$status_group[output.test$status_group == 2] = "non functional";
print("hello")
write.csv(output.test, file = "submissionSeed9_1.csv", row.names = F)

