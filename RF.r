setwd("C:/Workspace/")
#data2 <- read.csv("data.csv", header=T, na.strings=c("","NA","0","unknown","Not Known","-0.00000002","N","S","K","M"))
traindata <- read.csv("data.csv", header = T)
testdata <- read.csv("testdata.csv", header = T)

tmp <- format(as.Date(traindata$date_recorded, '%m/%d/%Y'),'%Y')
tmp <- as.numeric(tmp)

traindata$yip <- tmp - traindata$construction_year
traindata$yip[traindata$yip > 2000] <- 0

#Removing ID
trainid <- traindata$id
traindata$id <- NULL

testid <- testdata$id
testdata$id <- NULL

#Removing Attributes
traindata$num_private <- NULL
traindata$wpt_name <- NULL
traindata$subvillage <- NULL
traindata$region_code <- NULL
traindata$lga <- NULL
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
# traindata$funder <- NULL
# traindata$installer <- NULL
traindata$date_recorded <- NULL
traindata$district_code <- NULL

traindata$basin <- as.factor(traindata$basin)
traindata$region <- as.factor(traindata$region)
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
newnames <- names(summary(traindata$funder)[1:15])
funder <- factor(traindata$funder, levels = c(newnames,"Other"))
newnames1 <- names(summary(traindata$installer)[1:15])
installer <- factor(traindata$installer, levels = c(newnames1,"Other"))
traindata$public_meeting <- factor(traindata$public_meeting, levels = c("FALSE","TRUE",""))
traindata$public_meeting[is.na(traindata$public_meeting)] <- ""

funder[is.na(funder)] <- ""
traindata$funder <- funder

installer[is.na(installer)] <- ""
traindata$installer <- installer
print("hello")
cvresults <- rfcv(traindata, traindata$status_group, cv.fold = 10, ntree = 100)
print("hello")
rf_model <- randomForest(traindata$status_group~.,data = traindata, ntree = 500)


tmp1 <- format(as.Date(testdata$date_recorded, '%Y-%m-%d'),'%Y')
tmp1 <- as.numeric(tmp1)

testdata$yip <- tmp1 - testdata$construction_year
testdata$yip[testdata$yip > 2000] <- 0

testdata$num_private <- NULL
testdata$wpt_name <- NULL
testdata$subvillage <- NULL
testdata$region_code <- NULL
testdata$lga <- NULL
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
# testdata$funder <- NULL
# testdata$installer <- NULL
testdata$date_recorded <- NULL
testdata$district_code <- NULL

testdata$public_meeting <- casefold(testdata$public_meeting, upper = TRUE)
testdata$public_meeting <- as.factor(testdata$public_meeting)
levels(testdata$scheme_management) <- levels(traindata$scheme_management)

funder <- factor(testdata$funder, levels = c(newnames,"Other"))
funder[is.na(funder)] <- ""
testdata$funder <- funder

installer <- factor(testdata$installer, levels = c(newnames1,"Other"))
installer[is.na(installer)] <- ""
testdata$installer <- installer

print(mean(rf_model$predicted == traindata$status_group))
#*For RF START
predtemp <- predict(rf_model, testdata)
backuppred <- predtemp
output = data.frame(testid, predtemp)
#*For RF END

names(output) <- c("id","status_group")
#write.csv(x=output, file = "submission8.csv",row.names = F)

