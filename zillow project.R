
# 00. setting -------------------------------------------------------

library(dplyr)
library(ggplot2)
library(data.table)
install.packages("DT")
library(DT)
library(lubridate)
library(tidyr)
library(leaflet)
install.packages("mice")
library(mice)
install.packages("VIM")
library(VIM)
library(caret)
install.packages("h2o")
library(h2o)
# function
## data type transformations - factoring 범주형
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

# load data
train1 <- fread("C:/자료/스터디/워킹캐글/Zillow Prize/데이터셋/train_2016_v2.csv/train_2016_v2.csv")
propert <- fread("C:/자료/스터디/워킹캐글/Zillow Prize/데이터셋/properties_2016.csv/properties_2016.csv")
# sample <- fread("data/sample_submission.csv", header = TRUE)
sample <- fread("C:/자료/스터디/워킹캐글/Zillow Prize/데이터셋/sample_submission.csv/sample_submission.csv")

?fread

set.seed(1711)

# 01. feature sumamry ----------------------
# 변수 22개 + logerror 선택
useVar <- c("logerror","taxvaluedollarcnt"
            ,"structuretaxvaluedollarcnt"
            ,"landtaxvaluedollarcnt"
            ,"taxamount"
            ,"taxdelinquencyflag"
            # ,"taxdelinquencyyear" 보류 flag 영향도 높으면 사용
            ,"lotsizesquarefeet"
            ,"finishedsquarefeet12"
            ,"calculatedfinishedsquarefeet"
            ,"fullbathcnt"
            ,"bathroomcnt"
            ,"bedroomcnt"
            ,"roomcnt"
            ,"fireplaceflag"
            ,"hashottuborspa"
            ,"latitude"
            ,"longitude"
            ,"fips"
            ,"regionidzip"
            ,"regionidcity"
            ,"regionidcounty"
            #,"regionidneighborhood" NA 5만개이고 주소값이므로 대체불가
            ,"yearbuilt")
            

# 02. feature engineering ------------------
# 일단은 중복 제거하지 않고 진행 했습니다 ~_~
total <- merge(x = train1, y = propert, by = "parcelid", all.x = TRUE)

# 사용할 22개 변수 추출
total <- total %>% 
  select(useVar)

# NA 채울 변수 list (7개)
naDataVar <- c("taxvaluedollarcnt"
              ,"structuretaxvaluedollarcnt"
              ,"landtaxvaluedollarcnt"
              , "taxamount"
              ,"lotsizesquarefeet"
              , "finishedsquarefeet12"
              ,"calculatedfinishedsquarefeet")

# split data
naData <- total %>% 
  select(naDataVar)

noNaData <- total %>% 
  select(-one_of(naDataVar))
# mice 결과
# 왜인지 변수 한개가 자꾸 빠지네요...
# 2번 돌려주셔야 될 것 같아요.
miceResult <- mice(naData, m = 2)
completeMice <- complete(miceResult, 1)
miceResult <- mice(completeMice, m = 2)
completeMice <- complete(miceResult, 1)



# knn 결과
#install.packages("DMwR")
#library(DMwR)
#knnResult <- knnImputation(naData,k=5)

######## ADD SOME FEATURES ####

# Dist from centroid
p$longmean <- mean((p$longitude), na.rm=TRUE)/1e6
p$latmean <- mean((p$latitude), na.rm=TRUE)/1e6

# Adjusted long lat
p$longitude1 <- p$longitude/1e6
p$latitude1 <- p$latitude/1e6

# Haversine distance
ncol(p)
p$geodist <- distHaversine(p[,31:32], p[,33:34])

p[,longmean := NULL]
p[,latmean := NULL]
p[,longitude1 := NULL]
p[,latitude1 := NULL]

# Tax based info
pr[, landValRatio := (pr$landtaxvaluedollarcnt / (pr$landtaxvaluedollarcnt + pr$structuretaxvaluedollarcnt))]
#landtaxvaluedollarcnt	구획의 토지면적에 대한 평가가치		
#structuretaxvaluedollarcnt	구획(parcel)에 지어진 건물의 평가가치		

# Bathrooms are important
pr[, bathInteraction := (pr$bathroomcnt * pr$calculatedfinishedsquarefeet)]
#bathroomcnt	Bathroom 개수		


# Sq Ft / Room
pr[, sqftRoom := (pr$calculatedfinishedsquarefeet / pr$roomcnt)]

# Struc / Lanad
pr[, strucLand := (pr$calculatedfinishedsquarefeet / pr$lotsizesquarefeet)]

# Age
pr[, age := 2020 - pr$yearbuilt]



#############################
# cbind (mice 또는 knn)
train <- cbind(completeMice, noNaData)
#train <- cbind(knnResult, noNaData)

# 범주화
train$hashottuborspa <- ifelse(train$hashottuborspa == "", 0, 1)
train$fireplaceflag <- ifelse(train$fireplaceflag == "", 0 ,1)
train$taxdelinquencyflag <- ifelse(train$taxdelinquencyflag == "Y", 1,0 )

# 02. model(select importance var) -----------

# formular 지정
formula.init <- "logerror ~ ."
formula.init <- as.formula(formula.init)

# 5-fold cv
control <- trainControl(method = "cv", number = 5)

rm(train);rm(propert); 
rm(naData);rm(total);
# method RF / svm 등등 사용가능
model <- train(formula.init, data=train, method="rf", 
               trControl=control, na.action = na.omit, importance=TRUE)


importance <- varImp(model) #, scale=FALSE
plot(importance, cex.lab=0.5)
model$modelInfo$varImp
?varImp
####################
## xgboost dataset
trainMat<-model.matrix(~.,data = train)
params<-list(max.depth = 5, eta = 0.3, objective = "reg:linear", eval_metric = "error")
model_xgb<-xgboost(trainMat, label = trainMat[,9], params = params, nrounds = 5)


##########################################3
#TEST SET만들기
##########################################3
useVar <- c("taxvaluedollarcnt"
            ,"structuretaxvaluedollarcnt"
            ,"landtaxvaluedollarcnt"
            ,"taxamount"
            ,"taxdelinquencyflag"
            # ,"taxdelinquencyyear" 보류 flag 영향도 높으면 사용
            ,"lotsizesquarefeet"
            ,"finishedsquarefeet12"
            ,"calculatedfinishedsquarefeet"
            ,"fullbathcnt"
            ,"bathroomcnt"
            ,"bedroomcnt"
            ,"roomcnt"
            ,"fireplaceflag"
            ,"hashottuborspa"
            ,"latitude"
            ,"longitude"
            ,"fips"
            ,"regionidzip"
            ,"regionidcity"
            ,"regionidcounty"
            #,"regionidneighborhood" NA 5만개이고 주소값이므로 대체불가
            ,"yearbuilt")

# NA 채울 변수 list
naDataVar <- c("taxvaluedollarcnt"
               ,"structuretaxvaluedollarcnt"
               ,"landtaxvaluedollarcnt"
               , "taxamount"
               ,"lotsizesquarefeet"
               , "finishedsquarefeet12"
               ,"calculatedfinishedsquarefeet")
# 사용할 22개 변수 추출
total <- propert %>%  select(useVar)
naData <- total %>%   select(naDataVar) # mice/knn으로  na대체
#naData <- test %>% select(naDataVar)
noNaData <- total %>%   select(-one_of(naDataVar)) #그 외 변수
# TEST SET  mice 결과
miceResult <- mice(naData, m = 2)
completeMice <- complete(miceResult, 1)
#miceResult <- mice(completeMice, m = 5)
#completeMice <- complete(miceResult, 1)
#knnResult_submit <- DMwR :: knnImputation(as.matrix(na_submit),k=5)

test <- cbind(completeMice, noNaData)

test$hashottuborspa <- ifelse(test$hashottuborspa == "", 0, 1)
test$fireplaceflag <- ifelse(test$fireplaceflag == "", 0 ,1)
test$taxdelinquencyflag <- ifelse(test$taxdelinquencyflag == "Y", 1,0 )
#write.csv(test, "c:/data/test.csv",sep=",")
#write.csv(completeMice, "c:/data/completeMice.csv",sep=",")


####################
## xgboost testset

testMat<-model.matrix(~.,data = test)
xgb_pred<-predict(model_xgb, testMat)
predictions<-xgb_pred


################################################submit파일
na_submit <- propert %>%   select(naDataVar)
noNa_submit <- propert %>% select(-one_of(naDataVar))

submit <- cbind(knnResult_submit, na_submit)



################################################0917 제출파일
options(scipen = 10000)



predictions_target <- data.frame(predict(model, propert))

predictions <- round(as.vector(predictions_target$predict), 4)

result <- data.frame(cbind(propert$parcelid, predictions, predictions * .99,
                           predictions * .98, predictions * .97, predictions * .96,
                           predictions * .95))
colnames(result)<-c("parcelid","201610","201611","201612","201710","201711","201712")
options(scipen = 999)
write.csv(result, file = "submission_xgb_1016.csv", row.names = FALSE )

rm(total);rm(train1);rm(sample);rm(completeMice)
rm(train)
################################################lm sum


#model2 lm
lm_f<-logerror~taxvaluedollarcnt+structuretaxvaluedollarcnt+landtaxvaluedollarcnt+taxamount+finishedsquarefeet12+taxdelinquencyflag+latitude+yearbuilt
str(train)
m_lm <- lm(formula.init,data = train ,na.action = na.omit)
m_lm <- lm(lm_f,data = train ,na.action = na.omit)
summary(m_lm)

predictions_target <- data.frame(predict(m_lm, propert))

predictions <- round(as.vector(predictions_target$predict), 4)

result <- data.frame(cbind(propert$parcelid, predictions, predictions * .99,
                           predictions * .98, predictions * .97, predictions * .96,
                           predictions * .95))
colnames(result)<-c("parcelid","201610","201611","201612","201710","201711","201712")
write.csv(result, file = "submission_automl.csv", row.names = FALSE )


#####################################################xgboost
install.packages("xgboost")
library(xgboost)
?xgboost

m_xgb <- xgboost(train, )
