rm(list=ls())

###데이터 불러오기###

#경로 설정 
setwd("C:/Users/eunse/Desktop/ana")

#train set, test set 불러오기
train<-read.csv("taxi_train.csv")
test<-read.csv("taxi_test.csv")


#구조 보기
str(train)

#x index와 id, vendor_id는 의미가 없다고 판단하여 제거.
train<-train[,c(-1,-2,-3)] 


###전처리###

sum(is.na(train)) #0
#변수가 많지는 않으나 위도와 경도를 이용하여 새로운 변수 생성
#subtract_longi에 longitude의 subtraction 값을 저장
#subtract_lati에 latitude의 subtractoin값을 저장

train$subtract_longi<-train$pickup_longitude-train$dropoff_longitude
train$subtract_lati<-train$pickup_latitude-train$dropoff_latitude

#각 위도와 경도를 좌표로 보고, 유클리디안 거리처럼 계산함.
#dist 변수에 저장.
train$dist<-train$subtract_lati^2+train$subtract_longi^2

###구조 다시 살피기###
str(train) # 10 variables

train<-train[,-c(2:5)]
#dist를 제외한 변수 제거 

#구조보기
str(train) # 6 variables


###회귀모델 적합하기###
fit.full<-lm(trip_duration~.,data=train)
summary(fit.full)

#그래프 그려보기
par(mfrow=c(2,2))
plot(fit.full)


#변수 선택
fit.con<-lm(trip_duration~1,data=train)

fit.both <- step(fit.full, 
                   list(lower=fit.con, upper=fit.full),
                   direction = "both")
summary(fit.both)

library(car)

#이상치 보기 
outlierTest(fit.both)

#이상치라고 다 제거해야 하는 건 아니지만, 너무 낮은 accuracy 때문에 한 번 제거해 보도록 하겠습니다. 
train.2<-train[-c(615116,906300,196072,938311,303092,534637,1006926,501617,974665,19588),]

fit.full.2<-lm(trip_duration~.,train.2)

fit.con.2<-lm(trip_duration~1,train.2)

fit.both.2<-step(fit.full.2, list(lower=fit.con.2,upper=fit.full.2),direction="both")

summary(fit.both.2)

vif(fit.both.2)
#변수들 다중공선성 2 넘는 것이 없음.

par(mfrow=c(2,2))
plot(fit.both.2)

#예측해보기
pre<-predict(fit.both.2,train.2)
pre<-as.data.frame(pre)
head(pre)
dim(pre)

#MSE 얼마나 되는지 확인하기 
sum((pre-train.2$trip_duration)^2)/dim(pre)[1] #10092857로 매우 큰 값을 가진다 ㅠㅠ 

#예측한 값이 신뢰구간에 포함되는 지 확인해보기
pred<-predict(fit.both.2,train.2,interval = "predict")

pred<-as.data.frame(pred)
head(pred)

pred<-cbind(pred,train.2$trip_duration)
head(pred)
TF<-NA
pred<-cbind(pred,TF)
pred$TF[pred$`train.2$trip_duration`>=pred$lwr &pred$`train.2$trip_duration`<=pred$upr]<-T
pred$TF[is.na(pred$TF)]<-F
sum(pred$TF==T)/dim(pred)[1] #0.9985
#standard error가 너무 커서 대다수가 신뢰구간에 포함되는 것 같아보임.

###test set 전처리하기 

str(test) #target 변수 trip_duration을 제외한 9 variables

#train과 마찬가지로 x index, id, vendor id 제거함.
test<-test[,c(-1,-2,-3)] 

#변수가 많지는 않으나 위도와 경도를 이용하여 새로운 변수 생성
#subtract_longi에 longitude의 subtraction 값을 저장
#subtract_lati에 latitude의 subtractoin값을 저장

test$subtract_longi<-test$pickup_longitude-test$dropoff_longitude
test$subtract_lati<-test$pickup_latitude-test$dropoff_latitude

#각 위도와 경도를 좌표로 보고, 유클리디안 거리처럼 계산함.
#dist 변수에 저장.
test$dist<-test$subtract_lati^2+test$subtract_longi^2

###구조 다시 살피기###
str(test) # 10 variables

test<-test[,-c(2:5)]
#dist를 제외한 변수 제거 

#구조보기
str(test) #5 variables

pre.test<-predict(fit.both.2,test)
pre.test<-as.data.frame(pre.test)
write.csv(pre.test,"C:/Users/eunse/Desktop/ana/pre_test.csv")