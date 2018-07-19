############################# 과제 ############################################
rm(list=ls())
###데이터 불러오기###

#경로 설정
setwd("C:/Users/eunse/Desktop/9기1주차")

#데이터 불러오기
cow<-read.csv("cow_data.csv")

###데이터 구조 보기###
str(cow) #64999 x 11 /64999개의 데이터, 11개의 변수

###결측치 제거###
sum(is.na(cow)) #24개
colSums(is.na(cow)) #weight에 24개 모두 있음

#결측치가 포함되어 있는 24개의 행 제거
cow<-na.omit(cow)
str(cow) #64975 x 11

#code 열 제거
cow<-cow[,-1]

#범주형 변수 처리

cow$birthday<-factor(cow$birthday)
cow$kind<-factor(cow$kind)
cow$gender<-factor(cow$gender)

cow$age<-as.numeric(cow$age)
cow$weight<-as.numeric(cow$weight)
#price는 범주형 변수가 아니므로 숫자로 바꿈

cow$price<-as.numeric(cow$price)

str(cow)


# 1. 주어진 cow_data에서 is_edible 이라는 새로운 열을 추가하고
# 나이(age)가 50(개월)이상이면서 등급(grade)이 "3" 또는 "등외"이라면 "폐기용", 아니면 "식용"을 기입하는 함수를 작성해주세요.


#열 추가

cow$is_edible<-ifelse(cow$age>=50 & (cow$grade=="3" | cow$grade=="등외"),"폐기용","식용")
cow$is_edible<-factor(cow$is_edible)

#mutate와 pipe 함수 이용
cow<-cow %>%
  mutate(is_edible=ifelse(cow$age>=50 & (cow$grade=="3" | cow$grade=="등외"),"폐기용","식용"))


# 2."1++" 등급이 가장 많은 세 지역(변수 address)을 구하고 각 지역별로 "1++"등급이 츙 몇 마리인지 보여주세요 (시/군/구 단위로 구해주세요)
library(stringr)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)

#띄어쓰기를 기준으로 문자열 자르기
a<-str_split(cow$address," ")

#시, 군, 구 기준으로 변수 자르기
for(i in 1:length(a)){
  if(str_sub(a[[i]][2],-1)=="시"){
    if(str_sub(a[[i]][3],-1)=="구"){
      a[[i]]<-a[[i]][1:3]
    }
    else {
      a[[i]]<-a[[i]][1:2]}
  }
  else if(str_sub(a[[i]][2],-1)=="군"){
    a[[i]]<-a[[i]][1:2]
  }
}


b<-c()
for(i in 1: length(a)){
  if(length(a[[i]]==2)){
    b[i]<-paste(a[[i]][1],a[[i]][2])
  }
  else{
    b[i]<-paste(a[[i]][1],a[[i]][2],a[[i]][3])
  }
}

class(b) #character 형태이므로 factor로 변환.
b<-as.factor(b)
class(b) #factor

#add_1이라는 새로운 열에 저장. 원래 address변수는 삭제
cow$add_1<-b
cow<-cow[,-4]

#tibble 형태로 저장하여 상위 5개 확인.
grade_top<-cow %>%
  filter(grade=="1++") %>%
  group_by(add_1) %>%
  dplyr :: summarise(n=n())%>%
  arrange(desc(n)) %>%
  head(7)
grade_top
#1) 전라북도 정읍시 224
#2) 전라남도 고흥군 152
#3) 경기도 안성시 139

#그래프로 확인
ggplot(data=grade_top,aes(x=reorder(add_1,n),y=n))+
  geom_col() +
  coord_flip()


# 3. 위 세 도시 별로 각 등급마다 소의 평균가격(price)을 구해주세요

cow_topG1<-cow[cow$add_1=="전라북도 정읍시",]
cow_topG2<-cow[cow$add_1=="전라남도 고흥군",]
cow_topG3<-cow[cow$add_1=="경기도 안성시",]

top1<-ddply(cow_topG1, .(grade), summarize, mean(price))
top2<-ddply(cow_topG2, .(grade), summarize, mean(price))
top3<-ddply(cow_topG3, .(grade), summarize, mean(price))

top1
top2
top3
#plot

# 4. 위 세 도시 별로 총 몇 마리의 소가 도축됐는지 월 단위로 구하고 그래프로 표현해주세요 (변수 slaughter_date가 도축된 날짜를 의미함)

class(cow$slaughter_date) #integer

#factor 처리
cow$slaughter_date<-as.factor(cow$slaughter_date)

#날짜에서 월만 분리해서 저장
k<-str_sub(cow$slaughter_date,5,6)
k<-as.factor(k)
cow$date<-k
cow<-cow[,-5] #slaughter_date 변수 제거

cow_topG1<-cow[cow$add_1=="전라북도 정읍시",]
cow_topG2<-cow[cow$add_1=="전라남도 고흥군",]
cow_topG3<-cow[cow$add_1=="경기도 안성시",]

cow_topG1_1<-table(cow_topG1$date)
cow_topG2_1<-table(cow_topG2$date)
cow_topG3_1<-table(cow_topG3$date)


#그래프 그리기. 
par(mfrow=c(1,1))
plot(cow_topG1_1,type="l",col="blue",axes=F,xlab="Month",ylab="cow",main="Slaughtered Cow",lwd=1)
lines(cow_topG2_1,type="l",col="red",lwd=1)
lines(cow_topG3_1,type="l",col="green",lwd=1)

#축 설정하기
axis(1, at=seq.int(01:12),las=1,
     labels = c(paste0(1:12, "월")))
axis(2, las=1)

#legend 지정
legend("topright",c("전라북도 정읍시","전라남도 고흥군","경기도 안성시"),
       col=c("blue","red","green"),lwd=2,cex=0.8,bg="lightgrey")

# (기존기수만 해당) 
# 5. 소 가격(price)과 상관 관계가 있는 변수들이 있다면 찾아내고 그 관계를 설명해주세요 

#birthday는 age와 겹치는 개념이라 여겨 제거
cow<-cow[,-1]

str(cow)

par(mfrow=c(2,2))


plot(cow$kind,cow$price)
#종 별로 큰 차이를 보이지는 않는다.
plot(cow$gender,cow$price)
#gender도 마찬가지.
plot(cow$slaughter_house,cow$price)
#도축장에 따라서 큰 차이를 보이지는 않는다.
plot(cow$age,cow$price)
#전체적으로 30-50사이에 많이 분포하고 있으며 나이가 많아질 수록 그 수가 줄어듦.

plot(cow$grade,cow$price,ylim=c(10000,50000))
plot(cow$is_edible,cow$price)
plot(cow$add_1,cow$price)
plot(cow$date,cow$price)
plot(cow$weight,cow$price)


cor.test(cow$age,cow$price)
cor.test(cow$weight,cow$price)



