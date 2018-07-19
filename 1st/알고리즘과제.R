###Algorithm Assignment I ###

#############Question #1[Monte Carlo Method]

craps<-function(){
  d<-sum(sample(1:6,2,replace=T))
  k<-0
  if(d==7|d==11){
    k<-1
  }else if(d==2|d==3|d==12){
    k<-0
  }else{
    while(TRUE){
      nd<-sum(sample(1:6,2,rep=T))
      
      if(nd==d){
        k<-1
        break
      }
      else if(nd==7){
        k<-0
        break
      }
    }
   
  }
 return(k)
}


N<-1e+5 
x<-c()
x<-0
for(i in 1:N){
  x<-x+craps()
}
sum(x)/N #0.49158
########(a): Monte carlo Method를 통한 CRAPS 게임에 대한 승률은 0.49정도이다.

########(b)

final<-function(ky,sy){
  while(ky!=0 & sy!=0){
    if(craps()==1){
      ky<-ky+1
      sy<-sy-1
    }
    else {
      ky<-ky-1
      sy<-sy+1
    }
  }
  if(ky!=0) return(1)
  else return(0)
}

x<-0
for(i in 1:1000){
  x<-x+final(12,9)
}

x/1000 
#KY, SY의 코인 수가 각각 12, 9라 할 때 KY의 최종적인 승률은 대략 0.49이다.

y<-0
for(i in 1:1000){
  y<-y+final(20,9)
}

y/1000

#KY, SY의 코인 수가 각각 20, 9라 할 때 KY의 최종적인 승률은 대략 0.6이다.


#############Question #2 0과 1로 이루어진 정사각형에서 가장 큰 정사각형 찾아 넓이 반환하는 함수 Large



Large<-function(A){
  for(i in 2:dim(A)[1]){
    for(j in 2:dim(A)[1]){
      if(A[i,j]==1){
        A[i,j]<-min(A[i-1,j-1],A[i,j-1],A[i-1,j])+1
      }
    }
  }
  return(max(A)^2)
  
}

#확인하기
Mat1 <- matrix(c(1, 0, 1, 1, 1,                 0, 0, 0, 1, 1,                 0, 1, 1, 1, 1,                 0, 1, 1, 1, 1,                 0, 1, 1, 1, 1),5, 5, byrow=TRUE) 

Mat2 <- matrix(c(1, 0, 1, 1, 1,                  1, 1, 1, 1, 1,                  0, 1, 1, 1, 1,                  0, 1, 1, 1, 1,                  0, 1, 1, 1, 1),5, 5, byrow=TRUE) 
Large(Mat1) #9
Large(Mat2) #16


#############Question #3
chopchop<-function(a,b,c,d,e){
  k<-c(a,b,c,d,e)
  q1<-sort(k)[1]
  q2<-sort(k)[2]
  q3<-sort(k)[3]
  q4<-sort(k)[4]
  q5<-sort(k)[5]
  
  min_cost<-q1*q2*q4+q1*q3*q5+q1*q2*q3
  return(min_cost)
}


#############Question #4

solveEquation<-function(A,x,b){
  if(is.null(x)){
    n<-dim(A)[1]
    Ab<-cbind(A,b)
    x<-c()
    i<-1
    
    for(i in 1:(n-1)){
      if(Ab[i,i]==0){
        p<-i+1
        for(k in p:n){
          if(Ab[k,1]!=0){
            p<-k
          }
        }
        tmp<-Ab[i,]
        Ab[i,]<-Ab[p,]
        Ab[p,]<-tmp
      }
      for(j in (i+1):n){
        m<-Ab[j,i]/Ab[i,i]
        Ab[j,]<-Ab[j,]-m*Ab[i,]
      }
    }
    if(Ab[n,n]==0){
      stop("해를 구할 수 없습니다")
    }
    else x[n]<-Ab[n,n+1]/Ab[n,n]
    
    for(i in (n-1):1){
      tmp<-0
      for(j in (i+1):n){
        tmp<-tmp+Ab[i,j]*x[j]
      }
      x[i]<-(Ab[i,n+1]-tmp)/Ab[i,i]
    }
    return(x)
  }
  else if(is.null(b)){
    b<-A%*%x
    return(b)
  }
  else{
    if(all(A%*%x==b)){
      return("Correct")
    }
    else return("Incorrect")
  }
}

