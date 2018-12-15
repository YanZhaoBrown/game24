
library(combinat)
library(stringr)

#give every operation a numeric value that will be used inside the function game24
num_sign=function(a,b,sign){
  if(sign==1){
    return(a+b)
  }
  else if(sign==2){
    return(a-b)
  }
  else if(sign==3){
    return(a*b)
  }
  else if(sign==4){
    return(a/b)
  }
  else if(sign==5){
    return(b-a)
  }
  else if(sign==6){
    return(b/a)
  }
}



#function to show the steps of operations that will be used inside the function game24.
trans=function(b,c,a){
  stopifnot(a==1 | a==2 | a==3 | a==4 | a==5 | a==6)
  if(a==1){
    k1=paste("(",b,"+",c,")")
    return(k1)
  }
  else if(a==2){
    k2=paste("(",b,"-",c,")")
    return(k2)
  }
  else if(a==3){
    k3=paste(b,"*",c)
    return(k3)
  }
  else if(a==4){
    k4=paste(b,"/",c)
    return(k4)
  }
  else if(a==5){
    k5=paste("(",c,"-",b,")")
    return(k5)
  }
  else{
    k6=paste(c,"/",b)
    return(k6)
  }
}


game24_original =function(A,b=24){
  len=length(A)
  B=combinat::permn(A)
  stopifnot(len==4)
  stopifnot(A[1]%%1==0 & A[2]%%1==0 & A[3]%%1==0 & A[4]%%1==0)
  method=vector(mode="character",length=0)
  operation_1=vector(mode="character",length=0)
  operation_2=vector(mode="character",length=0)
  operation_3=vector(mode="character",length=0)
  rep=vector(mode="integer",length=0)
  result1=vector(mode="integer",length=0)
  result2=vector(mode="integer",length=0)
  for(s in 1:factorial(len)){
    for(i in 1:6){
      result1_temp=num_sign(B[[s]][1],B[[s]][2],i)
      for(j in 1:6){
        result2_temp=num_sign(result1_temp,B[[s]][3],j)
        for(k in 1:6){
          result3=num_sign(result2_temp,B[[s]][4],k)
          if(result3==b){
            operation_1=c(operation_1,trans(B[[s]][1],B[[s]][2],i))
            operation_2=c(operation_2,trans(result1_temp,B[[s]][3],j))
            operation_3=c(operation_3,trans(result2_temp,B[[s]][4],k))
            result1=c(result1,result1_temp)
            result2=c(result2,result2_temp)
          }
        }
      }
    }
  }

  if(length(result1) == 0) {
    return(FALSE)
  }

  for(q in 1:(length(result1)-1)){
    s=q+1
    for(p in s:length(result1)){
      if(result1[q]==result1[p]){
        if(result2[q]==result2[p]){
          rep=c(rep,p)
        }
      }
    }
  }
  operation_1=operation_1[-rep]
  operation_2=operation_2[-rep]
  operation_3=operation_3[-rep]
  result1=result1[-rep]
  result2=result2[-rep]

  for(f in 1:length(operation_1)){
    method=c(method,stringr::str_c(operation_1[f]," = ", result1[f], " then ",
                          operation_2[f]," = ", result2[f], " then ",
                          operation_3[f],  " = ", b))
  }

  for(m in 1:length(method)){
  print(paste("Method ", m, ":", method[m]))
  }

  return(T)
}


#If game 24 fail, then calculate other number
game_other = function(A, b) {
  stopifnot(is.vector(A) & is.vector(b) & length(A) == 4)
  if (game24_original(A,24) == F) {
    for (i in 1:length(b)) {
      if (game24_original(A,b[i]) != F) {
        break
      }
    }
  }
  return (game24_original(A,b[i]))
}


#test of game_other
#game_other(c(7,7,9,5), 25:30)
