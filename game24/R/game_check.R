library(combinat)
library(stringr)

## This function has been used many times in this package, it is explained somewhere else
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

## This function helps choose all of the combination of 2 elements in the given vector and
## make them do the all the operations which will leads to another number. Then we return
## a list of vectors which contains the result and the 2 choosen elements
num_reduc=function(A){
  len=length(A)
  B=vector(mode="list",length=0)
  for(i in 1:(len-1)){
    m=i+1
    for (j in m:len){
      for(k in 1:6){
        result=num_sign(A[i],A[j],k)
        temp_list=A[-c(i,j)]
        temp_list=list(c(temp_list,result))
        B=c(B,temp_list)
      }
    }
  }

  return(B)
}



## When the size of the given vector is greater than 2,this function will use function "num_red"
## several times to the final lists with vectors only having 1 elements of each
recur=function(A){
  B=num_reduc(A)
  for(i in 1:length(B)){
    B[[i]]=sort(B[[i]])

  }
  B=unique(B)
  times=length(A)-2
  for(j in 1:times){
  num=length(B)
  len=length(B[[1]])
  C=unlist(B)
  D=vector(mode="list",length=0)
  k=num-1
  for(i in 0:k){
    values=C[(i*len+1):((i+1)*len)]
    temp_list=num_reduc(values)
    D=c(D,temp_list)
    B=D
  }
  }
  return(B)
}
recur(c(10,10,10))

## This final function can handle the vector with different size and give the result that
## whether this number series can lead to the specific integer
game_check=function(A,b){
  len=length(A)
  if(len==1){
    if(A==b){
      print(paste(b,"is just what you want"))
    }
    else{
      print(paste("No one could do this!"))
    }
  }
    else if(len==2){
      K=num_reduc(A)
      K=unlist(K)
      K=unique(K)
      K=na.omit(K)

      count=0
      for(i in 1:length(K)){
        if(K[i]==b){
          count=count+1
        }
      }
        if(count>=1){
          print(paste("This is at least one way to get", b))
        }
        else{
          print(paste("We cant get",b,"with the numbers you provide"))
        }
      }

  else if(len>2){
    S=recur(A)
    S=unlist(S)
    S=unique(S)
    S=na.omit(S)

    count1=0

    for(i in 1:length(S)){
      if(S[i]==b){
        count1=count1+1
      }
    }
      if(count1>=1){
        print(paste("This is at least one way to get", b))
      } else{
        print(paste("We cant get",b,"with the numbers you provide"))
      }
    }

}



