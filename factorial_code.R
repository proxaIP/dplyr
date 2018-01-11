library(purrr)
library(microbenchmark)
library(tidyr)
library(magrittr)
library(dplyr)

# Q1: Factorial with for loop  
Factorial_loop <- function(n){
  f <- 1 # 'f' will be used to represent factorial for all four questions 
  if(n==0){
    f 
  }
  else{
   for(i in 1:n){
     f <- f*i
   }
    f
  }
  f
}

#Q2: Factorial with Reduce fn 

Factorial_reduce <- function(n){
  f <- 1
  if(n==0){
   f 
  }
  else{
  f <- c(1:n)%>%reduce(`*`)
  }
  f
}

#Q3: Factorial with Recursion

Factorial_func <- function(n){
  f <- n
  if(n==0){
    f <- 1
  }
  else{
    f <- f*Factorial_func(n-1)
  }
  f
}

#Q4: Factorial using memoization 

buildMemFactorial <- function(){
  res <-1
  Factorial_mem <-function(n){
    if(n==0) return (1)
    
    if(length(res) < n) res <<-`length<-`(res,n)
    
    if(!is.na(res[n])) return(res[n])
    
    res[n] <<- n*Factorial_func(n-1)
    res[n]
  }
  Factorial_mem
}
Factorial_mem <- buildMemFactorial()

#Benchmark 

factorial_benchmark <- function(n){
  
microbenchmark(Factorial_loop(n), Factorial_reduce(n),Factorial_func(n),Factorial_mem(n))

  #benchmark for range of values 
  
for(i in n){
microbenchmark(Factorial_loop(i),Factorial_reduce(i),Factorial_func(i),Factorial_mem(i))
}
  
}







