
#Stores partial sums
x<-vector()

#Stores time
times<-vector()

#Initial time and starting state are 0
x[1] =0

times[1]=0

i =2

#Generate 100000 steps
while(i<=10000) {
  
  #Generate a random variable with distribution(P(1)=1/2,P(-1)=1/2)
  temp = 1
  
  u1 = runif(1,0,1)
  
  if(u1<0.5) {
    
    temp = -1
  }
  
  else {
    temp =1
  }
  
  
  
  #Store partial sums
  x[i] = x[i-1] + temp
  #Store time
  times[i]=i-1
  
  #Iterate
  i = i+1
  
}

#plot
windows()
plot(times,x)