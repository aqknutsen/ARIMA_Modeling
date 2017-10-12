#Vector to store partial sums and time
x<-vector()
my_time<-vector()

#Start at the origin and time 0
x[1]=0
my_time[1]=0

i = 2

#Generate 10000 steps
while(i<=10000) {
  
  #Generate a random variable from the inverse gaussian distribution and add it to the partial sum
  u1 = runif(1,0,1)
  x[i] = x[i-1] + qnorm(u1,0,1)
  #Store the time
  my_time[i] = i-1;
 
  #Iterate
  i = i + 1
  
}

plot(my_time,x)

