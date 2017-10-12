
#Resource: http://www.phytools.org/eqg/Exercise_4.1/

#From our example were taking k=1
#We take a step at frequency 1 
#Each step size is 1
#The random variables we generate will be normal
#To simulate the random walk we will take a sum of normal random variables
#Note: That this simulation is a stochastic process that approaches the brownian motion stochastic process 

t <- 0:1000000  # time


## first, simulate a set of 100 normal random variables with mean=0, standard deviation of sig(2)
x <- rnorm(n = length(t) - 1)


## now compute their cumulative sum
#The cumulative sum stores the sum up to the index of the random variables
x <- c(0, cumsum(x))

#Plot the time and sum
plot(t, x, type = "l")

print("Mean")
print(mean(x))

print("Variance")
print(var(x))