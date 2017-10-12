#Number of columns in tranistion matrix 
num_cols = 6

#Number of rows in transition matrix
num_rows = 6

#Transition Matrix
p= matrix(data = NA, nrow = num_rows, ncol = num_cols)


#This loop will generate a probability vector at each row
#Then each row will be put into a transition matrix
i=1
while(i<=num_rows) {
  
  #This makes states 1 and 3 absorbing states
  if(i ==1 || i == 3) {
    
      my_row = c(1,rep(0,num_cols-1))
  }
  
  #All other states are non absorbing
  else {
    #Generates a vector of uniform random numbers of length num_prob
    my_row = runif(num_prob,0,1)
    
    #This makes the numbers we generate into a discrete probability density function
    my_row = my_row/sum(my_row) 
    
  }
    
  
  
  #Store the row in the transition matrix
  p[i,] = my_row
  
  
  #Iterate the loop
  i  = i + 1
  
}

print(p)





