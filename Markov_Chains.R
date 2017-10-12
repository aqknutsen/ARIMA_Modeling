library(expm)
library(markovchain)
library(diagram)
library(pracma)
library(markovchain)


#Function that plots a random Markov Chain
#Only use for small sizes
plot_markov <- function(num_rows,num_cols) {
  
  #Call regular_markov_chain function to print
  my_matrix = regular_markov_chain(num_rows,num_cols);
  
  #Plot markov chain using built in plotmat
  windows();
  plotmat(my_matrix, main = "Random Regular Marko Chain");
  
  
}

#Function that generates a rows * cols markove chain with number_absorbing absorbing states
generate_absorbing_markov_chain <- function(rows,cols,number_absorbing) {

  #Variable for number of rows
  num_rows = rows;
  
  #Variable for num cols
  num_cols = cols;
  
  #Variable for number of absorbing states
  num_absorbing = number_absorbing;
  #Randomly generates the (i,i) index of the absorbing state
  absorbing <- sample(1:num_rows, num_absorbing);
  
  #initial matrix
  my_matrix<-matrix(nrow=num_rows,ncol=num_cols);
  
  
  #For loop to initilize the rows of a matrix
  for(i in 1:num_rows) {
    
    #If i matches an index number for the row of an abosbring state
    if(i %in% absorbing) {
      
      
      #Store the absorbing state in the matrix
      my_matrix[i,]= c(rep(0,i-1),1,rep(0,num_cols-i));
      
    }
    
    #If we are not an absoring state
    else {
      #num_cols uniformly distrubted random variables
      p_rows = runif(num_cols);
      
      #Make each row sum to 1 [i.e. a pmf]
      final_prob_rows = p_rows/sum(p_rows);
      
      #Store the pmf in the row of the matrix
      my_matrix[i,]=final_prob_rows;
      
    }
    
    
    
  }
  
  return(my_matrix);
  
}



#Function that generates a rows * cols regular markov chain
regular_markov_chain <- function(rows,cols) {
  
  #Variable for number of rows
  num_rows = rows;
  
  #Variable for num cols
  num_cols = cols;
  
  #initial matrix
  my_matrix<-matrix(nrow=num_rows,ncol=num_cols);
  
  
  #For loop to initilize the rows of a matrix
  for(i in 1:num_rows) {
    
    
    #num_cols uniformly distrubted random variables
    p_rows = runif(num_cols);
    
    #Make each row sum to 1 [i.e. a pmf]
    final_prob_rows = p_rows/sum(p_rows);
    
    #Store the pmf in the row of the matrix
    my_matrix[i,]=final_prob_rows;
    
  
    
    
  }
  
  return(my_matrix);
  
  
}

#Function that generates a probability vector
prob_vector <- function(size) {

    
    #Generate a random vector
    u = runif(size);
    
    #Make the u vector a pmf 
    u_final = u/sum(u);
    
    
    return(u_final);
    
  
}

#Generates a regular markov chain, a starting probability vector
#Printsa number of different things at the end
markov_chain_with_prob_vector <-function(numr,numc,num_steps) {
  
  #Store the number of rows and columns
  num_rows = numr;
  num_cols = numc;
  
  #Have the starting probability vector created using prob_vector function
  u = prob_vector(numr);
  
  
  #Create a regular matrix use regular_markov_chain_function
  my_matrix = regular_markov_chain(numr,numc);
  
  #Transition matrix after steps
  my_matrix_steps = my_matrix %^% 5;
  
  #Probability after n steps of being in state s_i
  probs = u %*% my_matrix_steps;
  
  #Fixed row vector of the transition matrix
  fixed_row = my_matrix_steps[1,];
  
  #mean recurrence time for state s_i
  mean_reccurence = 1/fixed_row;
  
  #Create an identity matrix with dimension equal to the number of rows of the transition matrix
  i = diag(dim(my_matrix)[1]);
  
  #Create the fundamental matrix
  fundamental_matrix = solve(i - my_matrix + my_matrix_steps);
  
  #Create mean first passage matrix
  M<-matrix(nrow=numr,ncol=numc);
  
  #Generate mean first passage times [exepected number of steps to go from state i to j for the first time]
  for(i in 1:numr) {
    
    for(j in 1:numc) {
      M[i,j] = (fundamental_matrix[j,j]-fundamental_matrix[i,j])%/%fixed_row[j];
      
    }
  }
  
  #Print info
  print("My transition matrix");
  print(my_matrix);
  print("");
  print("Starting probabilites");
  print(u);
  print("");
  print("Transition Matrix after num_steps steps");
  print(my_matrix_steps);
  print("");
  print("Probability after n steps of being in state s_i");
  print(probs);
  print("");
  print("Fixed row vector");
  print(fixed_row);
  print("");
  print("Mean recurrence time for states[Start in state i and exepected steps until return to s_i");
  print(mean_reccurence);
  print("");
  print("Mean First Passage Times");
  print(M);
  
  
}




#Function assumes parameter M is a MarkovChain object with a transition matrix already in canonical form
#Canonical Form: I - top left, R - bottom left, 0 - top right, Q - bottom right
#Type if we want the q matrix of r matrix. Type is q by default
  getRQ <- function(M,type = "Q") {
  
  if(length(absorbingStates(M))==0) {
    
    stop("Not Absorbing Matrix");
  }
  
  #We assume M passed in is a Markov Chain Object
  #This gets the slot (variable) transition matrix property of the M matrix
  tm<-M@transitionMatrix;
  
  
  #Get the diagonal elements of the transition matrix
  d<-diag(tm);
  
  #This finds the max index of the diagonal elements which has a value of 1
  #Which d==1 returns the values of diagonal elements with 1
  #max finds the ax index
  m<-max(which(d==1));
  
  #Returns the length of the diagonal
  n<-length(d);
  
  
  #ifelse
  #type must be an object or logical vector
  #If type is true, then we return the first A
  #If type is false, we return the second A
  #Note: R has size num_trans * num_absorbing
  #Note: Q has size num_trans * num_trans
  ifelse(type=="Q", A<-tm[(m+1):n,(m+1):n],A<-tm[(m+1):n,1:m]);
  
  
  #Return either Q or R depending on the type
  return(A);
  
  }
  


#Function that prints out information about the Markov Chain matrix passed in as a parameter
absorbing_chains <- function() {
  
  #Call to generate_markov_chain function
  my_matrix = generate_absorbing_markov_chain(6,6,2);
  
  #Create a markov chain object
  dw_markov_chain <-new("markovchain",
             transitionMatrix = my_matrix,
             name = "Random Markov Chain")
  
  
  #Check if the matrix is absorbing
  if(length(absorbingStates(dw_markov_chain)) == 0) {
    
    print("Not absorbing matrix");
  }
  
  
  
  #Put the matrix in canonical form
  canonical_form <- canonicForm(dw_markov_chain);
  
  
  #Store Q using our function
  Q= getRQ(canonical_form,type="Q");
  
  
  #I needs to be the same size as Q which is num_trans * num_trans
  #dim - Returns num_rows,num_cols of matrix in a vector
  I<- diag(dim(Q)[1]);
    
  
  #Get the Inverse of I - Q. The fundamental matrix
  #The fundament matrix indices (i,j) store the expected number of times the state is in s_j, given its starts in s_i
  fundamental_matrix <- solve(I-Q);
  
  
  #Create a vector of all 1s with length as the number of rows of N
  c<-rep(1,dim(fundamental_matrix)[1]);
  
  #the ith index of t represents the expected number of steps until absorbing starting in state s_i
  t<- fundamental_matrix %*%c
  
  
  #Get the R matrix using the getRQ function
  R <- getRQ(canonical_form,"R");
  
  #The (i,j) index of B represents the probability that the chain will be absorbed in state s_j stating in state s_i
  B<- fundamental_matrix%*%R;
  
  
  #Print Statements Change as you would like
  print("Transition Matrix");
  print(dw_markov_chain);
  print("Fundamental Matrix");
  print(fundamental_matrix);
  print("T vector");
  print(t);
  print("B matrix");
  print(B);
  
  
  
}

markov_chain_with_prob_vector(5,5,10000);

    
  
  
  
