Accept_Reject <- function(n,T) {
  
  
  for(i in 0:n) {
    
   
    if(pbinom(i,n,0.2) > 0.025) {
      k1=i-1;
      break;
    }
    

    
  }
  
  for(i in 0:n) {
    
    
    if(1-(pbinom(i,n,0.2)) < 0.025) {
      k2=i;
      break;
    }
    
    
  }
 
  if(T <= k1 || T>= k2) {
    
    print("Reject H0");
  }
  
  else {
    print("Retain H0");
  }
  cat("\n");
  
  
  
}

Accept_Reject(56,8)
Accept_Reject(55,13)
Accept_Reject(58,12)
Accept_Reject(56,13)
Accept_Reject(57,14)
Accept_Reject(54,5)
Accept_Reject(56,14)
Accept_Reject(57,15)
Accept_Reject(54,11)
Accept_Reject(55,13)
Accept_Reject(57,10)
Accept_Reject(59,8)
Accept_Reject(54,10)
Accept_Reject(55,11)
Accept_Reject(56,12)
Accept_Reject(57,11)
Accept_Reject(54,6)
Accept_Reject(58,7)
Accept_Reject(58,12)
Accept_Reject(58,14)


