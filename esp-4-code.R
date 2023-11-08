# Sophie Bata-Madden s2037949, Cameron Donnelly s2018520, Nala Krisnanda s2587290

#Contributions:
# Sophie:  33.3% 
# Cameron:  33.3%
# Nala: 33.3%

#### Code Overview:
# 
# 
# 

#### Function:      netup
# Description: 
# Inputs:       
#     - d        - vector giving the number of nodes in each layer of a network

# Outputs: 
#     - h        - a list of nodes for each layer. 
#                  h[[l]] should be a vector of length d[l] which will contain the node values for layer l.
#     - W        - a list of weight matrices. 
#                  W[[l]] is the weight matrix linking layer l to layer l+1. 
#                  Initialize the elements with U (0, 0.2) random deviates.
#     - b        - a list of offset vectors. 
#                  b[[l]] is the offset vector linking layer l to layer l+1. 
#                  Initialize the elements with U (0, 0.2) random deviates.



netup <- function(d){
  
  # We construct a list of lists, h, where each sublist h[[i]] is or corresponding length d[i]
  h <- list()
  for (l in 1:length(d)){
    h[[l]] <- rep(0,d[l])
  }
  
  # W is a list of weight matrices, W[[i]] is the weight matrix linking layer i to layer i + 1
  # We initialise the elements with U(0,0.2) random deviates
  
  W <- list() 
  for (l in 1:(length(d)-1)){
    W[[l]] <- matrix(runif(d[l]*d[l+1],0,0.2),nrow = d[l+1],ncol = d[l])
  }
  
  # b is a list of offset vectors, b[[i]] is the offset vector linking layer i to layer i + 1
  # We initialise the elements with U(0,0.2) random deviates
  b <- list() 
  for (l in 1:(length(d)-1)){
    b[[l]] <- matrix(runif(d[l],0,0.2),nrow = d[l],ncol = 1)
  }
  
  return(list(h = h, W = W, b = b))
            
}

#### Function:                 forward
# Description:                 the function computes the remaining node values implied by inp, andr eturn the updated network list (as the only return object).
# Inputs:       
#     - nn                   -  a network list as returned by netup.
#     - inp                  -  a vector of input values for the first layer.

# Outputs: 
#     - h        - a list of nodes for each layer. 
#                  h[[l]] should be a vector of length d[l] which will contain the node values for layer l.
#     - W        - a list of weight matrices. 
#                  W[[l]] is the weight matrix linking layer l to layer l+1. 
#                  Initialize the elements with U (0, 0.2) random deviates.
#     - b        - a list of offset vectors. 
#                  b[[l]] is the offset vector linking layer l to layer l+1. 
#                  Initialize the elements with U (0, 0.2) random deviates.

forward <- function(nn, inp){
  h <- nn$h
  W <- nn$W
  b <- nn$b 


  h[[1]] <- inp

  for(l in 1:(length(h)-1)){
    h[[l+1]] <- t(h[[l]] %*% t(W[[l]])) + b[[l]]
    h[[l+1]] <- apply(h[[l+1]], 1, function(x) max(x,0))
  }

  return(list(h = h, W = W, b = b))
}



#### Function:      backward
# Description: 
# Inputs:       
#     - nn        - returned from forward
#     - k         - the output class of the input

# Outputs: 
#     - h        - a list of nodes for each layer. 
#                  h[[l]] should be a vector of length d[l] which will contain the node values for layer l.
#     - W        - a list of weight matrices. 
#                  W[[l]] is the weight matrix linking layer l to layer l+1. 
#                  Initialize the elements with U (0, 0.2) random deviates.
#     - b        - a list of offset vectors. 
#                  b[[l]] is the offset vector linking layer l to layer l+1. 
#                  Initialize the elements with U (0, 0.2) random deviates.
#     - dh       - derivatives with respect to the nodes
#     - dW       - derivatives with respect to the weights
#     - db       - derivatives with respect to the offsets

backward <- function(nn, k){
  h <- nn$h
  W <- nn$W
  b <- NN$b
  dh <- list()
  dW <- list()
  db <- list()
  
  # We compute the derivative of the loss for k w.r.t. h[[L]][j]
  # Where L is the output layer and j is the jth node in the output layer
  L <- length(h[[length(h)]]) # number of nodes in the output layer
  for (j in range(L)){ # for each node in the output layer
    dh_L <- exp(h[[L]][j])/(sum(exp(h[[L]]))) # derivative of the loss for k w.r.t. h[[L]][j]
    
    if (j == k){
      dh[[L]][j] <- dh_L - 1
    } else {
      dh[[L]][j] <- dh_L
    }
    
  }
  
  
  
  return(list(h = h, W = w, b = b, dh = dh, dW = dW, db = db))
  }


#### Function:      train
# Description: 
# Inputs:       
#     - nn        - returned from forward
#     - inp       - 
#     - k 
#     - eta       - step size
#     - mb        - number of data to randomly sample to compute the gradient
#     - nstep     - number of optimization steps to take

# Outputs: 
#     - ?



train <- function(nn,inp,k,eta=.01,mb=10,nstep=10000) {

  }

















