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
  b <- nn$b
  dh <- h
  dW <- W
  db <- b
  
  # We compute the derivative of the loss for k w.r.t. h[[L]][j]
  # Where L is the output layer and j is the jth node in the output layer
  L_nodes <- length(h[[length(h)]]) # number of nodes in the output layer
  L <- length(h)
  for (j in range(L_nodes)){ # for each node in the output layer
    dh_L <- exp(h[[L]][j])/(sum(exp(h[[L]]))) # derivative of the loss for k w.r.t. h[[L]][j]
    
    if (j == k){
      dh[[L]][j] <- dh_L - 1
    } else {
      dh[[L]][j] <- dh_L
    }
  }
  
  for (j in (L-1):1){
    d <- dh[[j+1]]
    d[which(dh[[j+1]]<=0)] <- 0

    dh[[j]] <- t(W[[j]]) %*% d    
    db[[j]] <- d
    dW[[j]] <- d %*% t(h[[j]])
  }
  
  return(list(h = h, W = w, b = b, dh = dh, dW = dW, db = db))
  }


#### Function:      train
# Description: Train the network nn given the imput data in the rows of matrix inp and the corresponding class labels in k.
# Inputs:       
#     - nn        - returned from forward
#     - inp       - matrix of input data, one row per data point
#     - k         - vector of the output classes of the input data
#     - eta       - step size
#     - mb        - number of data to randomly sample to compute the gradient
#     - nstep     - number of optimization steps to take

# Outputs: the trained netword nn
#    - h        - a list of nodes for each layer.
#                 h[[l]] should be a vector of length d[l] which will contain the node values for layer l.
#    - W        - a list of weight matrices.
#                 W[[l]] is the weight matrix linking layer l to layer l+1.
#    - b       - a list of offset vectors.



# We want to train a schocastic gradient descent
# We want to randomly sample mb data points from inp
# We want to compute the gradient for each of these mb data points
# we use forward and backward to compute the gradient
train <- function(nn,inp,k,eta=.01,mb=10,nstep=10000) {
  h <- nn$h
  W <- nn$W
  b <- nn$b
  
  # Initialise lists to store the gradients, to compute the averages
  dW_avg <- list()
  db_avg <- list()
  dh_avg <- list()
  
  # We want to randomly sample mb data points from inp 
  # We do this nstep times
  for (step in nstep) {
    
    # Compute the gradients for each of the mb data points and compute their average
      for (i in mb){
      # Sample a data point from inp
      inp_row <- sample(nrow(inp),1)
      
      # Compute the gradient for this data point
      nn <- forward(nn, inp[inp_row])
      h <- nn$h
      W <- nn$W
      b <- nn$b
      
      nn <- backward(nn, k[inp_row])
      # Add the gradients to the lists that store the gradients
      dW_avg[[i]] <- nn$dW
      db_avg[[i]] <- nn$db
      dh_avg[[i]] <- nn$dh
    } 
     # Compute the average of the mb gradients 
    # i.e. dW_avg_val should be the average of the mb dW values, and have the same dimensions as dW
    # ensure this is the correct way to compute the average
    dW_avg_val <- apply(dW_avg, 1, mean)
    db_avg_val <- apply(db_avg, 1, mean)
    dh_avg_val <- apply(dh_avg, 1, mean)
     
     # then update the weights and offsets
    W <- W - eta * dW_avg_val
    b <- b - eta * db_avg_val
    
    # 
    # update nn with these new values
    nn$W <- W
    nn$b <- b
  }

  return(list(h = nn$h, W = W, b = b))
}
 

## TO DO LIST:

# Implement train function
  # ensure avg is computed properly

# Check backward derivative using finite differences (section 14.6) 
  # This is manually calculating the derivative using a tiny difference

# Train the network on the Iris data set:
  # Split the data into training and test sets
    # To do this, we take every 5th row from the data set and put it into the test set
  # Train the network on the training set

# Write code to classify the test data to species according to the class predicted as the most likely by the network
# Compute the misclassification rate for the test set 

# Find a seed to use in which the training has worked and the loss is substantially reduced from pre to post training

# Ensure the code is well commented

# Make any efficiency changes if time allows







