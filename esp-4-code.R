# Sophie Bata-Madden s2037949, Cameron Donnelly s2018520, Nala Krisnanda s2587290

#Contributions:
# Sophie:  33.3% 
# Cameron:  33.3%
# Nala: 33.3%

#### Code Overview:
# 
# 
# 

cross_entropy <- function(yhat, y) {
  k_true <- cbind(y, seq_along(y))
  return(-sum(log(yhat[k_true]))/length(y))
}

softmax <- function(mat) {
  exp_mat <- exp(mat)
  exp_mat_colsum <- colSums(exp_mat)
  return(t(t(exp_mat)/exp_mat_colsum))
}

relu <- function (mat){
  mat[which(mat<0)] <- 0
  return (mat)
}

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
    W[[l]] <- matrix(runif(d[l]*d[l+1],0,0.2), nrow=d[l+1], ncol=d[l])
  }
  
  # b is a list of offset vectors, b[[i]] is the offset vector linking layer i to layer i + 1
  # We initialise the elements with U(0,0.2) random deviates
  b <- list() 
  for (l in 1:(length(d)-1)){
    b[[l]] <- runif(d[l+1],0,0.2)
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

  h[[1]] <- t(inp)

  for(l in 1:(length(h)-1)){
    # calculate h_l+1 = weight * t(h_l)
    h[[l+1]] <- W[[l]] %*% h[[l]] + b[[l]]

    # # Add bias
    # h[[l+1]] <- sweep(h[[l+1]], 1, b[[l]])

    # Apply activation function
    ## if it is the last layer then we apply softmax activation function (eq.2)
    if(l+1 == length(h)){
      h[[l+1]] <- softmax(h[[l+1]])
    }
    ## if it is not the last layer we apply relu activation function (eq.1)
    else{
      h[[l+1]] <- relu(h[[l+1]])
    }
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
  # L_nodes <- length(h[[length(h)]]) # number of nodes in the output layer
  L <- length(h)
  # for (j in range(L_nodes)){ # for each node in the output layer
  #   dh_L <- exp(h[[L]][j])/(sum(exp(h[[L]]))) # derivative of the loss for k w.r.t. h[[L]][j]
    
  #   if (j == k){
  #     dh[[L]][j] <- dh_L - 1
  #   } else {
  #     dh[[L]][j] <- dh_L
  #   }
  # }

  # derivative of the output layer
  dh[[L]] <- softmax(h[[L]])

  k_true <- cbind(k, seq_along(k))
  dh[[L]][k_true] <- dh[[L]][k_true]-1
  
  for (j in (L-1):1){
    d <- dh[[j+1]]
    d[which(dh[[j+1]]<=0)] <- 0

    dh[[j]] <- t(W[[j]]) %*% d
    db[[j]] <- d
    dW[[j]] <- d %*% t(h[[j]])
  }
  
  return(list(h = h, W = W, b = b, dh = dh, dW = dW, db = db))
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
train <- function(nn, inp, k, eta=.1, mb=10, nstep=10000) {
  loss <- list()
  for (step in (1:nstep)) {
    inp_row <- sample(nrow(inp),mb)
    nn <- forward(nn, inp[inp_row, ])
    nn <- backward(nn, k[inp_row])
    for(i in (1:length(nn$W))){
      nn$W[[i]] <- nn$W[[i]] - eta * (nn$dW[[i]]/mb) 
      nn$b[[i]] <- nn$b[[i]] - eta * rowMeans(nn$db[[i]])
    }
    loss[step] <- cross_entropy(nn$h[[length(nn$h)]], k[inp_row])
    # print(cross_entropy(nn$h[[length(h)]], t(k[inp_row,])))
  }
 
  return(list(nn=nn, loss=loss))
}

predict <- function(nn, inp) {
  nn <- forward(nn, inp)
  output <- nn$h[[length(nn$h)]]
  return(as.vector(apply(output, 2, which.max)))
}

accuracy <- function(yhat, y) {
  return(sum(y==yhat)/length(y))
}

# Create a function to convert species labels to binary vectors
generate_iris_label <- function() {
  y <- iris[,5]
  y_bin <- matrix(rep(0, length(y)), nrow=length(y), ncol=1)
  y_bin[which(y=="setosa"),] <- 1
  y_bin[which(y=="versicolor"),] <- 2
  y_bin[which(y=="virginica"),] <- 3
  return(y_bin)
}

# Convert the `Species` variable to a matrix of binary representations
X <- iris[,(1:4)]
y <- generate_iris_label()

test_index <- seq(5, dim(X)[1], length=dim(X)[1]%/%5)
X_train <- X[-test_index,]
y_train <- y[-test_index,]

X_test <- X[test_index,]
y_test <- y[test_index,]

nn <- netup(c(4,8,7,3))
train <- train(nn, X_train, y_train)
plot(matrix(train$loss), type='l')

nn <- train$nn
yhat <- predict(nn, X_test)
print(accuracy(y_test, yhat))
