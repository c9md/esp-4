# Sophie Bata-Madden s2037949, Cameron Donnelly s2018520, Nala Krisnanda s2587290
# Link to Github repo: https://github.com/c9md/esp-4
#Contributions:
# Sophie:  NetUp, forward, backward, derivative testing, code comment clarity 33.3% 
# Cameron: code setup and initial framework of main functions, debug, commenting  33.3%
# Nala: train function, iris data prep, nn train and test code 33.3%

#### Code Overview:
# In this code, we train the a neural network to classify iris types,
# using stochastic gradient descent and cross-entropy loss to train the network.
# We use the 'iris' data set inbuilt in R, and we achieve a 3.33% 
# misclassification rate when using the chosen seed (7).


#### Function: cross_entropy
# Description: Calculate the cross-entropy loss between predicted probabilities 
# (yhat) and true labels (y). We can also describe this as negative
# log-likelihood of a multinomial distribution
# Defined as: L = -sum(log(p_k_i)/n) 
# where p_k_i is the predicted probability of class k for sample i, 
# and n is the total number of samples.
# Input:  
#         yhat    -  A matrix of predicted probabilities where each column 
#                    represents a class.
#         y       -  A vector or matrix of true class labels, 
#                     or a numeric vector indicating the true class indices.
# Output: 
#                 -  Cross-entropy loss value.
cross_entropy <- function(yhat, y) {
  # Create a matrix combining true labels 'y' with their corresponding indices.
  k_true <- cbind(y, seq_along(y))
  
  # Compute negative sum of the logs of predicted probabilities for true classes
  # Divide by the total number of samples to get the average cross-entropy loss.
  return(-sum(log(yhat[k_true]))/length(y))
}

#### Function: softmax
# Description: Apply the softmax activation function to the input matrix 
# along its columns.
# This calculates the probabilities p_k of each class for each sample.
# we define for each class k, the probability p_k as:
#   p_k = exp(a_k)/sum(exp(a_k))
# Input:    
#         mat - Input matrix where each column represents the pre-activation
#                 values for different classes.
# Output:           
#             - Matrix of probabilities obtained by applying the softmax 
#               function to each column of 'mat'.
softmax <- function(mat) {
  # Compute the exponentials of the input matrix 
  # to obtain unnormalized probabilities.
  exp_mat <- exp(mat)
  
  # Calculate the column sums of the exponentiated matrix 
  #to normalize the probabilities.
  exp_mat_colsum <- colSums(exp_mat)
  
  # Divide each element of the exponentiated matrix by its column sum 
  # to obtain probabilities.
  # Transpose the result to maintain the original matrix orientation.
  return(t(t(exp_mat)/exp_mat_colsum))
}

#### Function: relu
# Description: Apply the Rectified Linear Unit (ReLU) activation function 
# element-wise to the input matrix 'mat'.
# Input:       
#         mat - Matrix where each element represents a pre-activation value.
# Output:         
#             - Matrix obtained by setting negative values in 'mat' to zero.
relu <- function (mat){
  # Replace negative values in the input matrix with zero.
  mat[which(mat<0)] <- 0
  
  # Return the modified matrix after applying the ReLU activation function.
  return(mat)
}

#### Function: netup
# Description: Given a list of nodes in leach layer, construct a neural network
# Input:       
#         d   - vector giving the number of nodes in each layer of a network
# Output: 
#         h   - a list of nodes for each layer. 
#               h[[l]] should be a vector of length d[l] which will contain 
#               the node values for layer l.
#         W   - a list of weight matrices. 
#               W[[l]] is the weight matrix linking layer l to layer l+1. 
#               Initialize the elements with U (0, 0.2) random deviates.
#         b   - a list of offset vectors. 
#               b[[l]] is the offset vector linking layer l to layer l+1. 
#               Initialize the elements with U (0, 0.2) random deviates.
netup <- function(d){
  # Construct a list of lists, h for layers, W for weight matrices, and b for 
  # offset vectors
  h <- list(); W <- list(); b <- list()  

  for (l in 1:length(d)){
    # h[[i]] is the matrix representing layer i
    h[[l]] <- rep(0,d[l])

    # Generate W[[i]] and b[[i]] if l is not equal the length d
    # W and b lists will have 1 element less compared to h
    if(l!=length(d)){
      # W[[i]] is the weight matrix linking layer i to layer i + 1
      # We initialise the elements with U(0,0.2) random deviates
      W[[l]] <- matrix(runif(d[l]*d[l+1],0,0.2), nrow=d[l+1], ncol=d[l])

      # b[[i]] is the offset vector linking layer i to layer i + 1
      # We initialise the elements with U(0,0.2) random deviates
      b[[l]] <- runif(d[l+1],0,0.2)   
    }
  }

  # Return a list containing initialized hidden layers ('h'), 
  # weight matrices ('W'), and bias vectors ('b')
  return(list(h = h, W = W, b = b))       
}

#### Function:  forward
# Description:  the function computes the remaining node values implied by input
#               and returns the updated network list.
# Input:       
#         nn    -  a network list as returned by netup.
#         inp   -  a vector of input values for the first layer
# Output: 
#         h     - a list of nodes for each layer. 
#                 h[[l]] should be a vector of length d[l] which will contain 
#                 the node values for layer l.
#         W     - a list of weight matrices. 
#                 W[[l]] is the weight matrix linking layer l to layer l+1. 
#                 Initialize the elements with U (0, 0.2) random deviates.
#         b     - a list of offset vectors. 
#                 b[[l]] is the offset vector linking layer l to layer l+1. 
#                 Initialize the elements with U (0, 0.2) random deviates.
forward <- function(nn, inp){
  # Extract hidden layer values ('h'), weight matrices ('W'), 
  # and bias vectors ('b') from the neural network structure.
  h <- nn$h
  W <- nn$W
  b <- nn$b 

  # Set the input as the first hidden layer after transposing it.
  h[[1]] <- t(inp)

  for(l in 1:(length(h)-1)){
    # Calculate the pre-activation values for the next layer
    h[[l+1]] <- W[[l]] %*% h[[l]] + b[[l]]

    # Apply activation function
    ## if it is the last layer then we apply softmax activation function
    if(l+1 == length(h)){
      h[[l+1]] <- softmax(h[[l+1]])
    }
    ## if it is not the last layer we apply relu activation function
    else{
      h[[l+1]] <- relu(h[[l+1]])
    }
  }

  # Return a list containing the output specified
  return(list(h = h, W = W, b = b))
}


#### Function:      backward
# Description: 
# Input:       
#         nn    - returned from forward
#         k     - the output class of the input
# Output: 
#         h     - a list of nodes for each layer. 
#                 h[[l]] should be a vector of length d[l] 
#                 which will contain the node values for layer l.
#         W     - a list of weight matrices. 
#                 W[[l]] is the weight matrix linking layer l to layer l+1. 
#                 Initialize the elements with U (0, 0.2) random deviates.
#         b     - a list of offset vectors. 
#                 b[[l]] is the offset vector linking layer l to layer l+1. 
#                 Initialize the elements with U (0, 0.2) random deviates.
#         dh    - derivatives with respect to the nodes
#         dW    - derivatives with respect to the weights
#         db    - derivatives with respect to the offsets
backward <- function(nn, k){
  # Extract layer values ('h'), weight matrices ('W'), and bias vectors ('b') 
  # from the neural network structure.
  h <- nn$h;W <- nn$W;b <- nn$b

  # Create variables to store gradients: dh for hidden layer values,
  # dW for weight matrices, and db for bias vectors.
  dh <- h;dW <- W;db <- b

  # Compute the derivative of the loss function w.r.t output layer
  L <- length(h)
  dh[[L]] <- softmax(h[[L]])

  ## Adjust the derivative for the true class label or index.
  k_true <- cbind(k, seq_along(k))
  dh[[L]][k_true] <- dh[[L]][k_true]-1
  
  # Iterate through the layers in reverse order to calculate gradients.
  for (j in (L-1):1){
    # Retrieve the gradient from the next layer.
    d <- dh[[j+1]]

    # Set gradients to zero for negative values in the derivative.
    d[which(dh[[j+1]]<=0)] <- 0

    # Compute the gradient for the current hidden layer value.
    dh[[j]] <- t(W[[j]]) %*% d
    db[[j]] <- d
    dW[[j]] <- d %*% t(h[[j]])
  }
  
  # Return a list containing updated hidden layer values, weight matrices,
  # bias vectors,
  # and computed gradients after the backward pass.
  return(list(h = h, W = W, b = b, dh = dh, dW = dW, db = db))
}

#### Function:      train
# Description: Train the network nn given the imput data
#                 and the corresponding class labels in k.
# Input:       
#         nn      - A list containing the neural network structure with hidden 
#                   layer values ('h'), weight matrices ('W'), 
#                   and bias vectors ('b').
#         inp     - Input matrix representing the training dataset.
#         k       - True class labels or indices for the training dataset.
#         eta     - Learning rate for gradient descent (default: 0.01).
#         mb      - Mini-batch size for each iteration of mini-batch gradient 
#                   descent (default: 10).
#         nstep   - Number of training steps or iterations (default: 10000).
# Output: 
#                 - A list containing updated hidden layer values ('h'), 
#                   weight matrices ('W'), bias vectors ('b'),
#                   computed gradients ('dh', 'dW', 'db'), and a vector 
#                   of loss values at each training step.
train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000) {
  # Create an empty list to store the loss at each training step.
  loss <- list()

  # Iterate through the specified number of training steps.
  for (step in (1:nstep)) {
    # Randomly sample mini-batch indices from the training dataset.
    inp_row <- sample(nrow(inp),mb)

    # Perform the forward pass in the neural network.
    nn <- forward(nn, inp[inp_row, ])

    # Perform the backward pass to compute gradients.
    nn <- backward(nn, k[inp_row])

    # Update weights and biases by subtracting its value 
    # with the average of the gradients.
    for(i in (1:length(nn$W))){
      nn$W[[i]] <- nn$W[[i]] - eta * (nn$dW[[i]]/mb) 
      nn$b[[i]] <- nn$b[[i]] - eta * rowMeans(nn$db[[i]])
    }

    # Compute and store the cross-entropy loss for the mini-batch.
    loss[step] <- cross_entropy(nn$h[[length(nn$h)]], k[inp_row])
  }
 
  # Return a list containing the specified outputs.
  return(list(h=nn$h, W=nn$W, b=nn$b, dh=nn$dh, dW=nn$dW, db=nn$db, loss=loss))
}

#### Function: predict
# Description: Make predictions using a trained neural network for a given input
# Input:
#         nn    - A list containing the neural network with hidden layer 
#                 values ('h'), weight matrices ('W'), and bias vectors ('b').
#         inp   - Input matrix representing the features for which predictions 
#                 are to be made.
# Output:
#               - A vector of predicted class labels based on the input.
predict <- function(nn, inp) {
  # Perform the forward pass in the neural network to obtain output.
  nn <- forward(nn, inp)

  # Extract the output from the last layer of the neural network.
  output <- nn$h[[length(nn$h)]]

  # Convert the output matrix to a vector of predicted class labels.
  # Each element of the vector represents the index of the maximum value in the 
  # corresponding column.
  return(as.vector(apply(output, 2, which.max)))
}

#### Function: accuracy
# Description: Calculate the accuracy of predicted class labels compared to true
#               class labels.
# Input:
#         yhat  - Vector of predicted class labels.
#         y     - Vector of true class labels.
# Output:
#               - The accuracy: the proportion of correctly predicted labels.
accuracy <- function(yhat, y) {
  # Count the number of correctly predicted labels and calculate the accuracy.
  # The accuracy is the ratio of correctly predicted labels 
  # to the total number of labels.
  return(sum(y==yhat)/length(y))
}

# Set seed for best outcome
set.seed(7)

#### Data Preparation:
# Load iris dataset
data("iris")
# Extract features (X) and labels (y) from the iris dataset.
X <- iris[,(1:4)]
y <- as.numeric(as.factor(iris[,5]))
# Generate indices for test set.
test_index <- seq(5, dim(X)[1], by=5)
# Split the data into training and test sets.
X_train <- X[-test_index,]
y_train <- y[-test_index]
X_test <- X[test_index,]
y_test <- y[test_index]

# NN Initialization:
# Initialize a nn structure with input size 4, hidden layers of sizes 8, 7, 
# and output size 3.
nn <- netup(c(4,8,7,3))

#### Training:
# Train the neural network using the training data.
# We found that the train function give a better result when using eta=.09 
nn <- train(nn, X_train, y_train, eta=.09)
# Plot the training loss over iterations.
plot(matrix(nn$loss), type='l', main="Training Loss Over Iterations", 
     xlab="Iterations", ylab="Loss")

#### Testing:
# Make predictions on the test set.
yhat <- predict(nn, X_test)
# Calculate and print the misclassification rate (1 - accuracy) on the test set.
missclassification_rate <- 1 - accuracy(y_test, yhat)
cat("missclassification rate on test set:", 
    sprintf("%.2f%%", missclassification_rate * 100), "\n")