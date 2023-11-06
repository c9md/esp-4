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
#                  Initialize theelements with U (0, 0.2) random deviates.



netup <- function(d){
h <- null
W <- null
b <- null

return(list(h = h, W = w, b = b))
            
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
#                  Initialize theelements with U (0, 0.2) random deviates.

forward <- function(nn, inp){
h <- null
W <- null
b <- null
  
return(list(h = h, W = w, b = b))
  }



#### Function:      backward
# Description: 
# Inputs:       
#     - nn        - returned from forward
#     - k         - 

# Outputs: 
#     - h        - a list of nodes for each layer. 
#                  h[[l]] should be a vector of length d[l] which will contain the node values for layer l.
#     - W        - a list of weight matrices. 
#                  W[[l]] is the weight matrix linking layer l to layer l+1. 
#                  Initialize the elements with U (0, 0.2) random deviates.
#     - b        - a list of offset vectors. 
#                  b[[l]] is the offset vector linking layer l to layer l+1. 
#                  Initialize theelements with U (0, 0.2) random deviates.
#     - dh       - derivatives with respect to the nodes
#     - dW       - derivatives with respect to the weights
#     - db       - derivatives with respect to the offsets

backward <- function(nn, k){
h <- null
W <- null
b <- null
dh <- null
dW <- null
db <- null
  
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

















