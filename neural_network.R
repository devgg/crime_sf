
create_neural_network <- function(layer_sizes) {
  neural_network <- list()
  for (i in 1:(length(layer_sizes)-1)) {
    epsilon = sqrt(6) / sqrt(layer_sizes[i] + layer_sizes[i + 1])
    
    rows = layer_sizes[i + 1]
    cols = layer_sizes[i] + (if(i==1) 0 else 1)
    neural_network[[i]] = matrix(runif(rows * cols, -epsilon, epsilon), rows, cols) 
  }
  return(neural_network)
}

forward_propagation <- function(network, records) {
  z <- list(t(apply(records, 1, function(record) network[[1]] %*% record)))
  a <- list(sigmoid(z[[1]]))
  
  for (i in 2:length(network)) {
    z[[i]] = t(apply(cbind(1, a[[i - 1]]), 1, function(a_row) network[[i]] %*% a_row))
    a[[i]] = sigmoid(z[[i]])
  }
  
  return(list(z = z, a = a, h = a[[length(a)]]))
}

back_propagation <- function(network, foward_data, y) {
  y <- outer(y, 1:ncol(foward_data$h), "==") * 1
  
  delta3 = foward_data$h - y
  delta2 = (delta3 %*% network[[2]])[,-1] * sigmoid_gradient(forward_data$z[[1]])
  
  grad1 = matrix(0, nrow(network[[1]]), ncol(network[[1]])) 
  grad2 = matrix(0, nrow(network[[2]]), ncol(network[[2]])) 
  for(i in 1:nrow(y)) {
    grad1 = grad1 + outer(delta2[i,], c(1, foward_data$a[[1]][i,]), "*")
    grad2 = grad2 + outer(delta3[i,], c(1, foward_data$a[[1]][i,]), "*")
  }
  
  return(list(grad1 / nrow(y), grad2 / nrow(y)))
}







calculate_cost <- function(y, h) {
  y <- outer(y, 1:ncol(h), "==") * 1
  return(sum(-y * log(h) - (1 - y) * log(1 - h)) / nrow(h))
}

sigmoid <- function(value) {
  return(1 / (1 + exp(-value)))
}

sigmoid_gradient <- function(value) {
  value_sigmoid = sigmoid(value)
  return(value_sigmoid * (1 - value_sigmoid))
}