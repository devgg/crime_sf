
create_neural_network <- function(layer_sizes, input_layer_col_names, output_layer_col_names) {
  network <- list()
  for (i in 1:(length(layer_sizes)-1)) {
    epsilon = sqrt(6) / sqrt(layer_sizes[i] + layer_sizes[i + 1])
    
    rows = layer_sizes[i + 1]
    cols = layer_sizes[i] + (if(i==1) 0 else 1)
    network[[i]] = matrix(runif(rows * cols, -epsilon, epsilon), rows, cols) 
  }
  colnames(network[[1]]) <- input_layer_col_names
  rownames(network[[length(network)]]) <- output_layer_col_names
  
  return(network)
}

forward_propagation <- function(network, records) {
  z <- list(t(apply(records, 1, function(record) network[[1]] %*% record)))
  a <- list(records, sigmoid(z[[1]]))
  
  for (i in 2:length(network)) {
    z[[i]] = t(apply(cbind(1, a[[i]]), 1, function(a_row) network[[i]] %*% a_row))
    a[[i + 1]] = sigmoid(z[[i]])
  }
  
  colnames(a[[length(a)]]) <- rownames(network[[length(network)]])
  
  return(list(z = z, a = a, h = a[[length(a)]]))
}

back_propagation <- function(network, forward_data, y) {
  y <- outer(y, 1:ncol(forward_data$h), "==") * 1
  
  delta3 = forward_data$h - y
  delta2 = (delta3 %*% network[[2]])[,-1] * sigmoid_gradient(forward_data$z[[1]])
  
  grad1 = matrix(0, nrow(network[[1]]), ncol(network[[1]])) 
  grad2 = matrix(0, nrow(network[[2]]), ncol(network[[2]])) 
  for(i in 1:nrow(y)) {
    grad1 = grad1 + outer(delta2[i,], forward_data$a[[1]][i,], "*")
    grad2 = grad2 + outer(delta3[i,], c(1, forward_data$a[[2]][i,]), "*")
  }
  
  return(list(grad1 / nrow(y), grad2 / nrow(y)))
}



batch_gradient_descent <- function(network, records_formated, iterations, alpha, momentum) {
  v_old <- list();
  v_new <- list();
  for (j in 1:iterations) {
    forward_data <- forward_propagation(network, records_formated$data)
    print(sprintf("%d: %f", j, calculate_cost(records_formated$y, forward_data$h)))
    gradients <- back_propagation(network, forward_data, records_formated$y)
    
    for (i in 1: length(network)) {
      v_new[[i]] = alpha * gradients[[i]]
      network[[i]] = network[[i]] - (v_new[[i]] + (if(j == 1) 0 else momentum * v_old[[i]]))
      v_old[[i]] = v_new[[i]]
    }
  }
  return(network)
}

mini_batch_gradient_descent <- function(network, records_formated, iterations, alpha, momentum, batch_size) {
  batch_indices <- c(1)
  
  while(batch_indices[length(batch_indices)] < nrow(records_formated[[1]])) {
    batch_indices[length(batch_indices) + 1] = min(batch_indices[length(batch_indices)] + batch_size, nrow(records_formated[[1]]))
  }
  
  
  v_old <- list();
  v_new <- list();
  for (j in 1:iterations) {
    for (b in 1:(length(batch_indices) - 1)) {
      forward_data <- forward_propagation(network, records_formated$data[batch_indices[b]:batch_indices[b+1],])
      print(sprintf("%d: %f", j, calculate_cost(records_formated$y[batch_indices[b]:batch_indices[b+1]], forward_data$h)))
      gradients <- back_propagation(network, forward_data, records_formated$y[batch_indices[b]:batch_indices[b+1]])
      
      for (i in 1: length(network)) {
        v_new[[i]] = alpha * gradients[[i]]
        network[[i]] = network[[i]] - (v_new[[i]] + (if(j == 1) 0 else momentum * v_old[[i]]))
        v_old[[i]] = v_new[[i]]
      }
    }
  }
  return(network)
}

calculate_cost <- function(y, h) {
  epsilon = 1e-15
  #h = matrix(sapply(h, function(h_row) min(1 - epsilon, max(epsilon, h_row))), nrow = nrow(h))    
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