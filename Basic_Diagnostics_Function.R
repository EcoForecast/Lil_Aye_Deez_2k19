library(rjags)

# function to run model diagnostic plots and return model with burn in removed

# inputs are the model parameter data and model predictive data

diagnostics <- function(model_out_params, model_out_pred, n.iter){
  
  # convergence diagnostics
  gelman.diag(model_out_params)
  GBR <- gelman.plot(model_out_params)
  
  burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2] > 1.1, 1, any)),1)+1]
  if(length(burnin) == 0){burnin = 1}
  
  total.iter <- dim(model_out_params[[1]])[1]
  burn.iter <- total.iter - burnin
  
  #remove burnin
  out.burn.params <- window(model_out_params,
                            start = burnin,
                            thin = floor(burn.iter/n.iter),
                            end = total.iter)
  
  out.burn.pred <- window(model_out_pred,
                          start = burnin,
                          thin = floor(burn.iter/n.iter),
                          end = total.iter)
  
  # convergence diagnostics after burnin
  #gelman.diag(out.burn.params)
  #GBR <- gelman.plot(out.burn.params)
  
  #plot model parameters
  #plot(out.burn.params)
  
  out.burn <- list()
  out.burn[[1]] = out.burn.params
  out.burn[[2]] = out.burn.pred
  names(out.burn) = c("params", "predict")
  
  return(out.burn)
  

  
  
}