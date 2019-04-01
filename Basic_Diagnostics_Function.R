library(rjags)

# function to run model diagnostic plots and return model with burn in removed

# inputs are the model parameter data and model predictive data

diagnostics <- function(model_out_params, model_out_pred, burnin){
  
  # convergence diagnostics
  gelman.diag(model_out_params)
  GBR <- gelman.plot(model_out_params)
  
  #remove burnin
  out.burn.params <- window(model_out_params,start=burnin)
  out.burn.pred <- window(model_out_pred, start=burnin)
  
  
  # convergence diagnostics after burnin
  gelman.diag(out.burn.params)
  GBR <- gelman.plot(out.burn.params)
  
  #plot model parameters
  plot(out.burn.params)
  
  out.burn <- list()
  out.burn[[1]] = out.burn.params
  out.burn[[2]] = out.burn.pred
  names(out.burn) = c("params", "predict")
  
  return(out.burn)
  

  
  
}