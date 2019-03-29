library(rjags)

diagnostics <- function(model_out, burnin){
  
  # convergence diagnostics
  gelman.diag(model_out)
  GBR <- gelman.plot(model_out)
  
  #remove burnin
  out.burn <- window(model_out,start=burnin)
  
  # convergence diagnostics after burnin
  gelman.diag(out.burn)
  GBR <- gelman.plot(out.burn)
  
  #plot model parameters
  plot(out.burn)
  
  #summarize model parameters
  summary(out.burn)
  
  out.matrix <- as.matrix(out.burn)
  
  
}