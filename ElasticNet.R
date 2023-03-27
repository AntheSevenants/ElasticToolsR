library(methods)
library(glmnet)
library(parallel)
library(Matrix)

# We'll need this
logit2p <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

parallel = FALSE
if (Sys.info()['sysname'] != "Windows") {
  library(doMC)
  numCores <- detectCores()
  # Then, register the multicore worker
  registerDoMC(cores = numCores)
  parallel = TRUE  
}

elastic_net <- setRefClass("ElasticNet", fields = list(
                                         ds = "Dataset",
                                         y = "numeric",
                                         x = "dgCMatrix",
                                         type.measure = "character"),
                                  methods = list(
                                    initialize = function(ds, nfolds=10,
                                                          type.measure="deviance") {
                                      ds <<- ds
                                      
                                      x <<- ds$as_matrix()
                                      y <<- ds$response_variables
                                      type.measure <<- type.measure
                                      
                                      ds$df$'foldid' <<- sample(1:nfolds,
                                                            size = length(y),
                                                            replace = TRUE)
                                    },
                                    do_elastic_net_regression = function(alpha=0.5) {
                                      fit <- cv.glmnet(x,
                                                       y,
                                                       type.measure=type.measure,
                                                       alpha=alpha,
                                                       family="binomial",
                                                       foldid=ds$df$'foldid',
                                                       parallel=parallel)
                                      
                                      return(fit)
                                    },
                                    do_ridge_regression = function() {
                                      return(do_elastic_net_regression(alpha=0))
                                    },
                                    do_lasso_regression = function() {
                                      return(do_elastic_net_regression(alpha=1))
                                    },
                                    do_elastic_net_regression_auto_alpha = function(k=10) {
                                      # Define the range of possible ks
                                      alpha_values <- lapply(0:k, function(i) { return(i/k) })
                                      alpha_values <- as.vector(alpha_values)

                                      # Actually compute the regression fits
                                      # We test k times, each time with a different alpha value
                                      # We save all results and then decide which model produces the best fit
                                      # Alpha will be 'i / k', so 0/k, 1/k ... k/k
                                      # To save on time, we do this using MULTI CORE PROCESSING !!!! gotta go fast
                                      # We give the parameter to our internal elastic net function
                                      # This will compute a model for this alpha value
                                      # The fit is saved in our list of fits
                                      regression_fits <- lapply(alpha_values,
                                        function(alpha) { 
                                          print(paste("Calculating for alpha =", alpha))
                                          return(do_elastic_net_regression(alpha=alpha)) })
                                      
                                      # For each fit, we check get the index of the 1se lambda
                                      # It's a good practice to pick this lambda
                                      l1se_indices <- sapply(regression_fits,
                                          function(fit) { fit$index["1se",] })
                                      
                                      # Then also collect all losses and other information
                                      losses <- sapply(1:length(alpha_values),
                                        function(index) {
                                          l1se_index = l1se_indices[[index]]
                                          return(regression_fits[[index]]$cvm[[l1se_index]])
                                      })
                                      
                                      nzeroes <- sapply(1:length(alpha_values),
                                        function(index) {
                                          l1se_index = l1se_indices[[index]]
                                          return(regression_fits[[index]]$nzero[[l1se_index]])
                                      })
                                      
                                      lambdas <- sapply(regression_fits,
                                        function(fit) {
                                          return(fit$lambda.1se)
                                      })
                                      
                                      intercepts <- sapply(1:length(alpha_values),
                                        function(index) {
                                          l1se_index = l1se_indices[[index]]
                                          return(regression_fits[[index]]$glmnet.fit$a0[[l1se_index]])
                                      })
                                      
                                      dev.ratios <- sapply(1:length(alpha_values),
                                        function(index) {
                                          l1se_index = l1se_indices[[index]]
                                          return(regression_fits[[index]]$glmnet.fit$dev.ratio[[l1se_index]])
                                      })
                                      
                                      # Create a data frame
                                      results <- data.frame(
                                         `_id` = 1:length(alpha_values),
                                         alpha=as.double(alpha_values),
                                         loss=as.double(losses),
                                         intercept=as.double(intercepts),
                                         dev.ratio=as.double(dev.ratios),
                                         nzero=as.numeric(nzeroes),
                                         lambda=as.double(lambdas))
                                      
                                      
                                      output <- list("results" = results,
                                                     "fits" = regression_fits)
                                      
                                      return(output)
                                    },
                                    attach_coefficients = function(fit) {
                                      coefficients <- as.vector(coef(fit)[,1])[-1]
                                      
                                      output <- data.frame(coefficient=as.double(coefficients),
                                                           feature=as.character(ds$as_feature_list()))
                                      
                                      output <- output[order(output$coefficient),]
                                      
                                      return(output)
                                    },
                                    get_coupled_information = function(output, from_column, to_column) {
                                      apply(output, 1, function(row) {
                                        row <- ds$df[ds$df[from_column] == row[["feature"]],][1,]
                                        return(row[[to_column]])
                                      })
                                    }
                                    ))
