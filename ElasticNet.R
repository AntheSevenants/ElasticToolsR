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

if (Sys.info()['sysname'] == "Windows") {
  numCores <- 1
} else {
  numCores <- detectCores()
  numCores
}


elastic_net <- setRefClass("ElasticNet", fields = list(
                                         ds = "Dataset",
                                         train_share = "numeric",
                                         y.test = "numeric",
                                         y.train = "numeric",
                                         x.test = "dgCMatrix",
                                         x.train = "dgCMatrix"),
                                  methods = list(
                                    initialize = function(ds, train_share=0.7) {
                                      ds <<- ds
                                      
                                      sparse_coords <- ds$get_sparse_coords()
                                      
                                      # Generate internal ids -> used for randomisation
                                      # Attach response variables
                                      df_shim <- data.frame("id" = 1:nrow(ds$df),
                                                            "response" = ds$response_variables)
                                      
                                      # Split the dataset into testing and training
                                      in_sample <- sample(c(TRUE, FALSE),
                                                           nrow(df_shim),
                                                           replace=TRUE,
                                                           prob=c(train_share, 1 - train_share))
                                      df.train <- df_shim[in_sample, ]
                                      df.test  <- df_shim[!in_sample, ]
                                      
                                      # Get the response variables from both datasets
                                      y.train <<- df.train$'response'
                                      y.test <<- df.test$'response'
                                      
                                      # Now, we look at the df.train and df.test data frames
                                      # We only retrain the rows from the sparse coordinates 
                                      # for which the x column values are part of the data frame
                                      x.train_ <- subset(sparse_coords,
                                                        x %in% df.train$`id`)
                                      x.test_ <- subset(sparse_coords,
                                                       x %in% df.test$`id`)
                                      
                                      # Then, convert the data frames to actual sparse matrices
                                      x.train <<- sparseMatrix(x.train_$x, x.train_$y, x=x.train_$value)
                                      x.test <<- sparseMatrix(x.test_$x, x.test_$y, x=x.test_$value)
                                      
                                      # Remove empty rows
                                      x.train <<- x.train[tabulate(summary(x.train)$i) > 0, , drop = FALSE]
                                      x.test <<- x.test[tabulate(summary(x.test)$i) > 0, , drop = FALSE]
                                      
                                      # Simple and easy!
                                      
                                      train_share <<- train_share
                                    },
                                    do_elastic_net_regression = function(alpha=0.5) {
                                      fit <- cv.glmnet(x.train,
                                                       y.train,
                                                       type.measure="deviance",
                                                       alpha=alpha,
                                                       family="binomial")
                                      
                                      return(fit)
                                    },
                                    do_ridge_regression = function() {
                                      return(do_elastic_net_regression(alpha=0))
                                    },
                                    do_lasso_regression = function() {
                                      return(do_elastic_net_regression(alpha=1))
                                    },
                                    do_cross_validation = function(k=10, cores_to_use=NA) {
                                      # Define the range of possible ks
                                      alpha_values <- lapply(0:k, function(i) { return(i/k) })
                                      alpha_values <- as.vector(alpha_values)
                                      # Pre-generate all regression fit names
                                      fit_names <- lapply(alpha_values, function(alpha) { return(paste0("alpha", alpha)) })
                                      # Actually compute the regression fits
                                      # We test k times, each time with a different alpha value
                                      # We save all results and then decide which model produces the best fit
                                      # Alpha will be 'i / k', so 0/k, 1/k ... k/k
                                      # To save on time, we do this using MULTI CORE PROCESSING !!!! gotta go fast
                                      # We give the parameter to our internal elastic net function
                                      # This will compute a model for this alpha value
                                      # The fit is saved in our list of fits
                                      
                                      # If the feature matrix is very large, we get OOM issues
                                      # So, decide on cores dynamically
                                      if (is.na(cores_to_use)) {
                                        cores_to_use <- numCores
                                        if (dim(x.train)[1] > 5000) {
                                          cores_to_use <- max(1, numCores / 2)
                                      }
                                      
                                      regression_fits <- mclapply(alpha_values,
                                                                  function(alpha) { 
                                                                    return(do_elastic_net_regression(alpha=alpha)) },
                                                                  mc.cores = cores_to_use)
                                      
                                      # Now we want to find out which model actually performs the best
                                      # To do this, we ask the model to predict the values given the predictors
                                      # Then, we choose the model which gives the best predictions
                                      losses <- lapply(1:length(alpha_values), function(i) {
                                        test.predict <- predict(regression_fits[[i]],
                                                                s=regression_fits[[i]]$lambda.1se,
                                                                newx=x.test)
                                        
                                        # Convert logits to probabilities
                                        test.predict <- logit2p(test.predict)
                                        
                                        # I decided to use cross entropy loss, since we're using a binary
                                        # classification task. Thanks to Sien Moens for teaching me this 
                                        # in the NLP course!
                                        loss <- sum(-(y.test * log(test.predict)))
                                        
                                        return(loss)
                                      })
                                      
                                      # Create a data frame
                                      results <- data.frame(
                                        `_id` = 1:length(fit_names),
                                        alpha_value=as.double(alpha_values),
                                        fit_name=as.character(fit_names),
                                        loss=as.double(losses))
                                      
                                      
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
                                    get_coupled_information = function(output, to_column) {
                                      apply(output, 1, function(row) {
                                        row <- ds$df[ds$df[ds$to_binary_column] == row[["feature"]],][1,]
                                        return(row[[to_column]])
                                      })
                                    }
                                    ))
