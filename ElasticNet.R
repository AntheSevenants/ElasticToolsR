library(methods)
library(glmnet)
library(parallel)

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
                                         x.test = "matrix",
                                         x.train = "matrix"),
                                  methods = list(
                                    initialize = function(ds, feature_matrix, train_share=0.7) {
                                      ds <<- ds
                                      
                                      # Generate internal ids -> used for randomisation
                                      ds$df$'_id' <<- 1:nrow(ds$df)
                                      # Attach response variables
                                      ds$df$'_response' <<- ds$response_variables
                                      
                                      # Split the dataset into testing and training
                                      df_sample <- sample(c(TRUE, FALSE),
                                                           nrow(ds$df),
                                                           replace=TRUE,
                                                           prob=c(train_share, 1 - train_share))
                                      df.train <- ds$df[df_sample, ]
                                      df.test  <- ds$df[!df_sample, ]
                                      
                                      # Get the response variables from both datasets
                                      y.train <<- df.train$'_response'
                                      y.test <<- df.test$'_response'
                                      
                                      # Now comes the jank
                                      # We can identify the rows we need for the train and test matrices 
                                      # using the train and test ids.
                                      # So, we create empty train and test matrices, 
                                      # and then fill them one by one with the data with
                                      # the correct index from the complete feature matrix
                                      # Still with me? OK, cool
                                      
                                      # Training set
                                      x.train <<- apply(df.train, 1, function(row) {
                                        row_id <- as.numeric(row['_id'])
                                        return(feature_matrix[row_id,])
                                      })
                                      x.train <<- t(x.train)
                                      
                                      
                                      # Testing set
                                      x.test <<- apply(df.test, 1, function(row) {
                                        row_id <- as.numeric(row['_id'])
                                        return(feature_matrix[row_id,])
                                      })
                                      x.test <<- t(x.test)

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
                                    do_cross_validation = function(k=10) {
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
                                      if (dim(feature_matrix)[1] > 5000) {
                                        cores_to_use <- numCores / 2
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
                                    }
                                    ))
