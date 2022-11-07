library(methods)
library(glmnet)

elastic_net <- setRefClass("ElasticNet", fields = list(
                                         ds = "Dataset",
                                         test_share = "numeric",
                                         y.test = "numeric",
                                         y.train = "numeric",
                                         x.test = "matrix",
                                         x.train = "matrix"),
                                  methods = list(
                                    initialize = function(ds, feature_matrix, test_share=0.7) {
                                      ds <<- ds
                                      
                                      # Generate internal ids -> used for randomisation
                                      ds$df$'_id' <<- 1:nrow(ds$df)
                                      # Attach response variables
                                      ds$df$'_response' <<- ds$response_variables
                                      
                                      # Split the dataset into testing and training
                                      df_sample <- sample(c(TRUE, FALSE),
                                                           nrow(ds$df),
                                                           replace=TRUE,
                                                           prob=c(test_share, 1 - test_share))
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

                                      test_share <<- test_share
                                      
                                      # Checks OK, pass data to weird R constructor
                                      #callSuper(ds=ds, test_share=test_share)
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
                                    }
                                    attach_coefficients = function(fit) {
                                      coefficients <- as.vector(coef(fit)[,1])[-1]
                                      
                                      output <- arrange(data.frame(coefficients,
                                                                   ds$context_features),
                                                        coefficients)
                                      
                                      return(output)
                                    }
                                    ))