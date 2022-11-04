library(methods)

elastic_net <- setRefClass("ElasticNet", fields = list(
                                         ds = "list",
                                         test_share = "double",
                                         y.test = "numeric",
                                         y.train = "numeric",
                                         x.test = "matrix",
                                         x.train = "matrix"),
                                  methods = list(
                                    initialize = function(ds, feature_matrix, test_share=0.7) {
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
                                      x.train <<- matrix(nrow = length(y.train),
                                                         ncol = dim(feature_matrix)[2])
                                      
                                      train_id <- 1
                                      for (id in df.train$'_id') {
                                        x.train[train_id,] <<- feature_matrix[id,]
                                        train_id <- train_id + 1
                                      }
                                      
                                      # Testing set
                                      x.test <<- matrix(nrow = length(y.test),
                                                        ncol = dim(feature_matrix)[2])
                                      
                                      test_id <- 1
                                      for (id in df.test$'_id') {
                                        x.test[test_id,] <<- feature_matrix[id,]
                                        test_id <- test_id + 1
                                      }
                                      
                                      # Checks OK, pass data to weird R constructor
                                      callSuper(ds=ds, test_share=test_share)
                                    }    
                                    ))