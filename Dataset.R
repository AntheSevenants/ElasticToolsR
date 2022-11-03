library(methods)

dataset <- setRefClass("Dataset", fields = list(
                                    df = "data.frame",
                                    response_variable_column = "character",
                                    to_binary_column = "character",
                                    context_features = "character",
                                    response_variables = "numeric"),
                                  methods = list(
                                    # check_column_exists
                                    check_column_exists = function(df, column) {
                                      if (!(column %in% names(df))) {
                                        stop(sprintf("Column '%s' is not part of the supplied dataframe.", column))
                                      }
                                    },
                                    
                                    # check_column_is_factor
                                    check_column_is_factor = function(df, column) {
                                      if (!is.factor(df[[column]])) {
                                        stop(sprintf("Column '%s' should be of type 'factor'", column))
                                      }
                                    },
                                    
                                    # check_column_is_binary
                                    check_column_is_binary = function(df, column) {
                                      if (length(unique(df[[column]])) != 2) {
                                        stop(sprintf("Column '%s' should only contain two unique values", column))
                                      }
                                    },
                                    
                                    # constructor
                                    initialize = function(df, response_variable_column, to_binary_column) {
                                      # We check for the response variable and to binary columns whether they are present
                                      check_column_exists(df, response_variable_column)
                                      check_column_exists(df, to_binary_column)
                                      
                                      # We check for the 'response variable' and 'to binary' columns whether they are factors
                                      check_column_is_factor(df, response_variable_column)
                                      check_column_is_factor(df, to_binary_column)
                                      
                                      # We check whether the response variable column is binary
                                      check_column_is_binary(df, response_variable_column)
                                      
                                      # Save the context features
                                      context_features <<- unique(as.character(df[[to_binary_column]]))
                                      
                                      # Save the response variables
                                      # We abuse the factor coding (1/2), and subtract one to get (0/1)
                                      response_variables <<- as.numeric(df[[response_variable_column]]) - 1
                                      
                                      # Checks OK, pass data to weird R constructor
                                      callSuper(df=df,
                                                response_variable_column=response_variable_column,
                                                to_binary_column=to_binary_column)
                                    },
                                    
                                    # as_matrix
                                    as_matrix = function() {
                                      context_feature_count <- length(context_features)
                                      
                                      # The total features consists of...
                                      # - the response variable
                                      # - the binary features
                                      # - the other columns (TODO)
                                      total_feature_count <- context_feature_count
                                      
                                      # Create the matrix
                                      # Size: dataframe rows X total feature count
                                      feature_matrix <- matrix(0, nrow(df), total_feature_count)
                                      
                                      # We go over each row and check what the value is for the 'to_binary' column
                                      for (row_index in 1:nrow(df)) {
                                        # Retrieve the row of this row index
                                        row <- df[row_index,]
                                        
                                        # We get the value of the column that we turn into multiple binary predictors
                                        to_binary_value <- row[[to_binary_column]]
                                        
                                        # We check what the index of this value is in the context features list
                                        # This index corresponds to the index of the column of this value in the matrix
                                        to_binary_index <- match(to_binary_value, context_features)
                                        
                                        # We then set the value for that column to 1 (= "this feature is present")
                                        feature_matrix[row_index, to_binary_index] <- 1
                                      }
                                      
                                      return(feature_matrix)
                                    },
                                    
                                    # as_feature_list
                                    as_feature_list = function() {
                                      return(context_features)
                                    }
                                  ))
