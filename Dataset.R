library(methods)

dataset <- setRefClass("Dataset", fields = list(
                                    df = "data.frame",
                                    response_variable_column = "character",
                                    to_binary_column = "character",
                                    context_features = "character"),
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
                                      
                                      # Checks OK, pass data to weird R constructor
                                      callSuper(df=df,
                                                response_variable_column=response_variable_column,
                                                to_binary_column=to_binary_column)
                                    }
                                  ))