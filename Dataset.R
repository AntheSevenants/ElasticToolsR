library(methods)

dataset <- setRefClass("Dataset", fields = list(
                                    df = "data.frame",
                                    response_variable_column = "character",
                                    to_binary_column = "character"),
                                  methods = list(
                                    # check_column_exists
                                    check_column_exists = function(df, column) {
                                      if (!(column %in% names(df))) {
                                        stop(sprintf("Column '%s' is not part of the supplied dataframe.", column))
                                      }
                                    },
                                    
                                    # constructor
                                    initialize = function(df, response_variable_column, to_binary_column) {
                                      # We check for the response variable and to binary columns whether they are present
                                      check_column_exists(df, response_variable_column)
                                      check_column_exists(df, to_binary_column)
                                      
                                      # Checks OK, pass data to weird R constructor
                                      callSuper(df=df,
                                                response_variable_column=response_variable_column,
                                                to_binary_column=to_binary_column)
                                    }
                                  ))