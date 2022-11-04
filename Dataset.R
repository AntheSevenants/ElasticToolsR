library(methods)

dataset <- setRefClass("Dataset", fields = list(
                                    df = "data.frame",
                                    response_variable_column = "character",
                                    to_binary_column = "character",
                                    context_features = "character",
                                    response_variables = "numeric",
                                    other_columns = "list"),
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
                                    initialize = function(df, response_variable_column, to_binary_column, other_columns=list()) {
                                      if (missing(response_variable_column)) {
                                        return()
                                      }
                                      
                                      # We check for the response variable and to binary columns whether they are present
                                      check_column_exists(df, response_variable_column)
                                      check_column_exists(df, to_binary_column)
                                      
                                      # We check for the 'response variable' and 'to binary' columns whether they are factors
                                      check_column_is_factor(df, response_variable_column)
                                      check_column_is_factor(df, to_binary_column)
                                      
                                      # We check whether the response variable column is binary
                                      check_column_is_binary(df, response_variable_column)
                                      
                                      # We go over each column in the other_columns argument and check a few things
                                      for (other_column in other_columns) {
                                        check_column_exists(df, other_column)
                                        
                                        if (!(typeof(df[[other_column]]) %in% list("integer", "double"))) {
                                          stop(sprintf("Column '%s' should be a factor or numeric", other_column))
                                        }
                                      }
                                      
                                      # Save the context features
                                      context_features <<- unique(as.character(df[[to_binary_column]]))
                                      
                                      # Save the response variables
                                      # We abuse the factor coding (1/2), and subtract one to get (0/1)
                                      response_variables <<- as.numeric(df[[response_variable_column]]) - 1
                                      
                                      # Checks OK, pass data to weird R constructor
                                      callSuper(df=df,
                                                response_variable_column=response_variable_column,
                                                to_binary_column=to_binary_column,
                                                other_columns=other_columns)
                                      
                                      # Change the dataframe so it carries indices
                                      df$'_id' <<- 1:nrow(df)
                                    },
                                    
                                    # as_matrix
                                    as_matrix = function() {
                                      # What is the total number of features?
                                      context_feature_count <- length(context_features)
                                      other_columns_count <- length(other_columns)
                                      
                                      # The total features consists of...
                                      # - the binary features
                                      # - the other columns
                                      total_feature_count <- context_feature_count + other_columns_count
                                      
                                      
                                      # We go over each row and check what the value is for the 'to_binary' column
                                      feature_matrix <- apply(df, 1, function(row) { 
                                        # Retrieve the row index of this row
                                        row_index <- row[["_id"]]
                                        
                                        # We get the value of the column that we turn into multiple binary predictors
                                        to_binary_value <- row[[to_binary_column]]
                                        
                                        # We check what the index of this value is in the context features list
                                        # This index corresponds to the index of the column of this value in the matrix
                                        to_binary_index <- match(to_binary_value, context_features)
                                        
                                        # We create a new matrix row with the leangth of the feature count
                                        matrix_row <- double(total_feature_count)
                                        
                                        # We then set the value for the right column to 1 (= "this feature is present")
                                        matrix_row[to_binary_index] <- 1
                                        
                                        return(matrix_row)
                                      })
                                      
                                      # Turn the results into a feature matrix by transposing the results
                                      feature_matrix <- as.matrix(t(feature_matrix))
                                      
                                      list_index = 1
                                      # We also go over the "other columns", they have values too
                                      for (other_column in other_columns) {
                                        other_column_index = context_feature_count + list_index
                                        
                                        # If we are dealing with a binary variable, 
                                        # set the value for its column to 1 if we see the reference value
                                        if (is.factor(df[[other_column]])) { 
                                          feature_matrix[,other_column_index] <- (as.numeric(df[[other_column]]) - 1)
                                        } else {
                                          feature_matrix[,other_column_index] <- df[[other_column]]
                                        }
                                      }
                                      
                                      return(feature_matrix)
                                    },
                                    
                                    # as_feature_list
                                    as_feature_list = function() {
                                      # Does this do a deep copy? Let's find out
                                      feature_list <- c(list(), context_features)
                                      
                                      for (other_column in other_columns) {
                                        if (is.factor(df[[other_column]])) {
                                          feature_list <- append(feature_list, paste0("is_", levels(df[[other_column]])[-1]))
                                        } else {
                                          feature_list <- append(feature_list, other_column)
                                        }
                                      }
                                      
                                      return(feature_list)
                                    }
                                  ))
