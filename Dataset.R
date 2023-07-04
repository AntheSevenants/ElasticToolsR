library(methods)
library(Matrix)

dataset <- setRefClass("Dataset", fields = list(
                                    df = "data.frame",
                                    response_variable_column = "character",
                                    to_binary_columns = "character",
                                    context_features = "character",
                                    response_variables = "numeric",
                                    other_columns = "character"),
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
                                        stop(sprintf("Column '%s' should contain exactly two unique values", column))
                                      }
                                    },
                                    
                                    # constructor
                                    initialize = function(df, response_variable_column, to_binary_columns, other_columns=character()) {
                                      if (missing(response_variable_column)) {
                                        return()
                                      }
                                      
                                      # We check for the response variable and to binary columns whether they are present
                                      check_column_exists(df, response_variable_column)
                                      for (to_binary_column in to_binary_columns) {
                                        check_column_exists(df, to_binary_column)
                                      }
                                      
                                      # We check for the 'response variable' and 'to binary' columns whether they are factors
                                      check_column_is_factor(df, response_variable_column)
                                      for (to_binary_column in to_binary_columns) {
                                        check_column_is_factor(df, to_binary_column)
                                      }
                                      
                                      # We check whether the response variable column is binary
                                      check_column_is_binary(df, response_variable_column)
                                      
                                      # We go over each column in the other_columns argument and check a few things
                                      for (other_column in other_columns) {
                                        check_column_exists(df, other_column)
                                        
                                        column_type <- typeof(df[[other_column]])
                                        
                                        if (!(column_type %in% list("integer", "double", "logical"))) {
                                          stop(sprintf("Column '%s' should be a factor, a logical value or numeric", other_column))
                                        } else if (column_type == "integer") {
                                          if (length(unique(df[[other_column]])) != 2) {
                                            stop(sprintf("Column '%s' should contain exactly two unique values", other_column))
                                          }
                                        }
                                      }
                                      
                                      # Save the context features
                                      # I first have to save the features to a temporary variable because else R will complain
                                      # that it can't check whether its datatype accords with what I force it to be
                                      context_features_ <- c()
                                      # We go over each column that should be converted to binary
                                      for (to_binary_column in to_binary_columns) {
                                        # Then, we get all unique values of that column and append it to the list
                                        context_features_ <- append(context_features_,
                                                                    unique(as.character(df[[to_binary_column]])))
                                      }
                                      context_features <<- context_features_
                                      rm(context_features_) # Remove temp variable
                                      
                                      # Save the response variables
                                      # We abuse the factor coding (1/2), and subtract one to get (0/1)
                                      response_variables <<- as.numeric(df[[response_variable_column]]) - 1
                                      
                                      # Checks OK, pass data to weird R constructor
                                      callSuper(df=df,
                                                response_variable_column=response_variable_column,
                                                to_binary_columns=to_binary_columns,
                                                other_columns=other_columns)
                                      
                                      # Change the dataframe so it carries indices
                                      df$'_id' <<- 1:nrow(df)
                                    },
                                    
                                    # get_sparse_coords
                                    get_sparse_coords = function() {
                                      # What is the total number of features?
                                      context_feature_count <- length(context_features)
                                      other_columns_count <- length(other_columns)
                                      
                                      # The total features consists of...
                                      # - the binary features
                                      # - the other columns
                                      total_feature_count <- context_feature_count + other_columns_count
                                      
                                      # We go over each row and check what the value is for the 'to_binary' column
                                      feature_matrix <- apply(df, 1, function(row) {
                                        lapply(to_binary_columns, function(to_binary_column) {
                                          # Retrieve the row index of this row
                                          # This will be the X coordinate in our sparse matrix
                                          row_index <- as.numeric(row[["_id"]])
                                          
                                          # We get the value of the column that we turn into multiple binary predictors
                                          to_binary_value <- row[[to_binary_column]]
                                          
                                          # We check what the index of this value is in the context features list
                                          # This index corresponds to the index of the column of this value in the matrix
                                          # This will be the Y coordinate in our sparse matrix
                                          to_binary_index <- match(to_binary_value, context_features)
                                          
                                          # Of course, the value for this coordinate in the sparse matrix
                                          # will always be 1 (= "this feature is present")
                                          coordinate_value <- 1
                                          
                                          return(c(x=row_index, y=to_binary_index, value=coordinate_value))
                                        })
                                      })

                                      # Flatten the list
                                      feature_matrix <- unlist(feature_matrix, recursive=FALSE)
                                      # Turn the list of lists into a data frame
                                      feature_matrix <- as.data.frame(do.call(rbind, feature_matrix))
                                      
                                      list_index = 1
                                      # We also go over the "other columns", they have values too
                                      for (other_column in other_columns) {
                                        # This is the index of the 'other' column in the matrix
                                        # This will be the Y coordinate in our sparse matrix
                                        other_column_index = context_feature_count + list_index
                                        
                                        # The X coordinates will, of course, always be the row indices
                                        x <- 1:nrow(df)
                                        # The Y coordinates are always the same (= the other column index)
                                        y <- rep(other_column_index, nrow(df))
                                        
                                        if (is.factor(df[[other_column]])) { 
                                          # If we are dealing with a binary variable, 
                                          # set the value for its column to 1 if we see the non-reference value
                                          values <- (as.numeric(df[[other_column]]) - 1)
                                        } else if (is.logical(df[[other_column]])) {
                                          # Naturally, logical types translate nicely to a 0/1 structure
                                          values <- as.numeric(df[[other_column]])
                                        } else {
                                          # Finally, for numeric variables, just use the value as-is
                                          values <- df[[other_column]]
                                        }
                                        
                                        # We create a new data frame with the x and y values from the "other column"
                                        # We combine this data frame with the one we made earlier
                                        feature_matrix <- rbind(feature_matrix,
                                                                data.frame(
                                                                  x=x,
                                                                  y=y,
                                                                  value=values
                                                                ))

                                        list_index <- list_index + 1
                                      }
                                      
                                      return(feature_matrix)
                                    },
                                    
                                    # as_matrix
                                    as_matrix = function() {
                                      sparse_coords <- get_sparse_coords()
                                      return(sparseMatrix(sparse_coords$x,
                                                          sparse_coords$y,
                                                          x=sparse_coords$value))
                                    },
                                    
                                    # as_feature_list
                                    as_feature_list = function() {
                                      # Does this do a deep copy? Let's find out
                                      feature_list <- c(list(), context_features)
                                      
                                      for (other_column in other_columns) {
                                        feature_name <- other_column
                                        if (is.factor(df[[other_column]])) {
                                          # The non-reference level is the second level in the factor
                                          feature_name <- paste0("_is_", levels(df[[other_column]])[-1])
                                        } else if (is.logical(df[[other_column]])) {
                                          # Since logical values are simply T/F, we can set the column name
                                          # as the feature since the non-reference level will always be
                                          # 'TRUE'
                                          feature_name <- paste0("_is_", other_column)
                                        } else {
                                          # For numeric columns, we only need to add _
                                          # This makes sure it appears as an 'other column' in Rekker
                                          feature_name <- paste0("_", other_column)
                                        }
                                        
                                        feature_list <- append(feature_list, feature_name)
                                      }
                                      
                                      return(feature_list)
                                    }
                                  ))
