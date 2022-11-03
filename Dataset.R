library(methods)

dataset <- setRefClass("Dataset", fields = list(
                                    df = "data.frame",
                                    response_variable_column = "character",
                                    to_binary_column = "character"),
                                  methods = list())