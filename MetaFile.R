meta.file <- setRefClass("meta.file", fields = list(
  df = "data.frame"),
  methods = list(
    initialize = function(df_) {
      columns <- c("subject", "predicate", "object") 
      if (missing(df_)) {
        df_ <- data.frame(matrix(nrow = 0, ncol = length(columns)))
        colnames(df_) = columns
      }
      df <<- df_
    },
    
    add_model_information = function(predicate, object) {
      triple <- list(subject="model", predicate=predicate, object=object)
      df <<- rbind(df, triple)
    },
    
    add_free_information = function(subject, predicate, object) {
      triple <- list(subject=subject, predicate=predicate, object=object)
      df <<- rbind(df, triple)
    },
    
    as.data.frame = function() {
      return(df)
    }
))