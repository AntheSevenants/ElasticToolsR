meta.file <- setRefClass("meta.file", fields = list(
  triples = "list"),
  methods = list(
    initialize = function() {
      triples <<- list()
    },
    
    add_model_information = function(predicate, object) {
      triple <- list(subject="model", predicate=predicate, object=object)
      triples <<- append(triples, list(triple))
    },
    
    add_free_information = function(subject, predicate, object) {
      triple <- list(subject=subject, predicate=predicate, object=object)
      triples <<- append(triples, list(triple))
    },
    
    as.data.frame = function() {
      do.call(rbind, triples)
    }
))