source("Dataset.R")

df <- data.frame(response = c("boring", "boring", "cool"),
                 verb = c("came", "saw", "conquered"),
                 length = c(4, 3, 9),
                 has_a = c(T, T, F),
                 region = c("Rome", "Greece", "Asia Minor"))

df$response <- as.factor(df$response)
df$verb <- as.factor(df$verb)
df$has_a <- as.factor(df$has_a)
df$region <- as.factor(df$region)

ds <- dataset(df=df,
              response_variable_column="response",
              to_binary_columns=c("verb", "region"),
              other_columns=c("length", "has_a"))

ds$context_features

ds$get_sparse_coords()
ds$as_matrix()
