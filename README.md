# ElasticToolsR
Tools for preparing datasets for Elastic Net regression in R

## What is ElasticTools?

ElasticTools is a library with which you can easily generate datasets suitable for analysis using Elastic Net regression. The main idea is that you have a dataset you would use for "normal" regression, which you can then convert for use with Elastic Net regression. This is done by converting a column's values to binary predictors for those values. For example:

- the dataset has a column *verb lemma* with values *eat*, *sleep*, *rave*
- this column is converted to the columns *is_eat*, *is_sleep* and *is_rave*
	- a row's value for *is_eat* will be 1 if the value for *verb lemma* was *eat*
	- a row's value for *is_sleep* will be 1 if the value for *verb lemma* was *sleep*
	- a row's value for *is_rave* will be 1 if the value for *verb lemma* was *rave*

ElasticTools handles the following aspects of the dataset conversion process:
- converting a column to multiple columns with binary values
- converting binary predictor columns to binary values
- converting the dataset to a feature matrix
- creating a list of all features for matrix column identification

It can also run the following regressions:
- ridge regression
- lasso regression
- elastic net regression
- elastic net regression with k-fold cross validation

The output of these regression fits can be exported to a [Rekker](https://github.com/AntheSevenants/Rekker)-compatible format.

## Installing ElasticTools

ElasticTools is not available on the CRAN package manager (yet). To use it, simply copy the scripts from this repository to your R project's directory (preferably using `git clone`). From there, you can simply include the scripts you need. More information on what scripts to import is given below.

## Using ElasticTools

### Importing the required scripts

As of writing, only the `Dataset` class is implemented. Include it as follows:
```r
source("Dataset.R")
```
⚠ "Dataset.R" should be present in your R script's directory.

### Defining a dataset

If you want to define an Elastic Tools dataset, you first need to import your data into a dataframe. Usually, this will be a CSV file of some sorts. For example, you could load your dataset as follows:

```r
df <- read.csv("dataset.csv")
```

Your dataset is now loaded as a dataframe. Be wary of the following restrictions:
- your response variable column should be defined as a factor
- the column which will be converted into binary variables should be defined as a factor
- other predictors (i.e. other fixed effects) can be either factors or continuous numeric data types

To coerce a column as a factor, refer to the following snippet:
```r
df$column <- as.factor(df$column)
```

ElasticTools dataset. The constructor for the Dataset class takes the following arguments:

| parameter | type    | description                                      | example |
| --------- | ------- | ------------------------------------------------ | -------| 
| `df` | data.frame | the dataframe which your dataset will be based on | / |
| `response_variable_column` | character  | the name of the column that holds your response variable | `"Variant"` |
| `to_binary_column` | character  | the name of the column that will be converted to binary columns | `"VerbLemma"` |
| `other_columns` (optional) | list(character)  | a list of names of other columns which will be used as predictors | `[ "VerbPosition", "NumberOfWords", "Country" ]` |

⚠ Currently, only binary response variables are supported (= logistic regression).

```r
ds <- dataset(df=df,
              response_variable_column="Variant",
              to_binary_column="VerbLemma",
              list("VerbPosition", "NumberOfWords", "Country"))
```

### Converting the dataset to a feature matrix

To convert the dataset to a feature matrix, simply use the `$as_matrix()` method. This will return an R matrix.

```
feature_matrix <- ds$as_matrix()
```

### Retrieving the feature list

In the conversion to a matrix, the names of the different columns of your dataset get lost. This is why we use `$as_feature_list()` to get the complete list of features corresponding to the matrix columns. The indices of the list correspond to the indices of the columns. The method will return a vector of type `character`.

```
feature_list <- ds$as_feature_list()
```

⚠ The feature list does not include the name of the response variable column, since this column does not strictly contain a feature.

### Defining an Elastic Net object

To run an Elastic net regression, you must first create an Elastic Net object. The constructor for the ElasticNet class takes the following arguments:

| parameter | type    | description                                      | example |
| --------- | ------- | ------------------------------------------------ | -------| 
| `ds` | Dataset | an ElasticTools Dataset instance | / |
| `feature_matrix` | matrix(double)  | the feature matrix exported associated with the Dataset | / |
| `train_share` (optional) | double  | the amount of data reserved for training | `0.7` |

```r
net <- elastic_net(ds=ds,
                   feature_matrix=feature_matrix)
```

### Running regressions

#### Running a ridge regression

To run a ridge regression, use the `$do_ridge_regression()` method. This will return a regression fit.

```r
fit <- net$do_ridge_regression()
```

#### Running a lasso regression

To run a lasso regression, use the `$do_lasso_regression()` method. This will return a regression fit.

```r
fit <- net$do_lasso_regression()
```

#### Running an Elastic Net regression

To run a lasso regression, use the `$do_lasso_regression()` method. This will return a regression fit. There is one argument:

| parameter | type    | description                                      | example |
| --------- | ------- | ------------------------------------------------ | -------| 
| `alpha` | double | the division between lasso and ridge in Elastic Net; 1 = only lasso, 0 = only ridge | `0.5` |

```r
fit <- net$do_elastic_net_regression(alpha=0.5)
```

#### Running Elastic Net regression using k-fold cross validation

You can also automatically find out the ideal alpha value by applying k-fold cross validation. We test from $i$ to $k$ with alpha = $\frac{i}{k}$ and calculate the [Cross Entropy Loss](https://en.wikipedia.org/wiki/Cross_entropy). The model with the lowest loss is able to predict the data the best, and is thus preferable.

To do cross validation, use the `$do_cross_validation()` method. There is one argument:

| parameter | type    | description                                      | example |
| --------- | ------- | ------------------------------------------------ | -------| 
| `k` | numeric | the number of folds for the k-fold cross validation | `10` |

```r
fit <- net$do_cross_validation(k=10)
```

This method will return a list with two named elements:
- `$results`: a data frame containing the results of the cross validation
- `$fits`: a list of regression fit objects associated with the different parameter values tried

### Attaching features to the the Elastic Net coefficients 

The coefficients found in the Elastic Net regression are unlabelled. Therefore, you can use the `$attach_coefficients()` method to link the coefficients with their associated features. There is one argument. This method will return a data frame.

| parameter | type    | description                                      | example |
| --------- | ------- | ------------------------------------------------ | -------| 
| `fit` | list | the regression object of your choice | / |

```r
coefficients_with_labels <- net$attach_coefficients(fit)
```

You can save this data frame as a CSV file and use it with [Rekker](https://github.com/AntheSevenants/Rekker)!