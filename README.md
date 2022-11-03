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