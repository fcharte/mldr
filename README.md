mldr
====

Exploratory data analysis and manipulation functions for multi-label data sets along
with interactive Shiny application to ease their use.

## Installation

You can install this package via `install_github` from the 
[devtools](https://github.com/hadley/devtools) package.

```R
devtools::install_github("fcharte/mldr")
```

<!-- Add installation from CRAN when released -->

## Usage and examples

This package provides a web GUI able to load, visualize and manipulate 
multi-label data sets. You can launch it using the R console:

```R
library(mldr)
mldrGUI()
```

There are several functions available as well, so that you can
use *mldr* in an R script. For example, to explore some data sets:

```R
library(mldr)

# Data sets birds, emotions and genbase are
# provided within the package
print(emotions)
summary(genbase)
plot(birds)
```

*mldr* enables you to create new multi-label data sets via the 
`mldr_from_dataframe` function, and export them to the standard 
ARFF format using `write_arff`:

```R
library(mldr)

df <- data.frame(matrix(rnorm(1000), ncol = 10))
df$Label1 <- c(sample(c(0,1), 100, replace = TRUE))
df$Label2 <- c(sample(c(0,1), 100, replace = TRUE))
mymldr <- mldr_from_dataframe(df, labelIndices = c(11, 12), name = "testMLDR")

# Writes .arff and .xml files for a multi-label dataset
write_arff(mymldr, "my_new_mldr")
```

For more examples and detailed explanation on available functions, 
please refer to the documentation.
