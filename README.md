mldr
====

[![Travis](https://img.shields.io/travis/fcharte/mldr.svg)](https://travis-ci.org/fcharte/mldr/) 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/mldr)](http://cran.r-project.org/web/packages/mldr)
[![Downloads](http://cranlogs.r-pkg.org/badges/mldr)](http://cran.rstudio.com/web/packages/mldr/index.html)

Exploratory data analysis and manipulation functions for multi-label data sets along
with an interactive Shiny application to ease their use.

## Installation

Use `install.packages` to install *mldr* and its dependencies:

```R
install.packages("mldr")
```

Alternatively, you can install it via `install_github` from the 
[devtools](https://github.com/hadley/devtools) package.

```R
devtools::install_github("fcharte/mldr")
```

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
