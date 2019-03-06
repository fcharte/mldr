[mldr](https://fcharte.github.io/mldr)
====

[![Travis](https://img.shields.io/travis/fcharte/mldr/master.svg)](https://travis-ci.org/fcharte/mldr/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/mldr)](https://cran.r-project.org/package=mldr)
[![Downloads](http://cranlogs.r-pkg.org/badges/mldr)](https://cran.r-project.org/package=mldr)
[![TotalDownloads](http://cranlogs.r-pkg.org/badges/grand-total/mldr?color=yellow)](https://cran.r-project.org/package=mldr)

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

## Building from source

Use `devtools::build` from [devtools](https://github.com/hadley/devtools)
to build the package:

```R
devtools::build(args = "--compact-vignettes=gs+qpdf")
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

## Citation

Please, cite *mldr* as follows:

```
@Article{charte-charte:2015,
  author       = {Francisco Charte and David Charte}, 
  title        = {Working with Multilabel Datasets in {R}: The mldr Package}, 
  journal      = {The R Journal},
  year         = 2015,
  volume       = 7,
  number       = 2,
  pages        = {149--162},
  month        = dec,
  url          = {https://journal.r-project.org/archive/2015-2/charte-charte.pdf}
}
```
