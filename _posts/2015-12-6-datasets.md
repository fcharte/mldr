---
layout: post
title: mldr.datasets and the Ultimate Multilabel Dataset Repository
---

Until now, multilabel datasets have been provided in different file formats for different pieces of software. mldr was created with compatibility in mind and allowed to read two widely-known formats: datasets from Mulan and MEKA repositories in ARFF format.

With the creation of the Ultimate Multilabel Dataset Repository (RUMDR) and a new R package, [mldr.datasets](https://cran.r-project.org/web/packages/mldr.datasets/), a huge set of multilabel datasets are now available in a common format and with the possibility of being converted into many more.

## mldr.datasets

### Multilabel datasets

~~~R
install.packages("mldr.datasets")
library(mldr.datasets)
~~~

>**Note**: mldr.datasets does not depend on mldr, but it's useful to have both of them installed to access all functionality.

After installing and loading the package, some pre-loaded datasets will be available directly in the environment:

* birds
* cal500
* emotions
* flags
* genbase
* langlog
* medical
* ng20
* slashdot
+ stackex_chess

These are accessible via their names and the usual members of `"mldr"` objects (`$measures`, `$labels`, `$datasets`...). Additionally, a `toBibtex()` method is provided for fast access to the citation information for each dataset.

Larger datasets are available to download from the repository (you can consult [the complete list of datasets](https://github.com/fcharte/mldr.datasets/blob/master/additional-data/README.md) or call `mldrs()`) via the `check_n_load.mldr()` function.

### Partitioning functions

The `random.kfolds()` and `stratified.kfolds()` functions partition multilabel datasets following a random strategy and a stratified one, respectively.

### Exporting functions

The `write.mldr()` function is able to export `"mldr"` and `"mldr.folds"` objects into several file formats: Mulan, MEKA, KEEL, LibSVM and CSV. This way, regular, partitioned and preprocessed datasets can be saved for later use in any well-known multilabel classification tool.

## mldr 0.3.18

We've updated mldr to integrate functionality from mldr.datasets when it's installed. Thus, now calling `mldr()` with simply a dataset name will trigger a search within the datasets in the repository. If a dataset isn't found, the function will attempt to read the dataset locally (this behavior can be forced using the `force_read_from_file` parameter).

Other changes in this update include exposing the `read.arff()` function, able to read ARFF files and differentiate input and output features without calculating any related measure, as suggested in [issue 26](https://github.com/fcharte/mldr/issues/26); several fixes related to dataset reading and calculation of measures, and slight changes in some calculations. For detailed information visit our [changelog](https://github.com/fcharte/mldr/blob/0.3.18/NEWS.md) or the [commit history](https://github.com/fcharte/mldr/commits/0.3.18).
