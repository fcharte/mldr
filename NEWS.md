# Changelog

## mldr 0.3.18

* Fix bug #21 when reading sparse datasets introducing zeroes instead of NAs 
* Fix #23 by ignoring case in `@attribute` tags
* Fix bug #24 in filtering function
* Improves parsing of attributes to correctly manage escaped apostrophes
* Fix calculation of SCUMBLE CV to prevent NaN values
* Optimizes some calculations (mean and CV of SCUMBLE are now 4x faster)
* Export `read.arff` function to allow reading multilabel data without calculating measures
* Add ability to load datasets from the [mldr.datasets](https://github.com/fcharte/mldr.datasets) package in `mldr()` function

## mldr 0.2.82

* Fix bug in demo code for rebuilding an mldr object
* Add global `scumble.cv` measure and `SCUMBLE.CV` measure per label
* Add new concurrence module to ease analysis of concurrence among imbalanced labels
* Display an analysis of concurrence between labels within the GUI
* Add `remedial` preprocessing algorithm
* Fix bug #18 when reading ARFF with capitalized '@relation' parameters
* A Citation file (accesible from `citation("mldr")`) has been added

## mldr 0.2.51

* Fix bug #14 when reading certain sparse datasets (e.g. Yahoo)
* Support for multiple plot types in one call
* Add `num.inputs` measure for input attributes
* Add color parameters for plotting functions

## mldr 0.2.33

* New redesigned GUI to ease usability
* Add ability to read MEKA datasets from GUI

## mldr 0.2.25

### New features

* New `mldr_evaluate()` function to assess multilabel classifier performance. Taking as input an `mldr` object and a matrix with predictions this function returns a list of metrics, including Accuracy, AUC, AveragePrecision, Coverage, FMeasure, HammingLoss, MacroAUC, MacroFMeasure, MacroPrecision, MacroRecall, MicroAUC, MicroFMeasure, MicroPrecision, MicroRecall, OneError, Precision, RankingLoss, Recall, and SubsetAccuracy
* Added more parameters to `mldr` function so labels can be identified via specific names,
indices or their count.
* Added vignette

### Fixes

* Fixed call to `chordDiagram` for newer versions of the [`circlize`](https://github.com/jokergoo/circlize) package.
* Fixed imports to avoid NOTEs on devel builds.
* Fixed parameters in calls to pROC functions.

## mldr 0.1.70

First release of *mldr*. This version includes:

* Ability to read multi-label data sets from ARFF and XML files in Mulan or MEKA format.
* Ability to write to ARFF and XML files in both Mulan and MEKA formats.
* Different ways to display data and relevant measures from data sets.
* Several plots for multi-label data visualisation.
* Functions to operate with `mldr` objects: filtering, joining and structure comparison.
* BR and LP transformations of multi-label datasets.
* Ability to create new `mldr` objects out of `data.frame`s.
* Sample datasets: `birds`, `emotions` and `genbase`.
