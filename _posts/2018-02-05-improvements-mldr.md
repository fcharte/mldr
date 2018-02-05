---
layout: post
title: "Improvements on evaluation metrics and read/write in mldr 0.4"
---

We've just released version 0.4 of mldr. Here's a brief look at the changes. Don't forget to update!
~~~R
update.packages("mldr")
~~~

## Reimplementation of evaluation metrics

The set of evaluation metrics included in mldr has been accumulating issues and bug reports in the last months. This fact has encouraged us to fully revise and reimplement these metrics. The new implementations have been thoroughly tested and compared to other in different libraries.

Evaluation metrics are now exported individually, but you can keep using the `mldr_evaluate` function for a full report on the performance of a classifier. Additionally, the `roc` method has been implemented for the `"mldr"` class, and it allows to obtain the ROC curve for a dataset and its predictions (provided the pROC package is installed).

## Treatment of undefined values in metrics

Several performance metrics can sometimes lead to undefined results, usually due to potential divisions by zero. With the aim of facilitating experimentation with different classifiers among diverse platforms, we have provided parameters that customize the behavior of performance metrics in these cases. In particular, mldr mimics the behavior of MULAN metrics by default, but other options are available. Let's look at some examples:

~~~R
true_labels <- matrix(c(
  1,1,1,
  0,0,0,
  1,0,0,
  1,1,1,
  0,0,0,
  1,0,0
), ncol = 3, byrow = TRUE)
predicted_labels <- matrix(c(
  1,1,1,
  0,0,0,
  1,0,0,
  1,1,0,
  1,0,0,
  0,1,0
), ncol = 3, byrow = TRUE)

# strategies for undefined values: "diagnose", "ignore", "na"
precision(true_labels, predicted_labels, undefined_value = "diagnose")

# single value to replace undefined values: e.g. 0, 1
recall(true_labels, predicted_labels, undefined_value = 0)

# custom strategy for undefined values
fmeasure(
  true_labels, predicted_labels,
  undefined_value = function(tp, fp, tn, fn) as.numeric(fp == 0 && fn == 0)
)
~~~

In the first example, we are using one of mldr's built-in strategies to treat undefined values. The `"diagnose"` strategy is the default behavior, and assigns undefined values a replacing value of 1 or 0 according to the accuracy of the prediction (whether no labels were predicted for an instance with no relevant labels). On the contrary, `"ignore"` would not count those cases for the averaging process (which is different from assigning them value zero and counting them). Last, the `"na"` strategy will propagate `NA` if encountered.

The second example is simple, it replaces any `NA` value with the specified number. A reasonable value can be 0, but this may wrongly penalize the classifier in datasets with very sparse labels.

The third example is the custom approach: you can provide a function accepting 4 integers (TP, FP, TN, FN), which will be called when an undefined value is encountered, with the true positives, false positive, true negatives and false negatives respectively. It should generally return a numeric value (in the range [0, 1] in order for the mean to make sense).

## Improvements on read and write of ARFF files

The parser for ARFF files is now more robust, including support for single-quoted and double-quoted attributes, as well as a negative amount of labels in the MEKA header (indicating labels are at the end of the attribute list). Additionally, the user can now choose to read categorical attributes as factors (passing the `stringsAsFactors` parameter).

Exporting to ARFF has seen some improvements as well, but you may want to check out [mldr.datasets](https://github.com/fcharte/mldr.datasets), which is able to export to a variety of other formats and provides more options.
