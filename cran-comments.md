## Test environments
* local Ubuntu 14.10 install, R 3.1.1 and R devel
* local Windows 8.1 install, R 3.1.2
* win-builder (devel and release)
* local OS X install, R 3.1.2

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Maintainer: ‘David Charte <fdavidcl@outlook.com>’
  New submission
  Possibly mis-spelled words in DESCRIPTION:
    Multi (2:54)
    multi (10:5)

The first part states the fact that this is our first 
submission.

`R CMD check` notes both "multi" (from "multi-label") and
"multilabel" as misspelled words, although these are the 
words used in the related literature, so this NOTE cannot 
be eliminated.

## Downstream dependencies
There are currently no downstream dependencies.
