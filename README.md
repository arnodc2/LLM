# LLM
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/LLM)](http://cran.r-project.org/package=LLM) [![Travis-CI Build Status](https://travis-ci.org/arnodc2/llm.svg?branch=master)](https://travis-ci.org/arnodc2/llm)

The Logit Leaf Model (LLM) package has three functions:
1) llm
  This function creates a llm object
2) llm.predict
  Use the object constructed with the llm function to make a prediction for (new) instances
3) llm.create.HTML.viz
  Renders HTML code to visualize the LLM

Working version. At the moment only binary classification tasks are supported.

This package can be installed using following command:

```r
devtools::install_github("arnodc2/LLM", upgrade_dependencies = FALSE)
```
