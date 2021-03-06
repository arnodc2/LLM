% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llm.predict.R
\name{llm.predict}
\alias{llm.predict}
\title{Create Logit Leaf Model}
\usage{
llm.predict(df, y_value, llm_object)
}
\arguments{
\item{df}{dataframe containing numerical independent variables}

\item{y_value}{numerical vector of dependent variable. Currently only two class classification is supported}

\item{llm_object}{output of the llm function. A LLM object containing decision rules that define segments and logistic regressions per segment}

\item{nbr_obs_leaf}{the minimum number of observations in a leaf node}
}
\value{
the predictions of the llm model
}
\description{
This function creates the logit leaf model. It assumes a dataframe with numeric
values as input and a corresponding vector with dependent values.
Decision tree parameters threshold for pruning and number of observations per
leaf can be set.
}
