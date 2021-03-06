% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey.R
\name{nhanes_survey}
\alias{nhanes_survey}
\title{Apply a function from the survey package to NHANES data}
\usage{
nhanes_survey(
  survey_fun,
  nhanes_data,
  column,
  comment_column = "",
  weights_column = "",
  filter = NULL,
  analyze = "values",
  callback = NULL,
  ...
)
}
\arguments{
\item{survey_fun}{the survey package function (e.g. svyquantile or svymean)}

\item{nhanes_data}{data frame containing NHANES data}

\item{column}{column name of the variable to compute quantiles for}

\item{comment_column}{comment column name of the variable}

\item{weights_column}{name of the weights column}

\item{filter}{logical expression used to subset the data}

\item{analyze}{one of "values" or "comments", whether to apply the survey function to the value or comment column.}

\item{callback}{optional function to execute on each row of the dataframe}

\item{...}{other arguments to pass to the survey function}
}
\value{
a data frame
}
\description{
Apply a function from the survey package to NHANES data
}
\details{
This function provides a generic way to apply any function from the survey package to NHANES data.
RNHANES provides specific wrappers for computing quantiles (nhanes_quantile) and detection frequencies (nhanes_detection_frequency),
and this function provides a general way to use any survey function.
}
\examples{
\dontrun{
library(survey)

nhanes_data <- nhanes_load_data("EPH", "2011-2012", demographics = TRUE)

# Compute the mean of triclosan using the svymean function
nhanes_survey(svymean, nhanes_data, "URXTRS", "URDTRSLC", na.rm = TRUE)

# Compute the variance using svyvar
nhanes_survey(svyvar, nhanes_data, "URXTRS", "URDTRSLC", na.rm = TRUE)

}

}
