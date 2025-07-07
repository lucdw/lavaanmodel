\name{get_model_info}
\alias{get_model_info}
\title{Get model info}
\usage{
get_model_info(model = NULL, infile = NULL)
}
\arguments{
\item{model}{A character vector specifying the model in lavaan syntax or a list
(or data.frame) with at least members lhs, op, rhs, label and fixed. Optional if
infile is given.}
\item{infile}{A character string specifying the file which contains the model
syntax.}
}
\value{
A list with members nodes and edges, containing the necessary data to create
a diagram.
}
\description{
Gets information from model necessary to produce a plot.
}
\examples{
model <- 'alfa  =~ x_1 + x_2 + x_3     # latent variable
          beta <~ x_4 + x_5 + x_6      # composite
          gamma =~ x_7 + x_8 + x_9     # latent variable
          Xi =~ x_10 + x_11 + x_12 + x_13  # latent variable
          # regressions
          Xi ~ v * alfa + t * beta
          alfa ~ tt * beta + ss * gamma + yy * Theta_1
          '
test <- get_model_info(model)
}
