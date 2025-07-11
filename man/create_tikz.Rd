\name{create_tikz}
\alias{create_tikz}
\title{Creates diagram code for tikz}
\usage{
create_tikz(model, infile = NULL, allowbottom = FALSE, cex = 1.75, outfile = NULL)
}
\arguments{
\item{model}{A character vector specifying the model in lavaan syntax or a list
(or data.frame) with at least members lhs, op, rhs, label and fixed. Optional if
infile is given.}
\item{infile}{A character string specifying the file which contains the model
syntax.}
\item{allowbottom}{A logical indicating if indicators can be placed at the
bottom of the graph.}
\item{cex}{Minimum distance between nodes in cm.}
\item{outfile}{Where to write the result.}
}
\value{
A character array - invisible - with the generated code.
}
\description{
Creates the code to make a tikz diagram in tikz.
}
\examples{
model <- 'alpha_11  =~ 1 * x_1 + x_2 + x_3     # latent variable
          alpha_12 <~ x_4 + x_5 + x_6      # composite
          gamma =~ 1 * x_7 + x_8 + x_9     # latent variable
          xi =~ 1 * x_10 + x_11 + x_12 + x_13  # latent variable
          x_1 ~~ x_3
          x_4 ~~ vx4 * x_4
          x_7 ~~ x_9
          x_10 ~~ x_11 + x_13
          # regressions
          xi ~ v * alpha_11 + t * alpha_12 + 1
          alpha_11 ~ yy * Theta_1 + tt_1 * 0.11234 * alpha_12 + ss * gamma
          '
tikzcode <- create_tikz(model)
modelml <- '
        level: 1
        fw =~ y_1 + y_2 + y_3 + y_4
        level: 2
        fb =~ y_1 + y_2 + y_3 + y_5
        y_2 ~~ cv24 * y_5
  '
tikzcodeml <- create_tikz(modelml)
}
