\name{show_diagram}
\alias{show_diagram}
\title{Show a diagram in an R-plot}
\usage{
show_diagram(nodes, edges)
}
\arguments{
\item{nodes}{The nodes data.frame as returned from position_nodes}
\item{edges}{The edges data.frame as returned from get_model_info}
}
\value{
NULL (invisible)}
\description{
Shows a simple diagram of the model as an R-plot.
}
\examples{
model <- 'alpha  =~ x_1 + x_2 + x_3     # latent variable
          beta <~ x_4 + x_5 + x_6      # composite
          gamma =~ x_7 + x_8 + x_9     # latent variable
          Xi =~ x_10 + x_11 + x_12 + x_13  # latent variable
          # regressions
          Xi ~ v * alpha + t * beta + 1
          alpha ~ yy * Theta_1 + tt * beta + ss * gamma
          '
test <- get_model_info(model)
nodes <- position_nodes(test$nodes, test$edges)
show_diagram(nodes, test$edges)
modelml <- '
        level: 1
        fw =~ 1*y_1 + y_2 + y_3 + y_5
        level: 2
        fb =~ 1*y_1 + y_2 + y_3 + y_4
        y_2 ~~ cv24 * y_4
  '
testml <- get_model_info(modelml)
nodesml <- position_nodes(testml$nodes, testml$edges)
show_diagram(nodesml, testml$edges)
}
