% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot_mesh.R
\name{autoplot.inla.mesh}
\alias{autoplot.inla.mesh}
\title{An autoplot (ggplot2) method for INLA mesh objects.}
\usage{
\method{autoplot}{inla.mesh}(
  object,
  ...,
  col = "blue",
  lwd = 0.5,
  linecol = "darkgrey",
  size = 1.2
)
}
\arguments{
\item{object}{An inla.mesh object}

\item{...}{Other arguments passed to specific methods}

\item{col}{Colour for data points}

\item{lwd}{Line width}

\item{linecol}{The colour for the mesh edges}

\item{size}{size Size of data points}
}
\description{
An autoplot (ggplot2) method for INLA mesh objects.
}
\examples{
\dontrun{
library(INLA)
m = 100
points = matrix(runif(m*2),m,2)
mesh = inla.mesh.create.helper(
  points=points,
  cutoff=0.05,
  offset=c(0.1,0.4),
  max.edge=c(0.05,0.5) )

autoplot(mesh)
p <- autoplot(mesh)

# As a ggplot2 object, the plot can be altered.
p + theme_dark()

# The size and colour of all objects can be controlled.
#   The order of the values is 
#   1: data points
#   2: edges
#   3: outer domain
#   4: inner domain
#   5: non data vertices
p + scale_colour_manual(values = c('red', 'grey', 'darkblue', 'steelblue', 'yellow'))

# The name of the variable that defines the different objects is 'type'
p + facet_grid(. ~ type)

# Plot projections with ggalt

library(ggalt)
p + ggalt::coord_proj("+proj=wintri")
}
}
