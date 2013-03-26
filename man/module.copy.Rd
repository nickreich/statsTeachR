\name{module.copy}
\alias{module.copy}
\title{module.copy}
\usage{
  module.copy(mod, to)
}
\arguments{
  \item{mod}{module to be copied, specified in quotes}

  \item{to}{path to which the module should be copied,
  should end with a folder name}
}
\value{
  NULL only moving files around
}
\description{
  The module copy function moves lesson-plan files from an
  internal package directory to a user-specified directory.
}
\examples{
module.copy(mod="permTestExample", to="~/test")
}

