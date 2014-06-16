\name{iView}
\alias{iView}
\title{Open a CardinaliView Window}

\description{
	Opens a CardinaliView window for visualization and exploration of a mass spectrometry imaging dataset.
}

\usage{
CardinaliView()

iView(data)
}

\arguments{
    \item{data}{The name of a \code{\linkS4class{MSImageSet}} to load.}
}
\author{
	Kyle D. Bemis
}
\examples{
data <- matrix(c(NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, 
 NA, NA, NA, NA, NA, 0, 1, 1, NA, NA, NA, NA, NA, 1, 0, 0, 1, 
 1, NA, NA, NA, NA, NA, 0, 1, 1, 1, 1, NA, NA, NA, NA, 0, 1, 1, 
 1, 1, 1, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
 1, NA, NA, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA), nrow=9, ncol=9)

set.seed(1)
msset <- generateImage(data, range=c(1000,5000), centers=c(3000,4000), resolution=100, step=3.3, as="MSImageSet")

iView(msset)
}
\keyword{iplot}
