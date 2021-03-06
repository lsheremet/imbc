% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Documentation.R
\name{minBetweenClust}
\alias{minBetweenClust}
\title{Return sorted data points using minimum between-cluster distance}
\usage{
minBetweenClust(data, estep, previousResultIndicies, distanceMethod)
}
\arguments{
\item{data}{the data you wish to use}

\item{estep}{output from the mclust function "estep"}

\item{previousResultIndicies}{indicies of rows the imbc algorithm previously queried the user on}

\item{distanceMethod}{a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."}
}
\value{
a vector of points sorted according to the minimum between-cluster distance method algorithm
}
\description{
Return sorted data points using minimum between-cluster distance
}
\examples{
#Load data
library(mclust)
data(banknote)

#Create new dataset with only continuous variables
bankdata <- banknote[,2:7]

#create Mclust object
object <- Mclust(bankdata)
#determine best model
model <- object$modelName
#extract model parameters
param <- object$parameters

#output from estep of EM algorithm
estepbank <- estep(model, bankdata, param)

#we assume we have not previously queried any points
#if this vector contained any row indicies, those would not show up in output sorted vector of the function
previousResultIndicies <- c()

#output from minimax algorithm: returns vector of points that are least confidently placed in their respective classes
minBetweenClust(bankdata, estep, previousResultIndicies)

#try with a different distance method
minBetweenClust(bankdata, estep, previousResultIndicies, distanceMethod = "manhattan")
}
