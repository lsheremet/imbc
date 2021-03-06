% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Documentation.R, R/examples.R
\name{MDSplot}
\alias{MDSplot}
\alias{MDSplot}
\title{Draw an MDS plotly Object with specific points circled}
\usage{
MDSplot(object, distanceMethod = "euclidean", Class.train, resultIndicies, G)

MDSplot(object, distanceMethod = "euclidean", Class.train, resultIndicies, G)
}
\arguments{
\item{object}{an Mclust object}

\item{distanceMethod}{a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."}

\item{Class.train}{a vector of cluster classifications associated with each data point}

\item{resultIndicies}{a vector of indicies of points we would like to circle}

\item{G}{the number of clusters}

\item{Class}{a vector of cluster classifications associated with each data point}

\item{object}{an Mclust object}

\item{distanceMethod}{a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."}

\item{resultIndicies}{a vector of indicies of points we would like to circle}

\item{G}{the number of clusters}
}
\value{
a plotly object of MDS coordinates for your data with points circled

a plotly object of MDS coordinates for your data with points circled
}
\description{
Draw an MDS plotly Object with specific points circled

Draw an MDS plotly Object with specific points circled
}
\examples{
data(banknote)

#create new dataset with only continuous data
bankdata <- banknote[,2:7]

#Mclust object
object <- Mclust(bankdata)

#Pull out classification of each row
Class <- object$classification

#rows you wish to identify in plot
resultIndicies <- c(125,50)

#create plot
MDSplot(object, Class = Class, resultIndicies = resultIndicies, G = 3)
place example here
}
