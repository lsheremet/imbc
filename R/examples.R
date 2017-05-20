#' Draw an MDS plotly Object with specific points circled
#'
#' @param object an Mclust object
#' @param distanceMethod a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."
#' @param Class.train a vector of cluster classifications associated with each data point
#' @param resultIndicies a vector of indicies of points we would like to circle
#' @param G the number of clusters
#' @return a plotly object of MDS coordinates for your data with points circled
#' @examples
#' place example here
MDSplot <- function(object, distanceMethod = "euclidean", Class.train, resultIndicies, G){
  rainbowColors <- c( "#BF914D", "#A8BF4D", "#63BF4D", 
                      "#4DBF7A", "#4DBFBF", "#4D7ABF", "#634DBF",
                      "#A84DBF", "#BF4D91")
  d <- dist(object$data, distanceMethod)
  fit <- cmdscale(d, eig = TRUE, k = 2)
  x <- fit$points[,1]
  y <- fit$points[,2]
  #Get coordinates for top n minimax points to be circled
  xlabel <- c()
  ylabel <- c()
  for (value in resultIndicies){
    xlabel <- c(xlabel, x[resultIndicies])
    ylabel <- c(ylabel, y[resultIndicies])
  }
  datMDS <- as.data.frame(cbind(x, y, 1:nrow(object$data)))
  Class <- as.factor(Class.train)[-resultIndicies]
  plot <- ggplot(datMDS[-resultIndicies,],aes(x = x, y = y)) +
    ggtitle("MDS Plot By Cluster") + 
    labs(x = "Coordinate 1", y = "Coordinate 2", color = "Cluster\n") +
    geom_point(aes(col = Class, text = paste("Index:", rownames(datMDS[-resultIndicies,]))), size = 3) + 
    geom_point(data=datMDS[resultIndicies,],
               pch=21, fill=NA, size=10, colour="red", stroke=2) +
    geom_text(data = datMDS[resultIndicies,], label = resultIndicies) +
    theme(plot.title = element_text(size = 18), 
          axis.title = element_text(size = 20)) + 
    scale_colour_manual(values = rainbowColors)
  return(plot)
}



#' Print a string!
#'
#' @param String character containing a string to
#' print
#' @return A printed string
#' @examples
#' leeR_demo()
leeR_demo <- function(string) {
  print(string)
}