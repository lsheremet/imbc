devtools::load_all(pkg = "C:/Users/Lina/OneDrive/Documents/Research/imbc")
devtools::document(pkg = "C:/Users/Lina/OneDrive/Documents/Research/imbc")
devtools::install_github("lsheremet/imbc")

#' Draw an MDS plotly Object with specific points circled
#'
#' @param object an Mclust object
#' @param distanceMethod a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."
#' @param Class a vector of cluster classifications associated with each data point
#' @param resultIndicies a vector of indicies of points we would like to circle
#' @param G the number of clusters
#' @return a plotly object of MDS coordinates for your data with points circled
#' @examples
#' data(banknote)
#'
#' #create new dataset with only continuous data
#' bankdata <- banknote[,2:7]
#'
#' #Mclust object
#' object <- Mclust(bankdata)
#'
#' #Pull out classification of each row
#' Class <- object$classification
#'
#' #rows you wish to identify in plot
#' resultIndicies <- c(125,50)
#'
#' #create plot
#' MDSplot(object, Class = Class, resultIndicies = resultIndicies, G = 3)
MDSplot <- function(object, distanceMethod = "euclidean", Class, resultIndicies, G){
  rainbowColors <-  c("#999999", "#E69F00", "#56B4E9", "#009E73",
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
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
  Class <- as.factor(Class)[-resultIndicies]
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

#' Draw an ellipse Plotly Object with specific points circled
#'
#' @param object an Mclust object
#' @param distanceMethod a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."
#' @param Class a vector of cluster classifications associated with each data point
#' @param resultIndicies a vector of indicies of points we would like to circle
#' @param G the number of clusters
#' @return a plotly object of an elipse plot for your data with points circled
#' @examples
#' data(banknote)
#'
#' #create new dataset with only continuous data
#' bankdata <- banknote[,2:7]
#'
#' #Mclust object
#' object <- Mclust(bankdata)
#'
#' #Pull out classification of each row
#' Class <- object$classification
#'
#' #rows you wish to identify in plot
#' resultIndicies <- c(125,50)
#'
#' #create plot
#' ellipsePlot(object, Class = Class, resultIndicies = resultIndicies, G = 3)
ellipsePlot <- function(object, distanceMethod = "euclidean", Class.train,
                        resultIndicies, G){
  require(ellipse)
  plot <- MDSplot(object, distanceMethod, Class.train,
                  resultIndicies, G)
  rainbowColors <-  c("#999999", "#E69F00", "#56B4E9", "#009E73",
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  d <- dist(object$data)
  fit <- cmdscale(d, eig = TRUE, k = 2)
  for (cluster in 1:G){
    xellipse <- fit$points[Class.train==cluster,1]
    yellipse <- fit$points[Class.train==cluster,2]
    ell1 <- ellipse(x = cor(xellipse,yellipse),
                    centre = c(mean(xellipse),mean(yellipse)),
                    level = c(0.50))
    ell2 <- ellipse(x = cor(xellipse,yellipse),
                    centre = c(mean(xellipse),mean(yellipse)),
                    level = c(0.68))
    ell3 <- ellipse(x = cor(xellipse,yellipse),
                    centre = c(mean(xellipse),mean(yellipse)),
                    level = c(0.95))
    elDat1 <- as.data.frame(cbind(ell1[,1],ell1[,2]))
    elDat2 <- as.data.frame(cbind(ell2[,1],ell2[,2]))
    elDat3 <- as.data.frame(cbind(ell3[,1],ell3[,2]))
    colnames(elDat1) <- c("v1", "v2")
    colnames(elDat2) <- c("v1", "v2")
    colnames(elDat3) <- c("v1", "v2")
    plot <- plot +
      geom_path(data = elDat1, aes(x = v1, y = v2),
                colour = rainbowColors[cluster]) +
      geom_path(data = elDat2, aes(x = v1, y = v2),
                colour = rainbowColors[cluster]) +
      geom_path(data = elDat3, aes(x = v1, y = v2),
                colour = rainbowColors[cluster]) +
      ggtitle("Contour Plot by Cluster")
  }
  return(ggplotly(plot))
}



#' Return sorted data points using minimax algorithm
#'
#' @param data the data you wish to use
#' @param estep output from the mclust function "estep"
#' @param previousResultIndicies indicies of rows the imbc algorithm previously queried the user on
#' @param distanceMethod a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."
#' @return a vector of points sorted according to the minimax algorithm
#' @examples
#' #Load data
#' library(mclust)
#' data(banknote)
#'
#' #Create new dataset with only continuous variables
#' bankdata <- banknote[,2:7]
#'
#' #create Mclust object
#' object <- Mclust(bankdata)
#' #determine best model
#' model <- object$modelName
#' #extract model parameters
#' param <- object$parameters
#'
#' #output from estep of EM algorithm
#' estepbank <- estep(model, bankdata, param)
#'
#' #we assume we have not previously queried any points
#' previousResultIndicies <- c()
#'
#' #output from minimax algorithm: returns vector of points that are least confidently placed in their respective classes
#' minimax(bankdata, estep, previousResultIndicies)
#'
#' #try with a different distance method
#' minimax(bankdata, estep, previousResultIndicies, distanceMethod = "manhattan")

minimax <- function(data, estep, previousResultIndicies, distanceMethod = "euclidean"){
  Maxs <- apply(estep$z, 1, max) #max of each row of probs
  MaxsFrame <- as.data.frame(cbind(1:length(Maxs), Maxs))
  if (is.null(previousResultIndicies)){
    MaxsFrame <- MaxsFrame
  }
  else{MaxsFrame <- MaxsFrame[-previousResultIndicies,]}
  MaxsSorted <- as.numeric(row.names(MaxsFrame[order(MaxsFrame$Max),]))
  return(MaxsSorted)
}

#' Return sorted data points using maximum within-cluster distance
#'
#' @param data the data you wish to use
#' @param estep output from the mclust function "estep"
#' @param previousResultIndicies indicies of rows the imbc algorithm previously queried the user on
#' @param distanceMethod a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."
#' @return a vector of points sorted according to the maximum within-cluster distance method algorithm
#' @examples
#' #Load data
#' library(mclust)
#' data(banknote)
#'
#' #Create new dataset with only continuous variables
#' bankdata <- banknote[,2:7]
#'
#' #create Mclust object
#' object <- Mclust(bankdata)
#' #determine best model
#' model <- object$modelName
#' #extract model parameters
#' param <- object$parameters
#'
#' #output from estep of EM algorithm
#' estepbank <- estep(model, bankdata, param)
#'
#' #we assume we have not previously queried any points
#' #if this vector contained any row indicies, those would not show up in output sorted vector of the function
#' previousResultIndicies <- c()
#'
#' #output from minimax algorithm: returns vector of points that are least confidently placed in their respective classes
#' maxWithinClust(bankdata, estep, previousResultIndicies)
#'
#' #try with a different distance method
#' maxWithinClust(bankdata, estep, previousResultIndicies, distanceMethod = "manhattan")
maxWithinClust <- function(data, estep, previousResultIndicies, distanceMethod){
  distances <- c()
  for (index in 1:nrow(data)){
    cluster <- Class.train[index] #which cluster is that point in
    IndexVector <- as.double(data[index,]) #data vector
    ClusterVector <- estep$parameters$mean[,cluster] #mean of cluster
    distanceVal <- dist(rbind(IndexVector, ClusterVector), distanceMethod) #dist
    distances <- c(distances, distanceVal) #add to vector of distances
  }
  MaxsFrame <- as.data.frame(cbind(1:length(Maxs), distances))
  if (is.null(previousResultIndicies)){
    MaxsFrame <- MaxsFrame
  }
  else{MaxsFrame <- MaxsFrame[-previousResultIndicies,]}
  MaxsSorted <- as.numeric(row.names(MaxsFrame[order(MaxsFrame$distances, decreasing = TRUE),]))
  return(MaxsSorted)
}

#' Return sorted data points using minimum between-cluster distance
#'
#' @param data the data you wish to use
#' @param estep output from the mclust function "estep"
#' @param previousResultIndicies indicies of rows the imbc algorithm previously queried the user on
#' @param distanceMethod a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."
#' @return a vector of points sorted according to the minimum between-cluster distance method algorithm
#' @examples
#' #Load data
#' library(mclust)
#' data(banknote)
#'
#' #Create new dataset with only continuous variables
#' bankdata <- banknote[,2:7]
#'
#' #create Mclust object
#' object <- Mclust(bankdata)
#' #determine best model
#' model <- object$modelName
#' #extract model parameters
#' param <- object$parameters
#'
#' #output from estep of EM algorithm
#' estepbank <- estep(model, bankdata, param)
#'
#' #we assume we have not previously queried any points
#' #if this vector contained any row indicies, those would not show up in output sorted vector of the function
#' previousResultIndicies <- c()
#'
#' #output from minimax algorithm: returns vector of points that are least confidently placed in their respective classes
#' minBetweenClust(bankdata, estep, previousResultIndicies)
#'
#' #try with a different distance method
#' minBetweenClust(bankdata, estep, previousResultIndicies, distanceMethod = "manhattan")
minBetweenClust <- function(data,estep, previousResultIndicies, distanceMethod){
distances2 <- c()
for (index in 1:nrow(data)){ #for each row
  cluster <- Class.train[index] #which cluster is that point in
  IndexVector <- as.double(data[index,]) #data vector
  clusterDists <- data.frame(nrow = unique(Class.train)-1) #initialize
  for (clusterPos in unique(Class.train)){ #for each cluster
    ClusterVector <- estep$parameters$mean[,clusterPos] #mean of cluster
    distanceVal <- dist(rbind(IndexVector, ClusterVector), distanceMethod) #dist
    clusterDists$nrow[clusterPos] <- distanceVal
  }
  mindist <- min(clusterDists[-cluster,]) #minimum distance discounting which cluster you actually in
  distances2 <- c(distances2, mindist) #add to vector of distances
}
MinsFrame <- as.data.frame(cbind(1:nrow(data), distances2))
if (is.null(previousResultIndicies)){
  MinsFrame <- MinsFrame
}
else{MinsFrame <- MinsFrame[-previousResultIndicies,]}
MinsSorted <- as.numeric(row.names(MinsFrame[order(MinsFrame$distances2),]))
return(MinsSorted)
}




#' Return a BIC object, such as one you would find in the mclust package
#'
#' @param modelNames names of possible models
#' @param G number of clusters
#' @param z A matrix whose [i,k]th entry is the probability that observation i in the test data belongs to the kth class
#' @param data the data you wish to use
#' @return a BIC object (see mclustBIC in the mclust package)
#' @examples
#' #Load data
#' library(mclust)
#' data(banknote)
#'
#' #Create new dataset with only continuous variables
#' bankdata <- banknote[,2:7]
#'
#' #determine best model
#' model <- object$modelName
#' #extract model parameters
#' param <- object$parameters
#'
#' #output from estep of EM algorithm
#' estepbank <- estep(model, bankdata, param)
#'
#' #Create a BIC object, as seen in the mclust package's mclustBIC function
#' #Outputs BIC values for all models, but only for specified number of clusters
#' createBicObject(modelNames, G = object$G, z = estepbank$z, bankdata)
createBicObject <- function(modelNames, G, z = estep$z, data){
  bicFrame = data.frame(matrix(NA, nrow = 1, ncol = 14))
  colnames(bicFrame) = modelNames
  for (i in modelNames){
    mme = me(i, data, z)
    bic = bic(i, mme$loglik, n = nrow(data), d = ncol(data), G = G)
    bicFrame[1,i] = bic}
  rownames(bicFrame) = as.character(G)
  RET = data.frame(matrix(0, nrow = 1, ncol = 14))
  colnames(RET) <- modelNames
  rownames(RET) <- as.character(G)
  bicFrame <<- bicFrame
  bicFrame <- as.matrix(bicFrame)
  struct = structure(bicFrame, G = G, modelNames = modelNames, prior = NULL,
                     control = emControl(), initialization = list(hcPairs = hc(data),
                                                                  subset = NULL, noise = NULL),
                     Vinv = NULL, warn = FALSE, n = nrow(data), d = ncol(data), oneD = FALSE,
                     criterion = "BIC", returnCodes = RET, class = "mclustBIC")
  struct
}


#' A function that combines model-based clustering as well as your input to cluster your data
#'
#' @param data the data you wish to use (must be continuous)
#' @param n the number of points you wish to be queried on at once
#' @param G number of clusters. The default allows Mclust to identify the number of clusters
#' @param query how you wish to be queried. This must be one of "minimax", "maxWithinClust" or "minBetweenClust", with a default of "minimax".
#' @param distanceMethod a method used to find distances between points. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski", with a default of "euclidean."
#' @param iterationMax the maximum number of iterations you wish to see when converging mstep and estep
#' @return An object providing the optimal (according to BIC) mixture model estimation.
#'
#' @return The details of the output components are as follows:
#' \item{call}{The matched call}
#' \item{data}{The input data matrix}
#' \item{modelName}{A character string denoting the model at which the optimal BIC occurs}
#' \item{n}{The number of observations in the data}
#' \item{d}{The dimension of the data}
#' \item{G}{The optimal number of mixture components}
#' \item{BIC}{All BIC values}
#' \item{bic}{Optimal BIC value}
#' \item{loglik}{The log-likelihood corresponding to the optimal BIC}
#' \item{df}{The number of estimated parameters}
#' \item{hypvol}{The hypervolume parameter for the noise component if required, otherwise set to NULL (see hypvol).}
#' \item{parameters}{A list with the following components:}
#' \item{pro}{A vector whose kth component is the mixing proportion for the kth component of the mixture model. If missing, equal proportions are assumed}
#' \item{mean}{The mean for each component. If there is more than one component, this is a matrix whose kth column is the mean of the kth component of the mixture model.}
#' \item{variance}{A list of variance parameters for the model. The components of this list depend on the model specification. See the help file for mclustVariance for details.}
#' \item{z}{A matrix whose [i,k]th entry is the probability that observation i in the test data belongs to the kth class}
#' \item{classification}{The classification corresponding to z, i.e. map(z).}
#' \item{uncertainty}{The uncertainty associated with the classification.}
#' @examples
#' #Load data
#' library(mclust)
#' data(banknote)
#'
#' #Create new dataset with only continuous variables
#' bankdata <- banknote[,2:7]
#'
#' #Run imbc while querying user on onyl 1 data point at a time
#' #Use default querying algorithm (minimax)
#' output <- imbc(bankdata)
#'
#' #query two points at once and using minimum between cluster distance as query method, and specifying 2 clusters
#' output2 <- imbc(bankdata, n = 2, G = 2, query = "minBetweenClust")
#'
#' #gives vector of classification of each row
#' output2$classification
#'
#' #classification probability matrix
#' #output2$z

imbc <- function(data, n = 1, G = NULL, query = "minimax", distanceMethod = "euclidean", iterationMax = 500){
  require(ggplot2)
  require(mclust)
  require(clustvarsel)
  require(plotly)
  options(warn = 0)
  call = match.call()
  #find G if G not specified
  if (is.null(G)){
    object <- Mclust(data)}
  #else use G specified
  else {object <- Mclust(data, G = G)}
  G = object$G
  modelName <- object$modelName
  modelNames <- mclust.options("emModelNames")
  #initialize values
  resultIndicies.Train <- "None"
  previousResultIndicies <- c()
  previousResultIndiciesTrain <- c()
  Class.train <- object$classification #for colors in plots
  #initialize estep
  estep <- mclust::estep(modelName, data, parameters = object$parameters)
  previousLogLik <- estep$loglik #first log liklihood of estep
  currentLogLik <- 0 #initial current log liklihood
  loglikVector <- c()
  while(resultIndicies.Train != "STOP"){
    originalEstep <- estep #start each model in same place
    estep <- originalEstep #start each model in same place
    iteration = 0
    #run once for original model until convergence
    while((abs(previousLogLik-currentLogLik)>1e-5)|iteration < iterationMax){
      #perform EM until convergence
      mstep <- mclust::mstep(modelName, data, z = estep$z)
      estep <- mclust::estep(modelName, data,
                             parameters = mstep$parameters)
      #update probabilities of each class
      estep$z[previousResultIndicies,][cbind(seq_along(previousResultIndiciesTrain), previousResultIndiciesTrain)] <- 1
      #go through each changed index
      for (i in previousResultIndicies){
        #find classes you need to change probability to 0
        clustersTo0 <- setdiff(unique(Class.train), Class.train[i])
        #for each of those classes change prob
        for (class in clustersTo0){
          estep$z[i,][cbind(seq_along(class), class)] <- 0
        }
      }
      #update previous and current log liklihoods
      previousLogLik <- currentLogLik
      currentLogLik <- estep$loglik
      iteration = iteration + 1}
    bestEstep <- estep #best
    bestBIC <- bic(modelName, bestEstep$loglik, nrow(data),
                   d = ncol(data), G = bestEstep$G)
    bestModelName <- modelName
    for (name in modelNames){ #try all possible models
      estep <- originalEstep #start each model in same place
      iteration = 0
      while(iteration < iterationMax){ #until convergence
        #perform EM until convergence
        mstep <- mclust::mstep(name, data, z = estep$z)
        estep <- mclust::estep(name, data,
                               parameters = mstep$parameters)
        estep$z[previousResultIndicies,][cbind(seq_along(previousResultIndiciesTrain), previousResultIndiciesTrain)] <- 1
        #go through each changed index
        for (i in previousResultIndicies){
          #find classes you need to change probability to 0
          clustersTo0 <- setdiff(unique(Class.train), Class.train[i])
          #for each of those classes change prob
          for (class in clustersTo0){
            estep$z[i,][cbind(seq_along(class), class)] <- 0
          }
        }
        #update previous and current log liklihoods
        previousLogLik <- currentLogLik
        currentLogLik <- estep$loglik
        iteration = iteration + 1}
      #features of the current model to compare
      currentBIC <- bic(name, estep$loglik, nrow(data),
                        d = ncol(data), G = estep$G)
      currentModelName <- name
      currentEstep <- estep
      #if current better than best, make current best
      if (abs(currentBIC) < abs(bestBIC)) {
        bestEstep <- estep
        bestBIC <- currentBIC
        bestModelName <- currentModelName}
    }
    estep <- bestEstep
    loglikVector <- c(loglikVector, estep$loglik)
    z <- estep$z
    modelName <- bestModelName
    Class.train <- apply(z,1,which.max) #how shall we classify
    if (resultIndicies.Train != "None"){
      Class.train[resultIndicies] <- resultIndicies.Train #change labels to what human says
    }
    Class.train[previousResultIndicies] <- previousResultIndiciesTrain
    #What to query
    queryName <- match.fun(query)
    MaxsSorted <- queryName(data,estep,previousResultIndicies, distanceMethod)
    resultIndicies <- MaxsSorted[1:n] #add to resulting vector
    plotType <- readline("What kind of plot would you like to see? (1 for MDS, 2 for Contour, 3 for Pairs)")
    #Plot
    if (plotType == "1"){
      plot <- MDSplot(object, distanceMethod, Class.train, resultIndicies, G)
    }
    if (plotType == "2"){
      plot <- ellipsePlot(object, distanceMethod, Class.train, resultIndicies, G)
    }
    #Pairs plot
    if (plotType == "3"){
      plot <- pairsPlot(object, data, distanceMethod, Class.train, resultIndicies, G)
    }
    print(ggplotly(plot))
    print(resultIndicies) #print indicies
    resultIndicies.Train <- readline("Which clusters are the circled points in? (Type STOP to Quit)  ")
    if (resultIndicies.Train=="STOP"){  #stop the loop if human says stop
      estep$z[previousResultIndicies,][cbind(seq_along(previousResultIndiciesTrain), previousResultIndiciesTrain)] <- 1
      Class.train[previousResultIndicies] <- previousResultIndiciesTrain
      uncertainty = 1-estep$z[cbind(seq_along(Class.train), Class.train)]
      bic1 = bic(modelName, estep$loglik, nrow(data), d = ncol(data), G = G)
      BIC = createBicObject(modelNames, G, z = estep$z, data)
      ans <- list(call, data, modelName, nrow(data), ncol(data), G, BIC, bic1, estep$loglik, df, NA, estep$parameters, estep$z, Class.train, uncertainty)
      orderedNames <- c("call", "data", "modelName", "n", "d",
                        "G", "BIC", "bic", "loglik", "df", "hypvol", "parameters",
                        "z", "classification", "uncertainty")
      names(ans) <- orderedNames
      assign("loglikVector", loglikVector, envir = .GlobalEnv)
      return(ans)
    }
    resultIndicies.Train <-as.integer(unlist(strsplit(resultIndicies.Train,",")))
    estep$z[resultIndicies,][cbind(seq_along(resultIndicies.Train), resultIndicies.Train)] <- 1 #change prob of being in that cluster to 1 in result indicies where human said that is definitely the cluster
    estep$z[previousResultIndicies,][cbind(seq_along(previousResultIndiciesTrain), previousResultIndiciesTrain)] <- 1
    previousResultIndicies <- c(previousResultIndicies, resultIndicies)
    previousResultIndiciesTrain <- c(previousResultIndiciesTrain, resultIndicies.Train)
  }
}
