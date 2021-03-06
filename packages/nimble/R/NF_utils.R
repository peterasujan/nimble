#' Basic nimbleFunctions for calculate, simulate, and getLogProb with a set of nodes
#'
#' simulate, calculate, or get existing log probabilities for the current values in a NIMBLE model
#'
#' @param model       A NIMBLE model
#' @param nodes       A set of nodes. If none are provided, default is all \code{model$getNodeNames()}
#' @author Perry de Valpine
#' @export
#' @aliases calcNodes getLogProbNodes
#' @details
#' These are basic nimbleFunctions that take a model and set of nodes and return a function that will call \code{calculate}, \code{simulate}, or \code{getLogProb} on those nodes.  Each is equivalent to a direct call from R, but in nimbleFunction form they can be be compiled and can be put into a nimbleFunctionList.  For example, \code{myCalc <- calcNodes(model, nodes); ans <- myCalc()} is equivalent to \code{ans <- calculate(model, nodes)}, but one can also do \code{CmyCalc <- compileNimble(myCalc)} to get a faster version.
#'
#' In nimbleFunctions, for only one set of nodes, it is equivalent or slightly better to simply use \code{calculate(model, nodes)} in the run-time code.  The compiler will process the model-nodes combination in the same way as would occur by creating a specialized \code{calcNodes} in the setup code.  However, if there are multiple sets of nodes, one can do the following:
#'
#' Setup code: \code{myCalcs <- nimbleFunctionList(calcNodes); myCalcs[[1]] <- calcNodes(model, nodes[[1]]); myCalcs[[2]] <- calcNodes[[2]]}
#'
#' Run code: \code{for(i in seq_along(myCalcs)) {ans[i] <- myCalcs[[i]]()} }
#' 
simNodes <- nimbleFunction(
    setup = function(model, nodes){
        if(missing(nodes) )
            nodes <- model$getNodeNames()
        else {
            nodes <- model$expandNodeNames(nodes)
            nodes <- model$topologicallySortNodes(nodes)
        }
    },
    run = function(){
        simulate(model, nodes)
    },
    where = getLoadingNamespace())


calcNodes <- nimbleFunction(
	setup = function(model, nodes){
		if(missing(nodes) )
                    depNodes <- model$getNodeNames()
                else
                    depNodes <- model$getDependencies(nodes)
	},
	run = function(){
            ans <- calculate(model, depNodes)
            return(ans)
            returnType(double())
	},	
    where = getLoadingNamespace())

getLogProbNodes <- nimbleFunction(
	setup = function(model, nodes) {
		if(missing(nodes) )
                    depNodes <- model$getNodeNames()
                else
                    depNodes <- model$getDependencies(nodes)
	},
	run = function(){
            ans <- getLogProb(model, depNodes)
            return(ans)
            returnType(double())
	},
	where = getLoadingNamespace()	
)


#' Basic nimbleFunctions for using a NIMBLE model with sets of stored values
#'
#' simulate, calculate, or get the existing log probabilities for values in a modelValues object using a NIMBLE model
#'
#' @param model		A nimble model. 
#' @param nodes		A set of nodes. If none are provided, default is all \code{model$getNodeNames()}
#' @param mv		A modelValues object in which multiple sets of model variables and their corresponding logProb values are or will be saved. \code{mv} must include the nodes provided
#' @author Clifford Anderson-Bergman
#' @export
#' @details
#' \code{simNodesMV} simulates values in the given nodes and saves them in \code{mv}. \code{calcNodesMV} calculates these nodes for each row of \code{mv} and returns a vector of the total log probabilities (densities) for each row. \code{getLogProbNodesMV} is like \code{calcNodesMV} without actually doing the calculations.
#'
#' Each of these will expand variables or index blocks and topologically sort them so that each node's parent nodes are processed before itself.
#'
#' \code{getLogProbMV} should be used carefully.  It is generally for situations where the logProb values are guaranteed to have already been calculated, and all that is needed is to query them.  The risk is that a program may have changed the values in the nodes, in which case \code{getLogProbMV} would collect logProb values that are out of date with the node values.
#' 
#' @aliases calcNodesMV getLogProbNodesMV
#' 
#' @section Run time arguments:
#' \itemize{
#'	\item{\code{m} }{
#'
#'	(\code{simNodesMV} only). Number of simulations requested.}
#'
#'      \item{\code{saveLP}}{
#' 
#'      (\code{calcNodesMV}only). Whether to save the logProb values in \code{mv}.  Should be given as \code{TRUE} unless there is a good reason not to.}
#'
#' }
#'
#' @return from \code{simNodesMV}: NULL.  from \code{calcNodesMV} and \code{getLogProbMV}: a vector of the sum of log probabilities (densities) from any stochastic nodes in \code{nodes}.  
#' 
#'	
#' @examples
#' code <- nimbleCode({
#'	for(i in 1:5)
#'	x[i] ~ dnorm(0,1)
#' })
#'
#' myModel <- nimbleModel(code)
#' myMV <- modelValues(myModel)
#'
#' Rsim <- simNodesMV(myModel, myMV)
#' Rcalc <- calcNodesMV(myModel, myMV)
#' Rglp <- getLogProbNodesMV(myModel, myMV)
#' \dontrun{
#'   cModel <- compileNimble(myModel)
#'   Csim <- compileNimble(Rsim, project = myModel)
#'   Ccalc <- compileNimble(Rcalc, project = myModel)
#'   Cglp <- compileNimble(Rglp, project = myModel)
#'   Csim$run(10)
#'   Ccalc$run(saveLP = TRUE)
#'   Cglp$run()	#Gives identical answers to Ccalc because logProbs were saved
#'   Csim$run(10)
#'   Ccalc$run(saveLP = FALSE)
#'   Cglp$run()	  #Gives wrong answers because logProbs were not saved
#' }
simNodesMV <- nimbleFunction(
    setup = function(model, mv, nodes) {
        if(missing(nodes) )
            nodes <- model$getNodeNames()
        else {
            nodes <- model$expandNodeNames(nodes)
            nodes <- model$topologicallySortNodes(nodes)
        }
    },
    run = function(m = integer(0)){
        resize(mv, m)
        for(i in 1:m){
            simulate(model, nodes)
            nimCopy(from = model, to = mv, nodes = nodes, row = i)
        } 
    },
    where = getLoadingNamespace())

## ### Basic nimble functions for simulating from a nimble model
## ###
## ### \code{calcNodes} computes the log probabilities of the stored node values in \code{mv} and returns a 
## ### a vector of computed log probabilities. 
## ###
## ### @param model		A nimble model. Must have nodes provided by \code{node} argument
## ### @param nodes		A set of nodes. If none are provided, default is all \code{model$getNodeNames}
## ### @param mv		A modelValues object to which the simulated values are saved (\code{simNodes}), 
## ### log probabilities are calculated (\code{calcNodes})
## ### or log probabilities are retreaved (\code{getLogProbNodes}). It is very important that the modelValues objects must have the nodes provided
## ### by \code{node} argument, along with the corresponding \code{'logProb_(nodeName)'}. 
## ### @author Clifford Anderson-Bergman
## ### @export
## ### @details
## ### Basic nimble functions that manipulate nimble models. \code{simNodes} simulates over the given nodes. \code{calcNodes} calculates the log probability 
## ### of these nodes and \code{getLogProbNodes} retreaves the stored log probabilities WITHOUT recomputing the log probabilities. \code{getLogProbNodes} saves
## ### time if the log probabilities have already been calculated.
## ### @section Run time arguments:
## ### \itemize{
## ###	\item{\code{saveLP}}{
## ###	
## ###	(\code{calcNodes} only) logical scalar. Whether log probabilities should be saved in model values after being calculated}
## ###	}
## ###	
## ### @examples
## ### code <- nimbleCode({
## ###	for(i in 1:5)
## ###	x[i] ~ dnorm(0,1)
## ### })
## ###
## ### myModel <- nimbleModel(code)
## ### myMV <- modelValues(myModel)
## ### cModel <- compileNimble(myModel)
## ###
## ### Rsim <- simNodes(myModel, myMV)
## ### Rcalc <- calcNodes(myModel, myMV)
## ### Rglp <- getLogProbNodes(myModel, myMV)
## ### Csim <- compileNimble(Rsim, project = myModel)
## ### Ccalc <- compileNimble(Rcalc, project = myModel)
## ### Cglp <- compileNimble(Rglp, project = myModel)
## ### Csim(10)
## ### Ccalc(saveLP = TRUE)
## ### Cglp()	#Gives identical answers to Ccalc because logProbs were saved
## ### Csim(10)
## ### Ccalc(saveLP = FALSE)
## ### Cglp()	#Gives wrong answers because logProbs were not saved
calcNodesMV <- nimbleFunction(
	setup = function(model, mv, nodes) {
		if(missing(nodes) )
                    nodes <- depNodes <- model$getNodeNames()
                else 
                    depNodes <- model$getDependencies(nodes)
		logPvec <- rep(0,2)
	},
	run = function(saveLP = logical()){
		m <- getsize(mv)
		setSize(logPvec, m)
		for(i in 1:m){
			nimCopy(from = mv, to = model, nodes = nodes, row = i)
			logPvec[i] <<- calculate(model, depNodes)	
			if(saveLP)
				nimCopy(from = model, to = mv, nodes = depNodes, rowTo = i, logProb = TRUE)
		}
	returnType(double(1))
	return(logPvec)
	},	
where = getLoadingNamespace())


## ### Basic nimble functions for simulating from a nimble model
## ###
## ### \code{getLogProbNodes} simply extracts the saved log probabilities of the given nodes without calculating.
## ### Note that using \code{getLogProbNodes} can save a lot of computation time, but requires the user to be very careful about always calling
## ### making sure that the log probabilities have been correctly entered (see example)
## ###
## ### @param model		A nimble model. Must have nodes provided by \code{node} argument
## ### @param nodes		A set of nodes. If none are provided, default is all \code{model$getNodeNames}
## ### @param mv		A modelValues object to which the simulated values are saved (\code{simNodes}), 
## ### log probabilities are calculated (\code{calcNodes})
## ### or log probabilities are retreaved (\code{getLogProbNodes}). It is very important that the modelValues objects must have the nodes provided
## ### by \code{node} argument, along with the corresponding \code{'logProb_(nodeName)'}. 
## ### @author Clifford Anderson-Bergman
## ### @export
## ### @details
## ### Basic nimble functions that manipulate nimble models. \code{simNodes} simulates over the given nodes. \code{calcNodes} calculates the log probability 
## ### of these nodes and \code{getLogProbNodes} retreaves the stored log probabilities WITHOUT recomputing the log probabilities. \code{getLogProbNodes} saves
## ### time if the log probabilities have already been calculated.
## ### @section Run time arguments:
## ### none
## ###	
## ### @examples
## ### code <- nimbleCode({
## ###     for(i in 1:5) {
## ###         x[i] ~ dnorm(0,1)
## ###     }
## ### })
## ###
## ### myModel <- nimbleModel(code)
## ### myMV <- modelValues(myModel)
## ### cModel <- compileNimble(myModel)
## ###
## ### Rsim <- simNodes(myModel, myMV)
## ### Rcalc <- calcNodes(myModel, myMV)
## ### Rglp <- getLogProbNodes(myModel, myMV)
## ### Csim <- compileNimble(Rsim, project = myModel)
## ### Ccalc <- compileNimble(Rcalc, project = myModel)
## ### Cglp <- compileNimble(Rglp, project = myModel)
## ### Csim(10)
## ### Ccalc(saveLP = TRUE)
## ### Cglp()	#Gives identical answers to Ccalc because logProbs were saved
## ### Csim(10)
## ### Ccalc(saveLP = FALSE)
## ### Cglp()	#Gives wrong answers because logProbs were not saved
getLogProbNodesMV <- nimbleFunction(
	setup = function(model, mv, nodes, expandNodes = TRUE, sortNodes = TRUE) {
		if(missing(nodes) )
                    nodes <- depNodes <- model$getNodeNames()
                else
                    depNodes <- model$getDependencies(nodes)
		logPvec <- rep(0,2)
	},
	run = function(){
		m <- getsize(mv)
		setSize(logPvec, m)
		for(i in 1:m){
			nimCopy(from = mv, to = model, nodes = depNodes, row = i, logProb = TRUE)
			logPvec[i] <<- getLogProb(model, depNodes)	
		}
	returnType(double(1))
	return(logPvec)
	},
	where = getLoadingNamespace()	
)


## NIMBLE DSL functions for creating vector or array strctures
## added by Daniel June 2015
## these deserve documentation!

#' @export
nimVector <- nimbleFunction(
    run = function(value = double(), length = double()) {
        declare(vec, double(1, length))
        for(i in 1:length)   vec[i] <- value
        returnType(double(1))
        return(vec)
    },  where = getLoadingNamespace()
)

#' @export
nimArray <- nimbleFunction(
    run = function(value = double(), nrow = double(), ncol = double()) {
        declare(arr, double(2, c(nrow, ncol)))
        for(i in 1:nrow)   for(j in 1:ncol)   arr[i, j] <- value
        returnType(double(2))
        return(arr)
    },  where = getLoadingNamespace()
)

#' @export
identityMatrix <- nimbleFunction(
    run = function(d = double()) {
        declare(arr, double(2, c(d, d)))
        for(i in 1:d)   for(j in 1:d)   arr[i, j] <- 0
        for(i in 1:d)                   arr[i, i] <- 1
        returnType(double(2))
        return(arr)
    },  where = getLoadingNamespace()
)




