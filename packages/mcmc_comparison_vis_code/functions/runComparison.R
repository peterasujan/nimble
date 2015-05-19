runComparison<-function(allModels,mcmcs,niter=10000,thin=1,standir,plot_on=F){
  library(nimble)
  library(rjags)
  library(rstan)
  library(xtable)
  library(ggplot2)
  library(gridExtra)
  library(igraph)
  
  x=list()
  for (i in 1:length(allModels)){
    x[[i]]<-readBUGSmodel(model=allModels[i],
                          dir=getBUGSexampleDir(allModels[i]),
                          returnModelComponentsOnly=TRUE)
  }
  
  mods=list()
  
  for (i in 1:length(allModels)){
    print(allModels[i])
    suite_output <- MCMCsuite(x[[i]]$model, constants = x[[i]]$data, inits = x[[i]]$inits, 
                               MCMCs = mcmcs,makePlot=plot_on,savePlot=plot_on,niter=niter,thin=thin
                              ,summaryStats=c('mean','median','sd','CI95_low','CI95_upp','effectiveSize')
                              #change
                              ,MCMCdefs = list(noConj = quote({ configureMCMC(Rmodel, useConjugacy=FALSE) }))
                              ,stan_model=paste(standir,allModels[i],'/',allModels[i],'.stan',sep="")
    )
    mods[allModels[i]][[1]]=list(suite_output$summary,create_time_df(suite_output$timing,length(mcmcs)))
  }
  return(mods)
}
  

