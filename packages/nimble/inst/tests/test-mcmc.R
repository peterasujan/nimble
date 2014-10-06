source(system.file(file.path('tests', 'test_utils.R'), package = 'nimble'))

context("Testing of default MCMC")

### TODO: add in the special cases for leuk,salm,air,jaw,dipper


### Beginning of actual tests

test_mcmc('blocker', numItsC = 1000, resampleData = TRUE)
# 100% coverage; looks fine

test_mcmc('bones', numItsC = 10000, resampleData = TRUE)
# 100% coverage; looks fine

test_mcmc('dyes', numItsC = 1000, resampleData = TRUE)
# 100% coverage; looks fine

test_mcmc('equiv', numItsC = 1000, resampleData = TRUE)
# looks good
# testing: tau[2]=97.95, 198.8 ; tau[1]=102.2,55
# phi = -.008,.052; pi = -.1805,.052

test_mcmc('line', numItsC = 1000, resampleData = TRUE)
# 100% coverage; looks fine

test_mcmc('oxford', numItsC = 1000, resampleData = TRUE)
# looks good

test_mcmc('pump', numItsC = 1000, resampleData = TRUE)
# 100% coverage; looks fine

test_mcmc('rats', numItsC = 1000, resampleData = TRUE)
# 93.8% coverage; looks fine

test_mcmc('dugongs', numItsC = 1000, resampleData = TRUE)
# 100% coverage; looks fine

if(FALSE) {
allModels <- c(# vol1
               'blocker', 'bones', 'dyes', 'equiv', 'line', 'oxford', 'pump', 'rats',
               # 'bones',
               # vol2
               'dugongs')

sapply(allModels, test_mcmc, numItsC = 1000)
}

test_mcmc('epil', model = 'epil2.bug', inits = 'epil-inits.R',
              data = 'epil-data.R', numItsC = 1000, resampleData = TRUE)
# looks ok

test_mcmc('epil', model = 'epil3.bug', inits = 'epil-inits.R',
              data = 'epil-data.R', numItsC = 1000, resampleData = TRUE)
# same deal as epil2.bug

test_mcmc('seeds', model = 'seedsuni.bug', inits = 'seeds-init.R',
              data = 'seeds-data.R', numItsC = 1000, resampleData = TRUE)
# looks fine - intervals for b's seem a bit large but probably ok
test_mcmc('seeds', model = 'seedssig.bug', inits = 'seeds-init.R',
              data = 'seeds-data.R', numItsC = 1000, resampleData = TRUE)
# looks fine - intervals for b's seem a bit large but probably ok

test_mcmc('birats', model = 'birats1.bug', inits = 'birats-inits.R',
              data = 'birats-data.R', numItsC = 1000, resampleData = TRUE)
# seems fine
test_mcmc('birats', model = 'birats3.bug', inits = 'birats-inits.R',
              data = 'birats-data.R', numItsC = 1000, resampleData = TRUE)
# seems fine
if(FALSE) { # don't run w/ Wish conj issue
  test_mcmc('birats', model = 'birats2.bug', inits = 'birats-inits.R',
            data = 'birats-data.R', numItsC = 1000, resampleData = TRUE)
}

test_mcmc('ice', model = 'icear.bug', inits = 'ice-inits.R',
              data = 'ice-data.R', numItsC = 1000, resampleData = TRUE)
# hmm - resampleData gives very large CIs - need to look into this

test_mcmc('beetles', model = 'beetles-logit.bug', inits = 'beetles-inits.R',
              data = 'beetles-data.R', numItsC = 1000, resampleData = TRUE)
# have not run beetles yet

system(paste("sed 's/mean(age)/mean(age\\[1:M\\])/g'", system.file('classic-bugs','vol2','jaw','jaw-linear.bug', package = 'nimble'), ">", file.path(tempdir(), "jaw-linear.bug"))) # alternative way to get size info in there
test_mcmc(model = file.path(tempdir(), "jaw-linear.bug"), inits = system.file('classic-bugs', 'vol2', 'jaw','jaw-inits.R', package = 'nimble'), data = system.file('classic-bugs', 'vol2', 'jaw','jaw-data.R', package = 'nimble'), numItsC = 1000)
# mcmcspec error with looking for mu[1:4] when checking conj of Omega

# vectorized version of jaw to try to deal with scalar/vec bug
model <- function() {
  for (i in 1:N) {
     Y[i,1:M] ~ dmnorm(mu[1:M], Omega[1:M,1:M]);  # The 4 measurements for each  
  }                                   # boy are multivariate normal

  mu[1:M] <- beta0 * ones[1:M] + beta1 * age[1:4] - mean(age[1:4])
  beta0.uncentred <- beta0 - beta1 * mean(age[1:4]);

  beta0 ~ dnorm(0.0, 0.001); 
  beta1 ~ dnorm(0.0, 0.001); 
  Omega[1:M,1:M] ~ dwish(R[1:M,1:M], 4);	# between-child variance in length at each age	
  #Sigma2[1:M,1:M] <- inverse(Omega[1:M,1:M]);

  for (i in 1:N) {
     for  (j in 1:M) {
        resid[i,j] <- Y[i,j] - mu[j];         # residuals
        resid2[i,j] <- resid[i,j]^2;     # squared residuals
     } 
  }
  RSS <- sum(resid2[1:N,1:M]);                    # Residual Sum of Squares
}
 
inits = list(beta0 = 40, beta1 = 0)
data =list(M=4,N=20, Y = matrix(c(47.8, 46.4, 46.3, 45.1, 47.6, 52.5, 51.2, 49.8, 48.1, 
45, 51.2, 48.5, 52.1, 48.2, 49.6, 50.7, 47.2, 53.3, 46.2, 46.3, 
48.8, 47.3, 46.8, 45.3, 48.5, 53.2, 53, 50, 50.8, 47, 51.4, 49.2, 
52.8, 48.9, 50.4, 51.7, 47.7, 54.6, 47.5, 47.6, 49, 47.7, 47.8, 
46.1, 48.9, 53.3, 54.3, 50.3, 52.3, 47.3, 51.6, 53, 53.7, 49.3, 
51.2, 52.7, 48.4, 55.1, 48.1, 51.3, 49.7, 48.4, 48.5, 47.2, 49.3, 
53.7, 54.5, 52.7, 54.4, 48.3, 51.9, 55.5, 55, 49.8, 51.8, 53.3, 
49.5, 55.3, 48.4, 51.8)  , 20, 4), age = c(8, 8.5, 9, 9.5),
  R = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), 4 ,4),
  ones = rep(1, 4))
test_mcmc(model = model, data = data, inits = inits, numItsC = 1000)

  
test_mcmc('pump', resampleData = TRUE, results = list(mean = list(
                    "theta[1]" = 0.06,
                    "theta[2]" = 0.10,
                    "theta[9]" = 1.58,
                    "theta[10]" = 1.97,
                    alpha = 0.73,
                    beta = 0.98)),
          resultsTolerance = list(mean = list(
            "theta[1]" = 0.01,
            "theta[2]" = 0.01,
            "theta[9]" = 0.05,
            "theta[10]" = 0.05,
            alpha = 0.1,
            beta = 0.1)))


### Daniel's world's simplest MCMC demo

code <- modelCode({
    x ~ dnorm(0, 2)
    y ~ dnorm(x+1, 3)
    z ~ dnorm(y+2, 4)
})
data = list(y = 3)

test_mcmc(model = code, data = data, resampleData = FALSE, results = list(
                                       mean = list(x = 6/5, z = 5),
                                       sd = list(x = 1/sqrt(5), z = 1/2)),
          resultsTolerance = list(mean = list(x = .1, z = .1),
            sd = list(x = .05, z = .05)))

### basic block sampler example

code <- modelCode({
    for(i in 1:3) {
        x[i] ~ dnorm(0, 1)
        y[i] ~ dnorm(x[i], 2)
    }
})
data = list(y = -1:1)

test_mcmc(model = code, data = data, resampleData = FALSE, results = list(
                                       mean = list(x = c(-2/3,0,2/3)),
                                       var = list(x = rep(1/3,3))),
          resultsTolerance = list(mean = list(x = rep(.1,3)),
            var = list(x = rep(.05,3))))

test_mcmc(model = code, data = data, resampleData = FALSE, results = list(
                                       mean = list(x = c(-2/3,0,2/3)),
                                       var = list(x = rep(1/3,3))),
          resultsTolerance = list(mean = list(x = rep(.1,3)),
            var = list(x = rep(.05,3))),
          samplers = list(
            list(type = 'RW_block', control = list(targetNodes = 'x[1]')),
            list(type = 'RW_block', control = list(targetNodes = 'x[2]')),
            list(type = 'RW_block', control = list(targetNodes = 'x[3]'))
            ), numItsC = 10000)

test_mcmc(model = code, data = data, resampleData = FALSE, results = list(
                                       mean = list(x = c(-2/3,0,2/3)),
                                       var = list(x = rep(1/3,3))),
          resultsTolerance = list(mean = list(x = rep(.1,3)),
            var = list(x = rep(.05,3))),
          samplers = list(
            list(type = 'RW_block', control = list(targetNodes = 'x', adaptInterval = 500))
            ), numItsC = 10000)



### slice sampler example

code <- BUGScode({
    z ~ dnorm(0, 1)
    normal5_10 ~ dnorm(5, sd = 10)
    beta1_1 ~ dbeta(1, 1)
    beta3_5 ~ dbeta(3, 5)
    binom10_p5 ~ dbin(size=10, prob=0.5)
    binom20_p3 ~ dbin(size=20, prob=0.3)
})

test_mcmc(model = code, resampleData = FALSE, results = list(
                                       mean = list(z = 0, "beta1_1" = 0.5, "beta3_5" = 3/(3+5),
                                         "binom10_p5" = 10*.5, "binom20_p3" = 20*.3),
                                         sd = list(z = 1, "beta1_1" = sqrt(1/12),
                                           "beta3_5" = sqrt(3*5/((3+5)^2*(3+5+1))),
                                          "binom10_p5" = sqrt(10*.5*.5),
                                           "binom20_p3" = sqrt(20*.3*.7))),
          resultsTolerance = list(
                                       mean = list(z = 0.1, "beta1_1" = 0.5, "beta3_5" = .2,
                                         "binom10_p5" = .25, "binom20_p3" = .25),
                                         sd = list(z = .1, "beta1_1" = .05, "beta3_5" = .03,
                                          "binom10_p5" = .2, "binom20_p3" = .25)),
          samplers = list(list(type = 'slice', control = list(targetNode = 'z', adaptInterval = 10)),
            list(type = 'slice', control = list(targetNode = 'normal5_10', adaptInterval = 10)),
           list(type = 'slice', control = list(targetNode = 'beta1_1', adaptInterval = 10)),
            list(type = 'slice', control = list(targetNode = 'beta3_5', adaptInterval = 10)),
            list(type = 'slice', control = list(targetNode = 'binom10_p5', adaptInterval = 10)),
            list(type = 'slice', control = list(targetNode = 'binom20_p3', adaptInterval = 10))))



### demo2 of check conjugacy

code <- BUGScode({
    x ~ dbeta(3, 13)
    y[1] ~ dbin(x, 10)
    y[2] ~ dbin(x, 20)
})
data = list(y = c(3,4))

test_mcmc(model = code, data = data, exactSample = list(x = c(0.195510839527966, 0.332847482503424,0.247768152764931, 0.121748195439553, 0.157842271774841, 0.197566496350904, 0.216991517500577, 0.276609942874852, 0.165733872345582, 0.144695512780252)), seed = 0)

### checkConjugacy_demo3_run.R - various conjugacies

code <- BUGScode({
    x ~ dgamma(1, 1)       # should satisfy 'gamma' conjugacy class
    a  ~ dnorm(0, x)     # should satisfy 'norm' conjugacy class
    a2 ~ dnorm(0, tau = 3*x+0)
    b  ~ dpois(0+5*x)
    b2 ~ dpois(1*x*1)
    c ~ dgamma(1, 7*x*5)
    for(i in 2:3) {
        jTau[i] <- 1
        jNorm[i] ~ dnorm(c * (a+3) - i, var = jTau[i])
        kTauSd[i] <- 2
        kLogNorm[i] ~ dlnorm(0 - a - 6*i, kTauSd[i])
    }
})

sampleVals = list(x = c(3.950556165467749, 1.556947815895538, 1.598959152023738, 2.223758981790340, 2.386291653164086, 3.266282048060261, 3.064019155073057, 3.229661999356182, 1.985990552839427, 2.057249437940977),
  c = c( 0.010341199485849559, 0.010341199485849559, 0.003846483017887228, 0.003846483017887228, 0.007257679932131476, 0.009680314740728335, 0.012594777095902964, 0.012594777095902964, 0.018179641351556003, 0.018179641351556003))

test_mcmc(model = code, data = data, exactSample = sampleVals, seed = 0, mcmcControl = list(scale=0.01))

### block sampler on MVN node

code <- modelCode({
    mu[1] <- 10
    mu[2] <- 20
    mu[3] <- 30
    x[1:3] ~ dmnorm(mu[1:3], prec = Q[1:3,1:3])    
})

Q = matrix(c(1.0,0.2,-1.0,0.2,4.04,1.6,-1.0,1.6,10.81), nrow=3)
data = list(Q = Q)
inits = list(x = c(10, 20, 30))

test_mcmc(model = code, data = data, seed = 0, numItsC = 10000,
          results = list(mean = list(x = c(10,20,30)),
          var = list(x = diag(solve(Q)))),
          resultsTolerance = list(mean = list(x = rep(1,3)),
            var = list(x = c(.1, .03, .01))),
          samplers = list(
            list(type = 'RW_block', control = list(targetNodes = 'x[1:3]'))))
# caution: setting targetNodes='x' works but the initial end sampler is not removed because x[1:3] in targetNode in default sampler != 'x' in targetNodes passed in
if(FALSE) {
    Rmodel <- nimbleModel(code, constants = list(Q=Q))
    mcmcspec <- MCMCspec(Rmodel, nodes = NULL)
    mcmcspec$addSampler(type = 'RW_block', control = list(targetNodes = 'x', adaptInterval=500))
    mcmcspec$getMonitors()
    Rmcmc <- buildMCMC(mcmcspec)
    Cmodel <- compileNimble(Rmodel)
    Cmcmc <- compileNimble(Rmcmc, project = Rmodel)
    Cmcmc(200000)    ## this runs nearly instantaneously on my computer -DT
    samples <- as.matrix(nfVar(Cmcmc, 'mvSamples'))
    samples <- samples[50001:200000,]
    dim(samples)
    apply(samples, 2, mean)
    solve(Q)
    cov(samples)
    propCov <- nfVar(Cmcmc, 'samplerFunctions')[[1]]$propCov
    scale <- nfVar(Cmcmc, 'samplerFunctions')[[1]]$scale
    propCov * scale^2
    
nfVar(Cmcmc, 'samplerFunctions')[[1]]$scaleHistory
nfVar(Cmcmc, 'samplerFunctions')[[1]]$acceptanceRateHistory
nfVar(Cmcmc, 'samplerFunctions')[[1]]$scale
nfVar(Cmcmc, 'samplerFunctions')[[1]]$propCov
# why is the proposal cov w/ .99 cross-corrs?
# also MCMC in C takes a surprisingly long time - this might be threaded lin alg behaving badly on small matrices
}

### DT's model
mu <- c(1,2,3)
corr <- matrix(c(1,.8,0.3,.8,1,0,0.3,0,1), nrow=3)
varr <- c(1,2,3)
Sig <- diag(sqrt(varr))
Q <- Sig %*% corr %*% Sig
P <- solve(Q)

code <- modelCode({
#    x[1:3] ~ dmnorm(mu[1:3], cov = Q[1:3,1:3])
    x[1:3] ~ dmnorm(mu[1:3], prec = P[1:3,1:3])
})
data = list(P = P, mu = mu)

test_mcmc(model = code, data = data, seed = 0, numItsC = 10000,
          results = list(mean = list(x = mu),
          var = list(x = varr)),
          resultsTolerance = list(mean = list(x = rep(.1,3)),
            var = list(x = c(.1,.1,.1))),
          samplers = list(
            list(type = 'RW_block', control = list(targetNodes = 'x[1:3]'))))



### MVN conjugate update

set.seed(0)
mu0 = 1:3
Q0 = matrix(c(1, .2, .8, .2, 2, 1, .8, 1, 2), nrow = 3)
Q = solve(matrix(c(3, 1.7, .9, 1.7, 2, .6, .9, .6, 1), nrow = 3))
a = c(-2, .5, 1)
B = matrix(rnorm(9), 3)

code <- modelCode({
  mu[1:3] ~ dmnorm(mu0[1:3], Q0[1:3, 1:3])
  y[1:3] ~ dmnorm(asCol(a[1:3]) + B[1:3, 1:3] %*% asCol(mu[1:3]), Q[1:3, 1:3])
})

## code <- modelCode({
##   mu[1:3] ~ dmnorm(mu0[1:3], Q0[1:3, 1:3])
##   y_mean[1:3] <- asCol(a[1:3]) + B[1:3, 1:3] %*% asCol(mu[1:3])
##   y[1:3] ~ dmnorm(y_mean[1:3], Q[1:3, 1:3])
## })

## Simplest version of model w/o 'a' and 'B'
## a = rep(0,3)
## B = diag(rep(1,3))
## code <- modelCode({
##   mu[1:3] ~ dmnorm(mu0[1:3], Q0[1:3, 1:3])
##   y[1:3] ~ dmnorm(mu[1:3], Q[1:3, 1:3])
## })


mu <- mu0 + chol(solve(Q0)) %*% rnorm(3)
# make sure y is a vec not a 1-col matrix or get a dimensionality error
y <- c(a + B%*%mu + chol(solve(Q)) %*% rnorm(3))
data = list(mu0 = mu0, Q0 = Q0, Q = Q, a = a, B = B, y = y)

muQtrue = t(B) %*% Q%*%B + Q0
muMeanTrue = c(solve(muQtrue, crossprod(B, Q%*%(y-a)) + Q0%*%mu0))

test_mcmc(model = code, data = data, seed = 0, numItsC = 10000,
          results = list(mean = list(mu = muMeanTrue),
                           cov = list(mu = solve(muQtrue))),
          resultsTolerance = list(mean = list(mu = rep(.02,3)),
            cov = list(mu = matrix(.01, 3, 3))))



### scalar RW updates in place of conjugate mv update

test_mcmc(model = code, data = data, seed = 0, numItsC = 100000,
          results = list(mean = list(mu = muMeanTrue),
                           cov = list(mu = solve(muQtrue))),
          resultsTolerance = list(mean = list(mu = rep(.03,3)),
            cov = list(mu = matrix(.03, 3, 3))),
          samplers = list(list(type = 'RW', control = list(targetNode = 'mu[1]')),
            list(type = 'RW', control = list(targetNode = 'mu[2]')),
            list(type = 'RW', control = list(targetNode = 'mu[3]'))),
          removeAllDefaultSamplers = TRUE)

