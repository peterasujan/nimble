context("Testing importance sampling")

betaBernCode <- nimbleCode({
    for (i in 1:SAMPS) {
        x[i] ~ dbern(prob = p)
    }
    p ~ dbeta(alpha, beta)
})

x <- rbinom(n = 100, size = 1, prob = .4)

betaBernModel <- nimbleModel(betaBernCode,
                             constants = list(SAMPS = length(x)),
                             data = list(x = x),
                             inits = list(alpha = 10, beta = 30))

propCode <- nimbleCode({
    p ~ dbeta(alpha, beta)
})

propModel <- nimbleModel(propCode,
                         inits = list(alpha = 1, beta = 1))


sampler <- buildImportanceSampler(betaBernModel, propModel, c("p"))

CbetaBern <- compileNimble(betaBernModel)
Cprop <- compileNimble(propModel)
Csampler <- compileNimble(sampler, project = betaBernModel)

Csampler$run(10000)
w <- as.numeric(Csampler$weights["weight"])
s <- as.numeric(Csampler$mvSamps["p"])
hist(s)
## resample <- sample(s, prob = w, replace = TRUE)
resample <- as.numeric(Csampler$mvResamps["p"])
## hist(resample, freq = FALSE)

t <- sum(x)
n <- length(x)
postAlpha <- 10 + t
postBeta <- 30 + n - t
postSample <- rbeta(10000, shape1 = postAlpha, shape2 = postBeta)
## curve(dbeta(x, postAlpha, postBeta), add = TRUE) ## analytic posterior


postAlpha / (postAlpha + postBeta) ## analytic posterior mean
mean(resample) ## empirical posterior mean

test_that("means are equal",
          expect_equal(mean(resample), postAlpha / (postAlpha + postBeta),
                       tolerance = 0.01))

test_that("variances are equal",
          expect_equal(var(resample),
                       postAlpha * postBeta /
                           ((postAlpha + postBeta)^2 * (postAlpha + postBeta + 1)),
                       tolerance = 0.01))

## is testing quantiles overkill??
resampQuantiles <- as.numeric(quantile(resample, probs = 1:99 / 100))
## quantilesCompare <- quantile(postSample, probs = 1:99 / 100)
quantilesCompare <- qbeta(p = 1:99 / 100, shape1 = postAlpha, shape2 = postBeta)

test_that("quantiles of sample match those of analytic posterior", {
    expect_equal(resampQuantiles, quantilesCompare, tolerance = 0.01)
})

## Test against MCMC on BUGS models

## Pump Model
pumpModel <- readBUGSmodel('pump', dir = getBUGSexampleDir('pump'))
compiled_pumpModel <- compileNimble(pumpModel)
# pumpMCMC <- buildMCMC(pumpModel)

pumpMCMCconfig <- configureMCMC(pumpModel)
pumpMCMCconfig$removeSamplers(c('theta', 'beta'))
pumpMCMC <- buildMCMC(pumpMCMCconfig)
compiled_pumpMCMC <- compileNimble(pumpMCMC, project = pumpModel)
compiled_pumpMCMC$run(100000)
mcmcSamps <- as.matrix(compiled_pumpMCMC$mvSamples)

propCode <- nimbleCode({
  alpha ~ dexp(1)
})

propModel <- nimbleModel(propCode)
compiled_propModel <- compileNimble(propModel)

pumpSampler <- buildImportanceSampler(pumpModel, propModel, c("alpha"))
compiled_pumpSampler <- compileNimble(pumpSampler, project = pumpModel)

compiled_pumpSampler$run(100000)
isSamps <- as.matrix(compiled_pumpSampler$mvResamps)

test_that("Means are equal for pump model", {
  expect_equal(mean(isSamps[, 'alpha']), mean(mcmcSamps[, 'alpha']), tolerance = 0.01)
})

test_that("Variances are equal for pump model", {
  expect_equal(var(isSamps[, 'alpha']), var(mcmcSamps[, 'alpha']), tolerance = 0.05)
})

test_that("Medians are equal for pump model", {
  expect_equal(median(isSamps[, 'alpha']), median(mcmcSamps[, 'alpha']), tolerance = 0.01)
})

## Seeds Model
seedsModel <- readBUGSmodel('seeds', dir = getBUGSexampleDir('seeds'))
compiled_seedsModel <- compileNimble(seedsModel)
# pumpMCMC <- buildMCMC(pumpModel)

seedsMCMCconfig <- configureMCMC(seedsModel)
seedsMCMCconfig$removeSamplers(c('b', 'alpha1', 'alpha2', 'alpha12', 'tau'))
seedsMCMC <- buildMCMC(seedsMCMCconfig)
compiled_seedsMCMC <- compileNimble(seedsMCMC, project = seedsModel)
compiled_seedsMCMC$run(10000)
mcmcSamps <- as.matrix(compiled_seedsMCMC$mvSamples)

propCode <- nimbleCode({
  alpha0  ~ dnorm(0.0,1.0e-1);  # intercept
#   alpha1  ~ dnorm(0.0,1.0e-1);  # seed coeff
#   alpha2  ~ dnorm(0.0,1.0e-2);  # extract coeff
#   alpha12 ~ dnorm(0.0,1.0e-2);
#   tau     ~ dgamma(1.0e-1,1.0e-3);    # 1/sigma^2
})

propModel <- nimbleModel(propCode)

compiled_propModel <- compileNimble(propModel)

seedsSampler <- buildImportanceSampler(seedsModel, propModel,
                                     c('alpha0'))
compiled_seedsSampler <- compileNimble(seedsSampler, project = seedsModel,
                                     resetFunctions = TRUE)

compiled_seedsSampler$run(10000)
isSamps <- as.matrix(compiled_seedsSampler$mvResamps)
w <- as.matrix(compiled_seedsSampler$weights)

test_that("Means are equal for seeds model", {
  expect_equal(mean(isSamps[, 'alpha0']), mean(mcmcSamps[, 'alpha0']),
               tolerance = 0.05)
})

test_that("Variances are equal for seeds model", {
  expect_equal(var(isSamps[, 'alpha0']), var(mcmcSamps[, 'alpha0']), tolerance = 0.05)
})

test_that("Medians are equal for seeds model", {
  expect_equal(median(isSamps[, 'alpha0']), median(mcmcSamps[, 'alpha0']),
               tolerance = 0.05)
})
