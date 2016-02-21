
cppVarSym2templateTypeCppVarSym <- function(oldSym) {
    newSym <- oldSym$copy()
    if(newSym$baseType == 'double')
        newSym$baseType <- 'TYPE_'
    
    if(newSym$baseType == 'NimArr') 
        if(newSym$templateArgs[[2]] == 'double')
            newSym$templateArgs[[2]] <- 'TYPE_'
    
    if(newSym$baseType == 'EigenMapStr')
        newSym$baseType <- 'EigenTemplateTypes<TYPE_>::typeEigenMapStr'
    
    if(newSym$baseType == 'Map') {
        if(newSym$templateArgs[[1]] == 'MatrixXd')
            newSym$templateArgs[[1]] <- 'EigenTemplateTypes<TYPE_>::typeMatrixXd'
    }
    newSym
}

symbolTable2templateTypeSymbolTable <- function(symTab) {
    newSymTab <- nimble:::symbolTable()
    symNames <- symTab$getSymbolNames()
    for(sn in symNames) {
        oldSym <- symTab$getSymbolObject(sn)
        newSym <- cppVarSym2templateTypeCppVarSym(oldSym)
        newSymTab$addSymbol(newSym)
    }
    newSymTab
}

## does some work similar to BUGScontextClass::embedCodeInForLoop
makeCopyingCodeBlock <- function(LHSvar, RHSvar, indexList, indicesRHS = TRUE, incrementIndex) {
    indexNames <- names(indexList)
    browser()
    indexedBracketExpr <- do.call('call', c(list('[', as.name('TO_BE_REPLACED')), lapply(indexNames, as.name)), quote = TRUE)
    RHS <- eval(substitute(substitute(indexedBracketExpr, list(TO_BE_REPLACED = RHSvar)), list(indexedBracketExpr = indexedBracketExpr)))
    LHS <- substitute(A[i], list(A = LHSvar, i = incrementIndex))
    if(!indicesRHS) {
        temp <- RHS
        RHS <- LHS
        LHS <- temp
    }
    innerCode <- substitute({LHS <- RHS; incrementIndex <- incrementIndex + 1;}, list(LHS = LHS, RHS = RHS, incrementIndex = incrementIndex))
    for(i in length(indexList):1) {
        newForLoop <- substitute(for(NEWINDEX_ in NEWSTART_:NEWEND_) INNERCODE, list(NEWINDEX_ = as.name(indexNames[i]), NEWSTART_ = indexList[[i]][1], NEWEND_ = indexList[[i]][2], INNERCODE = innerCode))
        innerCode <- newForLoop
    }
    innerCode
}
