`permTREND` <-
function (x, ...){
    UseMethod("permTREND")
}

`permTREND.formula` <-
function(formula, data, subset, na.action, ...){
    ## mostly copied from wilcox.test.formula
    if (missing(formula) || (length(formula) != 3) || (length(attr(terms(formula[-2]), 
        "term.labels")) != 1)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    groupname<-names(mf)[2]

    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- mf[[-response]]
    resp<-mf[[response]]
    out <- do.call("permTREND", c(list(x=resp,y=g), list(...)))
    out$data.name <- DNAME
    out
}

`permTREND.default` <-
function(x, y, alternative = c("two.sided", "less", "greater","two.sidedAbs"), 
    exact = NULL, method=NULL, methodRule=methodRuleTREND1, 
    control=permControl(),...){

    cm<-control$cm
    nmc<-control$nmc
    seed<-control$seed
    digits<-control$digits
    p.conf.level<-control$p.conf.level
    setSEED<-control$setSEED
    
    if (!is.numeric(x) | !is.numeric(y) | !is.vector(x) | !is.vector(y) ) stop("x and y must be numeric vectors")

    if (length(x)!=length(y)) stop("x and y must be the same length")
 
    if (is.null(method))    method<-methodRule(x,y,exact)

    method.OK<-(method=="pclt" | method=="exact.mc")
    if (!method.OK) stop("method not one of: 'pclt', 'exact.mc'")
    alternative <- match.arg(alternative)

    mout<-switch(method,
        pclt=trend.pclt(x,y),
        exact.mc=trend.exact.mc(x,y,alternative,nmc,seed,digits,p.conf.level,setSEED))
    p.values<-mout$p.values

    PVAL <- switch(alternative,
        two.sided=p.values["p.twosided"],
        greater=p.values["p.gte"],
        less=p.values["p.lte"]) 
    if (method=="pclt") METHOD<-"Permutation Test using Asymptotic Approximation"
    else if (method=="exact.mc") METHOD<-"Exact Permutation Test Estimated by Monte Carlo"
    xname<-deparse(substitute(x))
    yname<-deparse(substitute(y))
    if (length(xname)>1) xname<-c("x")
    if (length(yname)>1) yname<-c("y")
   
    DNAME <- paste(xname, "and", yname)
    Z<-mout$Z
    if (!is.null(Z)) names(Z)<-"Z"
 
    null.value<-0
    estimate<-cor(x,y)
    names(estimate)<-names(null.value)<-paste("correlation of ",xname," and ",yname,sep="")
    p.conf.int<-mout$p.conf.int
    if (method!="exact.mc") nmc<-NULL 
    OUT <- list(statistic = Z, estimate=estimate, parameter = NULL, p.value = as.numeric(PVAL), 
        null.value = null.value, alternative = alternative, method = METHOD, 
        data.name = DNAME, p.values=p.values, p.conf.int=p.conf.int, nmc=nmc)
    if (method=="exact.mc") class(OUT) <- "mchtest"
    else class(OUT) <- "htest"
     return(OUT)
}


