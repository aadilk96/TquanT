library(copBasic)

simCOP(n=100, cop=W) # lower Frechet bound copula
simCOP(n=100, cop=M) # upper Frechet bound copula 
simCOP(n=100, cop=P) # intermediate copula

sim <- simCOP(n=100, cop=CLcop, para=10, na.rm=TRUE, 
       graphics=TRUE, keept=TRUE, ploton=TRUE, 
       points=TRUE, snv=FALSE, infsnv.rm=TRUE)

print(sim)