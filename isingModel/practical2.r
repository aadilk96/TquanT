hamiltonian2 <- function(x, mu, omega){
    energy.x <- (-t(x)%*%mu)-.5*(t(x)%*%omega%*%x)
    return (energy.x)
}

matrixCreate <- function(n){
    mat <- as.matrix(expand.grid(rep(list(c(-1,1)),n)))
    return (mat)
}

shorterIsing <- function(mu, omega, beta){
    bolztmann <- function(x, b=1){exp(-b*x)}
    n = length(mu)
    x <- matrixCreate(n)
    E  = apply(x, 1, hamiltonian2, mu=mu, omega=omega)
    B  = bolztmann(E, b=beta)
    P = B / sum(B)

    y = cbind(x,E,B,P)
    colnames(y)[(n+1):ncol(y)] = c("Energy", "Bolztmann", "Probability")

    return (y)
}

sim.ising.full <- function(n, ising)
{
    s <- sample(1:8, n, replace = TRUE, prob = ising[,6])
    x <- ising[s, c(1:3)]
    return(x)
}

main <- function(){
    x = c(1,1,-1)
    mu = c(0,0,0)
    omega = matrix(c(0,1,-1,
                     1,0,0,
                    -1,0,0),3,3)
    beta = 1
    
    I1 = shorterIsing(mu, omega, beta)

    set.seed(1)
    sim.I1 <- sim.ising.full(100, I1)
    print(table(apply(as.matrix(sim.I1), 1, paste0, collapse="")))

    mu = c(0,.587,.123)
    I2 <- shorterIsing(mu,omega,beta)
    sim.I2 <- sim.ising.full(100,I2)
    print(table(apply(sim.I2,1,paste0,collapse="")))
}   

main()