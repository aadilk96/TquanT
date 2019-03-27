#program a function to calculate the the Energy, Boltzmann value and probabilities of the Ising model 


# hamiltonian1 <- function(x, mu, omega){
#     energy.x <- -sum(mu*x)-sum(omega*x)
#     return (energy.x)
# }

hamiltonian2 <- function(x, mu, omega){
    energy.x <- (-t(x)%*%mu)-.5*(t(x)%*%omega%*%x)
    return (energy.x)
}

matrixCreate <- function(n){
    mat <- as.matrix(expand.grid(rep(list(c(-1,1)),n)))
    return (mat)
}

bolztmannIsing <- function(x, mu, omega, beta){
    matr <- matrixCreate(nrow(omega))
    energy <- numeric()

    for (i in 1:nrow(matr)){
        energy[i] = hamiltonian2(matr[i,], mu, omega)
    }

    matr <- cbind(matr, energy)
    bMann <- exp(-beta*energy)
    matr <- cbind(matr, bMann)
    prob <- numeric()

    for (i in 1:(ncol(matr)-1)){
        prob[i] = bMann[i] / sum(bMann)
    }
    matr <- cbind(matr, prob)
    return (matr)
}

shorterIsing <- function(mu, omega, beta){
    bolztmann <- function(x, b = 1){exp(-b*x)}
    n = length(mu)
    x <- matrixCreate(n)
    E  = apply(x, 1, hamiltonian2, mu=mu, omega=omega)
    B  = bolztmann(E, b= beta)
    P = B/sum(B)

    y = cbind(x,E,B,P)
    colnames(y)[(n+1):ncol(y)] = c("Energy", "Bolztmann", "Probability")

    return (y)
}

main1 <- function(){
    x = c(1,1,-1)
    mu = c(0,0,0)
    omega = matrix(c(0,1,-1,1,0,0,-1,0,0),3,3)

    E1 <- hamiltonian2(x,mu,omega)
    print(E1) # -2

    mu = c(0,.587,.123)
    E2 <- hamiltonian2(x,mu,omega)
    print(E2) # -2.464

    print(bolztmannIsing(x, mu, omega, 1))
    # print(ising(mu, omega, 1))

    print("End of Main1")
}

main2 <- function(){
    omega = matrix(0,5,5)
    mu = c(0,0,0,0,0)
    beta = 1

    base.model <- shorterIsing(mu, omega, beta)
    print(base.model)

    sum = 0
    for (i in 1:32){
        for (j in 1:5){
            sum = sum + c(base.model[i, j])
        }
    }
    print(sum)
    print("End of Main2")
}

main1()
main2()