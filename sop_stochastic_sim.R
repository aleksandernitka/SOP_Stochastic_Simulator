p1 = 0.8
pd1 = 0.2
pd2 = pd1/5
stdur = 100
trdur = 300
nsim = 100
nelements = 1000


stochastic = TRUE # Set to TRUE if you want to add another source of noise
# this will take p1, pd1 and pd2 on each moment
# and get a random value from a gaussian distributed around that value and 
# a specified SD, each parameter will change from moment to moment 
stp1sd = 0.001
stpd1sd = 0.001
stpd2sd = 0.001



sopsim = function(
    
    # Default params
    p1=0.8,
    pd1=0.2,
    pd2=pd1/5,
    stdur=100,
    trdur=300,
    nsim=100,
    nelements=1000,
    stochastic=FALSE,
    stpd1sd = 0.001,
    stpd2sd = 0.001,
    stp1sd = 0.001){
    
    start = Sys.time()
    
    # memory reposentation of stimuli in I
    I = c(1:nelements)
    A1 = c()
    A2 = c()

    # set a simulation array
    # dims = row x column x Nmatrices
    # each sim is on a different matrix
    # matrix names = A1, A2, I, Total(A1+A2+I), p1, pd1, pd2
    sim = array(rep(NA,trdur*7*nsim), c(trdur,7,nsim))
    
    for (s in 1:nsim) {
        
        # run simulation for each TRIAL moment
        for (m in 1:trdur){
            
            # set parameters in stochastic
            if (stochastic == TRUE){
                p1 = rnorm(1, mean = p1, sd = stp1sd)
                pd1 = rnorm(1, mean = pd1, sd = stpd1sd)
                pd2 = rnorm(1, mean = pd2, sd = stpd2sd)
            }
            
            # activate to A1
            if (m <= stdur){
                
                pA1p = runif(length(I),0,1)
                A1 = c(A1, I[which(pA1p <= p1)])
                I = I[which(pA1p > p1)]
                
            } else {
                
                # No p1
            }
            
            # Decay to A2
            pA2p = runif(length(A1),0,1)
            A2 = c(A2, A1[which(pA2p <= pd1)])
            A1 = A1[which(pA2p > pd1)]
            
            # Decay to I
            pIp = runif(length(A2),0,1)
            I = c(A2[which(pIp <= pd2)], I)
            A2 = A2[which(pIp > pd2)]
            
            sim[m,1,s] = length(A1)
            sim[m,2,s] = length(A2)
            sim[m,3,s] = length(I)
            sim[m,4,s] = length(c(A1,A2,I))
            sim[m,5,s] = p1
            sim[m,6,s] = pd1
            sim[m,7,s] = pd2
        }
        
    }
    
    # Plot traces
    for (s in 1:nsim){
        if (s == 1){
            plot(sim[,1,s], type = 'l', col = rgb(1,0,0,alpha=0.1), lwd = 1, ylim = c(0,nelements))
            lines(sim[,2,s], col = rgb(0,1,0, alpha=0.1), lwd = 1)
            lines(sim[,3,s], col = rgb(0,0,1, alpha=0.1), lwd = 1)
        } else {
            lines(sim[,1,s], col = 'red', lwd = 0.2)
            lines(sim[,2,s], col = rgb(0,1,0, alpha=0.1), lwd = 1)
            lines(sim[,3,s], col = rgb(0,0,1, alpha=0.1), lwd = 1)
        }
    }

    # Plot p1s
    for (s in 1:nsim){
        if (s == 1){
            plot(sim[,5,s], type = 'l', col = rgb(1,0,0,alpha=0.2), lwd = 1, ylim = c(min(sim[,5,]),max(sim[,5,])))
        } else {
            lines(sim[,5,s], type = 'l', col = rgb(1,0,0,alpha=0.2), lwd = 1, ylim = c(min(sim[,5,]),max(sim[,5,])))
        }
    }
    
    
    simulations <<- sim
    
    end = Sys.time()
    
    print(paste('Simulation Duration:', round(end-start,3), sep = ' '))
}


sopsim()

