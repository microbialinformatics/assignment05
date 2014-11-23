rm(list=ls())
#create virtual community
cell <- c("C","S","R","E")
community <- sample(cell,50*50,replace=T)
lattice <- matrix(community,nrow=50,ncol=50,byrow=T)


update <- function(lattice)
{
for(i in 1:2500)
{
#randomly pick up a focal point
position <- sample(50*50,1,replace=T)
value <- lattice[position]
g.neighbor <- lattice
g.neighbor[position] <- NA
#probability
f.C <- length(which(g.neighbor=="C"))/(50*50-1)
f.S <- length(which(g.neighbor=="S"))/(50*50-1)
f.R <- length(which(g.neighbor=="R"))/(50*50-1)
f.E <- 1-f.C-f.S-f.R
delta.C <- 1/3
delta.S <- (1/4+3/4*f.C)
delta.R <- 10/32
#update this focal point by probability
if(value=="E")
  g.neighbor[position] <- sample(c("C","S","R","E"),1,replace=T,prob=c(f.C,f.S,f.R,f.E)) 
if(value=="S")
  g.neighbor[position] <- sample(c("S","E"),1,replace=T,prob=c(1-delta.S,delta.S))
if(value=="C")
  g.neighbor[position] <- sample(c("C","E"),1,replace=T,prob=c(1-delta.C,delta.C))
if(value=="R")
  g.neighbor[position] <- sample(c("R","E"),1,replace=T,prob=c(1-delta.R,delta.R))

lattice <- g.neighbor
}

return(lattice)
}




#x.y <- matrix(data=NA,nrow=5001,ncol=2,byrow=T)
#abundance.C <- length(which(lattice=="C"))
#x.y[1,1] <- 0
#x.y[1,2] <- log10(abundance.C)

#lattice <- replicate(1,update(lattice))
#abundance.C <- length(which(lattice=="C"))
#x.y[2,1] <- 1
#x.y[2,2] <- log10(abundance.C)

#lattice <- replicate(2,update(lattice))
#abundance.C <- length(which(lattice=="C"))
#x.y[3,1] <- 2
#x.y[3,2] <- log10(abundance.C)



rep <- function(n,lattice,cell)
{
  x.y <- matrix(data=NA,nrow=1001,ncol=2,byrow=T)
  i<-0
  while(i < n+1)
  {
    abundance <- length(which(lattice==cell))
    x.y[i+1,1] <- i
    x.y[i+1,2] <- log10(abundance)
    i <- i+1
    lattice<-replicate(1,update(lattice))
  }
  return(x.y)
}




#plot
x.y.C <- rep(1000,lattice,"C")
plot(x.y.C,type="l",col="red",xlab="Time",ylab="Log(abundance)",main="Global neighbourhood",xlim=c(0,1000),ylim=c(0,5))
x.y.S <- rep(1000,lattice,"S")
points(x.y.S[,1],x.y.S[,2],type="l",col="blue")
x.y.R <- rep(1000,lattice,"R")
points(x.y.R[,1],x.y.R[,2],type="l",col="green")



#local neighbor

update_local <- function(lattice)
{
  for(i in 1:2500)
  {
    #randomly pick up a focal point
    position <- sample(50*50,1,replace=T)
    
    value <- lattice[position]
    
    #row number
    if(position%%50!=0)
    {
      nrow <- position%%50} else nrow <- 50
    #colunm number
    if(nrow <= 50/2)
    {
      ncol <- round(position/50)+1} else ncol <- round(position/50)
    
    #find local neighbor
    if(nrow!=1&&nrow!=50&&ncol!=1&&ncol!=50){
      l.neighbor <- lattice[(nrow-1):(nrow+1),(ncol-1):(ncol+1)]
      l.neighbor[2,2] <- NA
    }else if(nrow==1&&ncol>1&&ncol<50){
      l.neighbor <- lattice[1:2,(ncol-1):(ncol+1)]
      l.neighbor[1,2] <- NA
    }else if(nrow==50&&ncol>1&&ncol<50){
      l.neighbor <- lattice[49:50,(ncol-1):(ncol+1)]
      l.neighbor[2,2] <- NA
    }else if(ncol==1&&nrow>1&&nrow<50){
      l.neighbor <- lattice[(nrow-1):(nrow+1),1:2]
      l.neighbor[2,1] <- NA
    }else if(ncol==50&&nrow>1&&nrow<50){
      l.neighbor <- lattice[(nrow-1):(nrow+1),49:50]
      l.neighbor[2,2] <- NA
    }else if(nrow==1&&ncol==1){
      l.neighbor <- lattice[1:2,1:2]
      l.neighbor[1,1] <- NA
    }else if(nrow==1&&ncol==50){
      l.neighbor <- lattice[1:2,49:50]
      l.neighbor[1,2] <- NA
    }else if(nrow==50&&ncol==1){
      l.neighbor <- lattice[49:50,1:2]
      l.neighbor[2,1] <- NA
    }else {
      l.neighbor <- lattice[49:50,49:50]
      l.neighbor[2,2] <- NA}
    
    divider <- length(l.neighbor)-1
    
    
    #probability
    f.C <- length(which(l.neighbor=="C"))/divider
    f.S <- length(which(l.neighbor=="S"))/divider
    f.R <- length(which(l.neighbor=="R"))/divider
    f.E <- 1-f.C-f.S-f.R
    delta.C <- 1/3
    delta.S <- (1/4+3/4*f.C)
    delta.R <- 10/32
    #update this focal point by probability
    if(value=="E")
      lattice[position] <- sample(c("C","S","R","E"),1,replace=T,prob=c(f.C,f.S,f.R,f.E)) 
    if(value=="S")
      lattice[position] <- sample(c("S","E"),1,replace=T,prob=c(1-delta.S,delta.S))
    if(value=="C")
      lattice[position] <- sample(c("C","E"),1,replace=T,prob=c(1-delta.C,delta.C))
    if(value=="R")
      lattice[position] <- sample(c("R","E"),1,replace=T,prob=c(1-delta.R,delta.R))
  }
  
  return(lattice)
}




#x.y <- matrix(data=NA,nrow=5001,ncol=2,byrow=T)
#abundance.C <- length(which(lattice=="C"))
#x.y[1,1] <- 0
#x.y[1,2] <- log10(abundance.C)

#lattice <- replicate(1,update(lattice))
#abundance.C <- length(which(lattice=="C"))
#x.y[2,1] <- 1
#x.y[2,2] <- log10(abundance.C)

#lattice <- replicate(2,update(lattice))
#abundance.C <- length(which(lattice=="C"))
#x.y[3,1] <- 2
#x.y[3,2] <- log10(abundance.C)



rep_local <- function(n,lattice,cell)
{
  x.y <- matrix(data=NA,nrow=1001,ncol=2,byrow=T)
  i<-0
  while(i < n+1)
  {
    abundance <- length(which(lattice==cell))
    x.y[i+1,1] <- i
    x.y[i+1,2] <- log10(abundance)
    i <- i+1
    lattice <- replicate(1,update_local(lattice))
  }
  return(x.y)
}

#plot
x.y.C_local <- rep(1000,lattice,"C")
plot(x.y.C_local,type="l",col="red",xlab="Time",ylab="Log(abundance)",main="Local neighbourhood",xlim=c(0,1000),ylim=c(0,5))
x.y.S_local <- rep(1000,lattice,"S")
points(x.y.S_local[,1],x.y.S_local[,2],type="l",col="blue")
x.y.R_local <- rep(1000,lattice,"R")
points(x.y.R_local[,1],x.y.R_local[,2],type="l",col="green")
