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
  g.neighbor[position] <- sample(c("C","S","R","E"),1,prob=c(f.C,f.S,f.R,f.E)) 
if(value=="S")
  g.neighbor[position] <- sample(c("S","E"),1,prob=c(1-delta.S,delta.S))
if(value=="C")
  g.neighbor[position] <- sample(c("C","E"),1,prob=c(1-delta.C,delta.C))
if(value=="R")
  g.neighbor[position] <- sample(c("R","E"),1,prob=c(1-delta.R,delta.R))

lattice <- g.neighbor
}

return(lattice)
}






















#replicate for n times
time.rep <- function(n,lattice)
{
  lattice.rep <- replicate(n,update(lattice))
  lattice.final <- lattice.rep[,,n] 
  return(lattice.final)
}



#get x and y value
x.y <- function(x,lattice,cell)
{
  x.y <- matrix(data=NA,nrow=x,ncol=2,byrow=T)
  i=1
  while(i < x+1)  
  {
    lattice.final <- time.rep(i,lattice)
    total <- length(lattice.final)
    n.cell <- length(which(lattice.final==cell))
    y <- log(n.cell)
    x.y[i,1] <- i
    x.y[i,2] <- y
    i <- i+1
  }
  
  return(x.y)
} 



#plot
x.y.C <- x.y(500,lattice,"C")
plot(x.y.C,type="l",col="red",xlab="Time",ylab="Log(abundance)",main="Global neighbourhood",xlim=c(0,500))
x.y.S <- x.y(100,lattice,"S")
points(x.y.S[,1],x.y.S[,2],type="l",col="blue")
x.y.R <- x.y(500,lattice,"R")
points(x.y.R[,1],x.y.R[,2],type="l",col="green")