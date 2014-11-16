#create virtual community
cell <- c("C","S","R","E")
community <- sample(cell,250*250,replace=T)
lattice <- matrix(community,nrow=250,ncol=250,byrow=T)

update <- function(lattice){

#get the location and value of start point
position <- sample(250*250,1,replace=T)
value <- lattice[position]
g.neighbor <- lattice


for (i in position:length(lattice))
{
#change state
 #global neighborhood
g.neighbor[position] <- NA

 #for empty focal point
if(value=="E")
{
  f.C <- length(which(g.neighbor=="C"))/(250*250-1)
  f.S <- length(which(g.neighbor=="S"))/(250*250-1)
  f.R <- length(which(g.neighbor=="R"))/(250*250-1)
  p.empty <- c(f.C,f.S,f.R)
  g.neighbor[position] <- sample(c("C","S","R"),1,prob=p.empty)
}
 #for occupied focal point
if(value=="C")
{
  delta.C <- 1/3
  p.occupied <- c(1-delta.C,delta.C)
  g.neighbor[position] <- sample(c("C","E"),1,prob=p.occupied)
}
if(value=="S")
{
  f.C <- length(which(g.neighbor=="C"))/(250*250-1)
  delta.S <- (1/4+3/4*f.C)
  p.occupied <- c(1-delta.S,delta.S)
  g.neighbor[position] <- sample(c("S","E"),1,prob=p.occupied)
}
if(value=="R")
{
  delta.R <- 10/32
  p.occupied <- c(1-delta.R,delta.R)
  g.neighbor[position] <- sample(c("R","E"),1,prob=p.occupied)
}

lattice.new <- g.neighbor


#sequentially change states
position <- position+1
value <- lattice.new[position]
}

return(lattice.new)
}



#replicate for n times
time.rep <- function(n,lattice)
{
  replicate(n,update(lattice))
}



#get x and y value
x.y <- function(x,lattice,cell)
{
  x.y <- matrix(data=NA,nrow=x,ncol=2,byrow=T)
  i=0
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