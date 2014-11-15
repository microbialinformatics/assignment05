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


