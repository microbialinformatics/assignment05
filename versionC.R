#Test code
# source("versionC.R")
# grid <- create_grid(50)
# output_update_local <- update_grid("local", grid, 50, 1000)
# output_update_global <- update_grid("global", grid, 50, 1000)
# pdf("try3_local1000.pdf")
# plot_abundance(output_update_local, 1000)
# pdf("try3_global1000.pdf")
# plot_abundance(output_update_global, 1000)
#install.packages("animation")

#Creation of grid
create_grid <- function(size){
  types <- c("E", "R", "C", "S")
  data <- sample(types, size*size, replace = T)
  grid <- matrix(data, size, size)

return(grid)
}

#Setting up local neighbourhood and calculating fractions
#Input = the location of the focal point. Size corresponds to the amount of rows = of columns in this grid.
find_local <- function (focalpoint_row, focalpoint_column, grid, size){  
#If the focal point is situated on the edges, the local neighbourhood will have strange shape. Therefore, all 
#edge cases are one by one implemented:
  # if first row: that row + next,
  # if last row: previous row + that row,
  # if inbetween: previous row + that row + next row,
  # if first column: that column + next,
  # if last column: previous column + that column,
  # if inbetween: previous column + that column + next column.
    
#When the focal point is found somehwere in the middle,...
### Do this first because will happen the most: save computation time! ###
if(focalpoint_row != 1 && focalpoint_row != size){
    local <- grid[(focalpoint_row-1):(focalpoint_row+1),]
    if (focalpoint_column != 1 && focalpoint_column != size){
      local <- local[, (focalpoint_column-1):(focalpoint_column+1)]
      local[2, 2] <- NA
      divider <- 8}      
    else if (focalpoint_column == size){
      local <- local[, (focalpoint_column-1):(focalpoint_column)]
      local[2, 2] <- NA
      divider <- 5}
    else{
      local <- local[, (focalpoint_column):(focalpoint_column+1)]
      local[2, 1] <- NA
      divider <- 5}
}
# When the focal point is found on the first row, ...
else if (focalpoint_row == 1){
    local <- grid[(focalpoint_row):(focalpoint_row+1),]
    if (focalpoint_column != 1 && focalpoint_column != size){
      local <- local[, (focalpoint_column-1):(focalpoint_column+1)]
      local[1, 2] <- NA
      divider <- 5}   
    else if (focalpoint_column == size){
      local <- local[, (focalpoint_column-1):(focalpoint_column)]
      local[1, 2] <- NA
      divider <- 3}
    else{
      local <- local[, (focalpoint_column):(focalpoint_column+1)]
      local[1, 1] <- NA
      divider <- 3}}
#When the focal point is found on the last row, ...
else{
    local <- grid[(focalpoint_row-1):(focalpoint_row),]
    if (focalpoint_column != 1 && focalpoint_column != size){
      local <- local[, (focalpoint_column-1):(focalpoint_column+1)]
      local[2, 2] <- NA
      divider <- 5}
    else if (focalpoint_column == size){
      local <- local[, (focalpoint_column-1):(focalpoint_column)]
      local[2, 2] <- NA
      divider <- 3}
    else{
      local <- local[, (focalpoint_column):(focalpoint_column+1)]
      local[2, 1] <- NA
      divider <- 3}}
  
  f_e <- length(grep("E", local))/divider
  f_r <- length(grep("R", local))/divider
  f_c <- length(grep("C", local))/divider
  f_s <- length(grep("S", local))/divider
  R_ab <- length(grep("R", grid))
  C_ab <- length(grep("C", grid))
  S_ab <- length(grep("S", grid))
  
  info <- list(f_e, f_r, f_c, f_s, R_ab, C_ab, S_ab)
  return(info)
}

find_global <- function (focalpoint_row, focalpoint_column, grid, size){
  #the global neighbourhood consists of all entries in the grid except for the given one. We can therefore
  #simply remove the given one from the grid and then calculate the abundances. 
  grid[focalpoint_row, focalpoint_column] <- NA
  #Could replace with 1-f_r-f_c-f_s to save computation time
  f_e <- length(grep("E", grid))/(size*size-1)
  f_r <- length(grep("R", grid))/(size*size-1)
  f_c <- length(grep("C", grid))/(size*size-1)
  f_s <- length(grep("S", grid))/(size*size-1)
  R_ab <- length(grep("R", grid))
  C_ab <- length(grep("C", grid))
  S_ab <- length(grep("S", grid))
  
  info <- list(f_e, f_r, f_c, f_s, R_ab, C_ab, S_ab)
  return(info)
}

#Setting up parameters. Should be converted into a function in the end, with the actual values as input. 
delta_r <- 10/32
delta_c <- 1/3
delta_s0 <-1/4
tau <- 3/4

replace_focal <- function(focalpoint, neighbourhood_info){
  #What is the value of the focal point? 
  if (focalpoint == "E"){
    #Update empty space based on the occurence of the different types in the neighbourhood.
    focalpoint <- sample(c("E", "R", "C", "S"), 1, replace = T, prob = c(neighbourhood_info[[1]], neighbourhood_info[[2]], neighbourhood_info[[3]], neighbourhood_info[[4]]))}
  #Kill microorganism, with a probability equal to the given delta's. 
  else if (focalpoint == "R"){
    focalpoint <- sample(c("E", "R"), 1, replace = T, prob = c(delta_r, (1-delta_r)))}
  else if (focalpoint == "C"){
    focalpoint <- sample(c("E", "C"), 1, replace = T, prob = c(delta_c, (1-delta_c)))}
  else {
    delta_s <- delta_s0 + tau*neighbourhood_info[[3]]
    focalpoint <- sample(c("E", "S"), 1, replace = T, prob = c(delta_s, (1-delta_s)))}
    
    return(focalpoint)
}

#Updating the grid. This happens randomly and 62500 updates are called an 'epoch'.
#Inputs: the type of interaction (either "local" or "global"), the grid you want to update, 
# the size of this grid, and the amount of updates you want to occur
update_grid <- function(interaction, grid, size, updates){
#update_grid_once <- function(grid, updates){
  #Initializing
  epoch <- size*size
  counter <- 0
  epochcounter <- 0
  R_abundance <- 0
  C_abundance <- 0
  S_abundance <- 0
  
  print(epoch)
  
  #Updating the grid using two approachinges: local interactions, or global interactions. 
  for (i in 1:(epoch*updates)){
    counter <- counter + 1
  #Randomly select a location and obtain its value in both grids: the one made with local interactions, 
  #and the one made with global interactions. 
  focalpoint_row = sample(1:nrow(grid), 1)
  focalpoint_column = sample(1:ncol(grid), 1)
  focalpoint <- grid[focalpoint_row, focalpoint_column]
  
  if (interaction == "local"){
    #Call upon find_local to find the probabilities.
    info <- find_local(focalpoint_row, focalpoint_column, grid, size)
    #Replace the focal points according to their neighbourhoods (call upon replace_focal).
    grid[focalpoint_row, focalpoint_column] <- replace_focal(focalpoint, info)}
  else{    
    #Call upon find_global to find the probabilities for both grids. 
    info <- find_global(focalpoint_row, focalpoint_column, grid, size)
    #Replace the focal points in both grids according to their neighbourhoods (call upon replace_focal).
    grid[focalpoint_row, focalpoint_column] <- replace_focal(focalpoint, info)}     

  #For each epoch, save the abundance information to generate the plots later on. This abundance information is
  #already present in both info_local and info_global, so we only have to save this information in new vectors.
  if (counter == epoch) {
    epochcounter <- epochcounter + 1
    counter <- 0
    #To follow progress:     
    print(epochcounter)
    
    R_abundance[epochcounter] <- info[[5]]
    C_abundance[epochcounter] <- info[[6]]
    S_abundance[epochcounter] <- info[[7]]}

  #Create the heatmap for this grid
  #Convert the letters to numbers: "E" = 1, "R" = 2, "C" = "3", "S"= 4.
  first <- gsub("E", 1, grid)
  second <- gsub("R", 2, first)
  third <- gsub("C", 3, second)
  fourth <- gsub("S", 4, third)
  vector_numbers <- as.numeric(fourth)
  matrix_numbers <- matrix(vector_numbers, nrow = size, ncol = size, byrow = FALSE)
  
  #GIF DOES NOT WORK YET
  #png(file="example%02d.png", width=200, height=200)
    heatmap(matrix_numbers, Rowv = NA, Colv = NA, col = c("white", "green", "red", "blue"), labRow = NA, labCol = NA)
  
  }
  #GIF DOES NOT WORK YET
  #system("convert -delay 80 *.png example_1.gif")
  
  output_update <- list(grid, R_abundance, C_abundance, S_abundance)
  return(output_update)
}

#Now we need to plot the abundances of the local neighbourhoods. Be certain to also input the amount of updates
#that were performed. 
plot_abundance <- function(output_update, updates){
  x <- c(1:updates)
  #plot the abundance of R
  plot <- plot(x, log10(output_update[[2]]), ylim = c(0,10), type = "l", col = "green")
  #plot the abundance of C
  points(x, log10(output_update[[3]]), type = "l", col = "red")
  #plot the abundance of S
  points(x, log10(output_update[[4]]), type = "l", col = "blue")
  #empty is not plotted
#   return(localplot)
}


##### TRY OUTS ####

#Made a function out of this
# if (focalpoint == "E"){
#   #Update empty space based on the occurence of the different types in the neighbourhood.
#   localgrid[focalpoint_row, focalpoint_column] <- sample(c("E", "R", "C", "S"), 1, replace = T, prob = c(info_local[[1]], info_local[[2]], info_local[[3]], info_local[[4]]))
#   globalgrid[focalpoint_row, focalpoint_column] <- sample(c("E", "R", "C", "S"), 1, replace = T, prob = c(info_global[[1]], info_global[[2]], info_global[[3]], info_global[[4]]))}
# #Kill microorganism, with a probability equal to the given delta's. 
# else if (focalpoint == "R"){
#   localgrid[focalpoint_row, focalpoint_column] <- sample(c("E", "R"), 1, replace = T, prob = c(delta_r, (1-delta_r)))
#   globalgrid[focalpoint_row, focalpoint_column] <- sample(c("E", "R"), 1, replace = T, prob = c(delta_r, (1-delta_r)))}
# else if (focalpoint == "C"){
#   localgrid[focalpoint_row, focalpoint_column] <- sample(c("E", "C"), 1, replace = T, prob = c(delta_c, (1-delta_c)))
#   globalgrid[focalpoint_row, focalpoint_column] <- sample(c("E", "C"), 1, replace = T, prob = c(delta_c, (1-delta_c)))}
# else {
#   delta_slocal <- delta_s0 + tau*info_local[[3]]
#   delta_sglobal <- delta_s0 + tau*info_global[[3]]
#   localgrid[focalpoint_row, focalpoint_column] <- sample(c("E", "S"), 1, replace = T, prob = c(delta_slocal, (1-delta_slocal)))
#   globalgrid[focalpoint_row, focalpoint_column] <- sample(c("E", "S"), 1, replace = T, prob = c(delta_sglobal, (1-delta_sglobal)))}
# #Just to check outcomes without having super big matrices
# smalldata <- sample(types, 25, replace = T)
# smallgrid <- matrix(smalldata, nrow = 5, ncol = 5)

# update_grid_epoch <- function(grid){
#   updated_grids <- replicate(62500, update_grid_once(grid))
#   epoch <- updated_grids[,,62500]
#   return(epoch)
#   #with replicate: takes more than 5 minutes (then stopped). Maybe I can find a way that replicate does not 
#   #add the new matrix to the old one, but rather replaces it. This could save computation time.
# }

#Generating the plot of the abundance over time, for the local neighbourhoods. 

#We take a random point, and evaluate the local neighbourhood for this one 
#=> is impossible: you need the neighbours to be able to update this section! 
#We update the entire grid 5000x62500 times and then 
#1. go through each point, saving the abundance information of each point and than adding these abundances(?)
#2. Select a certain location and merely save the abundance information of that neighbourhood (?)

#Need to 'update' this focalpoints 5000x62500 times. Every 62500 times, we need to calculate the abundances
#of E, R, C, and S and save these to create a plot.
#since neither my computer  nor VirtualSites can do this, I'm cutting the grid down to 5x5 to see
#if that gives us the good results. 

# updated_smallgrids<- replicate(3, update_grid(smallgrid))
# #updated_smallgrids<- replicate(5000, update_grid_epoch(smallgrid)))
# last_updated <- updated_smallgrids[,,3]
# columns <- col(last_updated)
# rows <- row(last_updated)

# info_local <- mapply(find_local, columns, rows, last_updated)

# abundance_local <- function(focalpoint_row, focalpoint_column, grid){
#   local <- info_local(focalpoint_row, focalpoint_column, grid)
#   E_abundance <- 
  
#}

# time_lapse <- seq(1,5000*62500,by=62500)
# updated_grids <- sapply(time_lapse, update_grid(grid, 62500))
# abundance_R <- sapply(time_lapse, "R" %in% update_grid(62500))
#abundance_R <- sapply(time_lapse, length(grep("R", update_grid(62500))))
#updated_grids<- mapply(update_grid, grid = grid, updates = 5000*62500)
#abundance_R <- sapply(update_grid(grid), pmatch("R"))