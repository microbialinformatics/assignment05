### Uncomment line 11 and line 18 to see the local and global GIFs


########  RUN LOCAL SIMULATIONS 
set.seed(1)
conditions <- c("S", "R", "C", "E")
max <- createMatrix(conditions, nrows = 50, ncols = 50)
bigmax3 <- runLocalSims2(max)
###### MAKE LINE PLOT FOR LOCAL
plotLogTime(bigmax3, "Local")
### TIME TO GIF IT OUT  
#gif(bigmax)

########  RUN GLOBAL SIMULATIONS 
cultbigmax3 <- runGlobalSims2(max) # cult = culture, we're going global
###### MAKE LINE PLOT FOR GLOBAL
plotLogTime(cultbigmax3, "Global")
### TIME TO GIF IT OUT
#gif(cultbigmax)


########  RUN GLOBAL SIMULATIONS 
expbigmax2 <- runGlobalSims2(max, Rdeath = 1/3) # cult = culture, we're going global
###### MAKE LINE PLOT FOR GLOBAL
plotLogTime(expbigmax2, "Global")



################GIF in HTML Output

saveHTML({
 gif(bigmax3)
}, img.name = "local_plot", imgdir = "local_dir", htmlfile = "local.html", autobrowse = FALSE, 
title = "Local GIF", description = "Original Local Simulation GIF")

saveHTML({
  gif(cultbigmax3)
}, img.name = "global_plot", imgdir = "global_dir", htmlfile = "global.html", autobrowse = FALSE, 
title = "Global GIF", description = "Original Global Simulation GIF")