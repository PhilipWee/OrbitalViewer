library(plot3D)

# save plotting parameters  
pm <- par("mfrow")

volcano

## =======================================================================
## Contours
## =======================================================================
par (mfrow = c(2, 2))

r <- 1:nrow(volcano)
c <- 1:ncol(volcano)
contour3D(x = r, y = c, z = 100, colvar = volcano, zlim = c(0, 150),
          clab = c("height", "m"))

contour3D(x = 100, y = r, z = c, colvar = volcano, clab = c("height", "m"))

contour3D(z = volcano, colvar = volcano, lwd = 2, 
          nlevels = 20, clab = c("height", "m"), colkey = FALSE)

contour3D(y = volcano, colvar = volcano, lwd = 2, 
          nlevels = 10, clab = c("height", "m"))

## =======================================================================
## Composite images and contours in 3D
## =======================================================================
persp3D(z = volcano, zlim = c(90, 300), col = "white", 
        shade = 0.1, d = 2, plot = FALSE)
contour3D(z = volcano, colvar = volcano, lwd = 2, add = TRUE, 
          nlevels = 20, clab = c("height", "m"), plot = FALSE,
          colkey = list(at = seq(90, 190, length.out = 5)))
contour3D(z = 300, colvar = volcano, lwd = 2, col = "grey",
          add = TRUE, nlevels = 5)

## =======================================================================
## the viewing depth of contours (dDepth)
## =======================================================================

# too low
persp3D(z = volcano, col = "white", shade = 0.1, plot = FALSE)
contour3D(z = volcano, colvar = volcano, lwd = 2, 
          add = TRUE, dDepth = 0, col = "black")

# default
persp3D(z = volcano, col = "white", shade = 0.1, plot = FALSE)
contour3D(z = volcano, colvar = volcano, lwd = 2, 
          add = TRUE, dDepth = 0.1, col = "black")

# too high
persp3D(z = volcano, col = "white", shade = 0.1, plot = FALSE)
contour3D(z = volcano, colvar = volcano, lwd = 1, 
          add = TRUE, dDepth = 0.5, col = "black")

# reset plotting parameters
par(mfrow = pm)

## =======================================================================
## creating a sphere
## =======================================================================
density <- 10
radius <- 5

c(-25:25)

coords_and_color <- data.frame(x_value = c(),
                               y_value = c(),
                               z_value = c(),
                               density = c())

for(x in c(-density:density)){
  for(y in c(-density:density)){
    for(z in c(-density:density)){
      if(sqrt(x^2+y^2+z^2) <= radius){
        new_row <- data.frame(x_value = x,
                              y_value = y,
                              z_value = z,
                              density = 1)
        coords_and_color <- rbind(coords_and_color, new_row)
      }
    }
  }
}



coords_and_color

contour3D(x = coords_and_color[,1],
          y = coords_and_color[,2],
          z = coords_and_color[,3],
          colvar = coords_and_color[,4],
          zlim = c(0, 150),
          clab = c("height", "m"))

xyz <- array(data = 1, dim = c(density+1,density+1,density+1))
xyz


for(i in c(1:(density+1))){
  for(j in c(1:(density+1))){
    for(k in c(1:(density+1))){
      x = i-(density/2)-1
      y = j-(density/2)-1
      z = k-(density/2)-1
      # x_and_y <- sprintf("x is %d and y is %d",x,y)
      # print(x_and_y)
      distance_of_point <- sqrt(x^2+y^2+z^2)
      print(distance_of_point)
      if(distance_of_point >= radius){
        print("far, convert to zero")
        print(xyz[i,j,k])
        xyz[i,j,k] <- 0
      }
    }
  }
}



xyz


