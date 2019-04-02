#Create the function for converting spherical to cartesian

spherical_to_cartesian <- function(sphericalCoords){
  r <- sphericalCoords[1]
  theta <- sphericalCoords[2]
  phi <- sphericalCoords[3]
  z <- r*cos(theta)
  xy_plane_length <- r*sin(theta)
  x <- xy_plane_length*cos(phi)
  y <- xy_plane_length*sin(phi)
  return(c(x,y,z))
}

#check the function is running as expected
                            
stopifnot(spherical_to_cartesian(c(3,0,pi)) == c(0.0, 0.0, 3.0))

#create the function for converting cartesian to spherical

cartesian_to_spherical <- function(cartesianCoords){
  x <- cartesianCoords[1]
  y <- cartesianCoords[2]
  z <- cartesianCoords[3]
  r <- sqrt(x*x + y*y + z*z)
  xy_plane_length <- sqrt(x*x + y)
  #handle the exception where it only has z value
  if(xy_plane_length == 0){
    phi = 0
  } else {
    phi = acos(x/xy_plane_length)
  }
  #handle the exception where it is the origin
  if(r == 0){
    theta = 0
  } else {
    theta = acos(z/r)
  }
  return(c(r,theta,phi))
}

#check that the function is working as expected
stopifnot(round(cartesian_to_spherical(c(0,3,0)),digits = 5) == c(3.0,1.5708,1.5708))

#find the coefficient for a particular angular function for a particular m and l
