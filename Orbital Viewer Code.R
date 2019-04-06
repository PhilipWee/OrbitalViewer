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
angular_coefficient <- function(m,l){
  #There are four parts to the equation
  top_left <- 2*l + 1
  top_right <- factorial(l - abs(m))
  bottom_left <- 4*pi
  bottom_right <- factorial(l + abs(m))
  #epsilon depends on these conditions
  if(m>1){
    epsilon <- (-1)^m
  } else {
    epsilon <- 1
  }
  #put it together neatly
  angular_coefficient <- epsilon*sqrt((top_left*top_right)/(bottom_left*bottom_right)) 
}

#makes the sin_part a function that doesnt have to go through the if else loop and just considers only the value itself
sin_part_finder <- function(m){
  if(m==0){
    return({
      function(theta){
        return(1)
      }
    })
  } else {
    return({
      function(theta){
        return(sin(theta)^m)
      }
    })
  } 
}

#check that it is working
sin_part_finder_m2 <- sin_part_finder(2)
stopifnot(round(sin_part_finder_m2(0.5),digits = 5) == 0.22985)

#all the cosine functions with all the division done before hand
cos_part_list <- c(function(theta) cos(theta),
                   function(theta) 1.5*cos(theta)^2-0.5,
                   function(theta) 2.5*cos(theta)^2-0.5,
                   function(theta) 7.5*cos(theta)^2-4.5*cos(x),
                   function(theta) 1)

#which particular function applies to the situation? extract it
cos_part_finder <- function(m,l){
  if(l-m == 1){
    return(cos_part_list[[1]])
  } else if(m == 0 && l == 2){
    return(cos_part_list[[2]])
  } else if(m == 1 && l == 3){
    return(cos_part_list[[3]])
  } else if(m == 0 && l == 3){
    return(cos_part_list[[4]])
  } else {
    return(cos_part_list[[5]])
  }
}

#Create the full angular function for a particular m and l
angular_function_finder <- function(m,l){
  sin_part_particular <- sin_part_finder(m)
  cos_part_particular <- cos_part_finder(m,l)
  angular_coefficient_particular <- angular_coefficient(m,l)
  return(function(theta,phi){
    exponential_part <- exp(1i*m*phi)
    angular_answer <- exponential_part*sin_part_particular(theta)*cos_part_particular(theta)*angular_coefficient_particular
    return(angular_answer)
  })
}

#check if the angular function maker is doing what its supposed to be doing
angular_function_m0_l0 <- angular_function_finder(0,0)
angular_function_m0_l1 <- angular_function_finder(0,1)
angular_function_m1_l1 <- angular_function_finder(1,1)
angular_function_m0_l2 <- angular_function_finder(0,2)
stopifnot(round(angular_function_m0_l0(0,0),digits= 5) ==0.28209+0i)
stopifnot(round(angular_function_m0_l1(pi,0),digits= 5)== -0.48860+0i)
stopifnot(round(angular_function_m1_l1(pi/2,pi),digits= 5) == -0.34549+0i)
stopifnot(round(angular_function_m0_l2(pi,0),digits= 5) == 0.63078+0i)

#find the radial coefficient
#take note this does not include the constant a, we will just include that at the end
radial_coefficient <- function(n,l){
  left_side <- (2/n)^3
  top_right <- factorial(n-l-1)
  bottom_right <- 2*n*(factorial(n+l))^3
  return(sqrt(left_side*top_right/bottom_right))
}

#all the polynomials are predefined as doing the differentiation would take too
#much computational power defining bohr radius
a <- 5.291772*10^-11

#hard coding the individual polynomial lists
polynomial_list_n1 <- list(
  function(r) 1
)

polynomial_list_n2 <- list(
  function(r) 1-r/(2*a),
  function(r) r/a
)

polynomial_list_n3 <- list(
  function(r) 27-18*r/a+2*(r/a)^2,
  function(r) r/a-((r/a)^2)/6,
  function(r) (r/a)^2
)

polynomial_list_n4 <-list(
  function(r) 1-3*(r/a)/4+((r/a)^2)/8 - ((r/a)^3)/192,
  function(r) 1-(r/a)/4+((r/a)^2)/80,
  function(r) 1-(r/a)/12,
  function(r) (r/a)^3
)

#polynomial finder list
polynomial_list <- list(
  polynomial_list_n1,
  polynomial_list_n2,
  polynomial_list_n3,
  polynomial_list_n4
)

#find the radial part of the wave function
radial_wave_func <- function(n,l){
  coefficient <- radial_coefficient(n,l)
  polynomial_function <- polynomial_list[[n]][[l-1]]
  return(function(r){
    exponential_part <- exp(-r/a/n)
    radial_part <- coefficient*polynomial_function(n)*exponential_part
    return(radial_part)
  })
}

radial_wave_func_n1_l0 <- radial_wave_func(1,0)
