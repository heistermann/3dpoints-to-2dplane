library(rgl)

# ALL BASED ON https://www.baeldung.com/cs/3d-point-2d-plane

# Some sample data in 3D space
data = data.frame(x=0:10, y=0:10)
data$z = (data$x-5)^2 + (data$y-5)^2
# x2, y2, z2 are the coordinates of above points projected to a plane
data$x2 = NA
data$y2 = NA
# x3 and y3 are the same (projected) points, 
# but in the native 2d coordinate system of the plane
data$x3 = NA

# THE PLANE IS DEFINED BY A POINT AND THE UNIT NORMAL VECTOR
# This is the point in the plane
origin = c(0,5,0)
# This is its unit normal vector
normal = c(0,1,0)
#normal = [-1/np.sqrt(2),1/np.sqrt(2),0]

normalize <- function(vec) {vec / sqrt(sum(vec^2))}
normal = normalize(c(1,2,0))

# This is the unit vector which defines the x dimension of the plane's own 2D-coordinate system - it is just the normal of the plane rotated by 90 degrees.
ex = c(normal[2], -normal[1], 0)
# and this is the z dimension (independent of how we define the plane)
ez = c(0,0,1)
# put them in a matrix to define the new coordinate system
e = as.matrix(rbind(ex,ez))

# Derive the typical parameter names and values for plane and normal definition
a = normal[1]
b = normal[2]
c = normal[3]
p = origin[1] 
q = origin[2] 
r = origin[3]
# this equation defines the plane to which we project
d = a*p + b*q + c*r

# projection function 
project = function(x, y, z) {
  k = (d - a*x- b*y - c*z) / (a**2 + b**2 + c**2)
  return(c(x+k*a, y+k*b, z+k*c))
}

for (i in 1:nrow(data)) {
  # project 3d points to plane
  newpt2 = project(data$x[i], data$y[i], data$z[i]) 
  data$x2[i] = newpt2[1]
  data$y2[i] = newpt2[2]
  # compute 2d coordinates of 3d projected points
  newpt3 = e %*% newpt2
  data$x3[i] = newpt3[1]
  print(newpt3)
}

# Plot everything in 3d
open3d()

plot3d( 
  x=data$x, y=data$y, z=data$z, 
  col = "black", 
  type = 's', 
  radius = 3,
  xlab="x", ylab="y", zlab="z",
  xlim=c(0,10), ylim=c(0,10), zlim=c(0,100))

plot3d( 
  x=data$x2, y=data$y2, z=data$z,
  col = "red", 
  type = 's', 
  radius = 3,
  xlab="x", ylab="y", zlab="z",
  xlim=c(0,10), ylim=c(0,10), zlim=c(0,100))

planes3d(a,b,c,-d)

# Save to a file:
htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
                        file = "projected3d.html",
                        libdir = "libs",
                        selfcontained = FALSE
)

clear3d()

# Plot in 2d
plot(data$x3, data$z, type="b")
