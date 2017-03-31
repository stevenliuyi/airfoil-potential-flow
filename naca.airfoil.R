yt <- function(t, x) {
    # Compute half thickness of symmetric NACA 4-digit airfoil
    #
    # Args:
    #  t: maximum thickness
    #
    # Return:
    #   half thickness
    
    5*t*(0.2969*sqrt(x) - 0.1260*x - 0.3516*x^2 + 0.2843*x^3 - 0.1015*x^4)
}

yc <- function(m, p, x) {
    # Computes y_c from x coordinate for a NACA 4-digit airfoil
    #
    # Args:
    #  m: maximum camber
    #  p: location of maximum camber
    #  x: x coordinate of a point on the airfoil
    #
    # Return:
    #  y_c of the camber line
    
    if (x <= p) {
        return(m/p^2*(2*p*x-x^2))
    } else {
        return(m/(1-p)^2*(1-2*p+2*p*x-x^2))
    }
}

dyc.dx <- function(m, p, x) {
    # Computes dy_c/dx from x coordinate for a NACA 4-digit airfoil
    #
    # Args:
    #  m: maximum camber
    #  p: location of maximum camber
    #  x: x coordinate of a point on the airfoil
    #
    # Return:
    #  dy_c/dx where y_c is the camber line equation
    
    if (x <= p) {
        return(2*m/p^2*(p-x))
    } else {
        return(2*m/(1-p)^2*(p-x))
    }
}

boundary.points <- function(four.digit, n) {
    # Generates coordinates of boundary points for a NACA 4-digit airfoil.
    #
    # Args:
    #   four.digit: a string represents a NACA 4-digit airfoil
    #   n: number of boundary points/panels (should be an even number)
    #
    # Return:
    #   two vectors containing x and y coordinates of the boundary points

    m <- as.numeric(substr(four.digit, 1, 1)) / 100 # maximum camber
    p <- as.numeric(substr(four.digit, 2, 2)) / 10 # location of maximum camber
    t <- as.numeric(substr(four.digit, 3, 4)) / 100 # maximum thickness
    
    # x coordinates (except leading and trailing edge points)
    x.coordinates <- 0.5 + 0.5 * cos((1:(n/2-1)) * (2*pi) / n)
    
    # compute the x coordinates for the camber line
    # solve xU = xc - yt * sin(theta) for xc
    xc.upper <- sapply(x.coordinates, function(x) {
        r <- uniroot(function(xc) xc - yt(t,xc) * sin(atan(dyc.dx(m,p,xc)))
                     - x, lower=0, upper=1)
        r$root
    })
    
    # solve xL = xc + yt * sin(theta) for xc
    xc.lower <- sapply(x.coordinates, function(x) {
        r <- uniroot(function(xc) xc + yt(t,xc) * sin(atan(dyc.dx(m,p,xc)))
                     - x, c(0, 1))
        r$root
    })
    
    # y coordinates on the upper and lower sufraces
    # yU = yc + yt * sin(theta)
    y.upper <- sapply(xc.upper, function(xc) {
        yc(m,p,xc) + yt(t,xc)*cos(atan(dyc.dx(m,p,xc)))
    })
    # yL = yc - yt * sin(theta)
    y.lower <- sapply(xc.lower, function(xc) {
        yc(m,p,xc) - yt(t,xc)*cos(atan(dyc.dx(m,p,xc)))
    })
    
    # combine upper/lower surface coordinates and leading/trailing edge points
    # trailing edge point is included twice for both first and last element
    x.combined <- c(1., x.coordinates, 0., rev(x.coordinates), 1.)
    y.combined <- c(0., y.lower, 0., rev(y.upper), 0.)
    boundary.points <- list("x" = x.combined, "y" = y.combined)
}
