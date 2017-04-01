coef <- function(n, panel.parameters) {
    # Compute the influence coefficients An_ij and At_ij
    # from panel parameters
    # Args:
    #  n: number of panels (should be an even number)
    #  panel.parameters: list contains boundary/control points, panel lengths
    #                    and panel orientation angles
    #
    # Return:
    #   coefficients An_ij and At_ij
    
    xb <- panel.parameters$x.boundary
    yb <- panel.parameters$y.boundary
    xc <- panel.parameters$x.control
    yc <- panel.parameters$y.control
    s <- panel.parameters$s
    theta <- panel.parameters$theta
    
    cn1 <- matrix(0, nrow=n, ncol=n)
    cn2 <- matrix(0, nrow=n, ncol=n)
    ct1 <- matrix(0, nrow=n, ncol=n)
    ct2 <- matrix(0, nrow=n, ncol=n)
    
    # compute Cn and Ct
    for (i in (1:n)) {
        for (j in (1:n)) {
            if (i == j) {
                cn1[i,j] <- -1; cn2[i,j] <- 1
                ct1[i,j] <- pi/2; ct2[i,j] <- pi/2
            } else {
                A <- -(xc[i]-xb[j])*cos(theta[j]) - (yc[i]-yb[j])*sin(theta[j])
                B <- (xc[i]-xb[j])^2 + (yc[i]-yb[j])^2
                C <- sin(theta[i]-theta[j])
                D <- cos(theta[i]-theta[j])
                E <- (xc[i]-xb[j])*sin(theta[j]) - (yc[i]-yb[j])*cos(theta[j])
                F <- log(1 + (s[j]^2+2*A*s[j])/B)
                G <- atan2(E*s[j], B+A*s[j])
                P <- (xc[i]-xb[j])*sin(theta[i]-2*theta[j]) +
                    (yc[i]-yb[j])*cos(theta[i]-2*theta[j])
                Q <- (xc[i]-xb[j])*cos(theta[i]-2*theta[j]) -
                    (yc[i]-yb[j])*sin(theta[i]-2*theta[j])
                cn2[i,j] <- D + 0.5*Q*F/s[j] - (A*C+D*E)*G/s[j]
                cn1[i,j] <- 0.5*D*F + C*G - cn2[i,j]
                ct2[i,j] <- C + 0.5*P*F/s[j] + (A*D-C*E)*G/s[j]
                ct1[i,j] <- 0.5*C*F - D*G - ct2[i,j]
            }
        }
    }
    
    # obtain influence coefficents An
    an <- matrix(0, nrow=n+1, ncol=n+1)
    for (i in 1:n) {
        an[i,1] <- cn1[i,1]
        for (j in 2:n) {
            an[i,j] <- cn1[i,j] + cn2[i,j-1]
        }
        an[i,n+1] <- cn2[i,n]
    }
    an[n+1,1] <- 1
    an[n+1,n+1] <- 1  # an[n+1,j] for j in 2 to n are already set to zeros
    
    # obtain influence coefficents At
    at <- matrix(0, nrow=n, ncol=n+1)
    for (i in 1:n) {
        at[i,1] < ct1[i,1]
        for (j in 2:n) {
            at[i,j] <- ct1[i,j] + ct2[i,j-1]
        }
        at[i,n+1] <- ct2[i,n]
    }
    
    coef <- list("an" = an, "at" = at)
}

panel.method <- function(four.digit, n, alpha) {
    # Use the vortex panel method to obtain the potential flow quantities
    # along the NACA 4-digit airfoil
    #
    # Args:
    #   four.digit: a string represents a NACA 4-digit airfoil
    #   n: number of panels (should be an even number)
    #   alpha: angle of attack (unit: degree)
    #
    # Return:
    #   local dimensionless circulation, dimensionless velocity
    #   and pressure coefficients along the airfoil
    
    source("./naca.airfoil.R")
    
    # obtain the control points (at the center of panels),
    # the panel lengths s and the panel orientation angles theta
    bp <- boundary.points(four.digit, n)
    x.boundary <- bp$x; y.boundary <- bp$y
    
    x.control <- NULL; y.control <- NULL; s <- NULL; theta <- NULL
    for (i in 1:n) {
        x.control <- c(x.control, .5*(x.boundary[i] + x.boundary[i+1]))
        y.control <- c(y.control, .5*(y.boundary[i] + y.boundary[i+1]))
        s <- c(s, sqrt((x.boundary[i+1] - x.boundary[i])^2 +
                       (y.boundary[i+1] - y.boundary[i])^2))
        theta <- c(theta, atan2(y.boundary[i+1] - y.boundary[i],
                               x.boundary[i+1] - x.boundary[i]))
    }
    
    parameters <- list("x.boundary" = bp$x,
                       "y.boundary" = bp$y,
                       "x.control" = x.control,
                       "y.control" = y.control,
                       "s" = s,
                       "theta" = theta)
    
    # coefficients
    coefficients <- coef(n, parameters)
    an <- coefficients$an
    at <- coefficients$at
    
    # obtain the RHS of the system of equations
    rhs <- sin(theta - alpha*pi/180)
    rhs <- c(rhs, 0)
    
    # solve the system to obtain local circulations
    gamma <- solve(an, rhs)
    
    # compute local dimensionless velocites
    vel <- abs(cos(theta - alpha*pi/180) + as.vector(at %*% gamma))
      
    # compute pressure coefficnet
    cp <- 1 - vel^2
    
    # omit when x > 0.995 because of numerical instability in this region
    sub <- x.control < 0.995
    
    results <- list("x.boundary" = x.boundary,
                    "y.boundary" = y.boundary,
                    "x" = x.control[sub],
                    "y" = y.control[sub],
                    "theta" = theta[sub],
                    "vel" = vel[sub],
                    "cp" = cp[sub])
    return(results)
}