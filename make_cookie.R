yearn::yearn(spatstat) #use the yearn package to install this if needed
yearn::yearn(distances) #use the yearn package to install this if needed


make_cookie <- function(n_chips=5, chip_relative_radius = 0.2, spacing=2.2) {
    plot(c(0,0), xaxt="n", yaxt="n", xlab="", ylab="", bty="n", type="n", xlim=2*c(-1,1), ylim=2*c(-1,1),asp=1)
    symbols(0,0,circles=1, add=TRUE, bg="tan", fg=NA, inches=FALSE)
    centers <- runifdisc(n_chips, radius = 1-chip_relative_radius)
    d <- distances(data.frame(x=centers$x,y=centers$y), normalize="none")
    while(min(as.matrix(d)[upper.tri(d)])<spacing*chip_relative_radius) {
        centers <- runifdisc(n_chips, radius = 1-chip_relative_radius)
        d <- distances(data.frame(x=centers$x,y=centers$y), normalize="none")
    }
    symbols(centers$x,centers$y,circles=rep(chip_relative_radius,n_chips), add=TRUE, bg="brown", fg=NA, inches=FALSE)
    return(centers)
}

while(1<2) {
    centers <<- make_cookie()
    line <- readline("press return to continue; press escape when you're happy: ")
}
print(data.frame(centers$x,centers$y))