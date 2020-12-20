## Functions Ex 3 / 4:

######################################################################
## 0.2 small helper functions
######################################################################

## some functions for making your work more convenient.
## just insert the code to R and proceed

MSE <- function(obs,pred){
  ### Mean squared error
  mean((obs-pred)^2,na.rm=TRUE)
}

R2 <- function(obs,pred){
  ### Coefficient of determination
  ### also known as Nash-Stutcliffe efficiency in the hydrology literature
  obs.mean <- mean(obs,na.rm=TRUE)
  1 -  sum((obs-pred)^2,na.rm=TRUE)/sum((obs-obs.mean)^2,na.rm=TRUE)
}



######################################################################
## 3.1: function for visualising the example data
######################################################################

plot.xyz.persp <- function(x,y,z,
                           xlim=c(-10,10),
                           ylim=c(-10,10),
                           zlim=c(-7,14),
                           nx=100,
                           ny=100,
                           axnames=TRUE,
                           title=NA,
                           return_pmat=F){
  ### function for visualising the artificial data of this exercise
  ###
  ### x: x values
  ### z: z values
  ### y: y values
  ### xlim,ylim,zlim: range of the x,y and z-values with default values.
  ### nx,ny: number of steps in x and y
  ### axnames: if TRUE axis names are plotted. Otherwise not.
  ###
  ### NOTE: this function is tailored to the test data
  ### used in this script. It will very likely not work for other data
  x.dummy <- seq(min(xlim),max(xlim),length.out=nx)
  y.dummy <- seq(min(ylim),max(ylim),length.out=nx)
  xy.dummy <- expand.grid(x=x.dummy,y=y.dummy)
  xy.dummy$ind <- 1:nrow(xy.dummy)
  ##
  xyz <- data.frame(x=x,y=y,z=z)
  ##
  xyz.full <- merge.data.frame(x=xy.dummy,y=xyz,by=c("x","y"),all.x=TRUE)
  xyz.full <- xyz.full[order(xyz.full$ind),]
  ## par(mfrow=c(1,2));image(as.matrix(xy.dummy[,c("x","y")]),main="target");image(as.matrix(xyz.full)[,c("x","y")])
  if(!all(xy.dummy[,c("x","y")]==xyz.full[,c("x","y")])){
    stop("x or y data are not compatible with xlim,zlim or ny,ny")
  }
  ##
  xx <- sort(unique(xyz.full$x))
  yy <- sort(unique(xyz.full$x))
  zz <- matrix(xyz.full$z,nrow=length(xx),ncol=length(yy))
  ##
  nrz <- nrow(zz)
  ncz <- ncol(zz)
  ##
  ncol <- 100
  color <- colorRampPalette(brewer.pal(9,"YlOrRd")[-1])(ncol)
  zfacet <- zz[-1, -1] + zz[-1, -ncz] + zz[-nrz, -1] + zz[-nrz, -ncz]
  facetcol <- cut(zfacet, ncol)
  if(axnames){
    x.lab <- "X"
    y.lab <- "Y"
    z.lab <- "Z"
  } else {
    x.lab <- y.lab <- z.lab <- ""
  }
  ##
  pmat <- persp(xx,yy,zz,
        theta = 30, phi = 30,
        expand = 0.5, col = color[facetcol],
        ltheta = 120, shade = 0.5,
        xlab=x.lab,ylab=y.lab,zlab=z.lab,border=NA,zlim=zlim,
        xlim=range(x),ylim=range(y),main=title)
  if(return_pmat) return(pmat)
}


# depth calculation function (adapted from Duncan Murdoch at https://stat.ethz.ch/pipermail/r-help/2005-September/079241.html)
depth3d <- function(x,y,z, pmat, minsize=0.2, maxsize=2) {

  # determine depth of each point from xyz and transformation matrix pmat
  tr <- as.matrix(cbind(x, y, z, 1)) %*% pmat
  tr <- tr[,3]/tr[,4]

  # scale depth to point sizes between minsize and maxsize
  psize <- ((tr-min(tr) ) * (maxsize-minsize)) / (max(tr)-min(tr)) + minsize
  return(psize)
}

modified.cv.tree <- function (object, rand, FUN = prune.tree, K = 10, ...) {
### based on cv.tree
###
### all arguments are identical to cv.tree
    if (!inherits(object, "tree"))
        stop("not legitimate tree")
    m <- model.frame(object)
    extras <- match.call(expand.dots = FALSE)$...
    FUN <- deparse(substitute(FUN))
    init <- do.call(FUN, c(list(object), extras))
    if (missing(rand))
        rand <- sample(K, length(m[[1L]]), replace = TRUE)
    cvdev <- 0
    ## start: modification no. 1
    ## (a) the main modification
    tree.call <- object$call ## get the original call of 'tree'
    tree.call$data <- as.name("m") ## overwrite the 'data' argument
    ## (b) for tracking progress
    my.counter <- 0
    ## end: modification no. 1
    for (i in unique(rand)) {
        ## start: modification no. 2
        ## (a) track and print progress on screen
        my.counter <- my.counter + 1
        cat(base::date(),"k =",my.counter,"of",K,"\n")
        ## comment original code
        ## tlearn <- tree(model = m[rand != i, , drop = FALSE])
        tree.call$subset <- rand != i ## specify the sub-set
        tlearn <- eval(tree.call) ## evaluate the function call
        ## end: modification no.2
        plearn <- do.call(FUN, c(list(tlearn, newdata = m[rand ==
            i, , drop = FALSE], k = init$k), extras))
        cvdev <- cvdev + plearn$dev
    }
    init$dev <- cvdev
    init
}
