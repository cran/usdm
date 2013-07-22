# Author: Babak Naimi, naimi.b@gmail.com
# Date :  July. 2013
# Version 1.1
# Licence GPL v3

if (!isGeneric("exclude")) {
  setGeneric("exclude", function(x,vif,...)
    standardGeneric("exclude"))
}  

setMethod ('exclude' ,signature(x='RasterStack', vif='VIF'),
           function (x,vif,...) {
             n <- names(x)
             for (i in 1:length(vif@results[,1])) if (!vif@results[i,1] %in% n) stop("One or all variables in VIF are not in the Raster object")
             x[[vif@results[,1]]]
            }
           )


setMethod ('exclude' ,signature(x='RasterBrick', vif='VIF'),
           function (x,vif,...) {
             n <- names(x)
             for (i in 1:length(vif@results[,1])) if (!vif@results[i,1] %in% n) stop("One or all variables in VIF are not in the Raster object")
             brick(x[[vif@results[,1]]])
           }
)

setMethod ('exclude' ,signature(x='data.frame', vif='VIF'),
           function (x,vif, ...) {
             n <- colnames(x)
             for (i in 1:length(vif@results[,1])) if (!vif@results[i,1] %in% n) stop("One or all variables in VIF are not in the data.frame object")
             x[,vif@results[,1]]
           }
)

setMethod ('exclude' ,signature(x='matrix', vif='VIF'),
           function (x,vif, ...) {
             n <- colnames(x)
             for (i in 1:length(vif@results[,1])) if (!vif@results[i,1] %in% n) stop("One or all variables in VIF are not in the matrix object")
             x[,vif@results[,1]]
           }
)

setMethod ('exclude' ,signature(x='RasterStack', vif='missing'),
           function (x,vif,th) {
             n <- names(x)
             if(missing(th)) th <- 10
             vif <- vifstep(x)
             print(vif)
             if (length(vif@excluded) > 0) x[[vif@results[,1]]]
             else x
           }
)

setMethod ('exclude' ,signature(x='RasterBrick', vif='missing'),
           function (x,vif, th) {
             n <- names(x)
             if(missing(th)) th <- 10
             vif <- vifstep(x,th=th)
             print(vif)
             if (length(vif@excluded) > 0) brick(x[[vif@results[,1]]])
             else x
           }
)

setMethod ('exclude' ,signature(x='data.frame', vif='missing'),
           function (x,vif, th) {
             n <- colnames(x)
             if(missing(th)) th <- 10
             vif <- vifstep(x,th=th)
             print(vif)
             if (length(vif@excluded) > 0) x[,vif@results[,1]]
             else x
           }
)

setMethod ('exclude' ,signature(x='matrix', vif='missing'),
           function (x,vif, th) {
             n <- colnames(x)
             if(missing(th)) th <- 10
             vif <- vifstep(x,th=th)
             print(vif)
             if (length(vif@excluded) > 0) x[,vif@results[,1]]
             else x
           }
)
