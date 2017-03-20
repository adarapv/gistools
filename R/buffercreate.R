#' Create buffers and extract a series of landscape metrics
#' Input is a raster and a matrix of with site coordinates
#' @inrast = input raster
#' @inpoint = column containing species names 
#' @bufsize = buffer radius size
#' @rastcell= cell size of the raster
#'

buffercreate<-function(inrast,inpoint,bufsize,rastcell){
  results<-NULL
  for (i in 1:dim(inpoint)[1]){
    cat(paste("... Calculating buffer for site",i," ...",sep=" "))  
    cat("\n")
    # create buffer
    points <- SpatialPoints(inpoint[i,])
    pbuf <- gBuffer(points, width=bufsize)
    buf <- mask(corine, pbuf)
    buffer <- trim(buf, pad=2)
    # extract information from buffer
    writeRaster(buffer,filename = "tmp.tif",overwrite=T)
    tmpr<-raster("tmp.tif")
    tmp<-ClassStat(tmpr,cellsize=rastcell)
    tmp1<-data.frame(x=inpoint[i,1],y=inpoint[i,2],tmp)
    results<-rbind(tmp1,results)
  }
  unlink("tmp.tif") # cleaning up
  return(results)
}

