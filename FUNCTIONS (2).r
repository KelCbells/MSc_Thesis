rm(list = objects())

library("raster")
library("rgdal")
library("randomForest")
library("maptools")
library("raster")
library("spatial")
library("sp")
library("matlab")
library("itertools")
library("parallel")
library("foreach")
library("doSNOW")
library("grDevices")


StackRasters <- function(pth, Polyfile, ...){
#pth <- "path"		#directory where all .tif files are
files <- list.files(pth, ".tif$")				
	
Ext <- list()
for(f in 1:length(files))
	Ext[[f]] <- extent(raster(paste0(pth, files[f])))
	
	Elist <- lapply(Ext, as.matrix)
	Emat <- matrix(unlist(Elist), ncol = length(Elist), dimnames = list(c("xmin", "ymin", "xmax", "ymax"), NULL))
	#find smallest extent size for cropping
	#MinExt <- c(max(Emat[1, ]), min(Emat[3, ]), max(Emat[2, ]), min(Emat[4, ]))
	
		M <- stack()
	for(i in 1:length(files))
		M <- stack(M, paste0(pth, files [i]))
		#S <- stack(S, crop(raster(paste0(pth, files[i])), MinExt))
		
		
		#remove hash tags and change variable name in lines 23/25 __ <- stack
		#PolyExt <- readShapePoly(Polyfile)
		#M <- mask(S, PolyExt, progress = "text")
		
	return(M)
}

	
	
SampleStack <- function(r, n, seed = 7438){
	#r = raster stack dataset, n = number of random points to sample
	set.seed(seed)
	x <- sampleRandom(r, n, na.rm = TRUE, xy = TRUE)
	return(x)
}





RFimplementation <- function(Rdata, N, Mtry = 5, Ntree = 250){	
	Rname <- colnames(Rdata)[3]
	Pnames <- colnames(Rdata)[-c(1, 2, 3)]
	set.seed(7438)
	seeds <- sample(seq(1, 1e6, 1), N)
	
		Imp <- as.data.frame(matrix(NA, nrow = length(seeds) * length(Pnames), ncol = 3, dimnames = list(NULL, c("Pnames", "%IncMSE", "IncNodePurity"))))
	for(s in 1:length(seeds)){
		set.seed(seeds[s])
		rf <- randomForest(x = Rdata[, Pnames], y = Rdata[, Rname], ntree = 250, mtry = Mtry, importance = TRUE, do.trace = TRUE)
		
		if(s == 1){
			i1 <- s; i2 <- i1 + length(Pnames) - 1
		}else{
			i1 <- i2 + 1; i2 <- i1 + length(Pnames) - 1
		}
		Imp[i1:i2, ] <- data.frame(rownames(importance(rf)), importance(rf), stringsAsFactors = FALSE)
	}
	return(Imp)
}


TileStack <- function(Rstack, Multiplier = 1000, Plot = TRUE, ...){
	#determine number of tiles to break the dataset in to	
		xdim <- ceiling( (extent(Rstack)@xmax - extent(Rstack)@xmin) / (res(Rstack)[1] * Multiplier) )# - 1
		ydim <- ceiling( (extent(Rstack)@ymax - extent(Rstack)@ymin) / (res(Rstack)[1] * Multiplier) )# - 1
	#grid to create tiles
	Grd <- GridTopology(c(extent(Rstack)@xmin, extent(Rstack)@ymin) + (0.5 * (res(Rstack) * Multiplier)), res(Rstack) * Multiplier, c(xdim, ydim))
	Sg <- SpatialGrid(Grd, proj4string = CRS(projection(Rstack)))
	Sp <- as(Sg, "SpatialPolygons")
	#visualize tiles (not necessary)
	if(Plot == TRUE){
		image(Rstack[[1]], xlim = c(extent(Sg)@xmin, extent(Sg)@xmax), ylim = c(extent(Sg)@ymin, extent(Sg)@ymax))
		plot(Sp, add = TRUE)	
	}
	
	return(Sp)
}	

RFsdev <- function(X, Predictions){
					SDev <- Mode <- rep(NA, nrow(Predictions[[X]]))
				for(i in 1:nrow(Predictions[[X]]))
					SDev[i] <- sd(Predictions[[X]][i,])
				#for(i in 1:nrow(Predictions[[X]])){
				#	h <- hist(Predictions[[X]][i,], seq(0, 7, 0.1), plot = FALSE)
				#	Mode[i] <- h$breaks[which(h$counts == max(h$counts))]
				#}
				#list(sd = SDev, md = Mode)	
				return(SDev)
}


