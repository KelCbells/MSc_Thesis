



******RESTART R BEFORE RUNNING THIS SCRIPT******


#doesnt work 1893 189, 77, 777, 472, 274, 385, 385, 951, 329, 6083, 7649, 92978, 56035, 87406, 5543, 8362, 6461. used 646
#using 4300072 for f14_int2_float b/c it has best ROV (october 5) -> 0.54 for f14, ~1 for MW sets
#38048 = big p values for MW but 0.52 and small p for f14
#7438 0.53-0.54 for 50k and 100k f14, run with that. **THIS IS A GOOD SEED**
SampleStack <- function(r, n, seed = 7438){
	#r = raster stack dataset, n = number of random points to sample
	set.seed(seed)
	x <- sampleRandom(r, n, na.rm = TRUE, xy = TRUE)
	return(x)
}


	#set path to directory where rasters (response variable and predictors) are located
	pth <- "E:/RF_Data_Current2019/Int_Predictors/RF/Int_Predictors/f14/"
	#Polyfile <- "F:/Castle_3m_RF/Castle_3m_RF/f14_int2_int2_qc.shp"
	#system.time(M <- StackRasters(pth, Polyfile))
	system.time(M <- StackRasters(pth))
	#call code to sample a number of random points from the raster stack
	system.time(Rdata <- SampleStack(M, n = 50000))
		Rdata[, "a_LSDM"] <- round(Rdata[, "a_LSDM"])
		Rdata[, "Aspect"] <- round(Rdata[, "Aspect"])
		Rdata[, "Cover"] <- round(Rdata[, "Cover"])
		Rdata[, "Elevation"] <- round(Rdata[, "Elevation"])
		Rdata[, "Slope"] <- round(Rdata[, "Slope"])
		Rdata[, "TPI"] <- round(Rdata[, "TPI"])

		
	system.time(Imp <- RFimplementation(Rdata, N = 1))
	write.table(Imp, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_.txt", sep = " ", quote = FALSE, append = TRUE)
	write.table(Rdata, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_RDATA_matrix.txt", row.names=FALSE, col.names=TRUE)
	
	plot.new()
		Pnames <- colnames(Rdata)[-c(1, 2, 3)]
		Unames <- unique(Imp[, 1])
		OverImp <- as.data.frame(matrix(NA, nrow = length(Unames), ncol = 3, dimnames = list(NULL, c("Pnames", "%IncMSE", "IncNodePurity"))))
	for(i in 1:length(Unames)){
		iii <- which(Imp[, 1] == Unames[i])
		OverImp[i, ] <- data.frame(Unames[i], mean(Imp[iii, 2]), mean(Imp[iii, 3]), stringsAsFactors = FALSE)
	}

	
	Mse <- OverImp[order(OverImp[, 2]), c(1, 2)]
	Purity <- OverImp[order(OverImp[, 3]), c(1, 3)]
	#Mean squared error importance
	jpeg("E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_results.jpeg", height = 2048, width = 2048, res = 300, pointsize = 6, bg = "white")
	par(mfrow = c(1, 2))
	par(mar = c(3.5, 6.5, 1, 1))
	plot(Mse[, 2], seq(1, 5, 1), xlab = "", ylab = "", yaxt = "n")
	for(i in 1:length(Pnames))
		abline(i, 0, lty = 2, col = "grey80")
	axis(2, at = seq(1, 5, 1), labels = Mse[, 1], las = 1)
	title(xlab = "Inc MSE (%)", line = 2.5)
	#Node purity importance
	par(mar = c(3.5, 6.5, 1, 1))
	plot(Purity[, 2], seq(1, 5, 1), xlab = "", ylab = "", yaxt = "n")
	for(i in 1:length(Pnames))
		abline(i, 0, lty = 2, col = "grey80")
	axis(2, at = seq(1, 5, 1), labels = Purity[, 1], las = 1)	
	title(xlab = "Inc node purity", line = 2.5)
	dev.off()
	
	sink("E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_ftest.txt", append=TRUE)
	cat("April 2016 integer")
	files <- list.files(pth, ".tif$")
	R <- raster(paste0(pth, files[1]))
	Rv <- getValues(R)
	Rv[Rv == 128] <- NA
	Rmv <- which(is.na(Rv))
		Population <- Rv
	if(length(Rmv) > 0)
		Population <- Rv[-Rmv]
	Sample <- Rdata[, 3]
	Ftest <- var.test(Population, Sample, alternate = "two.sided")
	print(Ftest)
	cat("=======================\n")
	sink()
	
	plot.new()
	jpeg("E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_histo.jpeg", height = 1024, width = 1024, res = 300, pointsize = 10, bg = "white")
	par(mar = c(3.5, 3.5, 1, 3.5))
	hist(Population, seq(0, 128, 1), xlab = "", ylab = "")
	par(new = TRUE)
	files <- list.files(pth, ".tif$")
	R <- raster(paste0(pth, files[1]))
	Rv <- getValues(R)
	Rv[Rv == 128] <- NA
	Rmv <- which(is.na(Rv))
		Sample <- Rv
	if(length(Rmv) > 0)
		Sample <- Rv[-Rmv]
	hist(Sample, seq(0, 128, 1), border = "grey50", lty = 2, lwd = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "")
	axis(4)
	title(xlab = "Snow depth (m)", ylab = "Frequency", line = 2.5)
	dev.off()
	

	

	
	#EXCEL FUNCTION TO SAVE HARD VALUES
	Mse
	Purity
	#setwd("C:/Kelsey/Spring_2017/Random_Forest/")
	write.table(Mse, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_3m_MSE_a.txt", sep = " ", quote = FALSE, append = TRUE)
	write.table(Purity, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_3m_PURITY_a.txt", sep = " ", quote = FALSE, append = TRUE)
	#Rdata <- read.table(file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_50_int_RDATA_matrix.txt", row.names=FALSE, col.names=FALSE)

	
	
	#model tuning 
	
	Response <- "a_LSDM"
	Imppredictors <- c("TPI", "Elevation", "Cover", "Slope", "Aspect")	
	Pdata <- Rdata[, c(Response, Imppredictors)]
	system.time(rf <- tuneRF(x = Pdata[, Imppredictors], y = Pdata[, Response], mtryStart = 5, doBest = TRUE, trace = TRUE))
	system.time(save(rf, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_250t.out", compress = TRUE))
	
	print(rf)
	
	
	
	

#trained RF file path including file
	Rdata <- read.table("E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_RDATA_matrix.txt")
	RFfile <- "E:/2019_RF_outputs/Thesis/f14/f14_int2_int2_250t.out"
#outline of what you want to predict -> Must be a folder with multiple tif files.
	Ipth <- "E:/RF_Data_Current2019/Int_Predictors/RF/Int_Predictors/f14/"
#output directory where sub-folders will be createad
	Opth <- "E:/2019_RF_outputs/Thesis/f14/"
	dir.create(Opth)
	dir.create(paste0(Opth, "/Tiles"))
	dir.create(paste0(Opth, "/SDev"))
	dir.create(paste0(Opth, "/Mosaics"))

#####START	
	#stack rasters & rename if necessary
		Rstack <- StackRasters(Ipth)
			#names(Rstack) <- c("")
	#create tiles
		Sp <- TileStack(Rstack, Multiplier = 1000, Plot = TRUE)
	#import trained RF model
		RF <- get(load(RFfile))

##PREDICTIONS

system.time(
for(s in 1:length(Sp@polygons)){	#1:length(Sp@polygons)
	#define tile to work with
		P <- SpatialPolygons( list( Polygons( list( Polygon(Sp@polygons[[s]]@Polygons[[1]]@coords) ), ID = s) ), proj4string = CRS(projection(Rstack)))
	#crop & import data values for tile
		system.time(T <- crop(Rstack, P))
		system.time(Zmat <- getValues(T))
	if(any(!is.na(Zmat)) == TRUE){
	#round all values to X decimals
		Zmat <- round(Zmat, 2)
	#remove all NA values from predictors			
			F <- function(X, Zmat){which(is.na(Zmat[, X]))}
		Rmvs <- lapply(1:length(names(Rstack)), F, Zmat)
			Rmvs <- unique(unlist(Rmvs))
		if(length(Rmvs) < 1)
			Rmvs <- -seq(1, nrow(Zmat), 1)
			
			Ncores <- 5
		if(nrow(Zmat[-Rmvs, ]) > 0){
		#cluster cores in preparation for RF predictions
			cl <- makeCluster(Ncores)
			registerDoSNOW(cl)	
			#make Rf predictions
				system.time(predRF <- foreach(d = isplitRows(Zmat[-Rmvs, ], chunks = Ncores), .combine = c, .packages = c("randomForest")) %dopar% { predict(RF, newdata = d, predict.all = TRUE) })
					print("Finished RF predictions...")
		#stop core cluster to relinquish memory
			stopCluster(cl)
			#prepare standard deviation classes for outputs
				system.time(tmp <- sapply(X = seq(2, Ncores * 2, 2), FUN = RFsdev, Predictions = predRF))
					print("Finished preparing standard deviation classes...")
			#SD <- tmp["sd", ]
			#Mode <- tmp["md", ]
			SD <- tmp
		
		#gather mode values
		#		Mde <- rep(NA, nrow(Zmat[-Rmvs, ]))
		#	for(i in 1:length(SD)){
		#		if(i == 1)
		#			i1 <- i
		#		i2 <- i1 + (length(Mode[[i]]) - 1)
		#		Mde[i1:i2] <- Mode[[i]]
		#		i1 <- i2 + 1
		#	}
		#place mode values in to a vector for output
		#	Mmat <- rep(-1, ncol(T) * nrow(T))
		#		rrr <- seq(1, ncol(T) * nrow(T), 1)[-Rmvs]
		#	Mmat[rrr] <- Mde	
		#prepare and save as tif
		#	sp <- SpatialPointsDataFrame((expand.grid(x = seq(extent(T)@xmin, extent(T)@xmax - res(T)[1], res(T)[1]) + (0.5 * res(T)[1]), y = rev(seq(extent(T)@ymin, extent(T)@ymax - res(T)[2], res(T)[2]) + (0.5 * res(T)[1])))), data = data.frame(Mode = Mmat), proj4string = CRS(projection(Rstack)))
		#		gridded(sp) <- TRUE
		#			print("Saving Mode...")
		#			writeGDAL(sp, fname = paste0(Opth, "/Mode/M_", s, ".tif"), drivername = "GTiff", setStatistics = TRUE, type = "Float32", mvFlag = -1)
		
		#gather standard deviation values
				SDev <- rep(NA, nrow(Zmat[-Rmvs, ]))
			for(i in 1:length(SD)){
				if(i == 1)
					i1 <- i
				i2 <- i1 + (length(SD[[i]]) - 1)
				SDev[i1:i2] <- SD[[i]]
				i1 <- i2 + 1
			}
		#place standard deviation values in to a vector for output
			Smat <- rep(-1, ncol(T) * nrow(T))
				rrr <- seq(1, ncol(T) * nrow(T), 1)[-Rmvs]
			Smat[rrr] <- SDev	
		#prepare and save as tif
			sp <- SpatialPointsDataFrame((expand.grid(x = seq(extent(T)@xmin, extent(T)@xmax - res(T)[1], res(T)[1]) + (0.5 * res(T)[1]), y = rev(seq(extent(T)@ymin, extent(T)@ymax - res(T)[2], res(T)[2]) + (0.5 * res(T)[1])))), data = data.frame(SDev = Smat), proj4string = CRS(projection(Rstack)))
				gridded(sp) <- TRUE
					print("Saving SDev...")
					writeGDAL(sp, fname = paste0(Opth, "/SDev/S_", s, ".tif"), drivername = "GTiff", setStatistics = TRUE, type = "Float32", mvFlag = -1)
		#gather predicted (response) variable values
				Variable <- rep(NA, nrow(T) * ncol(T))[-Rmvs]
			for(i in seq(1, length(predRF) - 1, 2)){
				if(i == 1)
					i1 <- i
				i2 <- i1 + (length(predRF[[i]]) - 1)
				Variable[i1:i2] <- round(predRF[[i]], 2)
				i1 <- i2 + 1
			}
		#place (response) variable values in to a vector for output
			Omat <- rep(-1, ncol(T) * nrow(T))
				rrr <- seq(1, ncol(T) * nrow(T), 1)[-Rmvs]
			Omat[rrr] <- Variable#[-Rmvs]
		#prepare and save as tif		
			sp <- SpatialPointsDataFrame((expand.grid(x = seq(extent(T)@xmin, extent(T)@xmax - res(T)[1], res(T)[1]) + (0.5 * res(T)[1]), y = rev(seq(extent(T)@ymin, extent(T)@ymax - res(T)[2], res(T)[2]) + (0.5 * res(T)[1])))), data = data.frame(kNN = Omat), proj4string = CRS(projection(Rstack)))
				gridded(sp) <- TRUE
					print("Saving predictions...")
					writeGDAL(sp, fname = paste0(Opth, "/Tiles/T_", s, ".tif"), drivername = "GTiff", setStatistics = TRUE, type = "Float32", mvFlag = -1)
		}
	}
})
	
		##Mosaic
	Rpth <- "E:/2019_RF_outputs/Thesis/f14/SDev/"
	Mfiles <- list.files(Rpth, ".tif$")
	
			Rlist <- list()
		for(j in 1:length(Mfiles))	#:length(Mfiles)
			Rlist[[j]] <- stack(paste0(Rpth, "/", Mfiles[j]))
	
	Rlist$fun <- mean
	Rlist$file <- paste0("E:/2019_RF_outputs/Thesis/f14/Mosaics/f14_int2_50_int_SDEV_a.tif")
	Rlist$format <- "GTiff"
	Rlist$datatype <- "FLT4S"
	Rlist$prj <- FALSE
	Rlist$options <- c("COMPRESS=LZW")
	print("Saving...")
	system.time(Mosaic <- do.call(mosaic, Rlist))
	system.time(writeRaster(Mosaic, filename = "E:/2019_RF_outputs/Thesis/f14/Mosaics/f14_int2_50_int_SDEV_a.tif", format = "GTiff", compress = LZW))
	
	
	
	##mosaic again
	Rpth <- "E:/2019_RF_outputs/Thesis/f14/Tiles/"
	Mfiles <- list.files(Rpth, ".tif$")
	
			Rlist <- list()
		for(j in 1:length(Mfiles))	#:length(Mfiles)
			Rlist[[j]] <- stack(paste0(Rpth, "/", Mfiles[j]))
	
	Rlist$fun <- mean
	Rlist$file <- paste0("E:/2019_RF_outputs/Thesis/f14/Mosaics/f14_int2_50_int_LSDM_a.tif")
	Rlist$format <- "GTiff"
	Rlist$datatype <- "FLT4S"
	Rlist$prj <- FALSE
	Rlist$options <- c("COMPRESS=LZW")
	print("Saving...")
	system.time(Mosaic <- do.call(mosaic, Rlist))
	system.time(writeRaster(Mosaic, filename = "E:/2019_RF_outputs/Thesis/f14/Mosaics/f14_int2_50_int_LSDM_a.tif", format = "GTiff", compress = LZW))
	
	
	
	#stats
	Reg <- lm(RF$y ~ RF$predicted)
	#RFy is true LSDM, right is predicted
	#r2
	s1 <- summary(Reg)$r.squared
	write.table(s1, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_50_int_R2.txt", sep = " ", quote = FALSE, append = TRUE)
	#r2 adj
	s2 <- summary(Reg)$adj.r.squared
	write.table(s2, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_50_int_R2.txt", sep = " ", quote = FALSE, append = TRUE)
	#MSE
	s3 <- summary(Reg)$sigma
	write.table(s3, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_50_int_R2.txt", sep = " ", quote = FALSE, append = TRUE)
	#write.table(Rdata, file = "E:/2019_RF_outputs/Thesis/f14/f14_int2_50_int_RDATA_matrix.txt", row.names=FALSE, col.names=TRUE)
	
	tree <- getTree(RF, k=1, labelVar=TRUE)
	write.table(tree, file =  "E:/2019_RF_outputs/Thesis/f14/f14_int2_50_int_tree.txt", row.names=FALSE, col.names=FALSE)
	