#rf imp function for finding optimal variable #
Mtry <- 5
RFimplementation <- function(Rdata, N, Mtry = 5, Ntree = 400){	
	Rname <- colnames(Rdata)[3]
	Pnames <- colnames(Rdata)[-c(1, 2, 3)]
	set.seed(274)
	seeds <- sample(seq(1, 1e6, 1), N)
	
		Imp <- as.data.frame(matrix(NA, nrow = length(seeds) * length(Pnames), ncol = 3, dimnames = list(NULL, c("Pnames", "%IncMSE", "IncNodePurity"))))
	for(s in 1:length(seeds)){
		set.seed(seeds[s])
		rfa <- randomForest(x = Rdata[, Pnames], y = Rdata[, Rname], ntree = 400, mtry = Mtry, importance = TRUE, do.trace = TRUE)
	
		if(s == 1){
			i1 <- s; i2 <- i1 + length(Pnames) - 1
		}else{
			i1 <- i2 + 1; i2 <- i1 + length(Pnames) - 1
		}
		Imp[i1:i2, ] <- data.frame(rownames(importance(rfa)), importance(rfa), stringsAsFactors = FALSE)
	}
	return(rfa)
	return(Imp)
}

		#set path to directory where rasters (response variable and predictors) are located
	### Note, 'pth' requires a '/' at the end ###
	pth <- "E://WCW//Predictors//a16//"
	#Polyfile <- "E:/Castle_RF/Castle_RF/wcw_a16_3m_m5_qc.shp"
	#call code to stack rasters
	#system.time(M <- StackRasters(pth, Polyfile))
	system.time(M <- StackRasters(pth))
	#call code to sample a number of random points from the raster stack
	system.time(Rdata <- SampleStack(M, n = 100000))
		Rdata[, "a_LSDM"] <- round(Rdata[, "a_LSDM"], 1)
		Rdata[, "Aspect"] <- round(Rdata[, "Aspect"])
		Rdata[, "Cover"] <- round(Rdata[, "Cover"], 2)
		Rdata[, "Elevation"] <- round(Rdata[, "Elevation"])
		Rdata[, "Slope"] <- round(Rdata[, "Slope"])
		Rdata[, "TPI"] <- round(Rdata[, "TPI"], 2)
	
	files <- list.files(pth, ".tif$")
	R <- raster(paste0(pth, files[1]))
	Rv <- getValues(R)
	Population <- Rv[-which(is.na(Rv))]
	Sample <- Rdata[, 3]
	Ftest <- var.test(Population, Sample, alternate = "two.sided")
	print(Ftest)
	
	#call code to implement RF N times and group importance data
	system.time(Imp <- RFimplementation(Rdata, N = 1))
	write.table(Imp, file = "F:/Summer2018/RF/a16_100k/MSE_NP_R2/wcw_a16_3m_m5_Imp_a.txt", sep = " ", quote = FALSE, append = TRUE)

	
	
		Pnames <- colnames(Rdata)[-c(1, 2, 3)]
		Unames <- unique(Imp[, 1])
		OverImp <- as.data.frame(matrix(NA, nrow = length(Unames), ncol = 3, dimnames = list(NULL, c("Pnames", "%IncMSE", "IncNodePurity"))))
	for(i in 1:length(Unames)){
		iii <- which(Imp[, 1] == Unames[i])
		OverImp[i, ] <- data.frame(Unames[i], mean(Imp[iii, 2]), mean(Imp[iii, 3]), stringsAsFactors = FALSE)
	}
	
	plot.new()
	Mse <- OverImp[order(OverImp[, 2]), c(1, 2)]
	Purity <- OverImp[order(OverImp[, 3]), c(1, 3)]
	jpeg("F:/Summer2018/RF/a16_100k/Graphics/a16_results_a.jpeg", height = 2048, width = 2048, res = 300, pointsize = 6, bg = "white")
	#Mean squared error importance
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
	
	
	

	
	jpeg("F:/Summer2018/RF/a16_100k/Graphics/a16_3m_m5_histo_a.jpeg", height = 1024, width = 1024, res = 300, pointsize = 10, bg = "white")
	par(mar = c(3.5, 3.5, 1, 3.5))
	hist(Population, seq(0, 10, 0.25), xlab = "", ylab = "")
	par(new = TRUE)
	hist(Sample, seq(0, 10, 0.25), border = "grey50", lty = 2, lwd = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "")
	axis(4)
	title(xlab = "Snow depth (m)", ylab = "Frequency", line = 2.5)
	dev.off()
	

	sink("F:/Summer2018/RF/a16_100k/MSE_NP_R2/wcw_a16_3m_m5_ftest_a.txt", append=TRUE)
	cat("February 2014 (50k)")
	files <- list.files(pth, ".tif$")
	R <- raster(paste0(pth, files[1]))
	Rv <- getValues(R)
	Population <- Rv[-which(is.na(Rv))]
	Sample <- Rdata[, 3]
	Ftest <- var.test(Population, Sample, alternate = "two.sided")
	print(Ftest)
	cat("=======================/n")
	sink()
	
	
	#EXCEL FUNCTION TO SAVE HARD VALUES
	Mse
	Purity
	#setwd("C:/Kelsey/Spring_2017/Random_Forest/")
	write.table(Mse, file = "F:/Summer2018/RF/a16_100k/MSE_NP_R2/wcw_a16_3m_m5_MSE_a.txt", sep = " ", quote = FALSE, append = TRUE)
	write.table(Purity, file = "F:/Summer2018/RF/a16_100k/MSE_NP_R2/wcw_a16_3m_m5_PURITY_a.txt", sep = " ", quote = FALSE, append = TRUE)

	#model training 
	
	Response <- "a_LSDM"
	Imppredictors <- c("TPI", "Elevation", "Cover", "Slope", "Aspect")	
	Pdata <- Rdata[, c(Response, Imppredictors)]
	system.time(rf <- tuneRF(x = Pdata[, Imppredictors], y = Pdata[, Response], mtryStart = 5, ntreeTry = 400, doBest = TRUE, trace = TRUE))
	system.time(save(rf, file = "F:/Summer2018/RF/a16_100k/Trained_Models/wcw_a16_100k_3m_400Treez.out", compress = TRUE))
	
	print(rf)
	
	
	##SET RIGHT PREDICTION OUTLINE˙
	

#trained RF file path including file
	RFfile <- "F:/Summer2018/RF/a16_100k/Trained_Models/wcw_a16_100k_3m_400Treez.out"
#directory of predictor rasters (outline of what you want to predict)
	Ipth <- "E://WCW//Predictors//a16//"
#output directory where sub-folders will be created
	Opth <- "F:/Summer2018/RF/a16_100k/Predictions/"
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
			
			Ncores <- 4
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
	Rpth <- "F:/Summer2018/RF/a16_100k/Predictions/SDev/"
	Mfiles <- list.files(Rpth, ".tif$")
	
			Rlist <- list()
		for(j in 1:length(Mfiles))	#:length(Mfiles)
			Rlist[[j]] <- stack(paste0(Rpth, "/", Mfiles[j]))
	
	Rlist$fun <- mean
	Rlist$file <- paste0("F:/Summer2018/RF/a16_100k/Predictions/Mosaics/wcw_a16_3m_m5_SDEV_a.tif")
	Rlist$format <- "GTiff"
	Rlist$datatype <- "FLT4S"
	Rlist$prj <- FALSE
	Rlist$options <- c("COMPRESS=LZW")
	print("Saving...")
	system.time(Mosaic <- do.call(mosaic, Rlist))
	system.time(writeRaster(Mosaic, filename = "F:/Summer2018/RF/a16_100k/Predictions/Mosaics/wcw_a16_3m_m5_SDEV_a.tif", format = "GTiff", compress = LZW))
	
	
	
	##mosaic again
	Rpth <- "F:/Summer2018/RF/a16_100k/Predictions/Tiles/"
	Mfiles <- list.files(Rpth, ".tif$")
	
			Rlist <- list()
		for(j in 1:length(Mfiles))	#:length(Mfiles)
			Rlist[[j]] <- stack(paste0(Rpth, "/", Mfiles[j]))
	
	Rlist$fun <- mean
	Rlist$file <- paste0("F:/Summer2018/RF/a16_100k/Predictions/Mosaics/wcw_a16_3m_m5_LSDM_a.tif")
	Rlist$format <- "GTiff"
	Rlist$datatype <- "FLT4S"
	Rlist$prj <- FALSE
	Rlist$options <- c("COMPRESS=LZW")
	print("Saving...")
	system.time(Mosaic <- do.call(mosaic, Rlist))
	system.time(writeRaster(Mosaic, filename = "F:/Summer2018/RF/a16_100k/Predictions/Mosaics/wcw_a16_3m_m5_LSDM_a.tif", format = "GTiff", compress = LZW))
	write.table(Rdata, file = "F:/Summer2018/RF/a16_100k/wcw_a16_RDATA_matrix.txt", row.names=FALSE, col.names=FALSE)
	
	
	RFfile <- "F:/Summer2018/RF/a16_3V/Trained_Models/wcw_a16_3V_3m_m3_400Treez.out"
	RF <- get(load(RFfile))
	Rdata <- read.table("F:/Summer2018/RF/a16_3V/MSE_NP_R2/wcw_a16_3V_3m_m5_Rdata.txt")
	#stats
	Reg <- lm(RF$y ~ RF$predicted)
	#RFy is true LSDM, right is predicted
	#r2
	s1 <- summary(Reg)$r.squared
	write.table(s1, file = "F:/Summer2018/RF/a16_3V/MSE_NP_R2/wcw_a16_3m_m5_a_R2.txt", sep = " ", quote = FALSE, append = TRUE)
	#r2 adj
	s2 <- summary(Reg)$adj.r.squared
	write.table(s2, file = "F:/Summer2018/RF/a16_3V/MSE_NP_R2/wcw_a16_3m_m5_a_R2.txt", sep = " ", quote = FALSE, append = TRUE)
	#MSE
	s3 <- summary(Reg)$sigma
	write.table(s3, file = "F:/Summer2018/RF/a16_3V/MSE_NP_R2/wcw_a16_3m_m5_a_R2.txt", sep = " ", quote = FALSE, append = TRUE)
	
	tree <- getTree(RF, k=1, labelVar=TRUE)
	write.table(tree, file =  "F:/Summer2018/RF/a16_3V/wcw_a16_3m_m5_a_tree.txt", row.names=FALSE, col.names=FALSE)
	