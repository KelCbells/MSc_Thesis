
data <- read.table("F:/Summer2018/RF/PCC_stack/wcw_stack_50Kpts.txt", header = TRUE, sep = "", nrows = 20001)
p1 <- cor.test(data$a16_orig, data$a16_LSDM, method=c("pearson"))
p2 <- cor.test(data$a16_orig, data$a16_m17_LSDM, method=c("pearson"))
p3 <- cor.test(data$f17_orig, data$f17_LSDM, method=c("pearson"))
p4 <- cor.test(data$f17_orig, data$f17_m17_LSDM, method=c("pearson"))
p5 <- cor.test(data$f14_orig, data$f14_LSDM, method=c("pearson"))
p6 <- cor.test(data$f14_orig, data$f14_m17_LSDM, method=c("pearson"))
data <- read.table("F:/Summer2018/RF/PCC_stack/m17_stack_12Kpts.txt", header = TRUE, sep = "", nrows = 3501)
p7 <- cor.test(data$m17_LSDM_overlap, data$m17_predicted_overlap, method=c("pearson"))
p8 <- cor.test(data$m17_LSDM_overlap, data$m17_40k_predOverlap, method=c("pearson"))
		
	sink("F:/Summer2018/RF/PCC_stack/PCC_Summary_Oct2018.txt", append=TRUE)
	cat("a16_wcw_RF")
	print(p1)
	cat("a16_m17_RF")
	print(p2)
	cat("f17_wcw_RF")
	print(p3)
	cat("f17_m17_RF")
	print(p4)
	cat("f14_wcw_RF")
	print(p5)
	cat("f14_m17_RF")
	print(p6)
	cat("m17_overlap")
	print(p7)
	cat("m17_40k_OL")
	print(p8)
	sink()
	
	

		

	