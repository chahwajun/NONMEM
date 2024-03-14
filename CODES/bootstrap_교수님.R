# Bootstrap dataset ------------------------------------------------

# Directory containing the orinigal nonmem code
WD <- "C:\\Users\\clex9\\pipetcpt\\zolpidemsim\\논문\\bootstrap\\교수님 코드\\"

# Directory containing the orinigal nonmem code
OC <- "C:\\Users\\clex9\\pipetcpt\\zolpidemsim\\논문\\bootstrap\\교수님 코드\\"

# Directory containing the orinigal nonmem dataset
DD <- "C:\\Users\\clex9\\pipetcpt\\zolpidemsim\\논문\\bootstrap\\교수님 코드\\"

setwd(paste(WD))
codename <- "zpd"   					  # Put the name of the code without extension (ctl)
dataname <- "PK_Zolpidem_MFDS_COV_1.csv"   		  

nmif <- paste(codename,".ctl", sep="") 			  # NONMEM input file
nmof <- paste(codename,".out", sep="") 			  # NONMEM output file

# make new directory for the bootstrap

bootd <- paste(WD, "\\bootstrap\\",sep="")  		  # bootstrap directory
dir.create(bootd)

n.data.d <- paste(bootd, "sampdata\\",sep="")  		  # bootstrap data directory
dir.create(n.data.d)

O.data <- read.csv(paste(DD,dataname, sep=""), na.strings=".", stringsAsFactors = F)     # Original dataset
UID <- unique(O.data$ID)

# Write the bootstap start and end number here ---------------------

bootst <- 1
bootend <- 500

for (i in bootst:bootend) { 
  set.seed(i) 
  sampID <- sample(UID, replace=TRUE) 
  k <- 1 
  sampdata <- NULL 
  boot.dat <- NULL 
  for (j in sampID) { 
    sampdata <- O.data[O.data$ID %in% j,] 
    sampdata$ID <- k 
    boot.dat <- rbind(boot.dat, sampdata) 
    k <- k + 1  
  }
  write.table(boot.dat, file=paste(n.data.d, paste(paste("boot",i,sep=""),".dat",sep=""),sep=""), na=".", quote=F, col.names=TRUE, row.names=FALSE)
}

# Bootstrap Run --------------------------------------------------------------

setwd(paste(bootd))                                            
O.code <- readLines(paste(OC,nmif,sep="")) 
O.code <- O.code[-(min(grep("TABLE ", O.code)):(max(grep("TABLE ", O.code))+1))]    # Remove Tables from the original code 
O.code <- O.code[-grep("COV", O.code)]                                              # Remove COVARIANCE STEP from the original code

bootcode <- NULL 
bootraw  <- NULL                                                                    # Boot raw data

for (i in bootst:bootend) { 
  cat(i,"\n") 
  OUT <- NULL 
  outraw <- NULL 
  bootdata <- paste("sampdata\\", paste(paste("boot",i,sep=""),".dat",sep=""), sep="") 
  bootcode <- sub(paste(DD, dataname, sep=""), bootdata, O.code, fixed = TRUE) 
  write(bootcode, file=paste(bootd, "nmboot.ctl" ,sep=""))     # bootd: bootstrap directory as define above 
  bootrun <- paste("nmfe7.bat", paste("nmboot.ctl", paste(paste("nmboot",i,sep=""),".out", sep=""), sep=" "))	 # NONMEM 7.2
  system(bootrun, invisible=T, show.output.on.console=F, wait = TRUE) 
  EXT <- read.table(paste("nmboot",".ext",sep=""), header=T, skip=1) 
  outraw <- EXT[which(EXT$ITERATION==-1000000000),-1] 
  OUT <- readLines(paste("nmboot",i,".out",sep="")) 
  outraw$MOFV <- EXT[which(EXT$ITERATION==-1000000000),which(colnames(EXT)=="OBJ")]
  outraw$SUCCESS <- as.numeric("0MINIMIZATION SUCCESSFUL" %in% OUT)
  outraw$ROUNDING <- as.numeric(" DUE TO ROUNDING ERRORS (ERROR=134)" %in% OUT)
  outraw$REPN <- i
  bootraw <- rbind(bootraw, outraw)
  write.csv(bootraw, paste0('../bootraw/bootraw', i, '.csv'), row.names=FALSE)
}

write.csv(bootraw, "../bootraw/bootraw.csv", row.names=F)

