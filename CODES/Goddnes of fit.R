#GOF IPP

library(lattice)
library(gridExtra)

getwd()
#setwd("C:/Users/qfitt/OneDrive/MS/BASIC2/IPP_S")

# Read data
a <- read.table("sdtab0041", skip=1, header=T)

# Filter data
PD <- a[a$CMT == 4, ]
PD <- PD[PD$MDV == 0, ]

a <- PD

# Create individual plots
p1 <- xyplot(DV ~ PRED, data=a, 
             xlab="Population Prediction",
             ylab="Observation",
             panel=function(x, y, ...) {
                 panel.xyplot(x, y, pch=21, col="black", cex=0.9)
                 panel.loess(x, y, lwd=2, lty=2, col="red")
                 panel.abline(0, 1, lwd=1, lty=1,col="blue")
             })

p2 <- xyplot(DV ~ IPRED, data=a,
             xlab="Individual Prediction",
             ylab="Observation",
             panel=function(x, y, ...) {
                 panel.xyplot(x, y, pch=21, col="black", cex=0.9)
                 panel.loess(x, y, lwd=2, lty=2, col="red")
                 panel.abline(0, 1, lwd=1, lty=1,col="blue")
             })

p3 <- xyplot(abs(IWRES) ~ IPRED, data=a,
             xlab="Individual Prediction",
             ylab="|IWRES|",
             panel=function(x, y, ...) {
                 panel.xyplot(x, y, pch=21, col="black", cex=0.9)
                 panel.loess(x, y, lwd=2, lty=2, col="red")
             })

p4 <- xyplot(a$CWRES ~ a$TIME, 
             xlab="Time after dose",
             ylab="CWRES",
             panel=function(x, y, ...) {
                 panel.xyplot(x, y, pch=21, col="black", cex=0.9)
                 panel.loess(x, y, lwd=2, lty=2, col="red")
                 panel.abline(0, 0, lwd=1, lty=1,col="blue")
             })

# Save to PDF
pdf("PD_fit_IPP.pdf", width=8, height=8)
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()
