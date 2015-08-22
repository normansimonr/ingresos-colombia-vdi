
# IPC base dic. de 2008
ipc <- read.csv("./datos/ipc.csv")
# IPC base 2015 (julio)
ipc2015 <- ipc$Ipc.dic2008/ipc$Ipc.dic2008[which(ipc$anho==2015)]
ipc2015 <- data.frame(ipc$anho,ipc2015)
colnames(ipc2015) <- c("anho","ipc2015")
#
salmin2014 <- 644350

library(vcd)
library(MASS)
library(ineq)
library(reldist)

# Datos

hogares2014 <- read.csv("./datos/HOGARES 2014.txt",sep="\t") #Lee el archivo base.
hogares2014 <- hogares2014[,c("Ingpcug","Nper","Fex_c")] # Extrae las columnas de interés.
deflac2014 <- ipc2015$ipc2015[which(ipc2015$anho==2014)]
hogares2014$Ingpcug <- hogares2014$Ingpcug/deflac2014
w <- rep(hogares2014$Ingpcug,hogares2014$Fex_c*hogares2014$Nper)
whogares <- rep(hogares2014$Ingpcug*hogares2014$Nper, hogares2014$Fex_c)
# Gini
gini2014 <- gini(hogares2014$Ingpcug,weights=hogares2014$Fex_c*hogares2014$Nper)

#
png("./graficos/histograma1.png")
hist(w, breaks=2000, xlim=c(0,1500000),
     col="mediumpurple4", xlab="Pesos constantes de julio de 2015",
     ylab="Millones de colombianos",
     main="", yaxt="n", xaxt="n", border="white")
title(main="Distribución del ingreso en Colombia 2014", 
      sub="Fuente: Vasija de Ideas con datos del DANE")
axis(2,at=axTicks(2), las=2, labels=c("0", "1", "2", "3", "4"))
axis(1,at=axTicks(1), las=0, labels=c("0", "$500 mil", "$1 mill.", "$1,5 mill."))
dev.off()
#
# Examinando los primeros intervalos de la distribución
histo <- hist(w, breaks=2000)
head(histo$counts)
head(histo$mids)

# Número de hogares que ganan menos del salario mínimo al mes.
NROW(subset(whogares, whogares<=salmin2014))
# Cuántas personas hay en esos hogares
wsalmin <- subset(hogares2014, hogares2014$Ingpcug*hogares2014$Nper <= salmin2014)
wsalmin <- rep(wsalmin$Ingpcug,wsalmin$Fex_c*wsalmin$Nper)
NROW(wsalmin)
NROW(wsalmin)/NROW(w)