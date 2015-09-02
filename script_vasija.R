# Bienvenido al script de análisis de la distribución del ingreso en Colombia.
# Puede encontrar el artículo explicativo en http://vasijadeideas.blogspot.com/2015/08/cuanto-ganamos-los-colombianos.html
# Versión 01 de septiembre de 2015

# IPC base dic. de 2008
# Fuente: http://www.banrep.gov.co/es/ipc#
# (IPC total, no desagregado por clase de gasto)
#
# IPC para 2015 correspondiente al mes de julio.
ipc <- read.csv("./datos/ipc.csv", dec=",")
ipc <- subset(ipc, mes==07)
# IPC base 2015 (julio)
ipc2015 <- ipc$ipc.dic.2008/ipc$ipc.dic.2008[which(ipc$anho==2015)]
ipc2015 <- data.frame(ipc$anho,ipc2015)
colnames(ipc2015) <- c("anho","ipc2015")

# Salario mínimo
# Fuente: http://www.banrep.gov.co/es/indice-salarios
salmin <- read.csv("./datos/salariomin.csv", dec=",")
colnames(salmin) <- c("anho", "smdiar", "smmlv", "varan", "decr")
# Salario mínimo real (pesos de julio de 2015)
salreal <- salmin$smmlv/subset(ipc2015, anho>=1984)$ipc2015
salreal <- data.frame(1984:2015, salreal)
colnames(salreal) <- c("anho", "salreal")
salmin2014r <- salreal[which(salreal$anho==2014),3]

# Líneas de pobreza e indigencia
# Fuente 2002 a 2010: "Misión para el Empalme de las Series de Empleo,
# Pobreza y Desigualdad (mesep)
# Pobreza monetaria en Colombia: Nueva metodología y cifras 2002-2010
# Resultados segunda fase de la Mesep"
# Link: https://colaboracion.dnp.gov.co/CDT/Desarrollo%20Social/Documento%20Mesep%20segunda%20fase_26-03-2012_FINAL.pdf
# Página 92
# Fuente 2011 a 2014: "Pobreza monetaria y Multidimensional 2014" (Anexos)
# Link: http://www.dane.gov.co/index.php/esp/estadisticas-sociales/pobreza/160-uncategorised/6020-pobreza-monetaria-y-multidimensional-2014
# Pesos corrientes mensuales por persona
lpobreza <- read.csv("./datos/lpobreza.csv", dec=",")

# Asignar valores aproximados a las líneas de pobreza de las que no hay datos (aproximación lineal)
# LP Nacional
reg <- lm(lp.nac~anho, lpobreza)
pred <- predict(reg,lpobreza)
sindatos <- which(is.na(lpobreza$lp.nac))
lpobreza$lp.nac[sindatos] = pred[sindatos]

# LP Urbana
reg <- lm(lp.urb~anho, lpobreza)
pred <- predict(reg,lpobreza)
sindatos <- which(is.na(lpobreza$lp.urb))
lpobreza$lp.urb[sindatos] = pred[sindatos]

# LP Rural
reg <- lm(lp.rur~anho, lpobreza)
pred <- predict(reg,lpobreza)
sindatos <- which(is.na(lpobreza$lp.rur))
lpobreza$lp.rur[sindatos] = pred[sindatos]

# LI Nacional
reg <- lm(li.nac~anho, lpobreza)
pred <- predict(reg,lpobreza)
sindatos <- which(is.na(lpobreza$li.nac))
lpobreza$li.nac[sindatos] = pred[sindatos]

# LI Urbana
reg <- lm(li.urb~anho, lpobreza)
pred <- predict(reg,lpobreza)
sindatos <- which(is.na(lpobreza$li.urb))
lpobreza$li.urb[sindatos] = pred[sindatos]

# LI Rural
reg <- lm(li.rur~anho, lpobreza)
pred <- predict(reg,lpobreza)
sindatos <- which(is.na(lpobreza$li.rur))
lpobreza$li.rur[sindatos] = pred[sindatos]

rm(reg, pred, sindatos)

# Líneas de pobreza en pesos reales de julio de 2015
lpobrezar <- lpobreza/subset(ipc2015, anho>=2000)$ipc2015
lpobrezar$anho <- 2000:2015

#
# Definición de las clases sociales (en pesos de 2014)
# Muy baja: Ingresos menores a la línea de indigencia.
muybaja <- c(0, lpobreza$li.nac[which(lpobreza$anho==2014)])
# Baja: Ingresos menores a la línea de pobreza.
baja <- c(muybaja[2]+1, lpobreza$lp.nac[which(lpobreza$anho==2014)])
# Media baja: Ingresos menores a 6 veces la línea de pobreza.
mediabaja <- c(baja[2]+1, 6*lpobreza$lp.nac[which(lpobreza$anho==2014)])
# Media alta: Ingresos menores a 12 veces la línea de pobreza.
mediaalta <- c(mediabaja[2]+1, 12*lpobreza$lp.nac[which(lpobreza$anho==2014)])
# Alta: Ingresos menores a 24 veces la línea de pobreza.
alta <- c(mediaalta[2]+1, 24*lpobreza$lp.nac[which(lpobreza$anho==2014)])
# Muy alta: Ingresos mayores a 24 veces la línea de pobreza.
muyalta <- c(alta[2]+1, NA)
#
clases2014 <- data.frame(muybaja, baja, mediabaja, mediaalta, alta, muyalta)
clases2014 <- t(clases2014)
clases2014 <- data.frame(clases2014)
colnames(clases2014) <- c("min", "max")

library(vcd)
library(MASS)
library(ineq)
library(reldist)

# Ingresos de las personas según la GEIH del DANE.
# Fuente: http://formularios.dane.gov.co/Anda_4_1/index.php/catalog/334/study-description

hogares2014 <- read.csv("./datos/HOGARES 2014.txt",sep="\t") #Lee el archivo base.
hogares2014 <- hogares2014[,c("Ingpcug","Nper","Fex_c")] # Extrae las columnas de interés.
#deflac2014 <- ipc2015$ipc2015[which(ipc2015$anho==2014)]
#hogares2014$Ingpcug <- hogares2014$Ingpcug/deflac2014
w <- rep(hogares2014$Ingpcug,hogares2014$Fex_c*hogares2014$Nper)

# Personas según clase social
clases2014$personas[1] <- NROW(subset(w, w>clases2014[1,1] & w<=clases2014[1,2]))
clases2014$personas[2] <- NROW(subset(w, w>clases2014[2,1] & w<=clases2014[2,2]))
clases2014$personas[3] <- NROW(subset(w, w>clases2014[3,1] & w<=clases2014[3,2]))
clases2014$personas[4] <- NROW(subset(w, w>clases2014[4,1] & w<=clases2014[4,2]))
clases2014$personas[5] <- NROW(subset(w, w>clases2014[5,1] & w<=clases2014[5,2]))
clases2014$personas[6] <- NROW(subset(w, w>clases2014[6,1]))
gc()

# Gini según clase social
clases2014$gini[1] <- gini(subset(w, w>clases2014[1,1] & w<=clases2014[1,2]))
clases2014$gini[2] <- gini(subset(w, w>clases2014[2,1] & w<=clases2014[2,2]))
clases2014$gini[3] <- gini(subset(w, w>clases2014[3,1] & w<=clases2014[3,2]))
clases2014$gini[4] <- gini(subset(w, w>clases2014[4,1] & w<=clases2014[4,2]))
clases2014$gini[5] <- gini(subset(w, w>clases2014[5,1] & w<=clases2014[5,2]))
clases2014$gini[6] <- gini(subset(w, w>clases2014[6,1]))
gc()

# Gini total nacional
gini2014 <- gini(hogares2014$Ingpcug,weights=hogares2014$Fex_c*hogares2014$Nper)

# Gráfico de clases sociales
png("./graficos/clases2014.png", width = 560, height = 480)
barplot(height=clases2014$personas, names.arg=c("Muy baja", "Baja", "Media baja", "Media alta", "Alta", "Muy alta")
        , col="mediumpurple4", border="white"
        , yaxt="n", ylim=c(0,30000000)
        , xlab="Clases"
        , ylab="Millones de colombianos")
title(main="Distribución del ingreso en Colombia", sub="Fuente: Vasija de ideas con datos del DANE y cálculo propio.")
axis(2,at=axTicks(2), las=2, labels=c("0", "5", "10", "15", "20", "25", "30"))
text(0.7, clases2014$personas[1]+1000000, round(clases2014$personas[1]/1000000, digits=1))
text(2, clases2014$personas[2]+1000000, round(clases2014$personas[2]/1000000, digits=1))
text(3, clases2014$personas[3]+1000000, round(clases2014$personas[3]/1000000, digits=1))
text(4.4, clases2014$personas[4]+1000000, round(clases2014$personas[4]/1000000, digits=1))
text(5.5, clases2014$personas[5]+1000000, round(clases2014$personas[5]/1000000, digits=1))
text(6.7, clases2014$personas[6]+1000000, round(clases2014$personas[6]/1000000, digits=1))
dev.off()

# Gráfico de Gini por clases sociales
png("./graficos/giniclases2014.png", width = 560, height = 480)
barplot(height=clases2014$gini*100, names.arg=c("Muy baja", "Baja", "Media baja", "Media alta", "Alta", "Muy alta")
        , col="mediumpurple4", border="white"
        , xlab="Clases"
        , ylab="Índice de Gini")
title(main="Índice de Gini por clases", sub="Fuente: Vasija de ideas con datos del DANE y cálculo propio.")
dev.off()

# Gráfico de salario mínimo real
png("./graficos/salreal.png")
plot(salreal,
     type="o", ylim=c(300000,700000)
     , yaxt="n"
     , pch=20, col="mediumpurple4", lwd=3
     , xlab="Año", ylab="")
axis(2,at=axTicks(2), las=2, labels=c("300 mil", "400 mil", "500 mil", "600 mil", "700 mil"))
title(main="Salario mínimo mensual real desde 1985\n (pesos de julio de 2015)"
      , sub="Fuente: Vasija de Ideas con datos del DANE y BanRep")
dev.off()

# Inflación 2005 a 2015
inf2005.2015 <- (ipc$ipc.dic.2008[which(ipc$anho==2015)]/ipc$ipc.dic.2008[which(ipc$anho==2005)])-1
inf2005.2015 <- inf2005.2015*100

# Incremento salario mínimo 2005 a 2015
dsal2005.2015 <- (salmin$smmlv[which(salmin$anho==2015)]/salmin$smmlv[which(salmin$anho==2005)])-1
dsal2005.2015 <- dsal2005.2015*100

# Cuántas líneas de pobreza compra el salario mínimo
sallp <- subset(salreal, anho>=2000)$salreal/lpobrezar$lp.nac
sallp <- data.frame(2000:2015, sallp)


# Regalándoles dinero a los pobres (clase muy baja)
subsidio <- c(5000,10000,15000,20000)
costosubsidio <- subsidio*clases2014$personas[1]
ginisubsidio <- c()
muybaja <- subset(w, w<clases2014$max[1])
muyalta <- subset(w, w>clases2014$max[5])
resto <- subset(w, w>=clases2014$max[1] & w<=clases2014$max[5])
for (i in 1:NROW(subsidio)) {
  wmbsubs <- muybaja+subsidio[i]
  wmasubs <- muyalta-(costosubsidio[i]/clases2014$personas[6])
  ws <- c(wmbsubs, resto, wmasubs)
  rm(wmbsubs,wmasubs)
  gc()
  ginisubsidio[i] <- gini(ws)
  rm(ws)
  gc()
}
rm(w)
gc()

ginisubsidio <- data.frame(subsidio, ginisubsidio)
ginisubsidio$costo <- costosubsidio
ginisubsidio <- rbind(c(0,gini2014,0), ginisubsidio)

# Gráfico efectos del subsidio
png("./graficos/efectossubs.png")
plot(x=ginisubsidio$subsidio, y=ginisubsidio$ginisubsidio*100
     , ylim=c(53,54), yaxt="n"
     , type="h", lwd=4, col="mediumpurple4"
     , xlab="Nivel de subsidio (pesos de 2014)"
     , ylab="Índice de Gini"
     )
axis(2,at=axTicks(2), las=2, labels=c("53.0", "53.2", "53.4", "53.6", "53.8","54.0"))
title(main="Efecto del subsidio en el índice de Gini"
      ,sub="Fuente: Vasija de Ideas, cálculo propio con datos del DANE")
dev.off()

avance <- c(1:NROW(ginisubsidio))
avance <- data.frame(avance)
for (i in 1:NROW(ginisubsidio)) {
  avance$avance[i] <- (ginisubsidio$ginisubsidio[i+1]-ginisubsidio$ginisubsidio[i])*100
  }
mean(avance$avance, na.rm=T)

# Vector de hogares
whogares <- rep(hogares2014$Ingpcug*hogares2014$Nper, hogares2014$Fex_c)
# Hogares que ganan el salario mínimo o cerca del salario mínimo (+- 10%)
hog.sm <- NROW(subset(whogares, 
              whogares>salmin$smmlv[which(salmin$anho==2014)]*0.9
              &
              whogares<salmin$smmlv[which(salmin$anho==2014)]*1.1
              ))

hog.sm
rm(whogares)