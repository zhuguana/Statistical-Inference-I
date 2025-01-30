#Paquete Distribución Pareto
install.packages("VGAM")
library("VGAM")
?Pareto



#Distribución de Pareto
scale_alpha <- 3
shape_X <- 2
datos <- rpareto(400,shape_X,scale_alpha)
muestras <- matrix(datos,10,40)

hist(muestras)
boxplot(muestras)



#Calculamos el nuevo estadístico
calcular_estadistico <- function (muestras) {
  muestra_ordenada <- sort(muestras) #ordenamos las muestras
  
  mitad_izquierda <- muestra_ordenada[1:floor(0.5*length(muestra_ordenada))] #truncamos las muestras de la mitad derecha
  
  Q1 <- quantile(mitad_izquierda, 0.25)
  Q2 <- quantile(mitad_izquierda, 0.5)
  Q3 <- quantile(mitad_izquierda, 0.75)
  
  media_cuartiles <- mean(c(Q1, Q2, Q3)) 
  #Calculamos los tres cuartiles con la mitad izquierda y hacemos una media aritmética
  print(media_cuartiles)
}

estadisticos_truncados <- apply (muestras, 1, calcular_estadistico)
print (estadisticos_truncados)

ggplot(resumen, aes(x=estadisticos_truncados))+
       geom_histogram(color="black", fill= "#8E85D7",binwidth=0.055)+
       labs(title= "HISTOGRAMA ESTADISTICOS TRUNCADOS", 
       x="VALOR DEL ESTADISTICO",y="FRECUENCIA" )



# Obtenemos media, varianza y SD de los estadísticos
media_estadisticos <- mean(estadisticos_truncados)
var_estadisticos <- var(estadisticos_truncados)
desvtip_estadisticos <- sd(estadisticos_truncados)



# Calculamos la media poblacional
media_poblacional <- (scale_alpha*shape_X)/(scale_alpha-1)
varianza_poblacional <- (scale_alpha*((shape_X)^2))/(((scale_alpha-1)^2)*(scale_alpha-2))



#Comparamos las medias muestrales con el estadístico que hemos elegido
medias_muestrales <- c()
for(i in 1:40){
  medias_muestrales[i] <- mean(muestras[,i]) 
}

ggplot(resumen, aes(x=medias_muestrales))+
       geom_histogram(color="black", fill= "#421429", binwidth=0.18)+
       labs(title= "HISTOGRAMA DE MEDIAS MUESTRALES", 
            x="VALOR DE LA MEDIA MUESTRAL",y="FRECUENCIA" )

aproximacion <- data.frame ("medias muestrales"=medias_muestrales, "media estadistico"=media_estadisticos)

variacion <- c()
for (i in 1:40) {
  variacion[i] <- abs(medias_muestrales[i]-media_estadisticos)
}

aproximacion$variacion <- variacion

resumen<- data.frame(estadisticos_truncados,
                     medias_muestrales)

ggplot(resumen, aes(x=estadisticos_truncados))+
       geom_histogram(color="black", fill= "#8E85D7",binwidth=0.055)+
       geom_histogram(aes(x=resumen$medias_muestrales, y=-..density..),
                      color="black", fill= "#421429", binwidth = 0.055)+
       labs(title= "HISTOGRAMA DE ESTADISTICOS Y MEDIAS MUESTRALES", 
            x="VALOR",y="FRECUENCIA" )

ggplot(resumen, aes(y = 0.5, x = estadisticos_truncados))+
       geom_boxplot(color="black", fill= "#8E85D7", width=0.2)+
       geom_boxplot(aes(y=1, x=resumen$medias_muestrales),
                    color="black", fill="#421429", width=0.2)+
       labs(title = "BOXPLOT DE MEDIAS MUESTRALES Y ESTADISTICO",
            y="ESTADISTICOS y MEDIAS", x= "VALOR")+
            scale_y_log10()

media_muestral <- mean(medias_muestrales)
var_muestral <- var(medias_muestrales)
desvtip_muestral <- sd(medias_muestrales)

parámetros <- data.frame (parámetro=c("media", "varianza"), 
                          poblacional=c(media_poblacional, varianza_poblacional),
                          muestral=c(media_muestral, var_muestral),
                          estadistico=c(media_estadisticos, var_estadisticos))



# Contaminamos la muestra para verificar la robustez
p <- runif(400, 0, 1)
m_contam <- c()

for (i in 1:400) {
  if (p[i]<=0.9) {
    m_contam[i] <- datos[i]
  } else {
    m_contam[i] <- rnorm (1, 1000, 0.5)
  }
}

m_contam <- matrix(m_contam, 10, 40)

calcular_estadistico_contam <- function (m_contam) {
  muestra_ordenada_contam <- sort(m_contam)
  
  mitad_izquierda <- muestra_ordenada_contam[1:floor(0.5*length(muestra_ordenada_contam))] 
  
  Q1 <- quantile(mitad_izquierda, 0.25)
  Q2 <- quantile(mitad_izquierda, 0.5)
  Q3 <- quantile(mitad_izquierda, 0.75)
  
  media_cuartiles <- mean(c(Q1, Q2, Q3)) 
  print(media_cuartiles)
}

estadisticos_truncados_contam <- apply (m_contam, 2, calcular_estadistico_contam)

media_estadisticos_contam <- mean(estadisticos_truncados_contam)
var_estadisticos_contam <- var(estadisticos_truncados_contam)
desvtip_estadisticos_contam <- sd(estadisticos_truncados_contam)

medias_muestrales_contam <- c()
for(i in 1:40){
  medias_muestrales_contam[i] <- mean(m_contam[,i]) 
}

media_muestral_contam <- mean(medias_muestrales_contam)
var_muestral_contam <- var(medias_muestrales_contam)
desvtip_muestral_contam <- sd(medias_muestrales_contam)

robustez <- data.frame (muestra=c("sin contaminar", "contaminada"),
                        estimador=c(media_estadisticos, media_estadisticos_contam),
                        DTestimador=c(desvtip_estadisticos, desvtip_estadisticos_contam),
                        media.muestral=c(media_muestral, media_muestral_contam),
                        DTmuestral=c(desvtip_muestral, desvtip_muestral_contam))



# Estimación por máxima verosimilitud y por momentos
min (muestras[,1])
10/sum(log(muestras[,1]/2))
2*mean(muestras[,1])/3
mean(muestras[,1])/(mean(muestras[,1])-2)



# Intervalos de confianza
scale_alpha <- 3
shape_X <- 2
datos <- rpareto(3000,shape_X,scale_alpha)
muestras <- matrix(datos,100,30)

intervalos_med <- data.frame (IC_90=I(vector("list", 100)), IC_95=I(vector("list", 100)))
# Creamos una tabla vacia que acepte un vector en cada celda
z_0.05 <- qnorm(0.95)
z_0.025 <- qnorm(0.975)
n <- 30

for (i in 1:100) {
  media <- mean(muestras[i,])
  S <- sd(muestras[i,]) # Por defecto, R calcula la cuasidesviación típica
  intervalos_med$IC_90[[i]] <- round(c(media-z_0.05*S/sqrt(n), media+z_0.05*S/sqrt(n)),3)
  intervalos_med$IC_95[[i]] <- round(c(media-z_0.025*S/sqrt(n), media+z_0.025*S/sqrt(n)),3)
}

intervalos_est <- data.frame (IC_90=I(vector("list", 100)), IC_95=I(vector("list", 100)))
estadisticos_truncados <- apply (muestras, 1, calcular_estadistico)
dt_muestral <- sd(estadisticos_truncados)/sqrt(n) # Calculamos la cuasivarianza de nuestros estadísticos

for(i in 1:100){
  intervalos_est$IC_90[[i]] <- round(c(estadisticos_truncados[i]-z_0.05*dt_muestral, estadisticos_truncados[i]+z_0.05*dt_muestral),3)
  intervalos_est$IC_95[[i]] <- round(c(estadisticos_truncados[i]-z_0.05*dt_muestral, estadisticos_truncados[i]+z_0.05*dt_muestral),3)
} 

# Conteo de los intervalos que contienen la esperanza real
cont <- function (x) {
  media_poblacional <- 3
  contador_si = 0
  contador_no = 0
  for (i in 1:100) {
    intervalo <- x[[i]] 
    if(media_poblacional >= intervalo[1] & media_poblacional <= intervalo[2]) {
      contador_si <- contador_si +1
    } else {
      contador_no <- contador_no+1
    }
  }
  cat (contador_si, "intervalos contienen la esperanza real", "\n")
  cat (contador_no, "intervalos no contienen la esperanza real", "\n")
}

cont (intervalos_med$IC_90)
cont (intervalos_est$IC_90)
cont (intervalos_med$IC_95)
cont (intervalos_est$IC_95)

