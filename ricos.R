library(MASS)
ricos<-read.csv("ricos.csv")
Edad<-ricos$Age
n<-length(Edad)

truehist(Edad)
plot(main ="Edad de las 100 personas m�s ricas del mundo" # T�tulo)
truehist(Edad,density=TRUE)### histograma
### descripci�n de la muestra: ver a los datos y detectar patrones dominantes
### y comportamientos at�picos en los datos
fivenum(Edad)
### min, cuartil 1 Q1 (acumula 25%), mediana X tilde (acumula el 50$%) cuartil 3 Q3(acumula 75%), m�ximo
mean(Edad)
### promedio
boxplot(Edad)
### muestra adyancente inferior, Q1, X tilde, Q3, adyacente superior y por ende at�picos
median(Edad)

### five y median y mean nos dan idea de d?nde se olcalizan los datos
### otra medida importante de un conjunto es c?mo se dispersan
var(Edad)
sd(Edad)
mad(Edad)

#vamos a computar la funci�n de distribuci�n emp�rica

nerve_ecdf<-ecdf(Edad)
plot(nerve_ecdf, verticals=TRUE, do.points=FALSE, main="Edad de las 100 personas m�s ricas del mundo",
     xlab="Edad", ylab="N�mero de personas",
     col="black")
# para su intervalo de confianza

alpha <-0.05
en <- sqrt(log(2/alpha)/(2*n))
L_DKW <- pmax(nerve_ecdf(Edad)-en,0)
U_DKW <- pmin(nerve_ecdf(Edad)+en,1)
points(sort(Edad), L_DKW[order(Edad)], "l", col="red")
points(sort(Edad), U_DKW[order(Edad)], "l", col="red")
lines(c(.5,.5), c(0,1))

