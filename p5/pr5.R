# PRIMERA PARTE

ICalpha<-function(ModeloA, ModeloB, alfa)
{
  n<-length(ModeloA)
  diferencias<-ModeloA-ModeloB
  mediad<-mean(diferencias)
  mediad2<-mean(diferencias^2)
  #s<-sqrt(var(xxxxxxx))
  s<-sqrt(mediad2-mediad^2)
  valort<-qt(alfa/2,n-1,lower.tail = F)
  valor<-valort * (s / sqrt(n))
  cotaInf<-mediad - valor
  cotaSup<-mediad + valor
  df<-data.frame(cotaInf, cotaSup)
  return(df)
}
df1 <- read.csv(file="datos1.csv") 

IC1<-ICalpha(df1[,2], df1[,3], 0.05)

IC1

install.packages("TeachingDemos")

library(TeachingDemos)

t.test(df1[,2],df1[,3],conf.level = 0.99)

x <- df1[,2]

y <- df1[,3]

z.test(x-y,stdev=1.58,conf.level = 0.95)

# SEGUNDA PARTE

datoslleg <- read.csv(file="datosllegada.csv") 

datossal <- read.csv(file="datossalida.csv")

#grafica
plot(datoslleg,datossal, col = c("red", "blue"))

tasaLlegadas  <- sum(datoslleg[,2])/100

tasaLlegadas

Productividad <- sum(datossal[,2])/100

Productividad


#ventanas que cumplen la ley de flujo equilibrado
datoslleg-datossal

