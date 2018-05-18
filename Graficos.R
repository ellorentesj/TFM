#install.packages("graphics")

library(graphics)

View(head(vuelos))
vueAux <- vuelos

# Relación entre los minutos de retraso en la llegada y como influyen en la salida
plot(vueAux$DepDelayMinutes~vueAux$ArrDelayMinutes, data=vueAux, xlab = "Arrival Delay Minutes",
     ylab = "Departure Delay Minutes", col = "red")
abline(lm(vueAux$DepDelayMinutes~vueAux$ArrDelayMinutes, data=vueAux))
grid()


