# Carichiamo i pacchetti necessari

library(readr)
library(knitr)
library(ggplot2)
library(ggpubr)
library(lmtest)
library(MASS)
library(car)



# Importazione del dataset

data <- read_csv("neonati.csv")

# Verifichiamo la struttura del dataset

str(data)

# Verifichiamo le prime righe del dataset

knitr::kable(head(data))

# Calcoliamo alcune statistiche descrittive

# Distribuzione di frequenze per le variabili categoriali (tipo di parto, ospedale, sesso)
table(data$Tipo.parto)
table(data$Ospedale)
table(data$Sesso)
table(data$Fumatrici)


# Statistiche descrittive per le variabili continue
summary(data[c("Anni.madre", "N.gravidanze", "Gestazione",
                   "Peso", "Lunghezza", "Cranio")])

plot(density(data$Peso))

# Test t per la media del peso

t.test(data$Peso, mu=3300)

# Test t per la media della lunghezza

t.test(data$Lunghezza, mu=500)

# <https://www.ospedalebambinogesu.it/da-0-a-30-giorni-come-si-presenta-e-come-cresce-80012/#:~:text=In%20media%20il%20peso%20nascita,pari%20mediamente%20a%2050%20centimetri.>

# Test t per verificare differenze significative nel peso tra maschi e femmine

t.test(Peso ~ Sesso, data = data)

# Test t per verificare differenze significative nella lunghezza tra maschi e femmine

t.test(Lunghezza ~ Sesso, data = data)

# Tabella di contingenza tra tipo di parto e ospedale

tabella_parti <- table(data$Tipo.parto, data$Ospedale)


ggballoonplot(as.data.frame(tabella_parti))

# Test chi-quadrato per verificare differenze significative

chisq.test(tabella_parti)

# Matrice di correlazione per le variabili numeriche

round(cor(data[c("Anni.madre", "N.gravidanze", "Gestazione",
           "Peso", "Lunghezza", "Cranio")]), digits = 2)

# Visualizziamo alcune distribuzioni graficamente
par(mfrow=c(2, 2)) 

boxplot(data$Peso ~ data$Sesso, main="Peso dei Neonati per Sesso", xlab='Sesso', ylab="Peso (grammi)")
boxplot(data$Peso ~ data$Ospedale, main="Peso dei Neonati per Ospedale",xlab = 'Ospedale', ylab="Peso (grammi)")
boxplot(data$Peso ~ data$Fumatrici, main="Peso dei Neonati per Fumatrici", xlab='Madre fumatrice', ylab="Peso (grammi)")
boxplot(data$Peso ~ data$Tipo.parto, main="Peso dei Neonati per Tipo parto", xlab = 'Tipo di parto', ylab="Peso (grammi)")

summary(aov(data$Peso ~ data$Ospedale))
t.test(Peso ~ Fumatrici, data = data)
t.test(Peso ~ Tipo.parto, data = data)




# Scatterplot per visualizzare le relazioni con la variabile risposta (peso)

ggplot(data, aes(x=Lunghezza, y=Peso)) + geom_point() +
  labs(title="Relazione tra lunghezza e peso del neonato", x="Lunghezza
(mm)", y="Peso (grammi)")

ggplot(data, aes(x=Cranio, y=Peso)) + geom_point() +
  labs(title="Relazione tra diametro del cranio e peso del neonato",
       x="Diametro del cranio (mm)", y="Peso (grammi)")

ggplot(data, aes(x=Gestazione, y=Peso)) + geom_point() +
  labs(title="Relazione tra settimane di gestazione e peso del neonato",
       x="Settimane di gestazione", y="Peso (grammi)")


# Modello di regressione lineare multipla


modello_intero = lm(Peso ~ Gestazione + Lunghezza + Cranio + Sesso + N.gravidanze + Tipo.parto + Fumatrici + Anni.madre,
                    data=data)

summary(modello_intero)

# Selezione del modello tramite stepwise (AIC)

modello_step <- stepAIC(modello_intero, direction = "both")

# Sommario del modello selezionato

summary(modello_step)


# Diagnostica del modello: grafici dei residui

par(mfrow=c(2, 2)) 

plot(modello_step)

vif(modello_step)

# Verifica dei valori influenti con la statistica Cook's distance
cooksd <- cooks.distance(modello_step)
plot(cooksd, main="Cook's Distance")
abline(h = 4/(nrow(data)-length(coef(modello_step))-1), col="red")

# Identificazione delle osservazioni con distanza di Cook superiore alla soglia
influential <- as.numeric(names(cooksd)[(cooksd > 4/(nrow(data)-length(coef(modello_step))-1))])
influential  # Stampa le osservazioni influenti

# Test sui residui
bptest(modello_step)
dwtest(modello_step)
shapiro.test(residuals(modello_step))



# Predizione sui dati esistenti

predizioni <- predict(modello_step, newdata = data)

# Grafico delle previsioni vs osservati

ggplot(data, aes(x=Peso, y=predizioni)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red") + labs(title="Peso
osservato vs predetto", x="Peso osservato (grammi)", y="Peso predetto
(grammi)")

# Creiamo un nuovo dataframe con le caratteristiche specifiche

nuovo_neonato <- data.frame(N.gravidanze = 3, Gestazione = 39, Sesso =
                              "F")

# Creiamo un nuovo modello senza includere lunghezza e diametro del cranio

modello_ridotto <- lm(Peso ~ N.gravidanze + Gestazione + Sesso,
                      data=data)

summary(modello_ridotto)

# Predizione con il modello ridotto

predizione_ridotta <- predict(modello_ridotto, newdata = nuovo_neonato)
predizione_ridotta
