# PREGUNTA 1

# modelo de Bernoulli --> se realiza un experimento con dos posibles 
# resultados, es decir, preguntamos a una persona si tiene mínimo dos 
# televisores

# variable aleatoria --> tener mínimo dos teles
# resultados --> sí = 1, no = 0
# P(X=x) --> sí = 0.32, no = 0.68

x <- c(0,1)
f <- c(0.68, 0.32)

plot(x, f, type='h', ylim=c(0,1), col='red')

# en este caso, no es modelo de Bernoulli, ya que el experimento se repite 
# 43 veces, porque se pregunta a 43 personas
# ahora la variable aleatoria es cuántas personas tienen 2 teles

# n = las veces que se repite el experimento, n = 43
# resultados --> 0, 1, 2,..., 43 porque se pregunta a 43 personas
# P(X=x) = (4 x)+p^(13)*(1-p)^(43-13) donde 13 son las personas sobre las cuales
# queremos saber la probabilidad de que esas 13 tengan más de 2 teles en una
# encuesta de 43 personas


n <- 43
sample(x,n,f,replace=TRUE) 
# el sample me genera cada vez diferentes resultados de 43 personas, al azar

sum(sample(x,n,f,replace=TRUE)) # me dice cuántas personas me han 
# respondido que sí tienen más de dos televisores en casa

Y <- function(i){sum(sample(x,n,f,replace=TRUE))} 
Y # cada vez que ejecuto 
# Y me da un resultado diferente porque hace un sample diferente, es la función
# de P(X=x)

encuestas <- sapply(1:40000, Y) # se realizan 40000 encuestas a 43 personas  
# cada una, y este comando me dice cuántas personas me han respondido que 
# sí a cada encuesta
fr <- table(encuestas)/40000
barplot(fr)
fr["13"]

# DE FORMA RÁPIDA

dbinom(13,43,0.32) # resultado sobre el que queremos saber P(X=x), número de veces
# que se repite el experimento, probabilidad de que una respuesta sea positiva

y <- 0:43
plot(y,dbinom(y,43,0.32), type='h',col='red')


# PREGUNTA 2
pbinom(16,44,0.32) # pbinom función de distribución
# dbinom función de masa de probabilidad

qbinom(0.5,44,0.32) # mediana de la variable aleatoria, segundo quartil


# PREGUNTA 3, nos preguntan como máximo un televisor, ejercicio diferente

# ahora, el modelo de Bernoulli sería:
# X = tienes como máximo un televisor?
# resultados --> sí = 1, no = 0
# P(X=x) --> sí = 0.68, no = 0.32

# pasamos a binomial:
# X = número de hogares que han respuesto que sí
# E(X) = n*p, siendo p la probabilidad de su correspondiente Bernoulli
# Var(X) = n*p*(1-p)
# quantil quart = primer quart, qbinom(0.25, 24, 0.68)

# PREGUNTA 4, me pide que lo haga de la forma lenta, pero realmente 
#  te pide lo siguiente

n <- 46
x <- c(0,1)
f <- c(0.68, 0.32)

# E(X) = n*p




