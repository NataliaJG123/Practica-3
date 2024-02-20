nuevo_dir = "C:/Practica 3"
setwd(nuevo_dir)

#Ejercicio 1

spear <- read_excel("C:/spearheads.xlsx")
View(spear)
spear = as.data.frame(spear)
class(spear)

#Ejercicio 2

names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Cond"] <- "Conservación"
names(spear)[names(spear) == "Loo"] <- "Loop"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Date"] <- "Fecha"
names(spear)[names(spear) == "Maxle"] <- "Longitud_encaje"

View(spear)
str(spear)

#Ejercicio 3

spear$Contexto = factor(spear$Contexto, levels=c('1', '2', '3'), labels=c("S/C", "Habitacional", "Funerario"))

spear$Conservación = factor(spear$Conservación, levels = c('1', '2', '3', '4'), labels = c("Excelente", "Bueno", "Regular", "Malo"))

spear$Remache = factor(spear$Remache, levels = c('1', '2'), labels = c("Si", "No"))

spear$Materiales = factor(spear$Materiales, levels = c('1', '2'), labels = c("Bronce", "Hierro"))

#Ejercicio 4

tabla_materiales <- table(spear$Materiales)
View(tabla_materiales)

tabla_contextos <- table(spear$Contexto)
View(tabla_contextos)

tabla_conservacion <- table(spear$Conservación)
View(tabla_conservacion)

#Ejercicio 5

tabla_materiales_contexto = xtabs(~Materiales + Contexto, data = spear)
tabla_materiales_conservación = xtabs(~Materiales + Conservación, data = spear)

#Ejercicio 6 


tabla_porcentaje_materiales = prop.table(tabla_materiales) * 100
print(tabla_porcentaje_materiales)

tabla_porcentaje_contextos = prop.table(tabla_contextos) * 100
print(tabla_porcentaje_contextos)

tabla_porcentaje_conservación = prop.table(tabla_conservacion)* 100
print(tabla_porcentaje_conservación)
View(tabla_conservacion)

#Ejercicio 7 


tabla_cruzada_porcentaje = prop.table(tabla_materiales_contexto, margin = 1) *100
tabla_cruzada_porcentaje_conservacion = prop.table(tabla_materiales_conservación, margin = 1) *100
View(tabla_cruzada_porcentaje)

#Ejercicio 8 


grafico_barras = barplot(tabla_materiales_conservación, 
                         main = "Frecuencia Conservación", 
                         xlab = "Conservación", 
                         ylab = "Frecuencia",
                         col = "pink")

grafico_barras = barplot(tabla_materiales_contexto, 
                         main = "Frecuencia Conservación", 
                         xlab = "Contexto", 
                         ylab = "Frecuencia",
                         col = "blue")
#Ejercicio 9 


grafico_barras = barplot(tabla_materiales,
                         horiz = TRUE,
                         main = "Frecuencia Conservación", 
                         xlab = "Contexto", 
                         ylab = "Frecuencia",
                         col = "yellow")

grafico_barras_remache = barplot(tabla_remache,
                         horiz = TRUE,
                         main = "Frecuencia Conservación", 
                         xlab = "Remache", 
                         ylab = "Frecuencia",
                         col = "green")

tabla_remache = table(spear$Remache)

#Ejercicio 10 

grafico_agrupados = barplot(tabla_materiales_conservación,
                            beside = TRUE,
                            main = " Grafico de barras",
                            xlab = " Grado de conservación",
                            ylab = "Frecuencia",
                            col = c("pink", "green"),
                            legend=TRUE)
    
#Ejercicio 11

Grafico = pie(tabla_conservacion,
              main = "Grado de conservación",
              col = c("yellow", "purple", "green", "blue"),
              labels = paste(names(tabla_conservacion), "(",(tabla_porcentaje_conservación), "%)"))
                
            View(Grafico)
#Ejercicio 12 
            
            var_continuas = spear[sapply(spear, is.numeric)]
            
            windows(width = 10, height = 10)
            
            histograma_probabilidad = hist(unlist(var_continuas), 
                                           main = "Histograma Probabilidad de Variables Continuas",
                                           xlab = "Valor", 
                                           prob = TRUE)
       
  
       