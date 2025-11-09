#1. Lectura del fichero -----------------------------------------------------
## Understat. City - Chelsea

shots_df <- read.csv(
  file = 'Understat_City_Chelsea.csv', 
  sep=";", header = TRUE, encoding = 'UTF-8')
shots_df[c(1:10),]

#2. Descripción y resumen estadístico del fichero ---------------------------
colnames(shots_df) # columnas
cat("Número de disparos entre ambos equipos:", ncol(shots_df))
str(shots_df)
# Conocemos algunas conclusiones: xG vs Goles
cat("xG total del Manchester City:", 
    sum(shots_df[shots_df$team == "Manchester City", "xG"]))
cat("xG total del Chelsea:", 
    sum(shots_df[shots_df$team != "Chelsea", "xG"]))
cat("Número de goles:", 
    nrow(shots_df[shots_df$result == 'Goal',]))

# 3. Calculo de resultado mas frecuente del partido analizado -------------
##a. Proceso de simulación de partidos.
N <- 1000 # numero de simulaciones
# Para cada simulación...
results_df <- data.frame()
for (j in 1:N){
  set.seed(j) # Semilla aleatoriedad
  result <- c(NA, NA) # Resultado en la simulaci?n
  k <- 1
  # Para cada equipo...
  for (t in unique(shots_df$team)){
    shots_team <- shots_df[shots_df$team == t, ]
    goals_t <- 0 # n? goles del equipo (inicializamos en 0)
    # Simulamos cada disparo
    for (i in 1:nrow(shots_team)){
      goals_t <- goals_t + rbinom(n=1, size=1, prob=shots_team$xG[i])
    } # Sumamos los ?xitos = n? goles del equipo en la simulaci?n
    result[k] <- goals_t
    k <- k+1
  }
  # Concatenamos resultados
  results_df <- rbind(results_df, result)
  # Renombramos las columnas
  colnames(results_df) <- unique(shots_df$team)
}

head(results_df) # Primeros registros

##b. Gráficas que ayuden a entender el resultado.
### Vamos a crear unas variables adicionales para poder generar las graficas
#### Calculamos el resultado de cada partido
results_df$result <- paste(as.character(results_df$`Manchester City`), 
                           "-", as.character(results_df$Chelsea))
#### Victoria local, visitante o empate
results_df$win <- ifelse(
  results_df$`Manchester City` > results_df$Chelsea, "Manchester City", 
  ifelse(results_df$`Manchester City` < results_df$Chelsea, "Chelsea", 
         "Empate"))
# 5. Cual hubiera sido el resultado mas frecuente?
results_table <- sort(table(results_df$result), 
                      decreasing=TRUE)
results_table # nro de veces que se repite cada resultado
pos_real_result <- which(names(results_table) == "1 - 2")
colors <- rep("gray", length(results_table))
colors[pos_real_result] <- "firebrick"
barplot(results_table, col = colors,
        main = "Resultado mas frecuente tras 1000 simulaciones, 
        entre Manchester City 1 - Chelsea 2", 
        ylab = "Partidos", xlab = "Resultado", las=2)
#6. Calculo de los xPoints --------------------------------------------------
## Ahora para poder calcular los xpoints necesitamos obtener 
## el Porcentaje de cada posible resultado (victoria local, visitante o empate)

win_proportions <- prop.table(table(results_df$win))
win_proportions # porcentaje de cada posible resultado
colors <- c("royalblue", "firebrick", "forestgreen")
teams <- names(win_proportions)
labels <- paste(round(100*win_proportions, 2), "%")
pie(win_proportions, col = colors, labels = labels,
    main = "Proporción de victoria despues 1000 partidos simulados")
legend("right", teams,
       fill = colors, pt.cex = 1, bty = 'n', cex = 0.9)

# 7. Calculo de xPoints
MCI_xPTS <- as.character(win_proportions["Manchester City"]*3 + 
                           win_proportions["Empate"])
CHE_xPTS <- as.character(win_proportions["Chelsea"]*3 + 
                           win_proportions["Empate"])
cat(paste("xPoints. Manchester City", MCI_xPTS, "-", 
          CHE_xPTS, "Chelsea"))


