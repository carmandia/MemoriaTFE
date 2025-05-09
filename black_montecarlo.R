# Simulación del método de Monte Carlo para Blackjack
set.seed(123)  # Para reproducibilidad

# Función para generar una carta aleatoria
generar_carta <- function() {
  valores <- c(2:10, 10, 10, 10, 11)  # Valores de las cartas (J, Q, K valen 10; As vale 11)
  sample(valores, 1)
}

# Función para jugar una mano de Blackjack
jugar_blackjack <- function() {
  # Mano del jugador
  mano_jugador <- c(generar_carta(), generar_carta())
  
  # Mano del dealer
  mano_dealer <- c(generar_carta(), generar_carta())
  
  # Sumar valores de la mano
  suma_mano <- function(mano) {
    total <- sum(mano)
    # Si la suma excede 21 y hay un As, convertir As de 11 a 1
    while (total > 21 && 11 %in% mano) {
      mano[mano == 11] <- 1
      total <- sum(mano)
    }
    total
  }
  
  # Juego del jugador: el jugador se planta en 17 o más
  while (suma_mano(mano_jugador) < 17) {
    mano_jugador <- c(mano_jugador, generar_carta())
  }
  
  # Si el jugador se pasa de 21, pierde
  if (suma_mano(mano_jugador) > 21) {
    return("dealer")  # Dealer gana
  }
  
  # Juego del dealer: el dealer se planta en 17 o más
  while (suma_mano(mano_dealer) < 17) {
    mano_dealer <- c(mano_dealer, generar_carta())
  }
  
  # Si el dealer se pasa de 21, pierde
  if (suma_mano(mano_dealer) > 21) {
    return("jugador")  # Jugador gana
  }
  
  # Determinar el ganador comparando las manos
  if (suma_mano(mano_jugador) > suma_mano(mano_dealer)) {
    return("jugador")  # Jugador gana
  } else if (suma_mano(mano_jugador) < suma_mano(mano_dealer)) {
    return("dealer")  # Dealer gana
  } else {
    return("empate")  # Empate
  }
}

# Simulación Monte Carlo
simular_blackjack <- function(n_simulaciones) {
  resultados <- replicate(n_simulaciones, jugar_blackjack())
  # Contar los resultados
  tabla_resultados <- table(resultados)
  return(prop.table(tabla_resultados))  # Proporciones de victorias, derrotas y empates
}

# Ejecutar la simulación con 100,000 juegos
resultados <- simular_blackjack(100000)
print(resultados)
###############Dada una mano#####################
# Simulación para estimar la probabilidad de ganar dada una mano inicial

set.seed(123)  # Para reproducibilidad

# Función para generar una carta aleatoria
generar_carta <- function() {
  valores <- c(2:10, 10, 10, 10, 11)  # Valores de las cartas (J, Q, K = 10; As = 11)
  sample(valores, 1)
}

# Función para calcular la suma de una mano
suma_mano <- function(mano) {
  total <- sum(mano)
  # Ajustar el valor del As si el total excede 21
  while (total > 21 && 11 %in% mano) {
    mano[mano == 11] <- 1
    total <- sum(mano)
  }
  total
}

# Función para jugar una mano de Blackjack desde un estado inicial
jugar_dada_mano <- function(mano_jugador_inicial) {
  # Mano inicial del jugador
  mano_jugador <- mano_jugador_inicial
  
  # Mano del dealer
  mano_dealer <- c(generar_carta(), generar_carta())
  
  # Juego del jugador: se planta en 17 o más
  while (suma_mano(mano_jugador) < 17) {
    mano_jugador <- c(mano_jugador, generar_carta())
  }
  
  # Si el jugador se pasa de 21, pierde
  if (suma_mano(mano_jugador) > 21) {
    return("dealer")  # Dealer gana
  }
  
  # Juego del dealer: se planta en 17 o más
  while (suma_mano(mano_dealer) < 17) {
    mano_dealer <- c(mano_dealer, generar_carta())
  }
  
  # Si el dealer se pasa de 21, el jugador gana
  if (suma_mano(mano_dealer) > 21) {
    return("jugador")  # Jugador gana
  }
  
  # Comparar las manos
  if (suma_mano(mano_jugador) > suma_mano(mano_dealer)) {
    return("jugador")  # Jugador gana
  } else if (suma_mano(mano_jugador) < suma_mano(mano_dealer)) {
    return("dealer")  # Dealer gana
  } else {
    return("empate")  # Empate
  }
}

# Simulación Monte Carlo para una mano inicial
simular_probabilidad_ganar <- function(mano_jugador_inicial, n_simulaciones) {
  resultados <- replicate(n_simulaciones, jugar_dada_mano(mano_jugador_inicial))
  # Contar los resultados
  tabla_resultados <- table(resultados)
  return(prop.table(tabla_resultados))  # Proporciones de victorias, derrotas y empates
}

# Ejemplo: Simular con una mano inicial de 10 y 7
mano_inicial <- c(10, 6)
n_simulaciones <- 100000
resultados <- simular_probabilidad_ganar(mano_inicial, n_simulaciones)

# Imprimir resultados
cat("Resultados para la mano inicial", paste(mano_inicial, collapse = ", "), ":\n")
print(resultados)
