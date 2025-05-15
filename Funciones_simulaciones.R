Cartas <- c("2","3","4","5","6","7","8","9","Figura","As")
Cantidad_de_cada_carta <- c(4,4,4,4,4,4,4,4,16,4)
Valor_cartas <- c(2,3,4,5,6,7,8,9,10,11)
Probabilidades_sacar_carta = Cantidad_de_cada_carta/sum(Cantidad_de_cada_carta)
Valor_cartas_mano_dura =c(2,3,4,5,6,7,8,9,10,1)
Valor_cartas_mano_blanda = Valor_cartas

#### Matriz de probabilidades del crupier ----
encuentra_as_en_mano <- function(mano){
  if ("As" %in% mano) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

cuenta_ases_mano <- function(mano){
  numero_ases=0
  for (carta in mano) {
    if (carta=="As") {
      numero_ases=numero_ases+1
    }
  }
  return(numero_ases)
}


Devuelve_salida <- function(suma,mano){
  if ((all(sort(mano) == sort(c("Figura", "As"))))) {
    return("BlackJack")
  } else{
    if (suma >21) {
      return("Se pasa")
    } else{
      return(as.character(suma))
    }
  } 
} 


Calcular_suma_mano_crupier <- function(mano){
  suma=0
  if ((all(sort(mano) == sort(c("Figura", "As"))))) {
    suma = 21
  } else {
    if (encuentra_as_en_mano(mano)) {
      mano_sin_ases <- mano[mano != "As"]
      mano_solo_ases <- mano[mano == "As"]
      for (carta in mano_sin_ases) {
        suma= suma + Valor_cartas[which(Cartas == carta)]
      }
      while (cuenta_ases_mano(mano_solo_ases)>0) {
        if ((suma + 11>=17) && (suma + 11 <= 21)) {
          suma = suma+11
        } else{
          suma=suma+1}
        mano_solo_ases =mano_solo_ases[-1]
      }
    } 
    else {
      for (carta in mano) {
        suma= suma + Valor_cartas[which(Cartas == carta)]
      }
    }
  }
  return(suma)
}

Mano_dada_crupier <- function(b){ #b es la carta que todos ven que tiene el crupier
  suma=0
  mano=c(b)
  while (suma<17) {
    carta_1 <- sample(Cartas,size = 1,prob = Probabilidades_sacar_carta)  
    mano = c(mano,carta_1)
    suma=Calcular_suma_mano_crupier(mano)
  }
  Devuelve_salida(suma,mano)
}

resultados_tabla <- data.frame()

# Calcular probabilidades para cada carta visible
for (carta in Cartas) {
  salida <- replicate(10000, Mano_dada_crupier(carta))
  tabla <- prop.table(table(factor(salida, 
                                   levels = c("17","18","19","20","21","BlackJack","Se pasa"))))
  resultados_tabla <- rbind(resultados_tabla, as.numeric(tabla))
  colnames(resultados_tabla)=c("17","18","19","20","21","BlackJack","Se pasa")
}
rownames(resultados_tabla)=c("2","3","4","5","6","7","8","9","Figura","As")




##### Calculo G_0----


Calculo_G_0 <- function(x, b, tabla, es_blackjack = FALSE) {
  probabilidades_final_crupier <- tabla[b, ]
  
  # Extraemos probabilidades
  pr_T_17_20 <- as.numeric(probabilidades_final_crupier[c("17", "18", "19", "20")])
  pr_T_21 <- probabilidades_final_crupier$"21"
  pr_T_bj <- probabilidades_final_crupier$"BlackJack"
  pr_T_pasa <- probabilidades_final_crupier$`Se pasa`
  
  if (x > 21) {
    return(-1)
  }
  
  if (x == 21 && es_blackjack) {
    return(1.5 * (1 - pr_T_bj))  # 0 * pr_T_bj es innecesario
  }
  
  pr_T_menor <- sum(pr_T_17_20[which(17:20 < x)])
  pr_T_igual <- sum(pr_T_17_20[which(17:20 == x)]) + ifelse(x == 21, pr_T_21, 0)
  pr_T_mayor <- sum(pr_T_17_20[which(17:20 > x)]) + ifelse(x < 21, pr_T_21, 0) + pr_T_bj
  
  ganancia <- (+1) * (pr_T_menor + pr_T_pasa) + (0) * pr_T_igual + (-1) * pr_T_mayor
  
  return(ganancia)
}

#### Estrategia y ganancia optima mano dura----

estrategia_G_optima <- matrix("", nrow = 28, ncol = 10)
rownames(estrategia_G_optima) <- as.character(4:31)
colnames(estrategia_G_optima) <- Cartas  # del 2 al As
ganancia_G_optima <- matrix(NA, nrow = 28, ncol = 10)
rownames(ganancia_G_optima) <- as.character(4:31)
colnames(ganancia_G_optima) <- Cartas  # del 2 al As
for (x in 22:31) {
  ganancia_G_optima[as.character(x),] <- -1
  estrategia_G_optima[as.character(x),] <- "Parar"
}

for (x in 21:4) {
  for (b in Cartas) {
    
    # Verifica si es blackjack (21 con 2 cartas), solo posible si x == 21
    es_blackjack <- (x == 21)  # Aquí podrías añadir verificación con número de cartas
    
    G0 <- Calculo_G_0(x, b, resultados_tabla, es_blackjack = es_blackjack)
    
    # Esperanza de continuar
    G_continuar <- 0
    for (j in 1:10) {
      nueva_x <- x + Valor_cartas_mano_dura[j]
      if (nueva_x > 21) {
        G_continuar <- G_continuar - Probabilidades_sacar_carta[j]
      } else {
        G_continuar <- G_continuar + Probabilidades_sacar_carta[j] *ganancia_G_optima[as.character(nueva_x),b] 
      }
    }
    
    if (G0 >= G_continuar) {
      estrategia_G_optima[as.character(x), b] <- "Parar"
      ganancia_G_optima[as.character(x),b] <- G0
    } else {
      estrategia_G_optima[as.character(x), b] <- "Continuar"
      ganancia_G_optima[as.character(x),b] <- G_continuar
    }
  }
}






## Algoritmo evaluacion mano blanda ----

estrategia_G_optima_mano_blanda <- matrix("", nrow = 28, ncol = 10)
rownames(estrategia_G_optima_mano_blanda) <- as.character(4:31)
colnames(estrategia_G_optima_mano_blanda) <- Cartas  # del 2 al As
ganancia_G_optima_mano_blanda <- matrix(NA, nrow = 28, ncol = 10)
rownames(ganancia_G_optima_mano_blanda) <- as.character(4:31)
colnames(ganancia_G_optima_mano_blanda) <- Cartas  # del 2 al As
for (x in 22:31) {
  ganancia_G_optima_mano_blanda[as.character(x),] <- -1
  estrategia_G_optima_mano_blanda[as.character(x),] <- "Parar"
}

for (x in 21:4) {
  for (b in Cartas) {
    
    # Verifica si es blackjack (21 con 2 cartas), solo posible si x == 21
    es_blackjack <- (x == 21)  # Aquí podrías añadir verificación con número de cartas
    
    G0 <- Calculo_G_0(x, b, resultados_tabla)
    
    # Esperanza de continuar
    G_continuar <- 0
    for (j in 1:10) {
      nueva_x <- x + Valor_cartas_mano_blanda[j]
      if (nueva_x > 21) {
        nueva_x_dura <- nueva_x-10
        G_continuar <- G_continuar - Probabilidades_sacar_carta[j]*ganancia_G_optima[as.character(nueva_x_dura),b]
      } else {
        G_continuar <- G_continuar + Probabilidades_sacar_carta[j] *ganancia_G_optima_mano_blanda[as.character(nueva_x),b] 
      }
    }
    
    if (G0 >= G_continuar) {
      estrategia_G_optima_mano_blanda[as.character(x), b] <- "Parar"
      ganancia_G_optima_mano_blanda[as.character(x),b] <- G0
    } else {
      estrategia_G_optima_mano_blanda[as.character(x), b] <- "Continuar"
      ganancia_G_optima_mano_blanda[as.character(x),b] <- G_continuar
    }
  }
}

#### Estrategia doblarse o no doblarse ----
estrategia_doblarse_NoDoblarse <- matrix("", nrow = 3, ncol = 10)
rownames(estrategia_doblarse_NoDoblarse) <- as.character(9:11)
colnames(estrategia_doblarse_NoDoblarse) <- Cartas  # del 2 al As
ganancia_doblarse_NoDoblarse <- matrix(NA, nrow = 3, ncol = 10)
rownames(ganancia_doblarse_NoDoblarse) <- as.character(9:11)
colnames(ganancia_doblarse_NoDoblarse) <- Cartas  # del 2 al As

for (x in 9:11) {
  for (b in Cartas) {
    G_estrella=ganancia_G_optima[as.character(x),b]
    # Verifica si es blackjack (21 con 2 cartas), solo posible si x == 21
    es_blackjack <- (x == 21)  # Aquí podrías añadir verificación con número de cartas
    # Esperanza de continuar
    G_continuar <- 0
    for (j in 1:10) {
      nueva_x <- x + Valor_cartas_mano_blanda[j]
      G_continuar <- G_continuar + Probabilidades_sacar_carta[j] * Calculo_G_0(nueva_x,b,resultados_tabla,es_blackjack = es_blackjack) 
    }
    if (2*G_continuar > G_estrella) {
      estrategia_doblarse_NoDoblarse[as.character(x), b] <- "Doblarse"
      ganancia_doblarse_NoDoblarse [as.character(x),b] <- 2*G_continuar
    } else {
      estrategia_doblarse_NoDoblarse[as.character(x), b] <- "No doblarse"
      ganancia_doblarse_NoDoblarse [as.character(x),b] <- G_estrella
    }
  }
}

#### Estrategia abrirse o no abrirse----

estrategia_abrirse_NoAbrirse <- matrix("", nrow = 10, ncol = 10)
rownames(estrategia_abrirse_NoAbrirse) <- as.character(2:11)
colnames(estrategia_abrirse_NoAbrirse) <- Cartas  # del 2 al As
ganancia_abrirse_NoAbrirse <- matrix(NA, nrow = 10, ncol = 10)
rownames(ganancia_abrirse_NoAbrirse) <- as.character(2:11)
colnames(ganancia_abrirse_NoAbrirse) <- Cartas  # del 2 al As

for (z in 2:10) {
  for (b in Cartas) {
    G_estrella=ganancia_G_optima[as.character(2*z),b]
    # Esperanza de continuar
    G_continuar <- 0
    for (j in 1:10) {
      nueva_z <- z + Valor_cartas_mano_blanda[j]
      G_continuar <- G_continuar + Probabilidades_sacar_carta[j] * ganancia_G_optima[as.character(nueva_z),b] 
    }
    if (2*G_continuar > G_estrella) {
      estrategia_abrirse_NoAbrirse[as.character(z), b] <- "Abrirse"
      ganancia_abrirse_NoAbrirse[as.character(z),b] <- 2*G_continuar
    } else {
      estrategia_abrirse_NoAbrirse[as.character(z), b] <- "No abrirse"
      ganancia_abrirse_NoAbrirse[as.character(z),b] <- G_estrella
    }
  }
}

for (b in Cartas) {
  G_estrella=ganancia_G_optima_mano_blanda[as.character(12),b]
  # Esperanza de continuar
  G_continuar <- 0
  for (j in 1:10) {
    nueva_z <- z + Valor_cartas_mano_blanda[j]
    G_continuar <- G_continuar + Probabilidades_sacar_carta[j] * Calculo_G_0(nueva_z,b,tabla = resultados_tabla)
  }
  if (2*G_continuar > G_estrella) {
    estrategia_abrirse_NoAbrirse[as.character(11), b] <- "Abrirse"
    ganancia_abrirse_NoAbrirse[as.character(11),b] <- 2*G_continuar
  } else {
    estrategia_abrirse_NoAbrirse[as.character(11), b] <- "No abrirse"
    ganancia_abrirse_NoAbrirse[as.character(11),b] <- G_estrella
  }
}



##### Calculo de la esperanza del juego ----

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
jugar_dada_mano <- function(carta1,carta2,carta_crupier) {
  # Mano inicial del jugador
  mano_jugador <- c(carta1,carta2)
  
  # Mano del dealer
  mano_dealer <- c(carta_crupier)
  
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









