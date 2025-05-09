Cartas <- c("2","3","4","5","6","7","8","9","Figura","As")
Cantidad_de_cada_carta <- c(4,4,4,4,4,4,4,4,16,4)
Valor_cartas <- c(2,3,4,5,6,7,8,9,10,11)
Probabilidades_sacar_carta = Cantidad_de_cada_carta/sum(Cantidad_de_cada_carta)

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



 # Matriz de probabilidades del crupier
######################
######################
###################### Algortimo evaluacion mano dura ----

P_c <- numeric(10)
names(P_c) <- as.character(1:10)
Valor_cartas_mano_dura <- c(2,3,4,5,6,7,8,9,10,1)
for (i in 1:length(Cartas)) {
  valor <- Valor_cartas_mano_dura[i]
  P_c[as.character(valor)] <- P_c[as.character(valor)] + Probabilidades_sacar_carta[i]
}

Calculo_G_0 <- function(x,b,tabla,es_blackjack=F) {
  probabilidades_final_crupier <- tabla[b,]
  probabilidades_17_a_20 <- probabilidades_final_crupier[c("17","18","19","20")]
  pr_21 <- probabilidades_final_crupier$"21"
  pr_blackjack <- probabilidades_final_crupier$BlackJack
  probabilidad_T_mayor_21<- probabilidades_final_crupier$`Se pasa` # Pr(T>21)
  if (x > 21) {
    return(-1)
  }
  if (x==21 && es_blackjack) {
    return(1.5*(1-pr_blackjack))
  }
  probabilidad_T_menor_x <- sum(probabilidades_17_a_20[which(17:20<x)])
  probailidad_T_entre_x_21 <- sum(probabilidades_17_a_20[which(17:20>x)]) + ifelse(x<21,pr_21,0)
  return(probabilidad_T_mayor_21+probabilidad_T_menor_x - probailidad_T_entre_x_21 )
}

Calculo_G_0(x = 10,b = "As",tabla = resultados_tabla)
# [1] -0.4343


Algoritmo_G_estrella <- function(tabla,probabilidades_cartas){
  filas=4:21
  tabla_que_hacemos =matrix(NA, nrow = length(filas), ncol = length(Cartas),dimnames = list(as.character(filas), Cartas))
  tabla_ganancia_esperada=matrix(NA, nrow = length(filas), ncol = length(Cartas),dimnames = list(as.character(filas), Cartas))
  for (x in filas) {
    for (b in Cartas) {
      pareja <- paste(x,b,sep="_")
      mano_jugador_blackjack <- x==21 # asumo que si el jugador tiene una mano de valor 21 entonces es de un blackjack
      G0 <- Calculo_G_0(x,b,tabla,es_blackjack = mano_jugador_blackjack)
      ganancia_esperada_seguir <- 0
        for (c in 1:10) {
          nueva_x <- x + Valor_cartas_mano_dura[c] # asi voy desde 2 hasta 10, mas el as
          if (nueva_x>21) {
            ganancia_esperada_seguir <- ganancia_esperada_seguir - Probabilidades_sacar_carta[c] 
          } else {
            ganancia_esperada_seguir <- ganancia_esperada_seguir + Probabilidades_sacar_carta[c]* Calculo_G_0(nueva_x,b,tabla)
          }        
        }
        if (G0 >= ganancia_esperada_seguir) {
          tabla_ganancia_esperada[as.character(x), b] <- G0
          tabla_que_hacemos[as.character(x), b] <- "Parar"
        } else {
          tabla_ganancia_esperada[as.character(x), b] <- ganancia_esperada_seguir
          tabla_que_hacemos[as.character(x), b] <- "Continuar"
        }
      }
    }
  return(list(Ganancia=tabla_ganancia_esperada,Procedimiento=tabla_que_hacemos))
}

h$G_estrella
h$Que_Hacemos




## Algoritmo evaluacion mano blanda ----

# Definimos la función para verificar si es una mano blanda (que incluye un As)
es_mano_blanda <- function(mano) {
  return("As" %in% mano)  # Si la mano contiene un As, es blanda
}

# Función para corregir el valor del As si la suma supera 21
ajustar_ase_blanda <- function(suma, mano) {
  if (suma > 21 && es_mano_blanda(mano)) {
    return(suma - 10)  # El As se ajusta de 11 a 1
  }
  return(suma)
}

# Función para calcular la ganancia esperada para manos blandas
Calculo_G_0_blanda <- function(x, b, tabla) {
  probabilidades_final_crupier <- tabla[b,]
  pr_21_total <- probabilidades_final_crupier$"21" + probabilidades_final_crupier$BlackJack # Asumimos que sacar 21 es 21 o BlackJack
  probabilidad_T_mayor_21 <- probabilidades_final_crupier$`Se pasa` # Pr(T>21)
  
  if (x > 21) {
    return(-1)
  }
  
  probabilidad_T_menor_x <- 0 # Pr(T<x)
  probabilidad_T_entre_x_21 <- 0 # Pr(x < T <= 21)
  valores_a_evaluar <- 17:20
  nombre_columnas <- c("17", "18", "19", "20")
  
  for (i in 1:4) {
    valor <- valores_a_evaluar[i]
    probab <- probabilidades_final_crupier[[nombre_columnas[i]]]
    if (valor < x) {
      probabilidad_T_menor_x <- probabilidad_T_menor_x + probab
    } else if (valor <= 20) {
      probabilidad_T_entre_x_21 <- probabilidad_T_entre_x_21 + probab
    }
  }
  
  if (21 < x) {
    probabilidad_T_menor_x <- probabilidad_T_menor_x + pr_21_total
  } else if (21 >= x) {
    probabilidad_T_entre_x_21 <- probabilidad_T_entre_x_21 + pr_21_total
  }
  
  return(probabilidad_T_mayor_21 + probabilidad_T_menor_x - probabilidad_T_entre_x_21)
}


P_c_blanda <- numeric(10)
names(P_c_blanda) <- as.character(1:10)
Valor_cartas_mano_blanda <- c(2,3,4,5,6,7,8,9,10,11)
for (i in 1:length(Cartas)) {
  valor <- Valor_cartas_mano_blanda[i]
  P_c_blanda[as.character(valor)] <- P_c_blanda[as.character(valor)] + Probabilidades_sacar_carta[i]
}

# Función para calcular la ganancia esperada de continuar con la mano blanda
Algoritmo_G_estrella_blanda <- function(tabla,probabilidades_cartas){
  valores_b <- rownames(tabla)
  valores_x <- 4:21
  Gestrella <- list()
  que_hacemos <- list()
  filas=rev(valores_x)
  tabla_que_hacemos =matrix(NA, nrow = length(filas), ncol = length(Cartas),dimnames = list(as.character(filas), Cartas))
  tabla_ganancia_esperada=matrix(NA, nrow = length(filas), ncol = length(Cartas),dimnames = list(as.character(filas), Cartas))
  for (x in rev(valores_x)) {
    for (b in valores_b) {
      pareja <- paste(x,b,sep="_")
      ganancia_esperada_seguir <- 0
      for (c in 1:10) {
        x_nuevo <- x+c
        if (x_nuevo >21) {
          valor_siguiente <- Algoritmo_G_estrella(resultados_tabla,probabilidades_cartas = Probabilidades_sacar_carta)$Ganancia[as.character(x_nuevo-10),b]
          ganancia_esperada_seguir <- ganancia_esperada_seguir + P_c_blanda[[as.character(c)]]*valor_siguiente
        } else if (x_nuevo==21) {
          valor_siguiente <- Calculo_G_0(x_nuevo,b,tabla = resultados_tabla)
          ganancia_esperada_seguir <- ganancia_esperada_seguir + P_c_blanda[[as.character(c)]]*valor_siguiente
        } else{
          pareja_siguiente <- paste(x+c,b,sep="_")
          valor_siguiente <- Gestrella[[pareja_siguiente]]
          ganancia_esperada_seguir <- ganancia_esperada_seguir + P_c_blanda[[as.character(c)]]*valor_siguiente
        }}
      if (Calculo_G_0(x,b,resultados_tabla) >= ganancia_esperada_seguir) {
        Gestrella[[pareja]] <- Calculo_G_0(x,b,resultados_tabla)
        que_hacemos[[pareja]] <- "parar"
      } else {
        Gestrella[[pareja]] <- ganancia_esperada_seguir
        que_hacemos[[pareja]] <- "continuar"
      }
      tabla_ganancia_esperada[as.character(x), b] <- Gestrella[[pareja]]
      tabla_que_hacemos[as.character(x), b] <- que_hacemos[[pareja]]
    }
  }
  return(list(Ganancia=tabla_ganancia_esperada,Procedimiento=tabla_que_hacemos))
}

m=Al(tabla = resultados_tabla,probabilidades_cartas = Probabilidades_sacar_carta)
m$Procedimiento





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









