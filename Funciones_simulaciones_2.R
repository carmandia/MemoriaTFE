library(knitr)
library(kableExtra)
library(tidyverse)

set.seed(47563)
Cartas <- c("2","3","4","5","6","7","8","9","Figura","As")
Cantidad_de_cada_carta <- c(4,4,4,4,4,4,4,4,16,4)
Valor_cartas <- c(2,3,4,5,6,7,8,9,10,11)
Probabilidades_sacar_carta = Cantidad_de_cada_carta/sum(Cantidad_de_cada_carta)
Valor_cartas_mano_dura =c(2,3,4,5,6,7,8,9,10,1)
Valor_cartas_mano_blanda = Valor_cartas

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
  if (length(mano) == 2 && all(c("As", "Figura") %in% mano)) {
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
  if (length(mano) == 2 && all(c("As", "Figura") %in% mano)) {
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
kable(round(resultados_tabla, 6), 
      caption = "Probabilidades de resultado final del crupier según carta visible",
      align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, font_size = 12)

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


estrategia_G_optima <- matrix("", nrow = 28, ncol = 10)
rownames(estrategia_G_optima) <- as.character(4:31)
colnames(estrategia_G_optima) <- Cartas  # del 2 al As
ganancia_G_optima <- matrix(NA, nrow = 28, ncol = 10)
rownames(ganancia_G_optima) <- as.character(4:31)
colnames(ganancia_G_optima) <- Cartas  # del 2 al As
for (x in 22:31) {
  ganancia_G_optima[as.character(x),] <- -1
  estrategia_G_optima[as.character(x),] <- "P"
}

for (x in 21:4) {
  for (b in Cartas) {
    
    # Verifica si es blackjack (21 con 2 cartas), solo posible si x == 21
    es_blackjack <- (x == 21)  # Aquí podrías añadir verificación con número de cartas
    
    G0 <- Calculo_G_0(x, b, resultados_tabla,es_blackjack = es_blackjack)
    
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
      estrategia_G_optima[as.character(x), b] <- "P"
      ganancia_G_optima[as.character(x),b] <- G0
    } else {
      estrategia_G_optima[as.character(x), b] <- "C"
      ganancia_G_optima[as.character(x),b] <- G_continuar
    }
  }
}

kable(estrategia_G_optima, 
      caption = "Tabla de procedimientos si el jugador posee una mano dura",
      align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, font_size = 12)


estrategia_G_optima_mano_blanda <- matrix("", nrow = 28, ncol = 10)
rownames(estrategia_G_optima_mano_blanda) <- as.character(4:31)
colnames(estrategia_G_optima_mano_blanda) <- Cartas  # del 2 al As
ganancia_G_optima_mano_blanda <- matrix(NA, nrow = 28, ncol = 10)
rownames(ganancia_G_optima_mano_blanda) <- as.character(4:31)
colnames(ganancia_G_optima_mano_blanda) <- Cartas  # del 2 al As
for (x in 22:31) {
  ganancia_G_optima_mano_blanda[as.character(x),] <- -1
  estrategia_G_optima_mano_blanda[as.character(x),] <- "P"
}

for (x in 21:4) {
  for (b in Cartas) {
    
    # Verifica si es blackjack (21 con 2 cartas), solo posible si x == 21
    es_blackjack <- (x == 21)  # Aquí podrías añadir verificación con número de cartas
    
    G0 <- Calculo_G_0(x, b, resultados_tabla,es_blackjack = es_blackjack)
    
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
      estrategia_G_optima_mano_blanda[as.character(x), b] <- "P"
      ganancia_G_optima_mano_blanda[as.character(x),b] <- G0
    } else {
      estrategia_G_optima_mano_blanda[as.character(x), b] <- "C"
      ganancia_G_optima_mano_blanda[as.character(x),b] <- G_continuar
    }
  }
}

kable(estrategia_G_optima_mano_blanda, 
      caption = "Tabla de procedimientos si el jugador posee una mano blanda",
      align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, font_size = 12)

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
      estrategia_doblarse_NoDoblarse[as.character(x), b] <- "D"
      ganancia_doblarse_NoDoblarse [as.character(x),b] <- 2*G_continuar
    } else {
      estrategia_doblarse_NoDoblarse[as.character(x), b] <- "No D"
      ganancia_doblarse_NoDoblarse [as.character(x),b] <- G_estrella
    }
  }
}

kable(estrategia_doblarse_NoDoblarse, 
      caption = "Tabla de procedimientos para decidir si doblarse o no",
      align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, font_size = 12)

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
      estrategia_abrirse_NoAbrirse[as.character(z), b] <- "A"
      ganancia_abrirse_NoAbrirse[as.character(z),b] <- 2*G_continuar
    } else {
      estrategia_abrirse_NoAbrirse[as.character(z), b] <- "No A"
      ganancia_abrirse_NoAbrirse[as.character(z),b] <- G_estrella
    }
  }
}

for (b in Cartas) {
  G_estrella=ganancia_G_optima_mano_blanda[as.character(12),b]
  # Esperanza de continuar
  G_continuar <- 0
  for (j in 1:10) {
    nueva_z <- 11 + Valor_cartas_mano_blanda[j]
    G_continuar <- G_continuar + Probabilidades_sacar_carta[j] * Calculo_G_0(nueva_z,b,tabla = resultados_tabla)
  }
  if (2*G_continuar > G_estrella) {
    estrategia_abrirse_NoAbrirse[as.character(11), b] <- "A"
    ganancia_abrirse_NoAbrirse[as.character(11),b] <- 2*G_continuar
  } else {
    estrategia_abrirse_NoAbrirse[as.character(11), b] <- "No A"
    ganancia_abrirse_NoAbrirse[as.character(11),b] <- G_estrella
  }
}
rownames(estrategia_abrirse_NoAbrirse) <- paste0(Cartas,"-",Cartas)
rownames(ganancia_abrirse_NoAbrirse) <- paste0(Cartas,"-",Cartas)

kable(estrategia_abrirse_NoAbrirse, 
      caption = "Tabla de procedimientos para decidir si abrirse o no",
      align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F, font_size = 12)



valor_de_carta <- function(carta) {
  if (carta == "Figura") {
    return(10)
  } else if (carta == "As") {
    return(11)
  } else {
    return(as.numeric(carta))
  }
}


tipo_de_mano <- function(mano,posibilidad_abrirse = T) {
  if (length(mano) == 2 && mano[1] == mano[2]) {
    return("Opcion abrirse")
  }
  valores <- sapply(mano, valor_de_carta)
  suma <- sum(valores)
  ases <- sum(mano=="As")
  while (suma>21 && ases >0) {
    suma <- suma-10
    ases = ases -1
  }
  if("As" %in% mano && suma <= 21 && any(sapply(mano,valor_de_carta)==11)){
    return("Blanda")
  } else{
    return("Dura")
  }
}

estrategia_juego <- function(mano,carta_crupier,posibilidad_abrirse=T){
  clase_de_mano <- tipo_de_mano(mano,posibilidad_abrirse)
  suma_de_mano <- valor_mano(mano)
  if(clase_de_mano == "Blanda" && suma_de_mano >21){
    suma_de_mano = suma_de_mano-10
  }
  # ya tenemos suma de la mano que tenemos y que tipo es
  
  if (clase_de_mano == "Opcion abrirse") {
    carta_repetida <- mano[1]
    valor_repetido <- valor_de_carta(carta_repetida)
    if (valor_repetido == 11) {
      return(estrategia_abrirse_NoAbrirse["As-As",carta_crupier])
    } else if (valor_repetido == 10) {
      return(estrategia_abrirse_NoAbrirse["Figura-Figura",carta_crupier])
    } else{
      return(estrategia_abrirse_NoAbrirse[paste0(valor_repetido,"-",valor_repetido),carta_crupier])
    }
  } else if(clase_de_mano == "Blanda") {
    return(estrategia_G_optima_mano_blanda[as.character(suma_de_mano),carta_crupier])
  } else if(clase_de_mano == "Dura") {
    if (suma_de_mano %in% 9:11) {
      return(estrategia_doblarse_NoDoblarse[as.character(suma_de_mano),carta_crupier])
    } else{
      return(estrategia_G_optima[as.character(suma_de_mano),carta_crupier])
      return(suma_de_mano)
    }
  }
}

# construimos el mazo y el reparto inicial de cada carta
mazo <- rep(Cartas, Cantidad_de_cada_carta)

repartir_cartas_iniciales <- function(){
  sample(mazo,size = 4,replace = T)
}

valor_mano <- function(mano){
  valores <- sapply(mano, valor_de_carta)
  suma <- sum(valores)
  as_en_mano <- sum(mano == "As")
  while (suma>21 && as_en_mano > 0) {
    suma <- suma-10
    as_en_mano <- as_en_mano-1
  }
  return(suma)
}

jugar_mano <- function(mano_jugador,carta_visible_crupier,mano_crupier,decision){
  if(decision == "D"){
    mano_jugador <- c(mano_jugador,sample(mazo,1))
    apuesta <- 2
  } else{
    if (is.null(decision) || length(decision) == 0) {
      decision <- "P"
    }
    apuesta <- 1
    decision <- estrategia_juego(mano_jugador,carta_visible_crupier)
    while(decision != "P"){
      mano_jugador <- c(mano_jugador,sample(mazo,1))
      valor_mano_jugador <- valor_mano(mano_jugador)
      decision <- estrategia_juego(mano_jugador,carta_visible_crupier)
    }
    
  }
  valor_final_jugador <- valor_mano(mano_jugador)
  if(valor_final_jugador>21) {
    return(-apuesta)
  }
  while (Calcular_suma_mano_crupier(mano_crupier)<17) {
    mano_crupier <- c(mano_crupier,sample(mazo,1))
  }
  valor_final_crupier <- Calcular_suma_mano_crupier(mano_crupier)
  
  if(valor_final_crupier>21 || valor_final_jugador>valor_final_crupier){
    return(apuesta)
  } else if(valor_final_jugador == valor_final_crupier){
    return(0)
  } else{
    return(-apuesta)
  }
  print(mano_jugador)
}


simular_partida_completa <- function() {
  cartas_iniciales <- repartir_cartas_iniciales()
  mano_jugador <- cartas_iniciales[1:2]
  mano_crupier <- cartas_iniciales[3:4]
  carta_visible_crupier <- mano_crupier[1]
  
  decision <- estrategia_juego(mano_jugador,carta_visible_crupier)
  
  if (decision == "A") {
    carta <- mano_jugador[1]
    mano1 <- c(carta,sample(mazo,1))
    mano2 <- c(carta,sample(mazo,1))
    decision1 <- estrategia_G_optima[]
    resultado_mano1 <- jugar_mano(mano1,carta_visible_crupier,mano_crupier,estrategia_juego(mano1,carta_crupier = carta_visible_crupier,posibilidad_abrirse = F))
    resultado_mano2 <- jugar_mano(mano2,carta_visible_crupier,mano_crupier,estrategia_juego(mano2,carta_crupier = carta_visible_crupier,posibilidad_abrirse = F))
    
    return(mean(c(resultado_mano1,resultado_mano2)))
  } else{
    resultado <- jugar_mano(mano_jugador,carta_visible_crupier,mano_crupier,decision)
    return(resultado)
  }
}

calcular_esperanza_jugador <- function(n_partidas = 10000) {
  resultados <- replicate(n_partidas, simular_partida_completa())
  esperanza <- mean(resultados)
  return(esperanza)
}


muestras = 100

salida=replicate(muestras,calcular_esperanza_jugador())

esperanza <- mean(salida)
desviacion_tipica <- sd(salida)/sqrt(muestras)
alpha <- 0.05
z <- qnorm(1-alpha/2)
Int_conf <- c(esperanza-z*desviacion_tipica,esperanza+z*desviacion_tipica)

datos <- data.frame(N_muestras = 1:muestras,
                    Esperanza = salida)
ggplot(datos,aes(x=N_muestras,y=Esperanza)) +
  geom_point(color="red") +
  geom_line(color="blue") +
  labs(title = "Valor esperado del juego",x="Nº de muestra",y="Esperanza de la muestra"
  )
