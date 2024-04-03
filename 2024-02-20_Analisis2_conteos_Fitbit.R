#Analisis de datos 2 , conteos de entradas por usuario
library(tidyverse)



#cantidad de entradas diarias por usuario EN ACTIVIDAD DIARIA
conteo_entradas_act_diaria <- actividad_diaria %>%
	count(Id)

print(conteo_entradas_act_diaria)

#CANTIDAD DE ENTRADAS USUARIO EN FRECUENCIA CARDIACA media por dia 

frecuencia_media_por_dia <- frecuencia_cardiaca_segundos %>%
	group_by(Id, Dia_de_actividad) %>%
	summarize(Media_frecuencia_cardiaca = mean(Valor))

conteo_entradas_frecuencia_cardiaca <- frecuencia_media_por_dia %>% 
	count(Id)

print(conteo_entradas_frecuencia_cardiaca)


#CANTIDAD DE ENTRADAS USUARIO 

conteo_mets_por_minuto <- mets_por_minuto_largo %>%
	count(Id)

print(conteo_mets_por_minuto)



conteo_registro_de_peso <- registro_de_peso %>%
	count(Id)

print(conteo_registro_de_peso)



conteo_dia_de_sueno <- dia_de_sueno %>%
	count(Id)

print(conteo_dia_de_sueno)



conteo_sueno_por_minuto <- sueno_por_minuto %>%
	count(Id)

print(conteo_sueno_por_minuto)


#TABLA DE CONTEOS

# Crear una lista de todos los recuentos
lista_conteos <- list(
	conteo_entradas_act_diaria,
	conteo_entradas_frecuencia_cardiaca,
	conteo_mets_por_minuto,
	conteo_registro_de_peso,
	conteo_dia_de_sueno,
	conteo_sueno_por_minuto
)
#########################################FALTA CODIGO QUE BORRE....
nombres_columnas <- c("act_diaria", "frecuencia_cardiaca", "mets_por_min", "registro_de_peso", "dia_de_sueno", "sueno_por_minuto")


#unir los conteos de cada actividad por ID
todos_los_conteos <- lista_conteos %>%
	reduce(left_join, by = "Id") %>%
	setNames(c("Id", nombres_columnas))



View(todos_los_conteos)

todos_los_conteos[is.na(todos_los_conteos)] <- 0
print(todos_los_conteos)


#Nueva columna con el calculo de mets por minutos para analizar las entradas
todos_los_conteos$mets_entradas <- todos_los_conteos$mets_por_min / 1440


print(todos_los_conteos)

#quitar columna de sueno_por_minuto y mets_por_min para hacer grafico . 

todos_los_conteos_plot <- select(todos_los_conteos, -c(mets_por_min, sueno_por_minuto))
print(todos_los_conteos_plot)

todos_los_conteos_redondeado <- round(todos_los_conteos_plot)

summary(round(todos_los_conteos_plot))


# calcular las sumas de cada variable
totales <- colSums(todos_los_conteos_plot[, -1])

# Crear un nuevo dataframe con los totales
totales_conteos <- data.frame(totales = totales)

round(totales_conteos)
print(totales_conteos)

grafico_conteos_t <- ggplot(data = totales_conteos, aes(x = "", y = totales, fill = rownames(totales_conteos))) +
	geom_bar(width = 1, stat = "identity") +
	coord_polar("y", start = 0) +
	theme_void() +
	scale_fill_brewer(palette = "Set2", name = "Tipo de aplicación") + 
	labs(title = "Resumen de uso por tipo de aplicación") +
	theme(
		plot.title = element_text( size = 16),
		legend.position = "right"
	)




#CUAL USUARIO CUMPLE CON TODOS LAS VARIABLES DE MANERA MAS CONSISTENTE 
# POCOS USUARIOS TIENEN EL REGISTRO DE PESO por dia. Solo 6962181067 seria interesante motivar al cliente para usar todas las acciones que incluye el dispositivo y asi poder evaluar de manera
#mas integral su bienestar fisico. 

#EL USUARIO 6962181067, TIENE TODOS LOS VALORES COMPLETOS EN CADA VARIABLE, ES POSIBLE ANALIZAR SU PERFIL. 

#Dia_de_actividad a formato de fecha por un error en el grafico
#cambio de tipo de dato para todos los df y que no haya errores
frecuencia_media_por_dia <- frecuencia_media_por_dia %>%
	mutate(Dia_de_actividad = as.Date(Dia_de_actividad))

calorias_diarias <- calorias_diarias %>%
	mutate(Dia_de_actividad = as.Date(Dia_de_actividad))


pasos_diarios <- pasos_diarios %>%
	mutate(Dia_de_actividad = as.Date(Dia_de_actividad))


intensidades_diarias <- intensidades_diarias %>%
	mutate(Dia_de_actividad = as.Date(Dia_de_actividad))



dia_de_sueno <- dia_de_sueno %>%
	mutate(Dia_de_actividad = as.Date(Dia_de_actividad))


registro_de_peso <- registro_de_peso %>%
	mutate(Dia_de_actividad = as.Date(Dia_de_actividad))



#ANALISIS POR USUARIO DE FRECUENCIAS CARDIACAS MEDIAS POR DIA.

ggplot(data = frecuencia_media_por_dia, aes(x = Dia_de_actividad, y = Media_frecuencia_cardiaca)) +
	geom_line() +
	facet_wrap(~ Id) +
	labs(x = "Fecha", y = "Media de frecuencia cardíaca") +
	theme_minimal()

#CREAR DF COMPLETO PARA EL ANALISIS INTEGRAL DEL USUARIO 6962181067


#############################################################
#Filtrar los datos para el usuario 6962181067 en cada tabla##
#############################################################

#FUNCIONABA Y AHORA DA ERROR TODO, QUIZAS COMPENSE HACER EL CAMBIO DE TIPO DE DATO ANTES DEL FILTRO. 



User_1067_calorias <- calorias_diarias %>%
	filter(Id == "6962181067") %>%
	select(Id, Dia_de_actividad, Calorias)

User_1067_pasos <- pasos_diarios %>%
	filter(Id == "6962181067") %>%
	select(Id, Dia_de_actividad, Total_pasos)

User_1067_intensidades <- intensidades_diarias %>%
	filter(Id == "6962181067" ) %>%
	select(Id, Dia_de_actividad, starts_with("Minutos"))

User_1067_frecuencias <- frecuencia_media_por_dia %>% 
	filter(Id== "6962181067" ) %>%
	select(Id, Dia_de_actividad, Media_frecuencia_cardiaca)

User_1067_dia_sueno <- dia_de_sueno %>% 
	filter (Id== "6962181067" ) %>%
	select(Id, Dia_de_actividad, Registros_totales_sueño, Minutos_totales_dormido, Tiempo_total_en_cama)

User_1067_registro_peso <- registro_de_peso %>% 
	filter (Id== "6962181067" ) %>%
	select(Id, Dia_de_actividad, Peso_kg, IMC)


#UNIR

User_1067 <- left_join(User_1067_calorias, User_1067_pasos, by = c("Dia_de_actividad", "Id")) %>%
	left_join(User_1067_intensidades, by = c("Dia_de_actividad", "Id")) %>% 
	left_join(User_1067_frecuencias, by = c("Dia_de_actividad", "Id")) %>% 
	left_join(User_1067_dia_sueno, by = c("Dia_de_actividad", "Id")) %>% 
	left_join(User_1067_registro_peso, by = c("Dia_de_actividad", "Id"))

View(User_1067)

#Error en join, no se si sea por ejecutar este script independiente de los anteriores, corregire el error para este.



#analisis  de correlacion entre las variables
#da error porque utiliza el id yla fecha como variables
str(User_1067)


User_1067_cor <- User_1067 %>%
	select(-Id, -Dia_de_actividad) %>%
	cor(use = "pairwise.complete.obs")



#redondear, salen con muchos decimales 

User_1067_cor_redondeado <- round(User_1067_cor, 2)
print(User_1067_cor_redondeado)
View(User_1067_cor_redondeado)


# Grafica lineal de calorias quemadas a lo largo del tiempo. 

ggplot( data= User_1067) +
	geom_line (mapping = aes(x = Dia_de_actividad, y = Calorias) ) +
	labs(x = "Fecha", y = "Calorias", title = "Calorias quemadas por día", subtitle = "Usuario 6962181067" ) 


# Grafica lineal con linea de total de pasos y calorias. CORREGIR GRAFICO.. 
ggplot( data= User_1067) +
	geom_line (mapping = aes(x = Dia_de_actividad, y = Calorias, color= "calorias") ) +
	geom_line(mapping = aes(x= Dia_de_actividad, y= Total_pasos, color= "Total_pasos"))
  labs(x = "Fecha", y = "Calorias", title = "Calorias quemadas por día", subtitle = "Usuario 6962181067" ) 
  theme_minimal()
	
#no es lo que necesito
  
  ggplot(data = User_1067) +
  	geom_line(mapping = aes(x = Dia_de_actividad, y = Total_pasos, color = "Pasos")) +
  	geom_point(mapping = aes(x = Dia_de_actividad, y = Calorias, color = "Calorias"), size = 3) +
  	scale_y_continuous(labels = scales::comma) + 
  	labs(x = "Fecha", y = "Cantidad", title = "Calorias y pasos por día", subtitle = "Usuario 6962181067") +
  	theme_minimal()
	
# =) esto si es
# en este grafico es facil comparar la linea de pasos dados junto con la quema de calorias, ahi se puede ver que hay dias como el que marca
#2174 calorias tuvo mayor numero de pasos que el de 2254, por lo que debe estar influyendo el tipo de actividad  
# el dia que hizo menos pasos y tuvo 1466 calorias y un dia con mas pasos, el ultimo , mas pasos en relacion con el comentado, tuvo menos quema de calorias
  
  
##############################  
#GRAFICO USAR PARA ENTREGABLE#
############################## 
  
  
grafico4 <- ggplot(data = User_1067) +
 	geom_line(mapping = aes(x = Dia_de_actividad, y = Total_pasos, color = "Pasos")) +
 	geom_point(mapping = aes(x = Dia_de_actividad, y = Total_pasos, color = "Calorias"), size = 3) +
 	geom_text(mapping = aes(x = Dia_de_actividad, y = Total_pasos, label = Calorias), 
 						size = 3, hjust = -0.3, vjust = 0.5, angle = 0, color = "black") +
  labs(x = "Fecha", y = "Cantidad de pasos", title = "Calorias y pasos por día", subtitle = "Usuario 6962181067 análisis del 12 de Abril al 12 de Mayo") +
 	theme_update()

  
#en la grafica siguiente no se puede analizar tan objetivamente, seria interesante mas adelante cruzar datos, de frecuencia cardiaca por minuto junto con la acrtividad con entradas por minuto.   
#para ver los siguientes dos graficos juntos hacer un grid 
  
install.packages("gridExtra")  
library(gridExtra)
library(grid)
  
grafico5 <- ggplot(data = User_1067) +
	geom_line(mapping = aes(x = Dia_de_actividad, y = Media_frecuencia_cardiaca, color= "Frecuencia cardiaca")) +
	geom_point(mapping = aes(x = Dia_de_actividad, y = Media_frecuencia_cardiaca, color= "Minutos muy activos"), size = 3) +
	geom_text (mapping = aes(x = Dia_de_actividad, y = Media_frecuencia_cardiaca, label = Minutos_muy_activos),
						 size = 3, hjust = -0.3, vjust = 0.5, angle = 0, color = "black" ) + 
	labs(x = "Fecha", y = "Frecuencia", title = "Frecuencia cardiaca y Minutos muy activos", subtitle = "Usuario 6962181067") +
	theme_update()

#######################################################
#grafica de frecuencia cardiaca con datos de calorias.#
#######################################################
grafico6 <- ggplot(data = User_1067) +
	geom_line(mapping = aes(x = Dia_de_actividad, y = Media_frecuencia_cardiaca, color= "Frecuencia cardiaca")) +
	geom_point(mapping = aes(x = Dia_de_actividad, y = Media_frecuencia_cardiaca, color= "Calorias"), size = 3) +
	geom_text (mapping = aes(x = Dia_de_actividad, y = Media_frecuencia_cardiaca, label = Calorias),
						 size = 3, hjust = -0.3, vjust = 0.5, angle = 0, color = "black" ) + 
	labs(x = "Fecha", y = "Frecuencia", title = "Frecuencia cardiaca y Calorias", subtitle = "Usuario 6962181067") +
	theme_update()

#juntar los dos graficos 
gid_plot_5_6 <- grid.arrange(grafico5, grafico6, ncol = 2)





#grafica para analizar la relacion entre las dos variables.
#hay una relación positiva entre la frecuencia cardíaca media y las calorías quemadas del usuario. 

grafico7 <- ggplot(data = User_1067, aes(x = Media_frecuencia_cardiaca, y = Calorias)) +
	geom_point(color = "blue") +
	geom_smooth()+
	labs(x = "Frecuencia cardíaca media", y = "Calorías quemadas", 
			 title = "Relación entre frecuencia cardíaca y calorías quemadas", subtitle = "Usuario 6962181067" ) +
	theme_update()
##################################################################################
# grafica para analizar la tendencia entre frecuencia cardiaca y minutos dormido #
##################################################################################

# Grafico muestra al tener la mayoria de observaciones dentro de la zona sombreada que existe una relacion positiva entre
#la frecuencia cardiaca y el tiempo de sueño, sin embargo la linea oscilante muyestra que hay otras variables que influyen
#que no estan presentes en el analisis del grafico. 

grafico8 <- ggplot(data = User_1067, aes(x = Media_frecuencia_cardiaca, y = Minutos_totales_dormido)) +
	geom_point(color = "blue") +
	geom_smooth()+
	labs(x = "Frecuencia cardíaca media", y = "Minutos totales dormido", 
			 title = "Relación entre frecuencia cardíaca y minutos totales dormido", subtitle = "Usuario 6962181067" ) +
	theme_update()

grid_plot_7_8 <- grid.arrange(grafico7, grafico8, ncol = 2)


grafico_tipo_sueno<- ggplot(data = User_1067, aes(x = Minutos_totales_dormido , y = Media_frecuencia_cardiaca)) +
	geom_point(color = "blue") +
	geom_smooth()+
	labs(x = "Minutos totales dormido" , y = "Frecuencia cardíaca media", 
			 title = "Analisis de la frecuencia cardiaca en el sueño", subtitle = "Usuario 6962181067" ) +
	theme_update()

#Calcular calidad de sueño, partiendo de que la diferencia entre minutos dormidos y tiempo total en cama, es el tiempo que 
#el usuario tarda en dormir (falta el dato de si es tiempo al despertar o dormir)

#agregar al df de usuario, una columna de eficiencia de sueño que seria los minutos totales dormidos entre el tiempo total en cama expresado en %
#formula a usar eficiencia del sueño = Minutos totales dormido / tiempo total en cama *100

User_1067 <- transform(User_1067, Eficiencia_del_sueno = Minutos_totales_dormido / Tiempo_total_en_cama * 100)

# Calcular tiempo en que tarda en dormir para encontrar patrones 

User_1067 <- transform(User_1067, Tiempo_conciliar_sueno = Tiempo_total_en_cama - Minutos_totales_dormido)


# Gráfico de eficiencia del sueño y latencia del sueño


grafico_sueno <- ggplot(data = User_1067) +
	geom_line(mapping = aes(x = Dia_de_actividad, y = Eficiencia_del_sueno, color= "Eficiencia del sueño")) +
	geom_point(mapping = aes(x = Dia_de_actividad, y = Eficiencia_del_sueno, color= "Tiempo en conciliar el sueño"), size = 3) +
	geom_text (mapping = aes(x = Dia_de_actividad, y = Eficiencia_del_sueno, label = Tiempo_conciliar_sueno),
						 size = 3, hjust = -0.3, vjust = 0.5, angle = 0, color = "black" ) + 
	labs(x = "Fecha", y = "Eficiencia", title = "Eficiencia del sueño y tiempo en conciliar", subtitle = "Usuario 6962181067") +
	theme_update()


correlacion_sueno <- cor(User_1067$Tiempo_conciliar_sueno, User_1067$Eficiencia_del_sueno)	

  #  [1] -0.9337998

#El resultado indica que existe una fuerte correlacion negativa entre las variables, quiere decir que cuanto mas tiempo se tarda en conciliar el sueño , la eficiencia del sueño disminuye.


#Grafico de distribucion de actividad 

grafico9 <- ggplot(data = User_1067, aes(x = Dia_de_actividad)) +
	geom_bar(aes(y = Minutos_sedentarios, fill = "Sedentarios"), stat = "identity", width = 0.5) +
	geom_bar(aes(y = Minutos_ligeramente_activos, fill = "Ligeramente activos"), stat = "identity", width = 0.5) +
	geom_bar(aes(y = Minutos_moderadamente_activos, fill = "Moderadamente activos"), stat = "identity", width = 0.5) +
	geom_bar(aes(y = Minutos_muy_activos, fill = "Muy activos"), stat = "identity", width = 0.5) +
	geom_text(aes(y = Minutos_sedentarios, label = Calorias), vjust = -0.5, size = 3, angle = 90) +
	labs(x = "Día de actividad", y = "Minutos", title = "Distribución de actividad por día, incluye calorías", subtitle = "Usuario 6962181067", fill= "Tipo de actividad") +
	scale_fill_manual(values = c("Sedentarios" = "lightsalmon", 
															 "Ligeramente activos" = "lightyellow",
															 "Moderadamente activos" = "skyblue",
															 "Muy activos" = "green")) +
	theme_update()





