#ANALISIS de los datos 1 analisis general 

# Actividad diaria, hacer otro df, donde solo muestre el promedio de cada  columna, para poder utilizar visualizaciones.

library(tidyverse)

resumen_actividad  <- actividad_diaria %>% 
	summarise(across(-c(Id, Dia_de_actividad), mean))  #que no incluya, id y fecha

print(resumen_actividad)


#visualizaciones con este resumen. CRI CRI . . . 

#REGRESO CON LA TABLA ORIGINAL. 

#agrupar por usuario y hacer medias 


datos_media_usuario <- actividad_diaria %>%
	group_by(Id) %>%
	summarise(across(-c(Dia_de_actividad), mean))

# Mostrar el nuevo dataframe con las medias por usuario
print(datos_media_usuario)
View(datos_media_usuario)

#Visualizaciones con la media de cada usuario. 
grafico_pasos <- ggplot(data = datos_media_usuario, aes(x = factor(Id), y = Pasos_totales)) +
	geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.5) +
	labs(x = "Usuario", y = "Pasos totales") +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1))

grafico_completo <- grafico_pasos + 
	geom_point(data = datos_media_usuario, aes(x = factor(Id), y = Calorias), color = "red", size = 3) +
	labs(title = "Pasos totales y calorías consumidas por usuario")

grafico_Calorias <- ggplot(data = datos_media_usuario, aes(x = factor(Id), y = Calorias)) +
	geom_bar(stat = "identity", fill = "gray", color = "black", width = 0.5) +
	labs(x = "Usuario", y = "Calorias") +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1))


#cantidad de entradas diarias por usuario
conteo_entradas <- actividad_diaria %>%
	count(Id)

print(conteo_entradas)

summary(conteo_entradas)

#vemos que no son el mismo numero de entrada

#GRAFICOS DE DISPERSION CON LOS DATOS DE MEDIA POR CADA USUARIO
ggplot(datos_media_usuario, aes(x = Pasos_totales, y = Calorias, color = Id)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +  # Agrega una línea de tendencia lineal
	labs(title = "Relación entre Calorías y Pasos Totales",
			 x = "Total de Pasos",
			 y = "Calorías")

ggplot(datos_media_usuario, aes(x = Minutos_muy_activos, y = Calorias, color = Id)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +  # Agrega una línea de tendencia lineal
	labs(title = "Relación entre Calorías y Minutos muy activos",
			 x = "Minutos muy activos",
			 y = "Calorías")

grafico1.2 <- ggplot(data = datos_media_usuario, aes(x = Pasos_totales, y = Calorias, size = Minutos_muy_activos, )) +
	geom_point(color = "skyblue") +
	labs( title= "Relación entre Pasos totales, Calorias y Minutos muy activos"  ,x = "Pasos totales", y = "Calorias", size = "Minutos_muy_activos") +
	theme_update()


grafico2.2 <- ggplot(data = datos_media_usuario, aes(x = Pasos_totales, y = Calorias, size = Distancia_mucha_actividad)) +
	geom_point(color = "red") +
	labs(title= "Relación entre Pasos totales, Calorias y Distancia de mucha actividad" ,x = "Pasos totales", y = "Calorias", size = "Distancia_mucha_actividad") +
  theme_update()





grid_plot_1_2 <- grid.arrange(grafico1.2, grafico2.2, ncol = 2)
#Visualizaciones de df ACTIVIDAD DIARIA PARA VER TENDENCIAS GENERALES ENTRE CALORIAS Y OTRA VARIABLE 

grafico3 <- ggplot(data = actividad_diaria) +
	geom_point(mapping = aes(x = Pasos_totales, y = Calorias, color = Id), alpha = 0.5) +
	geom_smooth(mapping = aes(x = Pasos_totales, y = Calorias),method = "lm")+
	labs(title= "Relación entre pasos totales y Calorias ", x = "Pasos totales", y = "Calorías", color = "Usuario")+
	theme_update()
	
#annotate("text", x = 3000, y = 2500, label= "Tendencia en general positiva en la relacion de Calorias con Pasos totales,\n cuantos más pasos mayor el consumo de calorias ", color= "blue" )

#no hay manera de alinear el texto donde lo quiero , linea comentada
#GRAFICO DISPERSION DISTANCIA MUCHA ACTIVIDA VS CALORIAS
grafico_dispersion1 <- ggplot(data = actividad_diaria) +
	geom_point(mapping = aes(x = Distancia_mucha_actividad, y = Calorias, color = Id), alpha = 0.5) +
	geom_smooth(mapping = aes(x = Distancia_mucha_actividad, y = Calorias),method = "lm")+
	labs(x = "Distancia de mucha actividad", y = "Calorías", color = "Usuario") 

grafico_dispersion2 <- ggplot(data = actividad_diaria) +
	geom_point(mapping = aes(x = Minutos_muy_activos, y = Calorias, color = Id), alpha = 0.5) +
	geom_smooth(mapping = aes(x = Minutos_muy_activos, y = Calorias),method = "lm")+
	labs(x = "Minutos muy activos", y = "Calorías", color = "Usuario") 


# visualizacion  por DATOS MEDIA USUARIO agrupado por id, 


#
#
#
#
#
#
#
#
#
#

#ANALISIS DE SEPARADO DE CADA USUARIO


ggplot(data = datos_media_usuario) +
	geom_point(mapping = aes(x = Pasos_totales, y = Calorias, color = Id)) +
	labs(x = "Pasos totales", y = "Calorías", color = "Usuario") +
	facet_wrap(~Id)

#usuario 5577150313 MENOS PASOS MAS CALORIAS QUEMADAS  INTERESANTE, USUARIO 3977333714 MAS PASOS POCAS CALORIAS. 

ggplot(data = datos_media_usuario) +
	geom_point(mapping = aes(x = Distancia_total, y = Calorias, color = Id)) +
	labs(x = "Distancia total", y = "Calorias", color = "Usuario") +
	facet_wrap(~Id)

ggplot(data = datos_media_usuario) +
	geom_point(mapping = aes(x = Distancia_mucha_actividad, y = Calorias, color = Id)) +
	labs(x = "Distancia mucha actividad", y = "Calorias", color = "Usuario") +
	facet_wrap(~Id)

#usuario 5577150313 una distancia media de mucha actividad, mayor gasto calorias, usuario 3977333714 distancia de 2 puntos gasto calorias super bajo
#Ver si tienen el minmo numero de entradas cada uno de estos usuarios


conteo_entradas <- actividad_diaria %>%
	count(Id) %>% 
	filter(Id == "5577150313" | Id == "3977333714")

print(conteo_entradas)

#mismo numero de entradas

#usuario que muestra en relacion mas calorias y menos pasos 
datos_usuario_313 <- actividad_diaria %>%
	filter(Id == 5577150313)

head(datos_usuario)
summary(datos_usuario)

registro_de_peso_usuario_313 <- registro_de_peso %>% 
	filter(Id == 5577150313 )

summary(registro_de_peso_usuario_313) #IMC 28  Peso estable en todo el registro 

conteo_entradas_registro_peso <- registro_de_peso %>% 
	count(Id)

conteo_entradas_registro_peso 

View(conteo_entradas_registro_peso)


#NO SE PUEDE HACER UN ANALISIS FIABLE YA QUE ESOS USUARIOS ESPECIFICICOS NO TIENENE ENTRADAS CONSISTENTES PARA CADA VARIABLE . 








	