library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(skimr)
library(tables)
library(kableExtra)

goals=read.csv("C:/Users/Connie/Desktop/CADEMI LABS/goals.csv")
roster= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/roster.csv")
id= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/id.csv")
matches= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/matches.csv")
lineups= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/lineups.csv")

roster$playername = roster$playername %>%
  iconv("UTF-8" )

### Pregunta 1: ¿Cuántos goles anota cada jugador en cada mes?


# Data de jugador, gol y fecha

goals= goals %>% 
  select(-is_own_goal, -is_penalty_goal, -minute) %>% 
  merge(id) %>%
  select(-id)
goals

# Juntamos todo en una sola data
data_final= matches %>% 
  merge(goals) %>% 
  merge(roster) %>%
  select(-team_name)
data_final$playername= data_final$playername %>%
  iconv("UTF-8" )

#Manipular fechas
data_final$match_date= as.Date(data_final$match_date)

# Data con los goles de los jugadores por mes
goles_jugador= select(data_final, playername,
                      is_goal, match_date) %>% 
  separate(match_date,c("año","mes"),sep="-") %>%
  select(mes, playername) %>%
  dcast(playername~mes) %>% rename(JAN=`01`,
                                   FEB=`02`,MAR=`03`,
                                   APR=`04`,
                                   MAY=`05`, AUG=`08`,
                                   SEP=`09`, OCT=`10`,
                                   NOV=`11`,DEC=`12`)
goles_jugador$total = rowSums (goles_jugador[ , 2:11])
# ALEXIS GOLEADOR (16 GOLES)

# Pregunta 2: jugador más joven
edades=data_final %>%
  select(playername,age)%>%
  group_by(playername) %>% 
  distinct() %>%
  arrange(age)
View(edades) #Calum Chambers 19 y Héctor Bellerín 19
# Tomas Rosicky 33

# Pregunta 3: jugador más alto/más bajo
alturas= data_final %>% 
  select(playername, height) %>% 
  distinct() %>% 
  arrange(height)
View(alturas)

# El jugador más bajo es Aléxis Sánchez (1.68), el más alto es
# Olivier Giroud (1.68)

# Pregunta 4: ¿De qué países son los goleadores?
goleadores = goles_jugador %>%
  select(playername,total) %>%
  merge(roster) %>% 
  arrange(desc(total))

# TOP: CHILE, FRANCIA, ESPAÑA, REINO UNIDO.

head(goleadores,5)
table(goleadores$region)
# 1 país de AMÉRICA y 15 países de europa.
table(goleadores$country)
# 7 Reino Unido, 4 Francia, 2 España

table(goleadores$foot)
# Hay 8 diestros, 4 zurdos y 4 ambos.

# Los principales goleadores:Sánchez diestro, Giroud zurdo.
top11= head(goleadores,11)

table(top11$foot)

# WORDCLOUD para portada