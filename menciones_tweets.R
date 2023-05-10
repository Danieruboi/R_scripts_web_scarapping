# Eliminar todos los objetos existentes
rm (list = ls ())
# Cargar las librerias que permiten la conexiC3n a las bases de datos
suppressMessages(suppressWarnings(library(DBI)))
suppressMessages(suppressWarnings(library(odbc)))
suppressMessages(suppressWarnings(library(dbplyr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(jsonlite)))

con = dbConnect(odbc::odbc(), "REDESYMEDIOS", timeout = 10,encoding= "latin1")
parametros <- lapply("TWITTER" , dbReadTable, conn = con) %>% as.data.frame()
head(parametros)


historico = lapply("TWITTER_MENCIONES" , dbReadTable, conn = con) %>% as.data.frame() %>%  select(usuario_nombre,id_str)


#readRenviron("Renviron")
library(rtweet)
library(plyr)
library(httpuv)
library(readr)
library(readxl)
library(tm)
library(tidytext)
library(stringr)

#create_token(
#  app = Sys.getenv("TWIITER_APP"),
#  consumer_key = Sys.getenv("TWIITER_CONSUMER_KEY"),
#  consumer_secret = Sys.getenv("TWIITER_CONSUMER_SECRET"),
#  access_token = Sys.getenv("TWIITER_ACCESS_TOKEN"),
#  access_secret = Sys.getenv("TWIITER_ACCESS_SECRET")
#) -> twitter_token
#
saveRDS(create_token(
  app = 'Daniknow',
  consumer_key = '9sMGORs198AtXJ7mByxU7WkZM',
  consumer_secret = 'gxbrPJFwIUP9jGi6niWL6CYfEds5IGEBEXbFeQUXbqfFBAlPqP',
  access_token = '1298693595625672708-aWc22oKGBD8xTFiv9TfxmfLjX6VOWN',
  access_secret = 'QwljAHzCbhmHap026sLT8eRZejLXAHnIN0uaSuABcLcYN',set_renv = FALSE), file = "DANIEL_t.rds")

Token = readRDS("DANIEL_t.rds")
Token <- get_tokens()
Token


#parametros <- subset(parametros, Boletin =="Noticiero, periodico o radio 1")
#ht <- parametros$palabra
#prueba=ht[1:5]
ht='@CGR_Colombia'
n_posts=1000

tt=search_tweets2(q= ht, n = n_posts, include_rts = FALSE, type = "recent", lang = "es" )
usuario=users_data(tt)

us=usuario[, c("name", "screen_name")]
twt=tt[, c("id_str", "full_text", "created_at")]

entidades=tt$entities


menciones=data.frame(usuario_twitter=NA,nombre_twitter=NA,hashtags=NA)[0,]

for(i in entidades){hashtags=paste(i$hashtags$text,collapse='|')
                    x=i$user_mentions
                    usuario_twitter=paste(i$user_mentions$screen_name,collapse  = '|')
                    nombre_twitter=paste(i$user_mentions$name,collapse  = '|')
                    mencion=cbind(usuario_twitter,nombre_twitter,hashtags)
                    menciones=rbind(menciones,mencion)}


consulta=gsub("\\.+[0-9]{1,2}","", substring(rownames(tt),2))


tabla=cbind(consulta,us,twt,menciones)
tabla$enlace = str_c("https://twitter.com/",tabla$consulta,"/status/", tabla$id_str)
fecha_extraccion = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
tabla=cbind(tabla,fecha_extraccion)
colnames(tabla)=c('consulta','nombre','usuario_nombre','id_str','texto','fecha','usuario_etiquetados','nombres_etiquetados','hashtags','enlace','fecha_extraccion')
tabla$texto=gsub("\n"," ", tabla$texto)
tabla=anti_join(tabla, historico, by=c("usuario_nombre","id_str"))


tabla$fecha=as.character(tabla$fecha)
tabla$texto=str_squish(tabla$texto)
tabla$texto=sub('á','a',tabla$texto)
tabla$texto=sub('é','e',tabla$texto)
tabla$texto=sub('í','i',tabla$texto)
tabla$texto=sub('ó','o',tabla$texto)
tabla$texto=sub('ú','u',tabla$texto)

tabla$texto=sub('Á','A',tabla$texto)
tabla$texto=sub('É','E',tabla$texto)
tabla$texto=sub('Í','I',tabla$texto)
tabla$texto=sub('Ó','O',tabla$texto)
tabla$texto=sub('Ú','U',tabla$texto)
tabla$texto=sub('Ü','U',tabla$texto)
tabla$texto=sub('ü','u',tabla$texto)

tabla$texto=iconv(tabla$texto, "latin1", "ASCII", sub="")

tabla=select(tabla,fecha,fecha_extraccion,consulta,nombre,usuario_nombre,id_str,texto,usuario_etiquetados,nombres_etiquetados,hashtags,enlace)


dbWriteTable(conn = con,name = "TWITTER_MENCIONES",tabla,append=TRUE)
