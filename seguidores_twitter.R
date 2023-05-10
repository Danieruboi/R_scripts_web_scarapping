# Eliminar todos los objetos existentes
#rm (list = ls ())
# Cargar las librerias que permiten la conexiC3n a las bases de datos
suppressMessages(suppressWarnings(library(DBI)))
suppressMessages(suppressWarnings(library(odbc)))
suppressMessages(suppressWarnings(library(dbplyr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(jsonlite)))


#readRenviron("Renviron")
library(rtweet)
library(plyr)
library(httpuv)
library(readr)
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

##set name of tweeter to look at (this can be changed)
usuario <- "CGR_Colombia"

extraccion=get_followers(usuario,n=Inf)
targetfollowers = data.frame(extraccion)
user_lookup = lookup_users(users=targetfollowers$from_id)
users_with_tweets_and_unprotected = filter(user_lookup, statuses_count != 0 & protected != TRUE)%>%select(id_str,name,screen_name,description,followers_count,friends_count,created_at)
colnames(users_with_tweets_and_unprotected)=c('id_str','nombre_seguidor','seguidor','descripcion','n_seguidores','n_seguidos','fecha_creacion')

fecha_extraccion = format(Sys.time(), "%Y-%m-%d %H:%M:%S")

seguidores=cbind(usuario,users_with_tweets_and_unprotected,fecha_extraccion)

seguidores$fecha_creacion=as.character(seguidores$fecha_creacion)
seguidores$descripcion=str_squish(seguidores$descripcion)
seguidores$descripcion=sub('á','a',seguidores$descripcion)
seguidores$descripcion=sub('é','e',seguidores$descripcion)
seguidores$descripcion=sub('í','i',seguidores$descripcion)
seguidores$descripcion=sub('ó','o',seguidores$descripcion)
seguidores$descripcion=sub('ú','u',seguidores$descripcion)
seguidores$descripcion=sub('Á','A',seguidores$descripcion)
seguidores$descripcion=sub('É','E',seguidores$descripcion)
seguidores$descripcion=sub('Í','I',seguidores$descripcion)
seguidores$descripcion=sub('Ó','O',seguidores$descripcion)
seguidores$descripcion=sub('Ú','U',seguidores$descripcion)
seguidores$descripcion=sub('Ü','U',seguidores$descripcion)
seguidores$descripcion=sub('ü','u',seguidores$descripcion)

seguidores$descripcion=iconv(seguidores$descripcion, "latin1", "ASCII", sub="")

con = dbConnect(odbc::odbc(), "REDESYMEDIOS", timeout = 10, encoding= "latin1")
historico = DBI::dbGetQuery(con, "select usuario,id_str,nombre_seguidor,seguidor from [REDESYMEDIOS].[dbo].[TWITTER_SEGUIDORES]")

tabla=anti_join(seguidores, historico, by=c("usuario","id_str",'nombre_seguidor'))

con = dbConnect(odbc::odbc(), "REDESYMEDIOS", timeout = 10, encoding= "latin1")
DBI::dbWriteTable(conn = con,name = "TWITTER_SEGUIDORES",tabla,append=TRUE)
