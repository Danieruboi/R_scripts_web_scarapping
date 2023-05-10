tw_thread <- tweet_threading("1600134324266967040")
usuario=users_data(tw_thread)

x=tweet_threading("1583247262855012356", traverse = c("backwards", "forwards"), verbose = FALSE)


#------------------------------------------------------------------------
# Eliminar todos los objetos existentes
rm (list = ls ())
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


con = dbConnect(odbc::odbc(), "REDESYMEDIOS", timeout = 10, encoding= "latin1")
seguidores = DBI::dbGetQuery(con, "select usuario,id_str,nombre_seguidor,seguidor from [REDESYMEDIOS].[dbo].[TWITTER_SEGUIDORES] where usuario='CarlosHernanR1' ")

usuario_tweets = function(usuario,n_posts){tweets=tryCatch(get_timeline(user=usuario, n=n_posts, since_id = NULL),error = function(e){NaN})
regla1=tryCatch(is.nan(tweets),error = function(e){FALSE})
if(regla1){llaves=data.frame(usuario,fecha=NA,id_str=NA,texto=NA)
menciones=data.frame(usuario_etiquetado=NA,nombre_etiquetado=NA,hashtags=NA)
menciones_rt=data.frame(id_str_rt=NA,usuario_twitter_rt=NA,nombre_twitter_rt=NA,usuario_etiq_rt=NA,nombre_etiq_rt=NA,hashtags_rt=NA,enlace=NA)
salida=cbind(llaves,menciones,menciones_rt)
return(salida)
}else{consulta=cbind(usuario,tweets)
tabla=subset(consulta,select = c(usuario,created_at,id_str,full_text))
colnames(tabla)[c(2,4)]=c('fecha','texto')
entidades=tweets$entities
menciones=data.frame(usuario_twitter=NA,nombre_twitter=NA,hashtags=NA)[0,]

for(i in entidades){hashtags=paste(i$hashtags$text,collapse='|')
usuario_etiquetado=paste(i$user_mentions$screen_name,collapse  = '|')
nombre_etiquetado=paste(i$user_mentions$name,collapse  = '|')
mencion=cbind(usuario_etiquetado,nombre_etiquetado,hashtags)
menciones=rbind(menciones,mencion)
}
salida=cbind(tabla,menciones)
}
regla2=all(is.na(tweets$retweeted_status)==TRUE)
if(regla2){salida_rt=data.frame(id_str_rt=NA,usuario_twitter_rt=NA,nombre_twitter_rt=NA,usuario_etiq_rt=NA,nombre_etiq_rt=NA,hashtags_rt=NA)
salida_f=cbind(salida,salida_rt)
salida_f$enlace = str_c("https://twitter.com/",salida_f$usuario,"/status/", salida_f$id_str)
salida_f$usuario_etiquetado[salida_f$usuario_etiquetado=='NA']<-NA
salida_f$nombre_etiquetado[salida_f$nombre_etiquetado=='NA']<-NA
salida_f$hashtags[salida_f$hashtags=='NA']<-NA
return(salida_f)
}
regla_rt=substr(salida$texto,1,2)=='RT'
no_retweet=salida[regla_rt==FALSE,]
if(nrow(no_retweet)==0){si_retweet=salida[regla_rt==TRUE,]
retweet=tweets$retweeted_status[regla_rt]

salida_rt=data.frame(id_str_rt=NA,full_text_rt=NA,usuario_twitter_rt=NA,nombre_twitter_rt=NA,usuario_etiq_rt=NA,nombre_etiq_rt=NA,hashtags_rt=NA)[0,]
for(i in retweet){tweet_rt=cbind(id_str_rt=i$id_str,full_text_rt=i$full_text,usuario_twitter_rt=i$user$screen_name, nombre_twitter_rt=i$user$name)
user_mentions=i$entities$user_mentions
screen_name=user_mentions[[1]]
screen_name$screen_name
usuario_etiq_rt=ifelse(length(screen_name)==0,NA,paste(screen_name$screen_name,collapse  = '|'))
nombre_etiq_rt=ifelse(length(screen_name)==0,NA,paste(screen_name$name,collapse  = '|'))
df_hash_rt=i$entities$hashtags[[1]]
hashtags_rt=paste(df_hash_rt$text,collapse  = '|')
mencion=cbind(tweet_rt,usuario_etiq_rt,nombre_etiq_rt,hashtags_rt)
salida_rt=rbind(salida_rt,mencion)
}
si_retweet=cbind(si_retweet,salida_rt)
salida_f=si_retweet
salida_f=mutate(salida_f,texto=ifelse(substr(texto,1,2)=='RT',full_text_rt,texto))
salida_f=subset(salida_f,select = c(usuario,fecha,id_str,texto,usuario_etiquetado,nombre_etiquetado,hashtags,id_str_rt,usuario_twitter_rt,nombre_twitter_rt,usuario_etiq_rt,nombre_etiq_rt,hashtags_rt))
salida_f$enlace = str_c("https://twitter.com/",salida_f$usuario,"/status/", salida_f$id_str)
salida_f$usuario_etiquetado[salida_f$usuario_etiquetado=='NA']<-NA
salida_f$nombre_etiquetado[salida_f$nombre_etiquetado=='NA']<-NA
salida_f$hashtags[salida_f$hashtags=='NA']<-NA
salida_f$hashtags_rt[salida_f$hashtags_rt=='NA']<-NA
salida_f$usuario_etiq_rt[salida_f$usuario_etiq_rt=='NA']<-NA
salida_f$nombre_etiq_rt[salida_f$nombre_etiq_rt=='NA']<-NA
salida_f$hashtags_rt[salida_f$hashtags_rt=='']<-NA
return(salida_f)
}

no_retweet=cbind(no_retweet,data.frame(id_str_rt=NA,full_text_rt=NA,usuario_twitter_rt=NA,nombre_twitter_rt=NA,usuario_etiq_rt=NA,nombre_etiq_rt=NA,hashtags_rt=NA))

si_retweet=salida[regla_rt==TRUE,]
retweet=tweets$retweeted_status[regla_rt]

salida_rt=data.frame(id_str_rt=NA,full_text_rt=NA,usuario_twitter_rt=NA,nombre_twitter_rt=NA,usuario_etiq_rt=NA,nombre_etiq_rt=NA,hashtags_rt=NA)[0,]
for(i in retweet){tweet_rt=cbind(id_str_rt=i$id_str,full_text_rt=i$full_text,usuario_twitter_rt=i$user$screen_name, nombre_twitter_rt=i$user$name)
user_mentions=i$entities$user_mentions
screen_name=user_mentions[[1]]
screen_name$screen_name
usuario_etiq_rt=ifelse(length(screen_name)==0,NA,paste(screen_name$screen_name,collapse  = '|'))
nombre_etiq_rt=ifelse(length(screen_name)==0,NA,paste(screen_name$name,collapse  = '|'))
df_hash_rt=i$entities$hashtags[[1]]
hashtags_rt=paste(df_hash_rt$text,collapse  = '|')
mencion=cbind(tweet_rt,usuario_etiq_rt,nombre_etiq_rt,hashtags_rt)
salida_rt=rbind(salida_rt,mencion)
}
si_retweet=cbind(si_retweet,salida_rt)
salida_f=rbind(si_retweet,no_retweet)   
salida_f=mutate(salida_f,texto=ifelse(substr(texto,1,2)=='RT',full_text_rt,texto))
salida_f=subset(salida_f,select = c(usuario,fecha,id_str,texto,usuario_etiquetado,nombre_etiquetado,hashtags,id_str_rt,usuario_twitter_rt,nombre_twitter_rt,usuario_etiq_rt,nombre_etiq_rt,hashtags_rt))
salida_f$enlace = str_c("https://twitter.com/",salida_f$usuario,"/status/", salida_f$id_str)
salida_f$usuario_etiquetado[salida_f$usuario_etiquetado=='NA']<-NA
salida_f$nombre_etiquetado[salida_f$nombre_etiquetado=='NA']<-NA
salida_f$hashtags[salida_f$hashtags=='NA']<-NA
salida_f$hashtags_rt[salida_f$hashtags_rt=='NA']<-NA
salida_f$usuario_etiq_rt[salida_f$usuario_etiq_rt=='NA']<-NA
salida_f$nombre_etiq_rt[salida_f$nombre_etiq_rt=='NA']<-NA
salida_f$hashtags_rt[salida_f$hashtags_rt=='']<-NA
return(salida_f)
}


#ht <- seguidores$seguidor
#consulta = bind_rows(lapply(ht[1:10],FUN = usuario_tweets,n_posts=10))


targettwittername='CarlosHernanR1'
fan='saulgarciarey2'
fecha_extraccion = as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
comentarios <- get_timeline(fan, n=1000)
followertl <- data.frame(comentarios)
followertl <- filter(followertl, in_reply_to_screen_name == 'CarlosHernanR1' | in_reply_to_screen_name == 'CGR_Colombia') %>% select(created_at,id_str,full_text,in_reply_to_status_id_str,in_reply_to_screen_name)

colnames(followertl)=c('fecha_comentario','id_str_comentario','comentario','id_str','usuario')
followertl=cbind(opin_usuario=fan,followertl,fecha_extraccion)
followertl$fecha_comentario=as.character(followertl$fecha_comentario)


followertl$comentario=str_squish(followertl$comentario)
followertl$comentario=sub('á','a',followertl$comentario)
followertl$comentario=sub('é','e',followertl$comentario)
followertl$comentario=sub('í','i',followertl$comentario)
followertl$comentario=sub('ó','o',followertl$comentario)
followertl$comentario=sub('ú','u',followertl$comentario)
followertl$comentario=sub('Á','A',followertl$comentario)
followertl$comentario=sub('É','E',followertl$comentario)
followertl$comentario=sub('Í','I',followertl$comentario)
followertl$comentario=sub('Ó','O',followertl$comentario)
followertl$comentario=sub('Ú','U',followertl$comentario)
followertl$comentario=sub('Ü','U',followertl$comentario)
followertl$comentario=sub('ü','u',followertl$comentario)

followertl$comentario=iconv(followertl$comentario, "latin1", "ASCII", sub="")




con = dbConnect(odbc::odbc(), "REDESYMEDIOS", timeout = 10, encoding= "latin1")
historico = DBI::dbGetQuery(con, "select opin_usuario,id_str_comentario,id_str,usuario from [REDESYMEDIOS].[dbo].[TWITTER_COMENTARIOS]")

tabla=anti_join(followertl, historico, by=c("opin_usuario","id_str_comentario",'id_str','usuario'))
nrow(tabla)
tabla$usuario

con = dbConnect(odbc::odbc(), "REDESYMEDIOS", timeout = 10, encoding= "latin1")
DBI::dbWriteTable(conn = con,name = "TWITTER_COMENTARIOS",tabla,append=TRUE)

