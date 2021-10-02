# LIBRERIAS ####################################################################
library(shiny)
library(shinythemes)
library(shinyauthr)
library(calendar)
library(odbc)
library(dbx)
library(DT)
library(DTedit)
library(DataEditR)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(glue)
library(lubridate)
library(RColorBrewer)
library(toastui)
library(hexView)
library(magick)

# CONFIGURACIONES GLOBALES #####################################################

con <- dbConnect(odbc::odbc(),
                 .connection_string = "Driver={PostgreSQL ANSI};Uid=ubuntu;Database=herbalife;", 
                 timeout = 10)

dbSendStatement(con, "SET search_path = reto21")

# FUNCIONES ####################################################################

get_coaches <- function() {
    consulta_sql <- "
        SELECT *
        FROM
            tbl_coaches
        ORDER BY nombre_coach
        "
    res <- dbGetQuery(con, consulta_sql) %>%
        mutate(across(starts_with("notificacion"), ~ as.logical(as.numeric(.))))
    return(res)
}

get_retos <- function() {
    consulta_sql <- "
        SELECT
            id_reto,
            nombre_reto,
            lower(duracion_reto) as fecha_inicio,
            upper(duracion_reto) - 1 as fecha_fin,
            reto_activo
        FROM
            tbl_retos
        ORDER BY fecha_inicio DESC"
    res <- dbGetQuery(con, consulta_sql) %>%
        mutate(reto_activo = as.logical(as.numeric(reto_activo)))
    return(res)
}

get_actividades <- function(reto) {
    consulta_sql <- glue_sql("
        SELECT
            id_actividad,
            actividad,
            lower(tiempo_actividad) as inicio,
            upper(tiempo_actividad) as fin,
            extract(EPOCH from (upper(tiempo_actividad) - lower(tiempo_actividad))) / 60 as duracion,
            tema_actividad,
            coach_expositor
        FROM
            tbl_actividades
        WHERE
            nombre_reto = {reto}
        ORDER BY 
            inicio DESC", .con = con)
    res <- dbGetQuery(con, consulta_sql) %>% 
        mutate(across(where(is.POSIXct), list('mostrar' = ~ format(., '%d/%m/%Y %I:%M %p'))))
    return(res)
}

get_actividades_resumen <- function() {
    consulta_sql <- glue_sql("
        SELECT
            tr.id_reto,
            actividad,
            lower(tiempo_actividad) as inicio,
            upper(tiempo_actividad) as fin,
            tema_actividad,
            coach_expositor
        FROM
            tbl_actividades ta INNER JOIN tbl_retos tr on ta.nombre_reto = tr.nombre_reto
        WHERE
            reto_activo = TRUE", .con = con)
    res <- dbGetQuery(con, consulta_sql) %>% 
        rename(calendarId = id_reto, title = actividad, start = inicio, end = fin) %>% 
        mutate(start = start + hours(5),
               end = end + hours(5),
               body = glue("{tema_actividad} - ({coach_expositor})"),
               category = 'time') %>% 
        select(calendarId, title, body, start, end, category)
    return(res)
}

get_participaciones <- function(reto) {
    consulta_sql <- glue_sql("
        SELECT
            id_participacion,
            tp.nombre_retador,
            objetivo_participacion,
            tr.nombre_coach,
            tr.num_celular_retador
            
        FROM
            tbl_participacion tp INNER JOIN tbl_retadores tr ON tp.nombre_retador = tr.nombre_retador
        WHERE
            nombre_reto = {reto}
        ORDER BY tp.nombre_retador", .con = con)
    res <- dbGetQuery(con, consulta_sql)
    return(res)
}

get_retadores <- function() {
    consulta_sql <- glue_sql("
        SELECT 
            nombre_retador,
            nombre_coach
        FROM tbl_retadores
        ORDER BY
            nombre_retador", .con = con)
    res <- dbGetQuery(con, consulta_sql)
    return(res)
}

get_retadores_reto <- function(reto) {
    consulta_sql <- glue_sql("
        select 
            tp.nombre_retador,
            tr.num_celular_retador,
        	nombre_coach
        from tbl_participacion tp inner join tbl_retadores tr on tp.nombre_retador = tr.nombre_retador 
        where nombre_reto = {reto}
        order by tp.nombre_retador", .con = con)
    res <- dbGetQuery(con, consulta_sql) %>% 
        mutate(lista = nombre_retador %>% setNames(paste(nombre_retador, "-", num_celular_retador)))
    return(res)
}

get_id_participacion <- function (reto, retador) {
    consulta_sql <- glue_sql("
        select 
        	id_participacion
        from tbl_participacion tp
        where nombre_retador = {retador} and nombre_reto = {reto}",
        .con = con)
    res <- dbGetQuery(con, consulta_sql) %>% pull(id_participacion)
    return(res)
}

get_indicadores <- function(seleccion) {
    consulta_sql <- "
            with 
            	habitos as (
            		select count(*) as num_habitos
            		from tbl_habitos th
            	),
            	actividades as (
            		select
            			tr.nombre_reto,
            			count(*) as num_actividades
            		from tbl_retos tr
            			left join tbl_actividades ta on ta.nombre_reto = tr.nombre_reto
            		where lower(ta.tiempo_actividad)::date <= current_date
                        and ta.actividad::varchar != 'Ceremonia'
            		group by tr.nombre_reto, tr.reto_activo
            		having tr.reto_activo = true
            	),
            	retadores as (
            		select 
            			tr.id_reto,
            			tp.nombre_reto,
            			count(nombre_retador) as num_retadores,
            			case 
            				when current_date < upper(tr.duracion_reto) then current_date - lower(tr.duracion_reto) + 1
                			else upper(tr.duracion_reto) - lower(tr.duracion_reto)
                		end num_dias
            		from
            			tbl_participacion tp 
            			inner join tbl_retos tr on tp.nombre_reto = tr.nombre_reto 
            		group by tr.id_reto, tp.nombre_reto, tr.duracion_reto, tr.reto_activo
            		having tr.reto_activo = true
            	),
            	registro_habitos as (
            		select 
            			tp.nombre_reto,
            			count(*) as cta_habitos
            		from tbl_registros_habitos trh 
            			left join tbl_participacion tp on trh.id_participacion = tp.id_participacion
            			left join tbl_retos tr on tp.nombre_reto = tr.nombre_reto 
            		group by tp.nombre_reto, reto_activo
            		having tr.reto_activo = true
            	),
            	registro_actividades as (
            		select 
            			tp.nombre_reto,
            			count(*) as cta_actividades
            		from tbl_registros_actividades tra inner join
            			tbl_participacion tp on tra.id_participacion = tp.id_participacion
            			left join tbl_actividades ta on tra.id_actividad = ta.id_actividad
            			left join tbl_retos tr on tp.nombre_reto = tr.nombre_reto
            		group by tp.nombre_reto, tr.reto_activo
            		having tr.reto_activo = true
            	)
            select
            	r.id_reto,
            	r.nombre_reto as \"Reto\",
                r.num_retadores as \"Retadores\",
            	round(cta_habitos / (num_retadores * num_dias * num_habitos)::numeric, 4) as \"Hábitos\",
            	round(cta_actividades / (num_actividades * num_retadores)::numeric, 4) as \"Actividades\"
            from retadores r 
            	cross join habitos h
            	full join actividades a on r.nombre_reto = a.nombre_reto
            	full join registro_habitos rh on r.nombre_reto = rh.nombre_reto
            	full join registro_actividades ra on r.nombre_reto = ra.nombre_reto"
    res <- dbGetQuery(con, consulta_sql) %>%
        filter(id_reto %in% seleccion) %>% 
        mutate(across(c(Hábitos, Actividades), .fns = ~ scales::percent(., accuracy = 0.01)),
               Retadores = as.integer(Retadores)) %>% 
        select(-id_reto)
    return(res)
}

to_bin <- function(raw) {
    as.raw(strtoi(substring(raw, seq(1,nchar(raw), by=2),
                            seq(2,nchar(raw), by=2)),
                  base=16))
}

consulta_sql <- "
SELECT
    type.typname AS variable,
    enum.enumlabel AS valor
FROM pg_enum AS enum
JOIN pg_type AS type
    ON (type.oid = enum.enumtypid)
GROUP BY 
    enum.enumlabel,
    type.typname"

enum <- dbGetQuery(con, consulta_sql)
