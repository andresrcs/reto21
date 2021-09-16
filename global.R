# LIBRERIAS ####################################################################
library(shiny)
library(shinythemes)
library(shinyauthr)
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
        	nombre_coach
        from tbl_participacion tp inner join tbl_retadores tr on tp.nombre_retador = tr.nombre_retador 
        where nombre_reto = {reto}
        order by tp.nombre_retador", .con = con)
    res <- dbGetQuery(con, consulta_sql)
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
