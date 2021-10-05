source("global.R")
source("tabs.R")
ui <- navbarPage(title = "RETO-21",
                 id = "tabs",
                 theme = shinytheme("cerulean"),
                 collapsible = TRUE,
                 login_tab
)

server <- function(input, output, session) {
    # Check database connection ################################################
    
    if (!dbIsValid(con)) {
        
        con <- dbConnect(odbc::odbc(),
                         .connection_string = "Driver={PostgreSQL ANSI};Uid=ubuntu;Database=herbalife;", 
                         timeout = 10)
        
        dbSendStatement(con, "SET search_path = reto21")
        
    }
    
    # Update functions #########################################################
    update_listas_retos <- function() {
        
        lista_retos <- get_retos() %>%
            filter(reto_activo) %>%
            pull(nombre_reto)
        
        lista_retos_named <- get_retos() %>%
            filter(reto_activo) %>% 
            mutate(lista = id_reto %>% setNames(nombre_reto)) %>% 
            pull(lista)
        
        updateSelectInput(session = session,
                          inputId = "reto_participacion",
                          choices = c("", lista_retos))

        updateSelectInput(session = session,
                          inputId = "reto_parametros",
                          choices = c("", lista_retos))
        
        updateSelectInput(session = session,
                          inputId = "reto_habito",
                          choices = c("", lista_retos))
        
        updateSelectInput(session = session,
                          inputId = "reto_reg_actividad",
                          choices = c("", lista_retos))
        
        updateSelectInput(session = session,
                          inputId = "reto_foto",
                          choices = c("", lista_retos))
        
        updateSelectInput(session = session,
                          inputId = "reto_calificacion",
                          choices = c("", lista_retos))
        
        updateSelectInput(session = session,
                          inputId = "reto_resultados",
                          choices = c("", lista_retos))
        
        updateSelectInput(session = session,
                          inputId = "reto_actividad",
                          choices = c("", lista_retos))
        
        updateCheckboxGroupInput(session = session,
                                 inputId = "calendarId",
                                 choices = lista_retos_named,
                                 selected = lista_retos_named)
    }
    
    update_lista_coaches <- function() {
        coach_expositor.Types(
            get_coaches() %>%
                filter(permiso != 'Inactivo') %>%
                pull(nombre_coach)
        )
    }
    
    update_listas_retadores <- function() {
        nombre_retador.Types(if (isolate(credentials()$info$permiso) != 'Administrador') {
            get_retadores() %>%
                filter(nombre_coach == isolate(credentials()$info$nombre_coach)) %>%
                pull(nombre_retador)
        } else {
            get_retadores() %>%
                pull(nombre_retador)
        })
        
        updateSelectInput(session = session,
                          'reto_parametros',
                          selected = ""
        )
        
        updateSelectInput(session = session,
                          'reto_habito',
                          selected = ""
        )
        
        updateSelectInput(session = session,
                          'reto_reg_actividad',
                          selected = ""
        )
        
        updateSelectInput(session = session,
                          'reto_foto',
                          selected = ""
        )
        
        updateSelectInput(session = session,
                          'reto_calificacion',
                          selected = ""
        )
        
        updateSelectInput(session = session,
                          'reto_resultados',
                          selected = ""
        )
    }
    
    update_resumen <- function() {
        output$resumen <- renderCalendar({
            calendar <- get_retos() %>%
                filter(reto_activo) %>% 
                rename(calendarId = id_reto, title = nombre_reto, start = fecha_inicio, end = fecha_fin) %>% 
                mutate(start = start + hours(5),
                       end = end + hours(29),
                       body = NA_character_,
                       category = 'time') %>% 
                select(calendarId, title, body, start, end, category) %>% 
                bind_rows(get_actividades_resumen())
            
            colors <- brewer.pal(max(n_groups(calendar %>% group_by(calendarId)), 3), "Accent")[group_indices(calendar %>% group_by(calendarId))]
            
            calendar %>% 
                mutate(bgColor = colors,
                       color = 'white',
                       borderColor = colors) %>% 
                calendar(useNavigation = TRUE, view = "month")%>%
                cal_month_options(
                    startDayOfWeek = 1,
                    daynames = c("Dom", "Lun", "Mar", "Mie", "Jue", "Vie", "Sab")
                ) %>%
                cal_week_options(
                    startDayOfWeek = 1,
                    daynames = c("Dom", "Lun", "Mar", "Mie", "Jue", "Vie", "Sab")
                ) %>% 
                cal_timezone(
                    timezoneName = "America/Lima",
                    displayLabel = "GMT-05:00",
                    tooltip = "Lima"
                )
        })
        
        updateCheckboxGroupInput(session = session,
                                 inputId = "calendarId",
                                 selected = get_retos() %>%
                                     filter(reto_activo) %>% 
                                     mutate(lista = id_reto %>% setNames(nombre_reto)) %>% 
                                     pull(lista)
                                 )
        
        output$indicadores <- renderTable({
            get_indicadores(input$calendarId)
        })
    }
    
    update_calendar <- function() {
        output$calendario_actividades <- renderCalendar({
            req(input$reto_actividad)
            
            actividades_reto <- get_actividades(reto = isolate(input$reto_actividad))
            
            colors <- brewer.pal(max(n_groups(actividades_reto %>% group_by(actividad)), 3), "Accent")[group_indices(actividades_reto %>% group_by(actividad))]
            
            actividades_reto %>% 
                rename(title = actividad,
                       body = tema_actividad, start = inicio, end = fin) %>% 
                mutate(calendarId = title,
                       start = start + hours(5),
                       end = end + hours(5),
                       bgColor = colors,
                       color = 'white',
                       borderColor = colors,
                       category = "time",
                       body = glue("{body} - ({coach_expositor})")) %>% 
                calendar(useNavigation = TRUE, view = "month") %>%
                cal_month_options(
                    startDayOfWeek = 1,
                    daynames = c("Dom", "Lun", "Mar", "Mie", "Jue", "Vie", "Sab")
                ) %>%
                cal_week_options(
                    startDayOfWeek = 1,
                    daynames = c("Dom", "Lun", "Mar", "Mie", "Jue", "Vie", "Sab")
                ) %>% 
                cal_timezone(
                    timezoneName = "America/Lima",
                    displayLabel = "GMT-05:00",
                    tooltip = "Lima"
                )
        })
    }
    
    # Lo-gin logic ##############################################################
    # Add the logout button to the navbar on app launch 
    insertUI(
        selector = ".navbar .container-fluid .navbar-collapse",
        ui = tags$ul(
            class="nav navbar-nav navbar-right",
            tags$li(
                div(
                    style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
                    shinyauthr::logoutUI("logout")
                )
            )
        )
    )
    # call the shinyauthr login and logout server modules
    credentials <- shinyauthr::loginServer(
        id = "login",
        data = get_coaches() %>% filter(permiso != 'Inactivo'),
        user_col = user_coach,
        pwd_col = password,
        sodium_hashed = TRUE,
        reload_on_logout = TRUE,
        log_out = reactive(logout_init())
    )
    
    # call the log out module with reactive trigger to hide/show
    logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
    )
    
    # Tab Rendering ############################################################
    observeEvent(credentials()$user_auth, {
        # if user logs in successfully
        if (credentials()$user_auth) { 
            # remove the login tab
            removeTab("tabs", "login")
            appendTab("tabs", resumen, select = TRUE)
            appendTab("tabs", retadores)
            appendTab("tabs", participaciones)
            appendTab("tabs", navbarMenu("Registros",
                                         parametros,
                                         fotos,
                                         habitos,
                                         reg_actividades)
                      )
            appendTab("tabs", calificaciones)
            appendTab("tabs", resultados)
            # render tab depending on permissions
            if (credentials()$info$permiso == "Administrador") {
                appendTab("tabs", navbarMenu("Configuración",
                                             actividades,
                                             retos,
                                             coaches,
                                             pesos
                                             )
                          )
            } else if (credentials()$info$permiso == "Coach") {
                appendTab("tabs", mi_cuenta)
            }
        }
    })
    
    # Tab Resumen #############################################################
    update_resumen()
    
    observeEvent(
        input$view_resumen,
        cal_proxy_view("resumen", input$view_resumen),
        ignoreInit = TRUE
    )
    
    observeEvent(input$calendarId, {
        calendars <- get_retos() %>%
            filter(reto_activo) %>% 
            mutate(lista = id_reto %>% setNames(nombre_reto)) %>% 
            pull(lista)
        cal_proxy_toggle("resumen", input$calendarId, toHide = FALSE)
        cal_proxy_toggle("resumen", setdiff(calendars, input$calendarId), toHide = TRUE)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    output$ics_calendario <- downloadHandler(
        filename = paste0("calendario-", Sys.Date(), ".ics"),
        content = function(file) {
            consulta_sql <- "
            select
            	tr.id_reto,
                actividad as \"SUMMARY\",
                tr.nombre_reto || '\\n - ' || tema_actividad || '\\n - ' || ta.coach_expositor as \"DESCRIPTION\",
            	lower(tiempo_actividad) as \"DTSTART\",
            	upper(tiempo_actividad) as \"DTEND\",
            	'Zoom' as \"LOCATION\",
                tr.nombre_reto || '-' || actividad || '-' || lower(tiempo_actividad) as \"UID\"
            from tbl_actividades ta inner join tbl_retos tr on ta.nombre_reto = tr.nombre_reto
            where tr.reto_activo = true"
            res <- dbGetQuery(con, consulta_sql) %>% 
                filter(id_reto %in% input$calendarId) %>%
                select(-id_reto)
            
            ic_write(ical(res), file)
        }
    )
    
    # Tab Coaches ##############################################################
    coaches_insert_callback <- function(data, row) {
        data <- data %>% 
            mutate(across(everything(), ~ ifelse(. == "", NA, .)))
        
        sql_query <- glue_sql(
            "INSERT INTO tbl_coaches (nombre_coach, user_coach, password,
                num_celular_coach, email_coach, permiso, notificacion_correo) VALUES
                ({data[row,]$nombre_coach},
                {data[row,]$user_coach},
                {case_when(
                    str_detect(data[row,]$password, '^[a-zA-Z0-9!¡/@#$¿?%^&*\"\\\\[\\\\]\\\\{\\\\}<>\\\\(\\\\)=\\\\-_´+`~:;,.€\\\\|]+$') ~ sodium::password_store(data[row,]$password),
                    TRUE ~ data[row,]$password)},
                {data[row,]$num_celular_coach},
                {data[row,]$email_coach},
                {data[row,]$permiso},
                {data[row,]$notificacion_correo})",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_lista_coaches()
        return(get_coaches())
    }
    
    coaches_delete_callback <- function(data, row) {
        sql_query <- glue_sql(
            "DELETE FROM tbl_coaches WHERE id_coach = {data[row,]$id_coach}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_lista_coaches()
        return(get_coaches())
    }
    
    coaches_update_callback <- function(data, olddata, row) {
        data <- data %>% 
            mutate(across(everything(), ~ ifelse(. == "", NA, .)))
        sql_query <- glue_sql(
            "UPDATE tbl_coaches SET
                nombre_coach = {data[row,]$nombre_coach},
                user_coach = {data[row,]$user_coach},
                password = {case_when(
                                str_detect(data[row,]$password, '^\\\\$7\\\\$C6\\\\.\\\\.\\\\.\\\\./\\\\.\\\\.\\\\.\\\\.') ~ data[row,]$password,
                                str_detect(data[row,]$password, '^[a-zA-Z0-9!¡/@#$¿?%^&*\"\\\\[\\\\]\\\\{\\\\}<>\\\\(\\\\)=\\\\-_´+`~:;,.€\\\\|]+$') ~ sodium::password_store(data[row,]$password),
                                TRUE ~ data[row,]$password)},
                num_celular_coach = {data[row,]$num_celular_coach},
                email_coach = {data[row,]$email_coach},
                permiso = {data[row,]$permiso},
                notificacion_correo = {data[row,]$notificacion_correo}
            WHERE id_coach = {data[row,]$id_coach}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_lista_coaches()
        return(get_coaches())
    }
    
    tbl_coaches <- get_coaches()
    
    coaches_result <- dtedit(input, output,
                            name = 'coaches',
                            thedata = tbl_coaches,
                            edit.cols = tail(names(tbl_coaches), -1),
                            edit.label.cols = c('Nombre', 'Usuario', 'Password',
                                                'Celular', 'email', 'Permiso',
                                                'Notificación email'),
                            input.types = c('permiso' = 'selectInput'),
                            input.choices = list('permiso' = enum$valor[enum$variable == 'permisos_coach']),
                            inputEvent = list(
                                nombre_coach = function(x, value) {
                                    if (value == '') {
                                        shinyFeedback::hideFeedback(x)
                                        shinyFeedback::showFeedbackDanger(
                                            inputId = x,
                                            text = "Campo obligatorio"
                                        )
                                    } else if (value != '' && !str_detect(value, "^.{3,}\\s.{3}")) {
                                        shinyFeedback::hideFeedback(x)
                                        shinyFeedback::showFeedbackWarning(
                                            inputId = x,
                                            text = "Nombre inválido, ingrese como mínio un nombre y un apellido"
                                        )
                                    } else {
                                        shinyFeedback::hideFeedback(x)
                                    }
                                },
                                user_coach = function(x, value) {
                                    if (value == '') {
                                        shinyFeedback::hideFeedback(x)
                                        shinyFeedback::showFeedbackDanger(
                                            inputId = x,
                                            text = "Campo obligatorio"
                                        )
                                    } else if (value != '' && !str_detect(value, '^[a-zA-Z0-9!¡/@#$¿?%^&*"\\[\\]\\{\\}<>\\(\\)=\\-_´+`~:;,.€\\|]{4,}$')) {
                                        shinyFeedback::hideFeedback(x)
                                        shinyFeedback::showFeedbackWarning(
                                            inputId = x,
                                            text = "El usuario debe contener 4 caracteres como mínimo y no contener espacios"
                                        )
                                    } else {
                                        shinyFeedback::hideFeedback(x)
                                    }
                                },
                                password = function(x, value) {
                                    if (value == '') {
                                        shinyFeedback::hideFeedback(x)
                                        shinyFeedback::showFeedbackDanger(
                                            inputId = x,
                                            text = "Campo obligatorio"
                                        )
                                    } else if (value != '' && !str_detect(value, '^[a-zA-Z0-9!¡/@#$¿?%^&*"\\[\\]\\{\\}<>\\(\\)=\\-_´+`~:;,.€\\|]+$')) {
                                        shinyFeedback::hideFeedback(x)
                                        shinyFeedback::showFeedbackWarning(
                                            inputId = x,
                                            text = "El password no puede estar vacio o contener espacios"
                                        )
                                    } else {
                                        shinyFeedback::hideFeedback(x)
                                    }
                                },
                                num_celular_coach = function(x, value) {
                                    if (value != "" && !str_detect(value, '^\\d{9}$')) {
                                        shinyFeedback::showFeedbackWarning(
                                            inputId = x,
                                            text = "Número de celular inválido, el campo debe contener 9 números"
                                        )
                                    } else {
                                        shinyFeedback::hideFeedback(x)
                                    }
                                },
                                email_coach = function(x, value) {
                                    if (value != "" && !str_detect(value, '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$')) {
                                        shinyFeedback::showFeedbackWarning(
                                            inputId = x,
                                            text = "Email inválido"
                                        )
                                    } else {
                                        shinyFeedback::hideFeedback(x)
                                    }
                                }
                            ),
                            view.cols = c('nombre_coach', 'user_coach', 'permiso'),
                            delete.info.label.cols = c('Nombre', 'Usuario', 'Permiso'),
                            show.copy = FALSE, 
                            title.delete = "Eliminar Coach",
                            title.edit = "Modificar Informacion de Coach",
                            title.add = "Agregar Coach",
                            label.add = "Agregar",
                            label.edit = "Modificar",
                            label.delete = "Eliminar",
                            label.cancel = "Cancelar",
                            label.save = "Guardar",
                            text.delete.modal = "¿Está seguro de que quiere eliminar este coach? Sólo se puede eliminar coaches que no hayan guardado registros",
                            callback.insert = coaches_insert_callback,
                            callback.delete = coaches_delete_callback,
                            callback.update = coaches_update_callback,
                            icon.add = shiny::icon("user-plus"),
                            icon.delete = shiny::icon("trash"), 
                            icon.edit = shiny::icon("edit"),
                            datatable.call = function(...) {
                                DT::datatable(...) %>%
                                    formatStyle(
                                        'nombre_coach',
                                        fontWeight = 'bold'
                                    )
                            },
                            datatable.options = list(columns = list(
                                list(title = 'Nombre'),
                                list(title = 'Usuario'),
                                list(title = 'Permiso'))
                            )
    )
    
    # Tab Mi Cuenta ############################################################
    output$datos_coach <- renderUI({
        tagList(
            h1(credentials()$info$nombre_coach),
            textInput("user_coach", "Usuario:", value = credentials()$info$user_coach),
            textInput("password", "Password:", value = credentials()$info$password),
            textInput("num_celular_coach", "Celular:", value = credentials()$info$num_celular_coach),
            textInput("email_coach", "email:", value = credentials()$info$email_coach),
            checkboxInput("notificacion_correo", "Notificación Correo:", value = as.logical(as.numeric(credentials()$info$notificacion_correo))),
            actionButton("guardar_coach", "Guardar", icon = icon("save"))
        )
    })
    
    observeEvent(input$user_coach, {
        value <- input$user_coach
        if (value == '') {
            shinyFeedback::hideFeedback("user_coach")
            shinyFeedback::showFeedbackDanger(
                inputId = "user_coach",
                text = "Campo obligatorio"
            )
        } else if (value != '' && !str_detect(value, '^[a-zA-Z0-9!¡/@#$¿?%^&*"\\[\\]\\{\\}<>\\(\\)=\\-_´+`~:;,.€\\|]{4,}$')) {
            shinyFeedback::hideFeedback("user_coach")
            shinyFeedback::showFeedbackWarning(
                inputId = "user_coach",
                text = "El usuario debe contener 4 caracteres como mínimo y no contener espacios"
            )
        } else {
            shinyFeedback::hideFeedback("user_coach")
        }
    })
    
    observeEvent(input$password, {
        value <- input$password
        if (value == '') {
            shinyFeedback::hideFeedback("password")
            shinyFeedback::showFeedbackDanger(
                inputId = "password",
                text = "Campo obligatorio"
            )
        } else if (value != '' && !str_detect(value, '^[a-zA-Z0-9!¡/@#$¿?%^&*"\\[\\]\\{\\}<>\\(\\)=\\-_´+`~:;,.€\\|]+$')) {
            shinyFeedback::showFeedbackWarning(
                inputId = "password",
                text = "El password no puede estar vacio o contener espacios"
            )
        } else {
            shinyFeedback::hideFeedback("password")
        }
    })
    
    observeEvent(input$num_celular_coach, {
        value <- input$num_celular_coach
        if (value != "" && !str_detect(value, '^\\d{9}$')) {
            shinyFeedback::showFeedbackWarning(
                inputId = "num_celular_coach",
                text = "Número de celular inválido, el campo debe contener 9 números"
            )
        } else {
            shinyFeedback::hideFeedback("num_celular_coach")
        }
    })
    
    observeEvent(input$email_coach, {
        value <- input$email_coach
        if (value != "" && !str_detect(value, '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$')) {
            shinyFeedback::showFeedbackWarning(
                inputId = "email_coach",
                text = "Email inválido"
            )
        } else {
            shinyFeedback::hideFeedback("email_coach")
        }
    })
    
    observeEvent(input$guardar_coach, {
        sql_query <- glue_sql(
            "UPDATE tbl_coaches SET
                user_coach = {input$user_coach},
                password = {case_when(
                                str_detect(input$password, '^\\\\$7\\\\$C6\\\\.\\\\.\\\\.\\\\./\\\\.\\\\.\\\\.\\\\.') ~ input$password,
                                str_detect(input$password, '^[a-zA-Z0-9!¡/@#$¿?%^&*\"\\\\[\\\\]\\\\{\\\\}<>\\\\(\\\\)=\\\\-_´+`~:;,.€\\\\|]+$') ~ sodium::password_store(input$password),
                                TRUE ~ input$password)},
                num_celular_coach = {if_else(input$num_celular_coach != '', input$num_celular_coach, NA_character_)},
                email_coach = {if_else(input$email_coach != '', input$email_coach, NA_character_)},
                notificacion_correo = {input$notificacion_correo}
            WHERE id_coach = {credentials()$info$id_coach}",
            .con = con
        )
        success <- tryCatch({
            dbSendQuery(con, sql_query)
            TRUE
            },
            error = function(cond) {
                showNotification(paste(cond), duration = 3, closeButton = TRUE, type = "error")
                return(FALSE)
                }
            )
        
        sql_query <- glue_sql(
            "SELECT *
                FROM tbl_coaches
                WHERE id_coach = {credentials()$info$id_coach}")
        datos_coach <- dbGetQuery(con, sql_query) %>%
            mutate(across(starts_with("notificacion"), ~ as.logical(as.numeric(.))))
        
        updateTextInput(session, inputId = "user_coach", value = datos_coach$user_coach)
        updateTextInput(session, inputId = "password", value = datos_coach$password)
        updateTextInput(session, inputId = "num_celular_coach", value = datos_coach$num_celular_coach)
        updateTextInput(session, inputId = "email_coach", value = datos_coach$email_coach)
        updateCheckboxInput(session, inputId = "notificacion_correo", value = datos_coach$notificacion_correo)
        
        if (success) {
            showNotification("Modificaciones guardadas", duration = 3, closeButton = TRUE, type = "message")
        }
    })
    
    # Tab Retadores ############################################################
    get_retadores <- function() {
        consulta_sql <- "
          SELECT *
          FROM
            tbl_retadores
          ORDER BY nombre_retador"
        res <- dbGetQuery(con, consulta_sql)
        return(res)
    }
    
    retadores_insert_callback <- function(data, row) {
        data <- data %>% 
            mutate(across(where(is.character), ~ ifelse(. == "", NA, .)),
                   across(where(is.numeric),  ~ ifelse(. == 0, NA, .)))
        
        sql_query <- glue_sql(
            "INSERT INTO tbl_retadores (nombre_retador, fecha_nacimiento, sexo,
                num_celular_retador, talla, nombre_coach) VALUES
                ({data[row,]$nombre_retador},
                {data[row,]$fecha_nacimiento},
                {data[row,]$sexo},
                {data[row,]$num_celular_retador},
                {as.numeric(data[row,]$talla)},
                {credentials()$info$nombre_coach})",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_listas_retadores()
        updateSelectInput(session = session,
                          'reto_participacion',
                          selected = ""
        )
        return(get_retadores())
    }
    
    retadores_delete_callback <- function(data, row) {
        sql_query <- glue_sql(
            "DELETE FROM tbl_retadores WHERE id_retador = {data[row,]$id_retador}",
            .con = con
        )
        
        tryCatch(
            {
                if (data[row,]$nombre_coach == isolate(credentials()$info$nombre_coach) | isolate(credentials()$info$permiso == 'Administrador')) {
                    dbSendQuery(con, sql_query)
                } else {
                    stop("No puede eliminar retadores de otros coach")
                }
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_listas_retadores()
        updateSelectInput(session = session,
                          'reto_participacion',
                          selected = ""
        )
        return(get_retadores())
    }
    
    retadores_update_callback <- function(data, olddata, row) {
        data <- data %>% 
            mutate(across(where(is.character), ~ ifelse(. == "", NA, .)),
                   across(where(is.numeric),  ~ ifelse(. == 0, NA, .)))
        sql_query <- glue_sql(
            "UPDATE tbl_retadores SET
                nombre_retador = {data[row,]$nombre_retador},
                fecha_nacimiento = {data[row,]$fecha_nacimiento},
                sexo = {data[row,]$sexo},
                num_celular_retador = {data[row,]$num_celular_retador},
                talla = {as.numeric(data[row,]$talla)}
            WHERE id_retador = {data[row,]$id_retador}",
            .con = con
        )
        
        tryCatch(
            {
                if (data[row,]$nombre_coach == isolate(credentials()$info$nombre_coach) | isolate(credentials()$info$permiso == 'Administrador')) {
                    dbSendQuery(con, sql_query)
                } else {
                    stop("No puede editar retadores de otros coach")
                }
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_listas_retadores()
        updateSelectInput(session = session,
                          'reto_participacion',
                          selected = ""
        )
        return(get_retadores())
    }
    
    tbl_retadores <- get_retadores()
    
    retadores_result <- dtedit(input, output,
                             name = 'retadores',
                             thedata = tbl_retadores,
                             edit.cols = c('nombre_retador', 'fecha_nacimiento', 'sexo',
                                           'num_celular_retador', 'talla'),
                             edit.label.cols = c('Nombres y Apellidos:', 'Fecha Nacimiento:', 'Sexo:',
                                                 'Celular:', 'Talla (m):'),
                             input.types = c('fecha_nacimiento' = 'dateInput',
                                             'sexo' = 'selectInput',
                                             'talla' = 'numericInput'),
                             input.choices = list('sexo' = enum$valor[enum$variable == 'sexos']),
                             view.cols = c('nombre_retador', 'num_celular_retador', 'nombre_coach'),
                             delete.info.label.cols = c('Nombre', 'Celular', 'Coach'),
                             show.copy = FALSE,
                             inputEvent = list(
                                 nombre_retador = function(x, value) {
                                     if (value == '') {
                                         shinyFeedback::hideFeedback(x)
                                         shinyFeedback::showFeedbackDanger(
                                             inputId = x,
                                             text = "Campo obligatorio"
                                         )
                                     } else if (value != '' && !str_detect(value, "^.{3,}\\s.{3}")) {
                                         shinyFeedback::hideFeedback(x)
                                         shinyFeedback::showFeedbackWarning(
                                             inputId = x,
                                             text = "Nombre inválido, ingrese como mínio un nombre y un apellido"
                                         )
                                     } else {
                                         shinyFeedback::hideFeedback(x)
                                     }
                                 },
                                 talla = function(x, value) {
                                     if (!is.na(value) && (value < 0 || value > 2)) {
                                         shinyFeedback::showToast("error", "Valor de talla inválido (talla debe estar en metros y ser un número positivo entre 0 y 2)")
                                         shiny::updateNumericInput(
                                             session = shiny::getDefaultReactiveDomain(),
                                             inputId = x,
                                             value = NA
                                         )
                                     }
                                 },
                                 num_celular_retador = function(x, value) {
                                     if (value == '') {
                                         shinyFeedback::hideFeedback(x)
                                         shinyFeedback::showFeedbackDanger(
                                             inputId = x,
                                             text = "Campo obligatorio"
                                         )
                                     } else if (value != '' && !str_detect(value, '^\\d{9}$')) {
                                         shinyFeedback::hideFeedback(x)
                                         shinyFeedback::showFeedbackWarning(
                                             inputId = x,
                                             text = "Número de celular inválido, el campo debe contener 9 números"
                                         )
                                     } else {
                                         shinyFeedback::hideFeedback(x)
                                     }
                                 },
                                 fecha_nacimiento = function(x, value) {
                                     if (as.numeric(difftime(Sys.Date(), as.Date(value))) / 360 < 18) {
                                         shinyFeedback::showFeedbackWarning(
                                             inputId = x,
                                             text = "Menor de 18 años"
                                         )
                                     } else {
                                         shinyFeedback::hideFeedback(x)
                                     }
                                 }
                                 ),
                             title.delete = "Eliminar Retador",
                             title.edit = "Modificar Informacion de Retador",
                             title.add = "Agregar Retador",
                             label.add = "Agregar",
                             label.edit = "Modificar",
                             label.delete = "Eliminar",
                             label.cancel = "Cancelar",
                             label.save = "Guardar",
                             text.delete.modal = "¿Está seguro de que quiere eliminar este retador? Sólo se puede eliminar retadores que no tengan registros",
                             callback.insert = retadores_insert_callback,
                             callback.delete = retadores_delete_callback,
                             callback.update = retadores_update_callback,
                             icon.add = shiny::icon("user-plus"),
                             icon.delete = shiny::icon("trash"), 
                             icon.edit = shiny::icon("edit"),
                             datatable.call = function(...) {
                                 DT::datatable(...) %>%
                                     formatStyle(
                                         'nombre_retador',
                                         fontWeight = 'bold'
                                     )
                             },
                             datatable.options = list(columns = list(
                                 list(title = 'Nombres y Apellidos'),
                                 list(title = 'Celular'),
                                 list(title = 'Coach'))
                             )
    )
    
    # Tab Retos ################################################################
    retos_insert_callback <- function(data, row) {
        sql_query <- glue_sql(
            "INSERT INTO tbl_retos (nombre_reto, duracion_reto) VALUES
                ({paste0('Reto del ', format(as.Date(data[row,]$fecha_inicio), '%d/%m/%Y'), ' al ', format(as.Date(data[row,]$fecha_inicio) + 20, '%d/%m/%Y'))},
                {paste0('[', data[row,]$fecha_inicio, ',', as.Date(data[row,]$fecha_inicio) + 20, ']')})",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(cond)
            }
        )
        update_resumen()
        update_listas_retos()
        return(get_retos())
    }
    
    retos_delete_callback <- function(data, row) {
        sql_query <- glue_sql(
            "DELETE FROM tbl_retos WHERE id_reto = {data[row,]$id_reto}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(cond)
            }
        )
        update_resumen()
        update_listas_retos()
        return(get_retos())
    }
    
    retos_update_callback <- function(data, olddata, row) {
        sql_query <- glue_sql(
            "UPDATE tbl_retos SET
                nombre_reto = {paste0('Reto del ', format(as.Date(data[row,]$fecha_inicio), '%d/%m/%Y'), ' al ', format(as.Date(data[row,]$fecha_inicio) + 20, '%d/%m/%Y'))},
                duracion_reto = {paste0('[', data[row,]$fecha_inicio, ',', as.Date(data[row,]$fecha_inicio) + 20, ']')},
                reto_activo = {data[row,]$reto_activo}
            WHERE id_reto = {data[row,]$id_reto}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(cond)
            }
        )
        update_resumen()
        update_listas_retos()
        return(get_retos())
    }
    
    tbl_retos <- get_retos()
    
    retos_result <- dtedit(input, output,
                             name = 'retos',
                             thedata = tbl_retos,
                             edit.cols = c('fecha_inicio', 'reto_activo'),
                             edit.label.cols = c('Fecha Inicio', 'Activo?'),
                             input.types = c('fecha_inicio' = 'dateInput'),
                             view.cols = c('nombre_reto', 'fecha_inicio', 'fecha_fin', 'reto_activo'),
                             delete.info.label.cols = c('Nombre', 'Fecha Inicio', 'Fecha Fin', 'Activo?'),
                             show.copy = FALSE, 
                             title.delete = "Eliminar Reto",
                             title.edit = "Modificar Informacion de Reto",
                             title.add = "Agregar Reto",
                             label.add = "Agregar",
                             label.edit = "Modificar",
                             label.delete = "Eliminar",
                             label.cancel = "Cancelar",
                             label.save = "Guardar",
                             text.delete.modal = "¿Está seguro de que quiere eliminar este reto? Sólo se puede eliminar retos que no tengan registros",
                             callback.insert = retos_insert_callback,
                             callback.delete = retos_delete_callback,
                             callback.update = retos_update_callback,
                             icon.add = shiny::icon("plus"),
                             icon.delete = shiny::icon("trash"), 
                             icon.edit = shiny::icon("edit"),
                             datatable.call = function(...) {
                                 DT::datatable(...) %>%
                                     formatStyle(
                                         'nombre_reto',
                                         fontWeight = 'bold'
                                     )
                             },
                             datatable.options = list(columns = list(
                                 list(title = 'Nombre'),
                                 list(title = 'Fecha Inicio'),
                                 list(title = 'Fecha Fin'),
                                 list(title = 'Activo?'))
                             )
    )
    
    # Tab Actividades ##########################################################
    actividades_insert_callback <- function(data, row) {
        sql_query <- glue_sql(
            "INSERT INTO tbl_actividades (nombre_reto, actividad, tiempo_actividad,
                tema_actividad, coach_expositor) VALUES
                ({input$reto_actividad},
                {data[row,]$actividad},
                {paste0('[', data[row,]$inicio - hours(5), ',', data[row,]$inicio - hours(5) + minutes(data[row,]$duracion), ']')},
                {data[row,]$tema_actividad},
                {data[row,]$coach_expositor})",
            .con = con
        )

        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_calendar()
        update_resumen()
        return(get_actividades(reto = isolate(input$reto_actividad)))
    }
    
    actividades_delete_callback <- function(data, row) {
        sql_query <- glue_sql(
            "DELETE FROM tbl_actividades WHERE id_actividad = {data[row,]$id_actividad}",
            .con = con
        )
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_calendar()
        update_resumen()
        return(get_actividades(reto = isolate(input$reto_actividad)))
    }
    
    actividades_update_callback <- function(data, olddata, row) {
        sql_query <- glue_sql(
            "UPDATE tbl_actividades SET
                actividad = {data[row,]$actividad},
                tiempo_actividad = {paste0('[', data[row,]$inicio - hours(5), ',', data[row,]$inicio - hours(5) + minutes(data[row,]$duracion), ']')},
                tema_actividad = {data[row,]$tema_actividad},
                coach_expositor = {data[row,]$coach_expositor}
            WHERE id_actividad = {data[row,]$id_actividad}",
            .con = con
        )
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_calendar()
        update_resumen()
        return(get_actividades(reto = isolate(input$reto_actividad)))
    }
    
    tbl_actividades <- reactiveVal(
        data.frame(id_actividad = numeric(),
                   actividad = character(),
                   inicio = as.Date(character()),
                   fin = as.Date(character()),
                   duracion = numeric(),
                   tema_actividad = character(),
                   coach_expositor = character(),
                   stringsAsFactors=FALSE)
    ) 
    
    coach_expositor.Types <- reactiveVal(
        get_coaches() %>%
            filter(permiso != 'Inactivo') %>%
            pull(nombre_coach)
    )
    
    tbl_actividades(get_actividades(reto = isolate(input$reto_actividad)))
    
    actividades_result <- dtedit(input, output,
                             name = 'actividades',
                             thedata = tbl_actividades,
                             edit.cols = c('actividad', 'inicio', 'duracion',
                                           'tema_actividad', 'coach_expositor'),
                             edit.label.cols = c('Tipo','Inicio', 'Duracion',
                                                 'Tema', 'Coach'),
                             input.types = c('actividad' = 'selectInput',
                                             'inicio' = 'datetimeInput',
                                             'duracion' = 'numericInput',
                                             'tema_actividad' = 'textAreaInput',
                                             'coach_expositor' = 'selectInputReactive'),
                             input.choices = list('actividad' = enum$valor[enum$variable == 'tipo_actividad'],
                                                  'coach_expositor' = 'coach_expositor.Types.list'),
                             input.choices.reactive = list(coach_expositor.Types.list = coach_expositor.Types),
                             view.cols = c('actividad', 'inicio_mostrar', 'fin_mostrar',
                                           'coach_expositor'),
                             delete.info.label.cols = c('Tipo', 'Inicio', 'Fin',
                                                        'Coach'),
                             show.copy = FALSE, 
                             title.delete = "Eliminar Actividad",
                             title.edit = "Modificar Informacion de Actividad",
                             title.add = "Agregar Actividad",
                             label.add = "Agregar",
                             label.edit = "Modificar",
                             label.delete = "Eliminar",
                             label.cancel = "Cancelar",
                             label.save = "Guardar",
                             text.delete.modal = "¿Está seguro de que quiere eliminar esta actividad? Sólo se puede eliminar actividades que no tengan registros",
                             callback.insert = actividades_insert_callback,
                             callback.delete = actividades_delete_callback,
                             callback.update = actividades_update_callback,
                             icon.add = shiny::icon("calendar-plus"),
                             icon.delete = shiny::icon("trash"), 
                             icon.edit = shiny::icon("edit"),
                             useairDatepicker = TRUE,
                             datatable.call = function(...) {
                                 DT::datatable(...) %>%
                                     formatStyle(
                                         'actividad',
                                         fontWeight = 'bold'
                                     )
                             },
                             datatable.options = list(columns = list(
                                 list(title = 'Actividad'),
                                 list(title = 'Inicio'),
                                 list(title = 'Fin'),
                                 list(title = 'Coach'))
                             )
    )
    
    observeEvent(input$reto_actividad, {
        tbl_actividades(get_actividades(reto = input$reto_actividad))
        update_calendar()
    })
    
    observeEvent(
        input$view,
        cal_proxy_view("calendario_actividades", input$view),
        ignoreInit = TRUE
    )
    
    # Tab Participacion ########################################################
    participaciones_insert_callback <- function(data, row) {
        sql_query <- glue_sql(
            "INSERT INTO tbl_participacion (nombre_reto, nombre_retador, objetivo_participacion) VALUES
                ({input$reto_participacion},
                {data[row,]$nombre_retador},
                {data[row,]$objetivo_participacion})",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_listas_retadores()
        return(get_participaciones(reto = isolate(input$reto_participacion)))
    }
    
    participaciones_delete_callback <- function(data, row) {
        sql_query <- glue_sql(
            "DELETE FROM tbl_participacion WHERE id_participacion = {data[row,]$id_participacion}",
            .con = con
        )
        tryCatch(
            {
                if (data[row,]$nombre_coach == isolate(credentials()$info$nombre_coach) | isolate(credentials()$info$permiso == 'Administrador')) {
                    dbSendQuery(con, sql_query)
                } else {
                    stop("No puede eliminar participaciones de otros coach")
                }
                
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_listas_retadores()
        return(get_participaciones(reto = isolate(input$reto_participacion)))
    }
    
    participaciones_update_callback <- function(data, olddata, row) {
        sql_query <- glue_sql(
            "UPDATE tbl_participacion SET
                nombre_retador = {data[row,]$nombre_retador},
                objetivo_participacion = {data[row,]$objetivo_participacion}
            WHERE id_participacion = {data[row,]$id_participacion}",
            .con = con
        )
        tryCatch(
            {
                if (data[row,]$nombre_coach == isolate(credentials()$info$nombre_coach) | isolate(credentials()$info$permiso == 'Administrador')) {
                    dbSendQuery(con, sql_query)
                } else {
                    stop("No puede editar participaciones de otros coach")
                }
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        update_listas_retadores()
        return(get_participaciones(reto = isolate(input$reto_participacion)))
    }
    
    tbl_participaciones <- reactiveVal(
        data.frame(id_participacion = numeric(),
                   nombre_retador = character(),
                   objetivo_participacion = character(),
                   nombre_coach = character(),
                   num_celular_retador = character(),
                   stringsAsFactors=FALSE)
    ) 
    
    nombre_retador.Types <- reactiveVal(
            get_retadores() %>%
                pull(nombre_retador)
    )
    
    tbl_participaciones(get_participaciones(reto = isolate(input$reto_participacion)))
    
    participaciones_result <- dtedit(input, output,
                             name = 'participaciones',
                             thedata = tbl_participaciones,
                             edit.cols = c('nombre_retador', 'objetivo_participacion'),
                             edit.label.cols = c('Retador','Objetivo'),
                             input.types = c('nombre_retador' = 'selectInputReactive',
                                             'objetivo_participacion' = 'textAreaInput'),
                             input.choices = list('nombre_retador' = 'nombre_retador.Types.list'),
                             input.choices.reactive = list(nombre_retador.Types.list = nombre_retador.Types),
                             view.cols = c('nombre_retador', 'num_celular_retador', 'nombre_coach'),
                             delete.info.label.cols = c('Retador', 'Celular', 'Coach'),
                             show.copy = FALSE, 
                             title.delete = "Eliminar Participación",
                             title.edit = "Modificar Informacion de Participación",
                             title.add = "Agregar Participación",
                             label.add = "Agregar",
                             label.edit = "Modificar",
                             label.delete = "Eliminar",
                             label.cancel = "Cancelar",
                             label.save = "Guardar",
                             text.delete.modal = "¿Está seguro de que quiere eliminar esta participación? Sólo se puede eliminar participaciones que no tengan registros",
                             callback.insert = participaciones_insert_callback,
                             callback.delete = participaciones_delete_callback,
                             callback.update = participaciones_update_callback,
                             icon.add = shiny::icon("user-plus"),
                             icon.delete = shiny::icon("trash"), 
                             icon.edit = shiny::icon("edit"),
                             useairDatepicker = TRUE,
                             datatable.call = function(...) {
                                 DT::datatable(...) %>%
                                     formatStyle(
                                         'nombre_retador',
                                         fontWeight = 'bold'
                                     )
                             },
                             datatable.options = list(columns = list(
                                 list(title = 'Retador'),
                                 list(title = 'Celular'),
                                 list(title = 'Coach'))
                             )
    )
    
    observeEvent(input$reto_participacion, {
        tbl_participaciones(get_participaciones(reto = input$reto_participacion))
    })
    # Tab Parametros ###########################################################
    get_parametros <- function (id_participacion) {
        consulta_sql <- glue_sql("
        with parametros as(
        	select *
        	from tbl_registros_parametros
        	where id_participacion = {id_participacion}
        )	
        select 
        	tp.nombre_parametro as \"Parametro\",
        	p.valor_inicial as \"Inicial\",
        	p.valor_final as \"Final\",
        	tp.unidad as \"Und\"
        from tbl_parametros tp left join parametros p on tp.nombre_parametro = p.nombre_parametro",
        .con = con)
        res <- dbGetQuery(con, consulta_sql)
        return(res)
    }
    
    parametros_empty_template <- data.frame(
        Parametro = NA_character_,
        Inicial = NA_real_,
        Final = NA_real_,
        Und = NA_character_
    )
    
    tbl_parametros <- reactiveVal(parametros_empty_template)
    
    parametros_data_update <- dataEditServer("parametros",
                                  data = reactive(tbl_parametros()),
                                  col_readonly = c('Parametro', 'Und'),
                                  col_edit = FALSE,
                                  row_edit = FALSE
    )
    
    observeEvent(input$reto_parametros, {
        output$retador_parametros <- renderUI({
            if (input$reto_parametros != "") {
                selectInput('retador_parametros',
                                "Retador:",
                                choices = c("", if (credentials()$info$permiso != 'Administrador') {
                                    get_retadores_reto(reto = input$reto_parametros) %>%
                                        filter(nombre_coach == credentials()$info$nombre_coach) %>%
                                        pull(nombre_retador)
                                } else {
                                    get_retadores_reto(reto = input$reto_parametros) %>%
                                        pull(nombre_retador)
                                }))
            } else if (input$reto_parametros == "") {
                return()
            }
        })
    })
    
    observeEvent({
        input$retador_parametros
        input$reto_parametros
        },
        {
        req(input$retador_parametros)
        tbl_parametros(parametros_empty_template) # Force control update
        tbl_parametros(get_parametros(id_participacion = get_id_participacion(input$reto_parametros, input$retador_parametros)))
        
        output$controles_parametros <- renderUI({
            if (input$retador_parametros != "" & input$reto_parametros != "") {
                tagList(
                    dataEditUI("parametros"),
                    actionButton('guardar_parametro', label = "Guardar")
                )
            } else if (input$retador_parametros == "" | input$reto_parametros == "") {
                return()
            }
        })
    })
    
    observeEvent(input$guardar_parametro, {
        req(input$retador_parametros, input$reto_parametros)
        
        db <- dbxConnect(adapter="postgres", dbname="herbalife", user = "ubuntu", variables=list(search_path="reto21"))
        
        success <- tryCatch({
            dbxUpsert(db,
                      "tbl_registros_parametros",
                      records = parametros_data_update() %>%
                          rename(nombre_parametro = Parametro,
                                 valor_inicial = Inicial,
                                 valor_final = Final) %>% 
                          drop_na(valor_inicial) %>% 
                          mutate(id_participacion = get_id_participacion(input$reto_parametros, input$retador_parametros)) %>% 
                          select(id_participacion, nombre_parametro, valor_inicial, valor_final),
                      where_cols = c("id_participacion", "nombre_parametro"))
            TRUE
        },
        error = function(cond) {
            showNotification(paste(cond), duration = 3, closeButton = TRUE, type = "error")
            return(FALSE)
        },
        finally = {
            dbxDisconnect(db)
            tbl_parametros(get_parametros(id_participacion = get_id_participacion(input$reto_parametros, input$retador_parametros)))
        }
        )
        
        if (success) {
            showNotification("Parametros guardados", duration = 3, closeButton = TRUE, type = "message")
        }
    })
    
    # Tab Habitos ##############################################################
    get_habitos <- function (id_participacion, fecha) {
        consulta_sql <- glue_sql("
        with habitos as(
        	select 
                id_registro_habito,
                nombre_habito,
                nombre_coach
        	from tbl_registros_habitos
        	where id_participacion = {id_participacion} and fecha_ocurrencia = {fecha}
        )
        select 
        	th.nombre_habito as \"Hábito\",
        	count(h.*) as \"Registro\",
            h.nombre_coach as \"Coach\"
        from tbl_habitos th left join habitos h on th.nombre_habito = h.nombre_habito
        group by th.id_habito, th.nombre_habito, h.nombre_coach
        order by th.id_habito",
        .con = con)
        res <- dbGetQuery(con, consulta_sql) %>% 
            mutate(Registro = Registro != 0)
        return(res)
    }
    
    habitos_empty_template <- data.frame(
        Hábito = NA_character_,
        Registro = FALSE,
        Coach = NA_character_
    )
    
    tbl_habitos <- reactiveVal(habitos_empty_template)
    
    habitos_data_update <- dataEditServer("habitos",
                                          data = reactive(tbl_habitos()),
                                          col_readonly = c('Hábito', 'Coach'),
                                          col_options = list(Registro = c(TRUE,FALSE)),
                                          col_edit = FALSE,
                                          row_edit = FALSE
    )
    
    observeEvent(input$reto_habito, {
        output$retador_fecha_habito <- renderUI({
            if (input$reto_habito != "") {
                tagList(
                    dateInput(inputId = 'fecha_habito',
                              label = 'Fecha:',
                              min = get_retos() %>% 
                                  filter(nombre_reto == input$reto_habito) %>% 
                                  pull(fecha_inicio),
                              max = get_retos() %>% 
                                  filter(nombre_reto == input$reto_habito) %>% 
                                  pull(fecha_fin),
                              format = "dd-mm-yyyy",
                              weekstart = 1,
                              language = "es"),
                    selectInput('retador_habito',
                                "Retador:",
                                choices = c("", get_retadores_reto(reto = input$reto_habito) %>%
                                        pull(lista))
                                )
                )
            } else if (input$reto_habito == "") {
                return()
            }
        })
    })
    
    observeEvent({
        input$retador_habito
        input$reto_habito
        input$fecha_habito
    },
    {
        req(input$retador_habito)
        tbl_habitos(habitos_empty_template) # Force control update
        tbl_habitos(get_habitos(id_participacion = get_id_participacion(input$reto_habito, input$retador_habito),
                                fecha = input$fecha_habito))
        
        output$controles_habitos <- renderUI({
            if (input$retador_habito != "" & input$reto_habito != "") {
                tagList(
                    dataEditUI("habitos"),
                    actionButton('guardar_habito', label = "Guardar")
                )
            } else if (input$retador_habito == "" | input$reto_habito == "") {
                return()
            }
        })
    })
    
    observeEvent(input$guardar_habito, {
        req(input$retador_habito, input$reto_habito, input$fecha_habito)
        
        db <- dbxConnect(adapter="postgres", dbname="herbalife", user = "ubuntu", variables=list(search_path="reto21"))
        
        success <- tryCatch({
            dbxUpsert(db,
                      "tbl_registros_habitos",
                      records = habitos_data_update() %>%
                          filter(Registro) %>% 
                          rename(nombre_habito = Hábito) %>% 
                          mutate(id_participacion = get_id_participacion(input$reto_habito, input$retador_habito),
                                 nombre_coach = credentials()$info$nombre_coach,
                                 fecha_ocurrencia = input$fecha_habito) %>% 
                          select(id_participacion, nombre_coach, fecha_ocurrencia, nombre_habito),
                      where_cols = c("id_participacion", "nombre_habito", "fecha_ocurrencia"),
                      skip_existing = TRUE)
            dbxDelete(db,
                      "tbl_registros_habitos",
                      where = habitos_data_update() %>%
                          filter(!Registro) %>% 
                          rename(nombre_habito = Hábito) %>% 
                          mutate(id_participacion = get_id_participacion(input$reto_habito, input$retador_habito),
                                 fecha_ocurrencia = input$fecha_habito,
                                 nombre_coach = credentials()$info$nombre_coach) %>% 
                          select(id_participacion, fecha_ocurrencia, nombre_habito, nombre_coach))
            TRUE
        },
        error = function(cond) {
            showNotification(paste(cond), duration = 3, closeButton = TRUE, type = "error")
            return(FALSE)
        },
        finally = {
            dbxDisconnect(db)
            tbl_habitos(get_habitos(id_participacion = get_id_participacion(input$reto_habito, input$retador_habito),
                                    fecha = input$fecha_habito))
        }
        )
        
        if (success) {
            showNotification("Hábitos guardados", duration = 3, closeButton = TRUE, type = "message")
        }
    })
    
    # Tab Reg Actividades #####################################################
    get_reg_actividades <- function (reto, id_actividad) {
        consulta_sql <- glue_sql("
        with asistencia as (
        	select 
        		id_registro_actividad,
        		id_participacion,
                nombre_coach
        	from tbl_registros_actividades
        	where id_actividad = {id_actividad}
        )
        select
            tp.id_participacion as id,
        	tp.nombre_retador as \"Retador\",
        	count(a.id_registro_actividad) as \"Registro\",
            a.nombre_coach as \"Coach\"
        from tbl_participacion tp left join asistencia a on tp.id_participacion = a.id_participacion
        where tp.nombre_reto = {reto}
        group by tp.nombre_retador, tp.id_participacion, a.nombre_coach
        order by tp.nombre_retador
        ",
        .con = con)
        res <- dbGetQuery(con, consulta_sql) %>% 
            mutate(Registro = Registro != 0)
        return(res)
    }
    
    actividades_empty_template <- data.frame(
        id = NA_integer_,
        Retador = NA_character_,
        Registro = NA,
        Coach = NA_character_
    )
    
    tbl_reg_actividades <- reactiveVal(actividades_empty_template)
    
    actividades_data_update <- dataEditServer("reg_actividades",
                                          data = reactive(tbl_reg_actividades()),
                                          col_readonly = c('id','Retador', 'Coach'),
                                          col_options = list(Registro = c(TRUE,FALSE)),
                                          col_edit = FALSE,
                                          row_edit = FALSE
    )
    
    observeEvent(input$reto_reg_actividad, {
        output$id_actividad <- renderUI({
            if (input$reto_reg_actividad != "") {
                req(input$reto_reg_actividad)
                selectInput('id_actividad',
                            "Actividad:",
                            choices = c("", get_actividades(reto = input$reto_reg_actividad) %>%
                                            filter(actividad != "Ceremonia") %>% 
                                            mutate(etiquetas = paste(actividad, format(inicio, "%d/%m/%Y")),
                                                   lista = id_actividad %>% setNames(etiquetas)) %>% 
                                            pull(lista))
                )
            } else if (input$reto_reg_actividad == "") {
                return()
            }
        })
    })
    
    observeEvent({
        input$reto_reg_actividad
        input$id_actividad
    },
    {
        req(input$reto_reg_actividad, input$id_actividad)
        
        tbl_reg_actividades(actividades_empty_template) # Force control update
        tbl_reg_actividades(get_reg_actividades(reto = input$reto_reg_actividad,
                                                id_actividad = input$id_actividad))
        
        output$controles_actividades <- renderUI({
            if (input$reto_reg_actividad != "" & input$id_actividad != "") {
                tagList(
                    dataEditUI("reg_actividades"),
                    actionButton('guardar_actividad', label = "Guardar")
                )
            } else if (input$reto_reg_actividad == "" | input$id_actividad == "") {
                return()
            }
        })
    })
    
    observeEvent(input$guardar_actividad, {
        req(input$reto_reg_actividad, input$id_actividad)
        
        db <- dbxConnect(adapter="postgres", dbname="herbalife", user = "ubuntu", variables=list(search_path="reto21"))
        
        success <- tryCatch({
            dbxUpsert(db,
                      "tbl_registros_actividades",
                      records = actividades_data_update() %>%
                          filter(Registro) %>% 
                          rename(id_participacion = id) %>% 
                          mutate(id_actividad = input$id_actividad,
                                 nombre_coach = credentials()$info$nombre_coach
                          ) %>% 
                          select(id_participacion, nombre_coach, id_actividad),
                      where_cols = c("id_participacion", "id_actividad"),
                      skip_existing = TRUE)
            dbxDelete(db,
                      "tbl_registros_actividades",
                      where = actividades_data_update() %>%
                          filter(!Registro) %>% 
                          rename(id_participacion = id) %>%  
                          mutate(id_actividad = input$id_actividad,
                                 nombre_coach = credentials()$info$nombre_coach) %>% 
                          select(id_participacion, id_actividad, nombre_coach))
            TRUE
        },
        error = function(cond) {
            showNotification(paste(cond), duration = 3, closeButton = TRUE, type = "error")
            return(FALSE)
        },
        finally = {
            dbxDisconnect(db)
            tbl_reg_actividades(get_reg_actividades(reto = input$reto_reg_actividad,
                                                    id_actividad = input$id_actividad))
        })
        
        if (success) {
            showNotification("Asistencias guardadas", duration = 3, closeButton = TRUE, type = "message")
        }
    })
    
    # Tab Foto #################################################################
    observeEvent(input$reto_foto, {
        output$retador_foto <- renderUI({
            if (input$reto_foto != "") {
                req(input$reto_foto)
                selectInput("retador_foto",
                            "Nombre Retador:", 
                            choices = c("", if (credentials()$info$permiso != 'Administrador') {
                                get_retadores_reto(reto = input$reto_foto) %>%
                                    filter(nombre_coach == credentials()$info$nombre_coach) %>%
                                    pull(nombre_retador)
                            } else {
                                get_retadores_reto(reto = input$reto_foto) %>%
                                    pull(nombre_retador)
                            })
                            
                )
            } else if (input$reto_foto == "") {
                return()
            }
        })
    })
    
    observeEvent({
        input$reto_foto
        input$retador_foto
    },
    {
        output$img <- renderImage({list(src = "", contentType = "image/jpeg")}, deleteFile=FALSE)
        output$controles_foto <- renderUI({
            if (input$reto_foto != "" & input$retador_foto != "") {
                tagList(
                    selectInput("tipo",
                                "Tipo:", 
                                choices = c("", enum$valor[enum$variable == 'tipo_foto']),
                                selected = ""
                                
                    ),
                    selectInput("estado",
                                "Estado:", 
                                choices = c("", enum$valor[enum$variable == 'estado_foto']),
                                selected = ""
                                
                    ),
                    fileInput("upload", "Seleccionar Imagen:", accept = c('image/jpeg')),
                    actionButton(inputId = "guardar_foto", label = "Guardar")
                )
            } else if (input$reto_foto == "" | input$retador_foto == "") {
                output$img <- renderImage({list(src = "", contentType = "image/jpeg")}, deleteFile=FALSE)
                return()
            }
        })
    })
    
    observeEvent(
        {
            input$tipo
            input$estado
        },
        {
            req(input$tipo, input$estado)
            
            output$img <- renderImage({
                tmp <- ""
                consulta_sql<- glue("
                SELECT
                    archivo
                from
                    tbl_fotos
                where
                    id_participacion = {get_id_participacion(input$reto_foto, input$retador_foto)} and
                    tipo = '{input$tipo}' and
                    estado = '{input$estado}'")
                res <- dbGetQuery(con, consulta_sql) 
                
                if (nrow(res) > 0) {
                    tmp <- res %>%
                        rowwise() %>%
                        mutate(archivo = archivo %>% 
                                   paste(collapse = "")) %>%
                        mutate(archivo = list(to_bin(archivo))) %>%
                        mutate(archivo = archivo %>% 
                                   image_read() %>%
                                   image_write(tempfile(fileext='.jpg'),
                                               format = 'jpg',
                                               quality = 100)) %>% 
                        pull(archivo)
                }
                
                list(src = tmp, contentType = "image/jpeg")
            }, deleteFile = TRUE)
        }
    )
    
    observeEvent(input$upload, {
        if (length(input$upload$datapath)) {
            output$img <- renderImage({
                tmp <-  image_read(input$upload$datapath) %>%
                    image_resize("300x") %>%
                    image_annotate(format(Sys.time(), "%d/%m/%Y %H:%M"), size = 20, gravity = "southwest", color = "red") %>%
                    image_write(tempfile(fileext='.jpg'),
                                format = 'jpg',
                                quality = 100)
                
                list(src = tmp, contentType = "image/jpeg")
            }, deleteFile=FALSE)  
        }
    })
    
    observeEvent(input$guardar_foto, {
        req(input$reto_foto, input$retador_foto, input$tipo, input$estado)
        tmp <-  image_read(input$upload$datapath) %>%
            image_resize("300x") %>% 
            image_annotate(format(Sys.time(), "%d/%m/%Y %H:%M"), size = 20, gravity = "southwest", color = "red") %>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        image_raw <- readRaw(file = tmp)
        image_raw <- image_raw$fileRaw

        success <- tryCatch({
            DBI::dbExecute(conn = con,
                      statement = "
                      INSERT INTO tbl_fotos (id_participacion, tipo, estado, archivo, nombre_coach) 
                      VALUES (?, ?, ?, ?, ?)
                      ON CONFLICT ON CONSTRAINT uq_participacion_tipo_estado
                      DO UPDATE SET 
                        archivo = EXCLUDED.archivo,
                        nombre_coach = EXCLUDED.nombre_coach",
                      params = list(get_id_participacion(input$reto_foto, input$retador_foto),
                                    input$tipo,
                                    input$estado,
                                    paste(image_raw, collapse = ""),
                                    credentials()$info$nombre_coach)
                      )
            TRUE
        },
        error = function(cond) {
            showNotification(paste(cond), duration = 3, closeButton = TRUE, type = "error")
            return(FALSE)
        },
        finally = {
            updateSelectInput(session = session, "retador_foto", selected = "")
        })
        
        if (success) {
            showNotification("Foto guardada", duration = 3, closeButton = TRUE, type = "message")
        }
    })
    
    
    # Tab Calificacion #########################################################
    
    get_calificaciones <- function (id_participacion, coach) {
        consulta_sql <- glue_sql("
        with calificaciones as (
        	select *
        	from tbl_registros_calificaciones
        	where id_participacion = {id_participacion} and
                nombre_coach = {coach}
        )	
        select 
        	tcc.criterio_calificacion as \"Criterio\",
        	c.calificacion
        from tbl_criterios_calificacion tcc left join calificaciones c on tcc.criterio_calificacion = c.criterio_calificacion",
        .con = con)
        res <- dbGetQuery(con, consulta_sql) %>% 
            mutate(calificacion = case_when(
                calificacion == -2 ~ "Mucho Peor",
                calificacion == -1 ~ "Peor",
                calificacion == 0 ~ "Igual",
                calificacion == 1 ~ "Mejor",
                calificacion == 2 ~ "Mucho Mejor",
                TRUE ~ NA_character_
            )) %>% 
            rename(Calificación = calificacion)
        return(res)
    }
    
    get_talla <- function(retador) {
        consulta_sql <- glue_sql("
        select 
        	talla
        from tbl_retadores
        where nombre_retador = {retador}",
        .con = con)
        res <- dbGetQuery(con, consulta_sql) %>% 
            pull(talla)
        return(res)
    }
    
    get_coach <- function(retador) {
        consulta_sql <- glue_sql("
        select 
        	nombre_coach
        from tbl_retadores
        where nombre_retador = {retador}",
        .con = con)
        res <- dbGetQuery(con, consulta_sql) %>% 
            pull(nombre_coach)
        return(res)
    }
    
    calificaciones_empty_template <- data.frame(
        Criterio = NA_character_,
        Calificación = NA_character_
    )
    
    tbl_calificaciones <- reactiveVal(calificaciones_empty_template)
    
    calificaciones_data_update <- dataEditServer("calificaciones",
                                             data = reactive(tbl_calificaciones()),
                                             col_readonly = c('Criterio'),
                                             col_options = list(Calificación = c(NA_character_,
                                                                                 "Mucho Peor",
                                                                                 "Peor",
                                                                                 "Igual",
                                                                                 "Mejor",
                                                                                 "Mucho Mejor")
                                                                ),
                                             col_edit = FALSE,
                                             row_edit = FALSE,
                                             col_stretch = TRUE,
                                             height = 200,
                                             width = 400
    )
    
    observeEvent(input$reto_calificacion, {
        output$retador_calificacion <- renderUI({
            if (input$reto_calificacion != "") {
                selectInput('retador_calificacion',
                            "Retador:",
                            choices = c("", get_retadores_reto(reto = input$reto_calificacion) %>%
                                            pull(nombre_retador))
                            )
            } else if (input$reto_calificacion == "") {
                return()
            }
        })
    })
    
    observeEvent({
        input$reto_calificacion
        input$retador_calificacion
    },
    {
        req(input$retador_calificacion)
        
        id_participacion = get_id_participacion(input$reto_calificacion, input$retador_calificacion)
        tbl_calificaciones(calificaciones_empty_template) # Force control update
        tbl_calificaciones(get_calificaciones(id_participacion, credentials()$info$nombre_coach))
        
        output$controles_calificacion <- renderUI({
            if (input$retador_calificacion != "" & input$reto_calificacion != "") {
                tagList(
                    dataEditUI("calificaciones"),
                    actionButton('guardar_calificacion', label = "Guardar")
                )
            } else if (input$retador_calificacion == "" | input$reto_calificacion == "") {
                output$edad <- renderText({""})
                output$objetivo <- renderText({""})
                output$talla <- renderText({""})
                output$coach <- renderText({""})
                output$parametros <- renderTable({return()})
                output$fotos <- renderTable({return()})
                return()
            }
        })
        
        output$objetivo <- renderText({
            consulta_sql <- glue_sql("
            select 
            	objetivo_participacion
            from tbl_participacion
            where id_participacion = {id_participacion}",
            .con = con)
            objetivo <- dbGetQuery(con, consulta_sql) %>% pull(objetivo_participacion)
            paste("Objetivo:", objetivo)
        })
        
        output$edad <- renderText({
            consulta_sql <- glue_sql("
            select 
                age(current_date, fecha_nacimiento) as edad
            from tbl_retadores
            where nombre_retador = {input$retador_calificacion}",
            .con = con)
            edad <- dbGetQuery(con, consulta_sql) %>% pull(edad) %>% 
                stringr::str_replace_all(c('years?' = "Años",
                                           'mons' = 'Meses',
                                           'mon\\s' = 'Mes ',
                                           'days' = 'Días',
                                           'day\\s' = 'Día'))
            paste("Edad:", edad)
        })
        
        output$talla <- renderText({
            paste("Talla:", get_talla(input$retador_calificacion), "m")
        })
        
        output$coach <- renderText({
            paste("Coach:", get_coach(input$retador_calificacion))
        })
        
        output$parametros <- renderTable({
            parametros_retador <- get_parametros(id_participacion)
            
            parametros_retador <- parametros_retador %>% 
                filter(Parametro == 'Peso') %>% 
                mutate(Parametro = 'IMC',
                       across(where(is.numeric), ~ round(./get_talla(input$retador_calificacion) ^ 2, 2)),
                       Und = '') %>% 
                bind_rows(parametros_retador) %>% 
                mutate(Variacion = Final - Inicial,
                       `Var %` = scales::percent(Variacion / Inicial, accuracy = 0.01) ) %>% 
                select(Parametro, Inicial, Final, Variacion, `Var %`, Und)
        })
        
        output$fotos <- renderTable({
            unlink("www/*.jpg")
            consulta_sql<- glue("
            SELECT
                tipo,
                estado,
                archivo
            from
                tbl_fotos
            where
                id_participacion = {id_participacion}")
            dbGetQuery(con, consulta_sql) %>%
                rowwise() %>%
                mutate(archivo = archivo %>% 
                           paste(collapse = "")) %>%
                mutate(archivo = list(to_bin(archivo))) %>%
                mutate(archivo = archivo %>% 
                           image_read() %>%
                           image_write(path = paste0("www/",tipo,"_",estado,"_",id_participacion,".jpg"),
                                       format = 'jpg',
                                       quality = 100),
                       archivo = paste0('<img src=', stringr::str_remove(archivo, "www/"), '></img>')) %>%
                mutate(id_estado = case_when(
                        estado == 'Inicial' ~ 1,
                        estado == 'Final' ~ 2),
                    id_tipo = case_when(
                        tipo == 'Frente' ~ 1,
                        tipo == 'Perfil' ~ 2,
                        tipo == 'Posterior' ~ 3
                    )) %>% 
                arrange(id_estado, id_tipo) %>% 
                select(-id_estado, -id_tipo, Tipo = tipo) %>% 
                pivot_wider(names_from = estado, values_from = archivo, id_cols = Tipo)
        }, sanitize.text.function = function(x) x)
    })
    
    observeEvent(input$guardar_calificacion, {
        req(input$reto_calificacion, input$retador_calificacion)
        
        db <- dbxConnect(adapter="postgres", dbname="herbalife", user = "ubuntu", variables=list(search_path="reto21"))
        
        success <- tryCatch({
            current_id = get_id_participacion(input$reto_calificacion, input$retador_calificacion)
            dbxUpsert(db,
                      "tbl_registros_calificaciones",
                      records = calificaciones_data_update() %>%
                          rename(criterio_calificacion = Criterio,
                                 calificacion = Calificación) %>% 
                          mutate(id_participacion = current_id,
                                 nombre_coach = credentials()$info$nombre_coach,
                                 calificacion = case_when(
                                     calificacion == "Mucho Peor" ~ -2,
                                     calificacion == "Peor" ~ -1,
                                     calificacion == "Igual" ~ 0,
                                     calificacion == "Mejor" ~ 1,
                                     calificacion == "Mucho Mejor" ~ 2,
                                     TRUE ~ NA_real_
                                 )) %>% 
                          drop_na(calificacion) %>% 
                          select(id_participacion, nombre_coach, criterio_calificacion, calificacion),
                      where_cols = c("id_participacion", "nombre_coach", "criterio_calificacion"))
            
            dbxDelete(db,
                      "tbl_registros_calificaciones",
                      where = calificaciones_data_update() %>%
                          filter(is.na(Calificación)) %>% 
                          mutate(id_participacion = current_id,
                                 nombre_coach = credentials()$info$nombre_coach) %>% 
                          select(id_participacion, nombre_coach))
            TRUE
        },
        error = function(cond) {
            showNotification(paste(cond), duration = 3, closeButton = TRUE, type = "error")
            return(FALSE)
        },
        finally = {
            dbxDisconnect(db)
            id_participacion = get_id_participacion(input$reto_calificacion, input$retador_calificacion)
            tbl_calificaciones(get_calificaciones(id_participacion, credentials()$info$nombre_coach))
        })
        
        if (success) {
            showNotification("Calificación guardada", duration = 3, closeButton = TRUE, type = "message")
        }
    })
    
    # Tab Resultados ###########################################################
    
    observeEvent(input$reto_resultados, {
        output$retador_resultados <- renderUI({
            if (input$reto_resultados != "") {
                selectInput('retador_resultados',
                            "Retador:",
                            choices = c("", get_retadores_reto(reto = input$reto_resultados) %>%
                                            pull(nombre_retador))
                            )
            } else if (input$reto_resultados == "") {
                return()
            }
        })
        
        output$resultados_reto <- renderPlot({
            req(input$reto_resultados)
            consulta_sql<- glue("
            with cuenta_habitos as (
            	select
            	concepto,
            	tp.nombre_retador,
            	nombre_habito,
            	count(nombre_habito) as n
            	from tbl_registros_habitos trh inner join
            		tbl_participacion tp on trh.id_participacion = tp.id_participacion
            	group by concepto, tp.nombre_reto, tp.nombre_retador, nombre_habito
            	having tp.nombre_reto = '{input$reto_resultados}')
            select
            	nombre_retador,
            	'Habitos' as concepto,
            	sum(n * peso_habito)/21 * peso_concepto * 100 as puntaje
            from cuenta_habitos ch inner join
            	tbl_habitos th on ch.nombre_habito = th.nombre_habito inner join
            	tbl_conceptos tc on ch.concepto = tc.concepto
            group by nombre_retador, peso_concepto")
            puntaje_habitos <- dbGetQuery(con, consulta_sql)
            
            consulta_sql<- glue("
            with actividades as (
            	select
            		nombre_reto,
            		count(id_actividad) as tot
            	from tbl_actividades
                where actividad::varchar != 'Ceremonia'
            	group by nombre_reto
            	having nombre_reto = '{input$reto_resultados}')
            select
            	tp.nombre_retador,
            	'Actividades' as concepto,
            	count(id_actividad) / a.tot::numeric * tc.peso_concepto * 100 as puntaje
            from tbl_registros_actividades tra inner join
            	tbl_participacion tp on tra.id_participacion = tp.id_participacion inner join
            	actividades a on tp.nombre_reto = a.nombre_reto inner join
            	tbl_conceptos tc on tra.concepto = tc.concepto
            group by a.tot, tp.nombre_reto, tp.nombre_retador, tc.peso_concepto")
            puntaje_actividades <- dbGetQuery(con, consulta_sql)
            
            consulta_sql<- glue("
            with promedio_calificacion as (
            	with coaches as (
            		select 
            			count(nombre_coach) as num_coaches
            		from
            			tbl_coaches tc
            		where tc.permiso != 'Inactivo'
            	)
            	select
            		concepto,
            		tp.nombre_retador,
            		criterio_calificacion,
            		sum(calificacion / num_coaches::numeric) as prom
            	from tbl_registros_calificaciones trc inner join
            		tbl_participacion tp on trc.id_participacion = tp.id_participacion 
            		cross join coaches c
            	group by concepto, tp.nombre_reto, tp.nombre_retador, criterio_calificacion
            	having tp.nombre_reto = '{input$reto_resultados}'
            )
            select
            	nombre_retador,
            	'Calificacion' as concepto,
            	sum(prom * peso_criterio)/2 * tc.peso_concepto * 100 as puntaje
            from promedio_calificacion pc inner join
            	tbl_criterios_calificacion tcc on pc.criterio_calificacion = tcc.criterio_calificacion inner join
            	tbl_conceptos tc on pc.concepto = tc.concepto
            group by nombre_retador, tc.peso_concepto")
            puntaje_calificaciones <- dbGetQuery(con, consulta_sql)
            
            puntaje_habitos %>% 
                bind_rows(puntaje_actividades) %>% 
                bind_rows(puntaje_calificaciones) %>%
                group_by(nombre_retador) %>% 
                mutate(puntaje = if_else(puntaje < 0, 0, puntaje),
                       total = sum(puntaje, na.rm = TRUE)) %>%
                ungroup() %>% 
                ggplot(aes(x = puntaje, y = reorder(nombre_retador, total), fill = concepto)) +
                geom_col(position = "stack") +
                geom_text(aes(label = round(total, 2), x = total), hjust = -0.5) +
                scale_x_continuous(limits = c(0,100), expand = expansion(add = c(0, 10))) +
                coord_cartesian(clip = "off") +
                labs(title = "Puntaje por Retador",
                     x = "Puntaje",
                     y = "Retador(a)",
                     fill = "Concepto") +
                theme_minimal() +
                theme(legend.position = "bottom")
        })
        
        output$boton_detalle_reto <- renderUI({
            if (input$reto_resultados != "") {
                downloadButton("detalle_reto", label = "Reporte Detallado Reto")
            } else if (input$reto_resultados == "") {
                return()
            }
        })
    })
    
    output$detalle_reto <- downloadHandler(
        filename = paste0("detalle_reto_",
                          dmy(str_extract(input$reto_resultados,
                                          "(?<=del\\s)\\d{2}/\\d{2}/\\d{4}")),
                          ".html"),
        content = function(file) {
            tempReport <- file.path(tempdir(), "detalle_reto.Rmd")
            file.copy("www/detalle_reto.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(reto = input$reto_resultados)
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    observeEvent({
        input$reto_resultados
        input$retador_resultados
    },
    {
        if (input$reto_resultados != "" & input$retador_resultados != "") {
            output$resultados_retador <- renderPlot({
                req(input$retador_resultados)
                consulta_sql<- glue("
                with cuenta_habitos as (
                	select
                	concepto,
                	fecha_ocurrencia,
                	nombre_habito,
                	count(nombre_habito) as n
                	from tbl_registros_habitos trh inner join
                		tbl_participacion tp on trh.id_participacion = tp.id_participacion
                	group by concepto, tp.nombre_reto, tp.nombre_retador, fecha_ocurrencia, nombre_habito
                	having tp.nombre_reto = '{input$reto_resultados}' and
                		tp.nombre_retador = '{input$retador_resultados}')
                select
                	'Habitos' as concepto,
                	fecha_ocurrencia,
                	sum(n * peso_habito)/21 * peso_concepto * 100 as puntaje
                from cuenta_habitos ch inner join
                	tbl_habitos th on ch.nombre_habito = th.nombre_habito inner join
                	tbl_conceptos tc on ch.concepto = tc.concepto
                group by fecha_ocurrencia, peso_concepto")
                puntaje_habitos <- dbGetQuery(con, consulta_sql)
                
                consulta_sql<- glue("
                with actividades as (
                	select
                		nombre_reto,
                		count(id_actividad) as tot
                	from tbl_actividades
                    where actividad::varchar != 'Ceremonia'
                	group by nombre_reto
                	having nombre_reto = '{input$reto_resultados}')
                select
                	'Actividades' as concepto,
                	max(upper(tiempo_actividad)::date) as fecha_ocurrencia,
                	count(tra.id_actividad) / sum(a.tot) * tc.peso_concepto * 100 as puntaje
                from tbl_registros_actividades tra inner join
                	tbl_participacion tp on tra.id_participacion = tp.id_participacion inner join
                	actividades a on tp.nombre_reto = a.nombre_reto inner join
                	tbl_conceptos tc on tra.concepto = tc.concepto inner join 
                	tbl_actividades ta on tra.id_actividad = ta.id_actividad 
                group by tp.nombre_reto, tp.nombre_retador, tc.peso_concepto, tiempo_actividad
                having tp.nombre_retador = '{input$retador_resultados}'")
                puntaje_actividades <- dbGetQuery(con, consulta_sql)
                
                consulta_sql<- glue("
                with promedio_calificacion as (
                	with coaches as (
                		select 
                			count(nombre_coach) as num_coaches
                		from
                			tbl_coaches tc
                		where tc.permiso != 'Inactivo'
                	)
                	select
                		concepto,
                		tp.nombre_retador,
                		criterio_calificacion,
                		upper(tr.duracion_reto) - 1 as fecha_ocurrencia,
                		sum(calificacion / num_coaches::numeric) as prom
                	from tbl_registros_calificaciones trc inner join
                		tbl_participacion tp on trc.id_participacion = tp.id_participacion inner join 
                		tbl_retos tr on tp.nombre_reto = tr.nombre_reto 
                		cross join coaches
                	group by concepto, tp.nombre_reto, tr.duracion_reto, tp.nombre_retador, criterio_calificacion
                	having tp.nombre_reto = '{input$reto_resultados}' and
                		tp.nombre_retador = '{input$retador_resultados}')
                select
                	'Calificacion' as concepto,
                	fecha_ocurrencia,
                	sum(prom * peso_criterio)/2 * tc.peso_concepto * 100 as puntaje
                from promedio_calificacion pc inner join
                	tbl_criterios_calificacion tcc on pc.criterio_calificacion = tcc.criterio_calificacion inner join
                	tbl_conceptos tc on pc.concepto = tc.concepto
                group by nombre_retador, fecha_ocurrencia, tc.peso_concepto")
                puntaje_calificaciones <- dbGetQuery(con, consulta_sql)
                
                plot_data <- puntaje_habitos %>% 
                    bind_rows(puntaje_actividades) %>% 
                    bind_rows(puntaje_calificaciones) %>%
                    group_by(fecha_ocurrencia) %>% 
                    mutate(puntaje = if_else(puntaje < 0, 0, puntaje),
                           total = sum(puntaje, na.rm = TRUE)) %>%
                    ungroup()
                
                plot_data %>% 
                    ggplot(aes(x = fecha_ocurrencia, y = puntaje, fill = concepto)) +
                    geom_col(position = "stack", width = 0.9) +
                    geom_text(aes(label = round(total, 2), y = total), vjust = -0.5) +
                    coord_cartesian(clip = "off") +
                    scale_x_date(date_breaks = "1 day",
                                 labels = scales::label_date_short(),
                                 expand = c(0.005,0.005)) +
                    labs(title = glue("Puntaje de {input$retador_resultados} por Fecha"),
                         x = "Fecha",
                         y = "Puntaje",
                         fill = "Concepto") +
                    theme_minimal() +
                    theme(legend.position = "bottom")
            })
            
            output$boton_detalle_retador <- renderUI({downloadButton("detalle_retador", label = "Reporte Detallado Retador")})
            
        } else if (input$reto_resultados == "" | input$retador_resultados == "") {
            updateSelectInput(session = session, "retador_resultados", selected = "")
            output$resultados_retador <- renderPlot({return()})
            output$boton_detalle_retador <- renderUI({return()})
        }
    })
    
    output$detalle_retador <- downloadHandler(
        filename = paste0("detalle_reto_",
                          dmy(str_extract(input$reto_resultados,
                                          "(?<=del\\s)\\d{2}/\\d{2}/\\d{4}")),
                          "_",
                          str_replace_all(input$retador_resultados, "\\s", "_"),
                          ".html"),
        content = function(file) {
            tempReport <- file.path(tempdir(), "detalle_retador.Rmd")
            file.copy("www/detalle_retador.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(reto = input$reto_resultados, retador = input$retador_resultados)
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    # Tab Pesos y Parametros ###################################################
    # Conceptos
    get_tbl_conceptos <- function() {
        sql_query <- glue_sql(
            "select *
            from tbl_conceptos",
            .con = con
        )
        res <- dbGetQuery(con, sql_query)
        return(res)
    }
    
    tbl_conceptos_update_callback <- function(data, olddata, row) {
        sql_query <- glue_sql(
            "UPDATE tbl_conceptos SET
                peso_concepto = {data[row,]$peso_concepto}
            WHERE id_concepto = {data[row,]$id_concepto}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_conceptos())
    }
    
    tbl_conceptos <- get_tbl_conceptos()
    
    conceptos_result <- dtedit(input, output,
                               name = 'tbl_conceptos',
                               thedata = tbl_conceptos,
                               edit.cols = c('peso_concepto'),
                               edit.label.cols = c('Peso Concepto'),
                               input.types = c('peso_concepto' = 'numericInput'),
                               view.cols = c('concepto', 'peso_concepto'),
                               show.copy = FALSE,
                               show.delete = FALSE,
                               show.insert = FALSE,
                               title.edit = "Modificar Peso de Concepto",
                               label.edit = "Modificar",
                               label.cancel = "Cancelar",
                               label.save = "Guardar",
                               callback.update = tbl_conceptos_update_callback,
                               icon.edit = shiny::icon("edit"),
                               datatable.call = function(...) {
                                   DT::datatable(...) %>%
                                       formatStyle(
                                           'concepto',
                                           fontWeight = 'bold'
                                       )
                               },
                               datatable.options = list(columns = list(
                                   list(title = 'Concepto'),
                                   list(title = 'Peso Concepto')))
    )
    
    # Habitos
    get_tbl_habitos <- function() {
        sql_query <- glue_sql(
            "select *
            from tbl_habitos",
            .con = con
        )
        res <- dbGetQuery(con, sql_query)
        return(res)
    }
    
    tbl_habitos_insert_callback <- function(data, row) {
        sql_query <- glue_sql(
            "INSERT INTO tbl_habitos (nombre_habito, peso_habito) VALUES
                ({data[row,]$nombre_habito},
                {data[row,]$peso_habito})",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_habitos())
    }
    
    tbl_habitos_delete_callback <- function(data, row) {
        sql_query <- glue_sql(
            "DELETE FROM tbl_habitos WHERE id_habito = {data[row,]$id_habito}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_habitos())
    }
    
    tbl_habitos_update_callback <- function(data, olddata, row) {
        sql_query <- glue_sql(
            "UPDATE tbl_habitos SET
                nombre_habito = {data[row,]$nombre_habito},
                peso_habito = {data[row,]$peso_habito}
            WHERE id_habito = {data[row,]$id_habito}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_habitos())
    }
    
    tbl_habitos_pesos <- get_tbl_habitos()
    
    tbl_habitos_pesos_result <- dtedit(input, output,
                                       name = 'tbl_habitos',
                                       thedata = tbl_habitos_pesos,
                                       edit.cols = c('nombre_habito', 'peso_habito'),
                                       edit.label.cols = c('Hábito', 'Peso Hábito'),
                                       input.types = c('peso_habito' = 'numericInput'),
                                       view.cols = c('nombre_habito', 'peso_habito'),
                                       delete.info.label.cols = c('Hábito', 'Peso Hábito'),
                                       show.copy = FALSE, 
                                       title.delete = "Eliminar Hábito",
                                       title.edit = "Modificar Informacion de Hábito",
                                       title.add = "Agregar Hábito",
                                       label.add = "Agregar",
                                       label.edit = "Modificar",
                                       label.delete = "Eliminar",
                                       label.cancel = "Cancelar",
                                       label.save = "Guardar",
                                       text.delete.modal = "¿Está seguro de que quiere eliminar este hábito? Sólo se puede eliminar hábitos sin registros guardados",
                                       callback.insert = tbl_habitos_insert_callback,
                                       callback.delete = tbl_habitos_delete_callback,
                                       callback.update = tbl_habitos_update_callback,
                                       icon.add = shiny::icon("plus"),
                                       icon.delete = shiny::icon("trash"), 
                                       icon.edit = shiny::icon("edit"),
                                       datatable.call = function(...) {
                                           DT::datatable(...) %>%
                                               formatStyle(
                                                   'nombre_habito',
                                                   fontWeight = 'bold'
                                               )
                                       },
                                       datatable.options = list(columns = list(
                                           list(title = 'Hábito'),
                                           list(title = 'Peso Hábito')))
    )
    
    # Criterio de Calificacion
    get_tbl_criterios <- function() {
        sql_query <- glue_sql(
            "select *
            from tbl_criterios_calificacion",
            .con = con
        )
        res <- dbGetQuery(con, sql_query)
        return(res)
    }
    
    tbl_criterios_insert_callback <- function(data, row) {
        sql_query <- glue_sql(
            "INSERT INTO tbl_criterios_calificacion (criterio_calificacion, peso_criterio) VALUES
                ({data[row,]$criterio_calificacion},
                {data[row,]$peso_criterio})",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_criterios())
    }
    
    tbl_criterios_delete_callback <- function(data, row) {
        sql_query <- glue_sql(
            "DELETE FROM tbl_criterios_calificacion 
            WHERE id_criterio_calificacion = {data[row,]$id_criterio_calificacion}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_criterios())
    }
    
    tbl_criterios_update_callback <- function(data, olddata, row) {
        sql_query <- glue_sql(
            "UPDATE tbl_criterios_calificacion SET
                criterio_calificacion = {data[row,]$criterio_calificacion},
                peso_criterio = {data[row,]$peso_criterio}
            WHERE id_criterio_calificacion = {data[row,]$id_criterio_calificacion}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_criterios())
    }
    
    tbl_criterios <- get_tbl_criterios()
    
    tbl_criterios_result <- dtedit(input, output,
                                   name = 'tbl_criterios',
                                   thedata = tbl_criterios,
                                   edit.cols = c('criterio_calificacion', 'peso_criterio'),
                                   edit.label.cols = c('Criterio', 'Peso Criterio'),
                                   input.types = c('peso_criterio' = 'numericInput'),
                                   view.cols = c('criterio_calificacion', 'peso_criterio'),
                                   delete.info.label.cols = c('Criterio', 'Peso Criterio'),
                                   show.copy = FALSE, 
                                   title.delete = "Eliminar Criterio",
                                   title.edit = "Modificar Informacion de Criterio",
                                   title.add = "Agregar Criterio",
                                   label.add = "Agregar",
                                   label.edit = "Modificar",
                                   label.delete = "Eliminar",
                                   label.cancel = "Cancelar",
                                   label.save = "Guardar",
                                   text.delete.modal = "¿Está seguro de que quiere eliminar este criterio? Sólo se puede eliminar criterios sin registros guardados",
                                   callback.insert = tbl_criterios_insert_callback,
                                   callback.delete = tbl_criterios_delete_callback,
                                   callback.update = tbl_criterios_update_callback,
                                   icon.add = shiny::icon("plus"),
                                   icon.delete = shiny::icon("trash"), 
                                   icon.edit = shiny::icon("edit"),
                                   datatable.call = function(...) {
                                       DT::datatable(...) %>%
                                           formatStyle(
                                               'criterio_calificacion',
                                               fontWeight = 'bold'
                                           )
                                   },
                                   datatable.options = list(columns = list(
                                       list(title = 'Criterio'),
                                       list(title = 'Peso Criterio')))
    )
    
    # Parametros
    get_tbl_parametros <- function() {
        sql_query <- glue_sql(
            "select *
            from tbl_parametros",
            .con = con
        )
        res <- dbGetQuery(con, sql_query)
        return(res)
    }
    
    tbl_parametros_insert_callback <- function(data, row) {
        sql_query <- glue_sql(
            "INSERT INTO tbl_parametros (nombre_parametro, unidad) VALUES
                ({data[row,]$nombre_parametro},
                {data[row,]$unidad})",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_parametros())
    }
    
    tbl_parametros_delete_callback <- function(data, row) {
        sql_query <- glue_sql(
            "DELETE FROM tbl_parametros 
            WHERE id_parametro = {data[row,]$id_parametro}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_parametros())
    }
    
    tbl_parametros_update_callback <- function(data, olddata, row) {
        sql_query <- glue_sql(
            "UPDATE tbl_parametros SET
                nombre_parametro = {data[row,]$nombre_parametro},
                unidad = {data[row,]$unidad}
            WHERE id_parametro = {data[row,]$id_parametro}",
            .con = con
        )
        
        tryCatch(
            {
                dbSendQuery(con, sql_query)
            },
            error = function(cond) {
                stop(paste(cond))
            }
        )
        return(get_tbl_parametros())
    }
    
    tbl_parametros_mod <- get_tbl_parametros()
    
    tbl_parametros_result <- dtedit(input, output,
                                   name = 'tbl_parametros',
                                   thedata = tbl_parametros_mod,
                                   edit.cols = c('nombre_parametro', 'unidad'),
                                   edit.label.cols = c('Parámetro', 'Unidad'),
                                   input.types = c('unidad' = 'selectInput'),
                                   input.choices = list('unidad' = enum$valor[enum$variable == 'unidades']),
                                   view.cols = c('nombre_parametro', 'unidad'),
                                   delete.info.label.cols = c('Parámetro', 'Unidad'),
                                   show.copy = FALSE, 
                                   title.delete = "Eliminar Parámetro",
                                   title.edit = "Modificar Informacion de Parámetro",
                                   title.add = "Agregar Parámetro",
                                   label.add = "Agregar",
                                   label.edit = "Modificar",
                                   label.delete = "Eliminar",
                                   label.cancel = "Cancelar",
                                   label.save = "Guardar",
                                   text.delete.modal = "¿Está seguro de que quiere eliminar este parámetro? Sólo se puede eliminar parámetros sin registros guardados",
                                   callback.insert = tbl_parametros_insert_callback,
                                   callback.delete = tbl_parametros_delete_callback,
                                   callback.update = tbl_parametros_update_callback,
                                   icon.add = shiny::icon("plus"),
                                   icon.delete = shiny::icon("trash"), 
                                   icon.edit = shiny::icon("edit"),
                                   datatable.call = function(...) {
                                       DT::datatable(...) %>%
                                           formatStyle(
                                               'nombre_parametro',
                                               fontWeight = 'bold'
                                           )
                                   },
                                   datatable.options = list(columns = list(
                                       list(title = 'Parámetro'),
                                       list(title = 'Unidad')))
    )
}

onStop(function() {
    dbDisconnect(con)
})

shinyApp(ui = ui, server = server)
