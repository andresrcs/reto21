# login tab ui to be rendered on launch
login_tab <- tabPanel(
    title = icon("lock"), 
    value = "login", 
    shinyauthr::loginUI("login")
)

resumen <- tabPanel(title = icon("home"),
                    # Omitir mensajes de error
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    ),
                    fluidPage(
                        h1("Resumen de Retos Activos"),
                        radioButtons(
                            inputId = 'view_resumen',
                            label = 'Cambiar vista:',
                            choices = c('Dia' = 'day', 'Semana' = 'week', 'Mes' = 'month'),
                            selected = 'month',
                            inline = TRUE
                        ),
                        calendarOutput('resumen'),
                        fluidRow(
                            column(width = 10,
                                   checkboxGroupInput(
                                       inputId = "calendarId",
                                       label = "Mostrar retos:",
                                       choices = get_retos() %>%
                                           filter(reto_activo) %>% 
                                           mutate(lista = id_reto %>% setNames(nombre_reto)) %>% 
                                           pull(lista),
                                       selected = get_retos() %>%
                                           filter(reto_activo) %>% 
                                           mutate(lista = id_reto %>% setNames(nombre_reto)) %>% 
                                           pull(lista)
                                   ))
                        )
                    )
)

retadores <- tabPanel("Retadores",
                      fluidPage(
                          h1("Registro General de Retadores"),
                          uiOutput('retadores')
                      )
)

participaciones <- tabPanel("Participaciones",
                      fluidPage(
                          h1("Incripción de Participaciones en Retos"),
                          selectInput('reto_participacion',
                                      "Reto:",
                                      choices = c("", get_retos() %>%
                                                      filter(reto_activo) %>%
                                                      pull(nombre_reto)),
                                      selected = ""),
                          uiOutput('participaciones')
                      )
)

parametros <- tabPanel("Parametros",
                       fluidPage(
                           h1("Registro de Parametros de Retador por Reto"),
                           selectInput('reto_parametros',
                                       "Reto:",
                                       choices = c("", get_retos() %>%
                                                       filter(reto_activo) %>%
                                                       pull(nombre_reto)),
                                       selected = ""),
                           uiOutput('retador_parametros'),
                           uiOutput('controles_parametros')
                       )
    
)

habitos <- tabPanel("Hábitos",
                       fluidPage(
                           h1("Registro de Hábitos Diarios"),
                           selectInput('reto_habito',
                                       "Reto:",
                                       choices = c("", get_retos() %>%
                                                       filter(reto_activo) %>%
                                                       pull(nombre_reto)),
                                       selected = ""),
                           uiOutput('retador_fecha_habito'),
                           uiOutput('controles_habitos')
                       )
)

reg_actividades <- tabPanel("Actividades",
                    fluidPage(
                        h1("Registro de Asistencia a Actividades"),
                        selectInput('reto_reg_actividad',
                                    "Reto:",
                                    choices = c("", get_retos() %>%
                                                    filter(reto_activo) %>%
                                                    pull(nombre_reto)),
                                    selected = ""),
                        uiOutput('id_actividad'),
                        uiOutput('controles_actividades')
                    )
)

fotos <- tabPanel("Fotos",
                        sidebarLayout(
                            sidebarPanel(
                                h1("Registro de Fotos de Participación"),
                                selectInput('reto_foto',
                                            "Reto:",
                                            choices = c("", get_retos() %>%
                                                            filter(reto_activo) %>%
                                                            pull(nombre_reto)),
                                            selected = ""),
                                uiOutput('retador_foto'),
                                uiOutput('controles_foto')
                            ),
                            mainPanel(
                                h3("Vista Previa"),
                                imageOutput("img")
                            )
                        )
)

calificaciones <- tabPanel("Calificaciones",
                         sidebarLayout(
                             sidebarPanel(
                                 h1("Calificación de Retadores"),
                                 selectInput('reto_calificacion',
                                             "Reto:",
                                             choices = c("", get_retos() %>%
                                                             filter(reto_activo) %>%
                                                             pull(nombre_reto)),
                                             selected = ""),
                                 uiOutput("retador_calificacion"),
                                 uiOutput("controles_calificacion")
                             ),
                             mainPanel(
                                 h3(textOutput("objetivo")),
                                 h3(textOutput("edad")),
                                 tableOutput("parametros"),
                                 tableOutput("fotos")
                             )
                         )
)

resultados <- tabPanel(title = "Resultados",
                       fluidPage(
                           fluidRow(h1("Resultados")),
                           fluidRow(
                               column(width = 6,
                                      selectInput('reto_resultados',
                                                  "Reto:",
                                                  choices = c("", get_retos() %>%
                                                                  filter(reto_activo) %>%
                                                                  pull(nombre_reto)),
                                                  selected = "")
                               ),
                               column(width = 6,
                                      uiOutput("retador_resultados")
                               )),
                           fluidRow(
                               column(width = 6,
                                      plotOutput("resultados_reto")
                               ),
                               column(width = 6,
                                      plotOutput("resultados_retador")
                               )),
                           fluidRow(
                               column(width = 6,
                                      uiOutput("boton_detalle_reto")
                               ),
                               column(width = 6,
                                      uiOutput("boton_detalle_retador")
                               ))
                       )
)

coaches <- tabPanel(title = "Coaches",
                    fluidPage(
                        h1("Registro de Coaches"),
                        uiOutput('coaches')
                    )
)

mi_cuenta <- tabPanel("Mi Cuenta",
                      fluidPage(
                          uiOutput('datos_coach')
                      )
)

retos <- tabPanel("Retos",
                  fluidPage(
                      h1("Registro de Retos"),
                      uiOutput('retos')
                  )
)

actividades <- tabPanel("Actividades",
                        fluidPage(
                            h1("Programación de Actividades"),
                            selectInput('reto_actividad',
                                        "Reto:",
                                        choices = c("", get_retos() %>%
                                                        filter(reto_activo) %>%
                                                        pull(nombre_reto)),
                                        selected = ""),
                            uiOutput('actividades'),
                            radioButtons(
                                inputId = 'view',
                                label = 'Cambiar vista:',
                                choices = c('Dia' = 'day', 'Semana' = 'week', 'Mes' = 'month'),
                                selected = 'month',
                                inline = TRUE
                            ),
                            calendarOutput("calendario_actividades")
                        )
)