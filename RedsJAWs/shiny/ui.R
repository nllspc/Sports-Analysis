# ui 



library(shiny)
library(shinydashboard)
library(DT)
library(ggiraph)



dashHeader <- dashboardHeader(
      title="Evaluating Nominees for the Reds HOF",
      titleWidth = 375
)



dashSidebar <- dashboardSidebar(
      sidebarMenu(
            menuItem(
                  text = "Home",
                  tabName = "homeTab",
                  icon = icon("home")
            ),
            menuItem(
                  text = "JAWS-4",
                  tabName = "jawsTab",
                  icon = icon(name = "line-chart")
            ),
            menuItem(
                  text = "Profile",
                  tabName = "profTab",
                  icon = icon("folder-open-o")
            ),
            menuItem(
                  text = "Stat Rank",
                  tabName = "rankTab",
                  icon = icon("percent")
            ),
            menuItem(
                  text = "Numbers",
                  tabName = "numTab",
                  icon = icon("calculator")
            )
      )
)



dashBody <- dashboardBody(
      tags$head(
            tags$style(HTML('
                            /* Changes color of title portion of header */
                            .skin-blue .main-header .logo {
                            background-color: #C6011F;
                            }
                            
                            .skin-blue .main-header .logo:hover {
                            background-color: #C6011F;
                            }
                            
                            /* Changes color of rest of header */
                            .skin-blue .main-header .navbar {
                            background-color: #C6011F;
                            }
                            
                            /* Changes color of sidebar toggle when hovered */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #000000;
                            }
                            
                            '))
      ),
      
      tabItems(
            tabItem(
                  tabName = 'homeTab',
                  fluidRow(
                        box(
                              width = 8,
                              DTOutput(outputId = 'hTable')
                        ),
                        box(
                              width = 4,
                              plotOutput(outputId = 'ridge')
                        )
                  )
            ),
            
            tabItem(
                  tabName = 'jawsTab',
                  fluidRow(
                        box(
                              width = 6,
                              
                              
                              DTOutput(outputId = 'jTable')
                              
                        ),
                        
                        box(
                              width = 6,
                              textInput(
                                    inputId = 'jplayer',
                                    label = 'Enter Player Name',
                                    placeholder = 'Johnny Bench'
                              ),
                              
                              ggiraphOutput(outputId = 'lineChart'),
                              
                              box(
                                    width = 6,
                                    plotOutput(outputId = 'warCleve')
                              ),
                              box(
                                    width = 6,
                                    plotOutput(outputId = 'jawsCleve')
                              )
                              
                              
                        )
                  )
            ),
      
            
            tabItem(
                  tabName = 'profTab',
                  fluidRow(
                        tabBox(
                              width = 12,
                              tabPanel(
                                    title = 'Batting',
                                    column(
                                          width = 7,
                                          box(
                                                textInput(
                                                      inputId = 'prof_bat_player',
                                                      label = 'Enter Player Name',
                                                      placeholder = 'Ken Griffey Jr'
                                                ),
                                                DTOutput(outputId = 'prof_bat_Table'),
                                                DTOutput(outputId = 'prof_field_Table'),
                                                DTOutput(outputId = 'prof_psb_Table'),
                                                DTOutput(outputId = 'prof_awa_Table')
                                          )
                                    ),
                                    column(
                                          width = 5,
                                          box(
                                                plotOutput(outputId = 'bat_dev')
                                          )
                                    )
                              ),
                              tabPanel(
                                    title = 'Pitching',
                                    column(
                                          width = 7,
                                          box(
                                                textInput(
                                                      inputId = 'prof_pit_player',
                                                      label = 'Enter Player Name',
                                                      placeholder = 'Jose Rijo'
                                                ),
                                                DTOutput(outputId = 'prof_pit_Table'),
                                                DTOutput(outputId = 'prof_psb_Table'),
                                                DTOutput(outputId = 'prof_awa_Table')
                                          )
                                    ),
                                    column(
                                          width = 5,
                                          box(
                                                plotOutput(outputId = 'pit_dev')
                                          )
                                    )
                              )
                        )
                  )
            ),
            
            tabItem(
                  tabName = 'rankTab',
                  textInput(
                        inputId = 'rplayer',
                        label = 'Enter Player Name',
                        placeholder = 'Eric Davis'
                  )
            ),
            
            tabItem(
                  tabName = 'numTab',
                  'Numbers'
            )
      )
)



dashboardPage(
      header = dashHeader,
      sidebar = dashSidebar,
      body = dashBody
)