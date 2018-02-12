# ui 



library(shiny)
library(shinydashboard)



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
                  'Home'
            ),
            
            tabItem(
                  tabName = 'jawsTab',
                  fluidRow(
                        box(
                              width = 6,
                              collapsible = TRUE,
                              
                              textInput(
                                    inputId = 'jplayer',
                                    label = 'Enter Player Name',
                                    placeholder = 'Johnny Bench'
                              ),
                              
                              DTOutput(outputId = 'jTable')
                        ),
                        
                        box(
                              width = 6,
                              collapsible = TRUE,
                              
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
                  textInput(
                        inputId = 'pplayer',
                        label = 'Enter Player Name',
                        placeholder = 'Ken Griffey Jr'
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