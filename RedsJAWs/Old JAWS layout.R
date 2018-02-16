# Old jaws pg layout


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
