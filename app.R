library(shiny)
library(googleway)
library(rsconnect)

load("ports.rda")
load("countryCodes.rda")
portCountries = mapply(ports, FUN = function(p) p$country)
countries = sort(as.character(mapply(unique(portCountries), FUN = function(code) countryCodes[which(countryCodes$V2 == code), 1])))
gearTypes = names(ports[[1]]$counts)

vars = reactiveValues(chat = NULL, users = NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat = readRDS("chat.Rds")
} else {
  vars$chat = "Welcome to the Global Ports Database!"
}

#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix = function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

ui = pageWithSidebar(

  headerPanel('Global Ports Database'),
  
  sidebarPanel(
    width = 4,
    selectInput('cname', 'Country', choices = countries, selected = 'Portugal'),
    tableOutput("wpigeo"),
    tableOutput("gearCounts"),
    h4("Encoded polygon"),
    verbatimTextOutput("encodedPoly")
  ),

  mainPanel(
    width = 8,
    google_mapOutput("map"),
  
    ############# SHINY CHAT UI #####################################################
    #
    # Create a spot for reactive variables specific to this particular session
    # The MIT License (MIT)
    #
    # Copyright (c) 2014 Jeff Allen
    #
    #Permission is hereby granted, free of charge, to any person obtaining a copy of
    #this software and associated documentation files (the "Software"), to deal in
    #the Software without restriction, including without limitation the rights to
    #use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
    #the Software, and to permit persons to whom the Software is furnished to do so,
    #subject to the following conditions:
    #
    #  The above copyright notice and this permission notice shall be included in all
    #copies or substantial portions of the Software.
    #
    #THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    #IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
    #FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
    #COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
    #IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    #CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
    #
  
    includeCSS("shinychat.css"),
    includeScript("sendOnEnter.js"),
  
    # Create a spot for a dynamic UI containing the chat contents.
    uiOutput("chat"),
  
    # Create the bottom bar to allow users to chat.
    textInput(width = '100%', "entry", ""),
    actionButton("send", "Send")

  )
)

server = function(input, output, session) {

  api_key = "your-key"
  
  getCode = function(fullName) {
    as.character(countryCodes[which(countryCodes$V1 == fullName), 2])
  }

  makedf = function() {
    npoly = length(ports)
    pl = mapply(1:npoly, FUN = function(i) {
      encode_pl(lat = ports[[i]]$lat, lon = ports[[i]]$lon)
    })

    df = data.frame(id = 1:length(pl), polyline = pl, stringsAsFactors = FALSE)

    df = aggregate(polyline ~ id, data = df, list)
    df$editable = TRUE
    df
  }

  df <- makedf()

  output$map <- renderGoogle_map({
    google_map(key = api_key) %>%
      add_polygons(data = df[portCountries == getCode(input$cname),], polyline = "polyline", id = "id", editable = "editable")
  })

  observeEvent(input$cname, {
    google_map_update(map_id = "map") %>%
      clear_polygons() %>%
      add_polygons(data = df[portCountries == getCode(input$cname),], polyline = "polyline", id = "id", editable = "editable")
  })

  output$encodedPoly <- eventReactive(input$map_polygon_click,
                                      paste0(input$map_polygon_click$id, '-', input$map_polygon_click$outerPath))

  output$gearCounts <- eventReactive(input$map_polygon_click, {
    pt = ports[[input$map_polygon_click$id]]
    paste(colnames(pt$counts), pt$counts)
  })
  
  output$wpigeo = renderTable({
    data.frame(Attribute = c('name', 'dist. (km)'), 
               WPI = c('-', '-'), 
               City = c('-', '-'))
  })
  
  output$gearCounts = renderTable({
    data.frame(Vessel = gearTypes, Counts = rep('_', length(gearTypes)))
  })
  
  output$encodedPoly = renderText("-")
  
  observeEvent(input$map_polygon_click, {
    pt = ports[[input$map_polygon_click$id]]
    output$wpigeo = renderTable({
      data.frame(Attribute = c('name', 'dist. (km)'), 
                 WPI = c(pt$wpi$PORT_NAME, round(pt$wpiDistance, 1)), 
                 City = c(pt$city$name, round(pt$cityDistance, 1)))
    })
    
    output$gearCounts = renderTable({
      data.frame(Vessel = gearTypes, Counts = as.numeric(pt$counts))
    })
    
    output$encodedPoly <- renderText(paste0(input$map_polygon_click$id, '-', input$map_polygon_click$outerPath))
  })

  ############################### SHINY CHAT SERVER ##############################
  # Create a spot for reactive variables specific to this particular session
  # The MIT License (MIT)
  #
  # Copyright (c) 2014 Jeff Allen
  #
  #Permission is hereby granted, free of charge, to any person obtaining a copy of
  #this software and associated documentation files (the "Software"), to deal in
  #the Software without restriction, including without limitation the rights to
  #use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
  #the Software, and to permit persons to whom the Software is furnished to do so,
  #subject to the following conditions:
  #
  #  The above copyright notice and this permission notice shall be included in all
  #copies or substantial portions of the Software.
  #
  #THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  #IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
  #FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  #COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
  #IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  #CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  #

  sessionVars <- reactiveValues(username = "")

  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.

  init <- FALSE

  # When a session is ended, remove the user and note that they left the room.

  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                                       tags$span(class="user-exit",
                                                 sessionVars$username,
                                                 "left the room.")))
    })
  })

  # Observer to handle changes to the username

  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user

    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }

        # Updating username
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]

        # Note the change in the chat log
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-change",
                                                    paste0("\"", sessionVars$username, "\""),
                                                    " -> ",
                                                    paste0("\"", input$user, "\""))))

        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })

  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user",
                    value=sessionVars$username)
  })

  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })

  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      vars$chat <<- c(vars$chat,
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })

  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")

    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
}

shinyApp(ui, server)
