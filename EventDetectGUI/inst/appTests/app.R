#Main Shiny Application Call
require("EventDetectGUI")
shinyApp(ui = getUIPage, server = getServer)
