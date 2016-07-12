shinyUI(navbarPage("NTAP Data Explorer",
	#google analytics
	#header=list(tags$head(includeScript("www/google_analytics.js"))),
   tabPanel("Drug Screens",
            drugScreenModuleUI(id = "demo", data = df1) 
    ) # END tabPanel
  )#END navnarPage
)#END shinyUI

