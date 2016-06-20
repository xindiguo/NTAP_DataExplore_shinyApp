shinyServer(function(input, output, session) {
  callModule(drugScreenModule,id = "demo",session = session, summarizedData = df ,tag="demo")
})