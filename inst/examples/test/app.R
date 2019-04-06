##test app
library(shiny)
library(shinyWidgets)

## data ################
mD <- structure(c("2019", "2018", "2017", "2016", "2015", "2014", "2013",
                  "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005",
                  "2004", "2003", "2002", "2001", "2000", "1999", "1998", "1997",
                  "1996", "1995", "1994", "1993", "1992", "1991", "1990", "0",
                  "0", "0", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
                  "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
                  "0", "0", "0", "<div class='jhr'>2019</div>", "<div class='jhr'>2018</div>",
                  "<div class='jhr'>2017</div>", "<div class='jhr'>2016</div>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2015</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2014</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2013</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2012</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2011</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2010</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2009</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2008</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2007</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2006</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2005</div></img>",
                  "<img src='img/noedit.jpg' width=20px><div class='jhr'>2004</div></img>",
                  "<div class='jhr'>2003</div>", "<div class='jhr'>2002</div>",
                  "<div class='jhr'>2001</div>", "<div class='jhr'>2000</div>",
                  "<div class='jhr'>1999</div>", "<div class='jhr'>1998</div>",
                  "<div class='jhr'>1997</div>", "<div class='jhr'>1996</div>",
                  "<div class='jhr'>1995</div>", "<div class='jhr'>1994</div>",
                  "<div class='jhr'>1993</div>", "<div class='jhr'>1992</div>",
                  "<div class='jhr'>1991</div>", "<div class='jhr'>1990</div>"), .Dim = c(30L,
                                                                                          3L), .Dimnames = list(NULL, c("Jahr", "lock", "img")))
#######################

renderUI1 <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  renderFunc <- function(shinysession, name, ...) {
    result <- func()
    if (is.null(result) || length(result) == 0)
      return(NULL)
    shiny:::processDeps(result, shinysession)
  }
  markRenderFunction(uiOutput, renderFunc, outputArgs = NULL)
}

## shiny app  #################
ui <- fluidPage(uiOutput("yearFinal")
                ,uiOutput("yearFinalNEW")
                ,uiOutput("yearFinalNEW1")
                )
server <- function(input, output, session) {
  output$yearFinal <- renderUI({
    print("make yearFinal UI -------------------")
    pickerInput(inputId = "year", label = "", choices = mD[,"Jahr"], selected = "2019",
                width = "120px", choicesOpt = list(content = mD[,"img"]),  inline = TRUE)
  })
  output$yearFinalNEW <- renderUI({
    print("make yearFinal UI -------------------")
    pickerInputNEW(inputId = "year", choices = mD[,"Jahr"], selected = "2019",
                choicesOpt = list(content = mD[,"img"]))
  })
  output$yearFinalNEW1 <- renderUI1({
    print("make yearFinal UI -------------------")
    pickerInputNEW(inputId = "year", choices = mD[,"Jahr"], selected = "2019",
                choicesOpt = list(content = mD[,"img"]))
  })
}

shinyApp(ui, server)

## profvis #################
# library(profvis)
# profvis(interval=0.005, {
#   shiny::runApp(appDir = "C:/Users/Bobo/Documents/TraffiCon/VDE_TIROL/shinyWidgets_fork/shinyWidgets/inst/examples/test")
# })

# mc <- microbenchmark::microbenchmark(
#   a=pickerInput(inputId = "year", label = "", choices = mD[,"Jahr"], selected = "2019",
#                    width = "120px", choicesOpt = list(content = mD[,"img"])),
#   b=pickerInputNEW(inputId = "year", choices = mD[,"Jahr"], selected = "2019",
#                    choicesOpt = list(content = mD[,"img"]))
# ); mc
#
# pickerInputNEW(inputId = "year", choices = mD[,"Jahr"], selected = "2019",
#                choicesOpt = list(content = mD[,"img"]))
#
#
# choices = mD[,"Jahr"]
# my_check <- function(values) {all(sapply(values[-1], function(x) identical(values[[1]], x)))}
# mc <- microbenchmark::microbenchmark(check = my_check,
#   a={choicesWithNames(choices)},
#   b={
#     chl <- as.list(choices)
#     names(chl) <- choices
#     chl
#   }
# ); mc
