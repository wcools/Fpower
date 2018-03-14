
# freePar ~ dropdown("",...) || sst ~ textfield 	|| e1 ~ textfield || e2 ~ textfield (indicator power) || fs ~ textfield || Bvar/Wvar ~ textfields 0-1

# nrPrdtF ~ dropdown(2,3)	-> nrPrd				|| ncp ~ text || crf ~ text || dfn ~ text || dfd ~ text 
# sample sizes:
# prd 1:: ss1 ~ textfields for number 0-100
# prd 2:: ss2
# prd 3:: ss3
# sample sizes:
# prd 1:: av1 ~ textfields for number -100 - 100
# prd 2:: av2
# prd 3:: av3
# sample sizes:
# prd 1:: sd1 ~ textfields for number 0 - 100
# prd 2:: sd2
# prd 3:: sd3

shinyUI(fluidPage(

	titlePanel("Exploring power for one-way ANOVA with F-distribution"),
		fluidRow(
			column(4,
				wellPanel(
					# selectInput("dbFree", "free parameter:", c("select one"="NA","sample size"="ss","effect size"="es","type II error"="b"),selected="select one"),
					# uiOutput("dd.free"),
					selectInput("dbFree", "free parameter:", c("select one"="NA","sample size"="ss","effect size"="es","type II error"="b"),selected="select one"),
					uiOutput("sld.ntotal"),
					uiOutput("sld.eff"),
					uiOutput("sld.beta"),
					selectInput("dbAlpha", "type I error:", c(".001" = ".001",".01" = ".01",".05" = ".05",".1" = ".1"),selected=".01"),
					numericInput("dbPred", "number of categories:", 2, min = 2, max = 8)
					# selectInput("dbPred", "number of categories:", as.character(2:8),selected=2)
					# ,
					# uiOutput("sld.bvar"),
					# uiOutput("sld.wvar")
				)
			),
			column(4,
				wellPanel(
					htmlOutput('txt.out')
				),
				plotOutput("plotHoHa")
			),
			column(4,
				wellPanel(
					textOutput('')
				),
				plotOutput("plot1"),
				plotOutput("plotGetHoF"),
				plotOutput("plotHoHaF"),
				plotOutput("plotPowerCurve")
			)
		)
	)
)