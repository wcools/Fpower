
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
					selectInput("dbFree", "free parameter:", c("select one"="NA","sample size"="ss","effect size"="es","power"="b"),selected="select one"),
					uiOutput("sld.ntotal"),
					uiOutput("sld.eff"),
					uiOutput("sld.pwr"),
					selectInput("dbAlpha", "type I error:", c(".001" = ".001",".01" = ".01",".05" = ".05",".1" = ".1"),selected=".01"),
					numericInput("dbPred", "number of groups:", 3, min = 2, max = 8),
					htmlOutput('txt.out.2')
				)
			),
			column(4,
				# wellPanel(
					# htmlOutput('txt.out.1')
				# ),
				selectInput("dbPlots", "select plot", c("Power Curve" = "1","Ho and Ha" = "2","Ho" = "3"),selected="1"),
				conditionalPanel("condition=input.dbPlots == 1",plotOutput("plotPowerCurve")),
				conditionalPanel("condition=input.dbPlots == 2",plotOutput("plotHoHaF")),
				conditionalPanel("condition=input.dbPlots == 3",plotOutput("plotHoF"))#,
				# uiOutput("checkPlotPowercurve"),
				# uiOutput("checkPlotHoF"),
				# uiOutput("checkPlotHoHaF"),
				# plotOutput("plotPowerCurve"),
				# plotOutput("plotHoHaF"),
				# plotOutput("plotHoF")
			),
			column(4,
				br(),
				br(),
				br(),
				plotOutput("plotHoHa2")
				# ,
				# wellPanel(
					# htmlOutput('txt.out.2')
				# )
			)
		)
	)
)