
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
			column(2,
				wellPanel(
					uiOutput("dd.free")
				),
				wellPanel(
					uiOutput("sld.bvar"),
					uiOutput("sld.wvar")
				)
			),
			column(2,
				wellPanel(
					uiOutput("sld.ntotal"),
					uiOutput("sld.eff"),
					uiOutput("sld.beta"),
					# uiOutput("db.alpha"),
					selectInput("dbAlpha", "type I error:", c(".001" = ".001",".01" = ".01",".05" = ".05",".1" = ".1"),selected=".01")
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
				plotOutput("plot1")
			)
		),
		fluidRow(
			column(2,
					h4("sample sizes"),
					uiOutput("sld.ng1"),
					uiOutput("sld.ng2"),
					uiOutput("sld.ng3"),
					uiOutput("sld.ngs")
			),
			column(2,
					h4("averages"),
					uiOutput("sld.avg1"),
					uiOutput("sld.avg2"),
					uiOutput("sld.avg3")
			),
			column(2,
					h4("standard deviations"),
					uiOutput("sld.sdg1"),
					uiOutput("sld.sdg2"),
					uiOutput("sld.sdg3")
			),
			column(6,
				plotOutput("distPlot")
			)
		)
	)
)