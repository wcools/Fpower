
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
			column(6,
				wellPanel(
					selectInput("freePar", "free parameter:", c("sample size" = "sst","effect size" = "esf","type I error" = "e1","type II error" = "e2"))
				)
			),
			column(6,
					verbatimTextOutput('outpars')
			)
		),
		fluidRow(
			column(2,
					sliderInput("sst", "sample size:", min = 2, max = 256, value = 128),
					sliderInput("fs", "effect size:", min = 0, max = 16, value = .25,step=.001)
			),
			column(2,
					selectInput("e1", "type I error:", c(".001" = ".001",".01" = ".01",".05" = ".05",".1" = ".1")),
					selectInput("e2", "type II error:", c(".2" = ".2",".15" = ".15",".1" = ".1",".05" = ".05"))
			),
			column(2,
					sliderInput("Bvar", "between variance:", min = 0, max = 32, value = 1),
					sliderInput("Wvar", "within variance:", min = 0, max = 32, value = 6)
			),
			column(6,
				wellPanel(
					# plotOutput("plot2"),
					plotOutput("plot2")
				)
			)
			
			
		# Bvar/Wvar ~ textfields 0-1

# nrPrdtF ~ dropdown(2,3)	-> nrPrd				|| ncp ~ text || crf ~ text || dfn ~ text || dfd ~ text 
# sample sizes:
		),
		fluidRow(
				  # tags$head(
					# tags$style(type="text/css", 
					  # "label.control-label, .selectize-control.single { 
						 # display: table-cell; 
						 # text-align: center; 
						 # vertical-align: middle; 
					  # } 
					  # label.control-label {
						# padding-right: 10px;
					  # }
					  # .form-group { 
						# display: table-row;
					  # }
					  # .selectize-control.single div.item {
						# padding-right: 15px;
					  # }")
				  # ),

			column(2,
					h4("sample sizes"),
					sliderInput("ss1", "level 1:", min = 0, max = 100, value = 32),
					sliderInput("ss2", "level 2:", min = 0, max = 100, value = 32),
					sliderInput("ss3", "level 3:", min = 0, max = 100, value = 0)
			),
			column(2,
					h4("average"),
					sliderInput("av1", "level 1:", min = -100, max = 100, value = -2),
					sliderInput("av2", "level 2:", min = -100, max = 100, value = 0),
					sliderInput("av3", "level 3:", min = -100, max = 100, value = 2)
			),
			column(2,
					h4("standard deviation"),
					sliderInput("sd1", "level 1:", min = 0, max = 100, value = 4),
					sliderInput("sd2", "level 2:", min = 0, max = 100, value = 4),
					sliderInput("sd3", "level 3:", min = 0, max = 100, value = 4)
			),

			column(9,
				plotOutput("distPlot")
			)
		)
	)
)