# Final Assignment BIOL01301

>Due as a **Pull Request** to `sjspielman/datascience_final_assignment` by Tuesday May 12th at 11:59 pm. Due to university grade deadlines there are absolutely NO exceptions!!
> 
> This assignment grade is worth *two* regular assignments

#### Setting up


1. Fork the repository `sjspielman/datascience_final_assignment` into your github account, and clone your repository for use
2. Create a directory `lastname_firstname`, and use `git mv` to move the files `covid_data_load.R` and `app.R` into your directory. Add, commit, push, and you're off to the races!

#### Part One: Prepare the data in `covid_data_load.R`

You will read in three files (two from JHU and one from NYT) and ultimately produce *two TIDY tibbles* for use in the shiny app:

1) A tibble of the NYT data *exactly called* `nyt_data` that should ultimately contain these final columns *that use these exact names*:

+ `date`
+ `county`
+ `state`
+ `fips` (this is a location code used by maps, stands for "Federal Information Processing Standard")
+ `covid_type` (A categorical variable containing either "cases" or "deaths")
+ `total_number` (The number associated with `covid_type`)

2) A tibble of the JHU data *exactly called* `jhu_data` that should ultimately contain these final columns *that use these exact names*:

+ `province_or_state`
+ `country_or_region`
+ `latitude`
+ `longitude`
+ `date`
+ `covid_type` (A categorical variable containing either "cases" or "deaths")
+ `total_number` (The number associated with `covid_type`)


**Notes and hints:**

+ *After* you finish making the JHU tibble, you need to re-cast the date column for each to clearly be treated as a date: `jhu_data$date <- lubridate::as_date(jhu_data$date)`. The date column in the NYT data should have been read in properly as a date (since this was a tidy column in the first place). You can also do the re-casting as `mutate()` if you are comfortable with that approach.
+ You do NOT need to save the data with `write_csv()`!! Because this script is *sourced* in the shiny app, all variables you create in this script are fully usable within the app itself. Don't make your life harder than it needs to be.

#### Part Two: Make a shiny application!

Your shiny app will live in the file `app.R` - **never change the name of this file.** Your final application will have two panels (one for NYT data and one for JHU data), each with its own input sidepanel and mainpanel for output - this has already been templated for you!

Each panel will reveal a *line plot* of its associated data. The line

with plotting preferences chosen by the user! 



All data here is *cumulative* (the total number of cases or deaths up to and including that day!)

1) NYT plot user-input options

+ Select the viridis color scheme to use (this input widget is templated for you)
+ Select how color will be applied
+ Select which state to display
+ Starting time point (i.e., where will the X-axis start?), one of:
	+ Since beginning of data record
	+ Since first *case* in state (HINT: filter for case > 0!
+ How to display counties in the plot
	+ Ignore county differences - just make one plot for the whole state
	+ Facet the plot by county
+ Whether to display the plot legend 


**Notes and hints:**

+ For each NYT and JHU, you should define a *baseline plot* 
+ As part of your code, you will need to write `if statements`. Remember that there are examples in the class slides for this, and also here! For example, how your plot is displayed

	```
	# user input variable called here `input_thing` might be either True/False. Whether it's T/F will indicate how the plot should be made
	input_thing <- True
	if (input_thing) {
		ggplot(....) -> p
	} else
	{
		ggplot(....) -> p
	}
	```
	
### Resources

1. [All the shiny control widgets (inputs!)](https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/)
2. [All the shiny outputs](https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/)


### Bonus opportunities

+ Instead of using default shiny widgets, use widgets associated with the library [shinyWidgets](https://github.com/dreamRs/shinyWidgets)
+ Toss the template out the window and make a [*Shiny Dashboard*](https://rstudio.github.io/shinydashboard/) with your own beautiful design
+ Include a third tabPanel displaying a map of USA (using the NYT data) where states are colored by either cases or deaths. There should be an input option indicating whether cases or deaths should be displayed, and you can use whatever color scheme you want!
+ *Serve* the application with a public-facing URL with a free-tier account on [https://www.shinyapps.io/](https://www.shinyapps.io/)