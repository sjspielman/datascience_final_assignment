# Final Assignment BIOL01301

>Due as a **Pull Request** to `sjspielman/datascience_final_assignment` by Tuesday May 12th at 11:59 pm. Due to university grade deadlines there are absolutely NO exceptions!!
> 
> This assignment grade is worth *two* regular assignments

#### Setting up

1. Fork the repository `sjspielman/datascience_final_assignment` into your github account, and clone your repository for use
2. Create a directory `lastname_firstname`, and use `git mv` to move the files `covid_data_load.R` and `app.R` into your directory. Add, commit, push, and you're off to the races!

#### Part One: Prepare the data in `covid_data_load.R`

You will read in three files (two from JHU and one from NYT) and ultimately produce *two TIDY tibbles* for use in the shiny app. Bear in mind: All data here is *cumulative* (the total number of cases or deaths up to and including that day!).


1) A tibble of the NYT data *exactly called* `nyt_data` that should ultimately contain these final columns *that use these exact names*:

	+ `date`
	+ `county`
	+ `state`
	+ `fips` (this is a location code used by maps, stands for "Federal Information Processing Standard")
	+ `covid_type` (A categorical variable containing either "cases" or "deaths")
	+ `cumulative_number` (The number associated with `covid_type`)

2) A tibble of the JHU data *exactly called* `jhu_data` that should ultimately contain these final columns *that use these exact names*:

	+ `province_or_state`
	+ `country_or_region`
	+ `latitude`
	+ `longitude`
	+ `date`
	+ `covid_type` (A categorical variable containing either "cases" or "deaths")
	+ `cumulative_number` (The number associated with `covid_type`)


**Notes and hints:**

+ *After* you finish making the JHU tibble, you need to re-cast the date column for each to clearly be treated as a date: `jhu_data$date <- lubridate::as_date(jhu_data$date)`. The date column in the NYT data should have been read in properly as a date (since this was a tidy column in the first place). You can also do the re-casting as `mutate()` if you are comfortable with that approach.
+ You do NOT need to save the data with `write_csv()`!! Because this script is *sourced* in the shiny app, all variables you create in this script are fully usable within the app itself. Don't make your life harder than it needs to be.

#### Part Two: Make a shiny application!

Your shiny app will live in the file `app.R` - **never change the name of this file.** Your final application will have two panels (one for NYT data and one for JHU data), each with its own input sidepanel and mainpanel for output - this has already been templated for you!

Each panel will reveal a *line plot* of its associated data. Components of a line plot should include:

+ Time along the X-axis and cumulative number on the Y-axis
+ Use points within your line plot to emphasize the time points
+ Color lines based on `covid_type`

There should be user-input widgets associated with each plot that indicate what should be plotted:

**For NYT data, there must be at least FIVE widgets:** (more widgets might get you some bonus if they make sense and work!)

+ Choice for which state to plot
+ Option to show counties as facets (using `facet_wrap()`), OR "ignore" county distinctions and show all data for the state in a single plot
+ Option to start the X-axis on the date of first CASE for that state (HINT: filter data for cases > 0!!), versus show all dates with data
+ Theme for the plot (users should have at least FIVE options to choose from). These can either be built-in ggplot themes, or they can be from a different library of your choosing.
+ Options for colors to use (this has been templated for you, with defaults - please choose your own defaults!)

**For JHU data, there must be at least FOUR widgets:** (more widgets might get you some bonus if they make sense and work!)

+ Choice for which country/region to plot
+ Option to start the X-axis on the date of first CASE for that state, versus show all dates with data
+ Theme for the plot (users should have at least FIVE options to choose from). These can either be built-in ggplot themes, or they can be from a different library of your choosing.
+ Options for colors to use (this has been templated for you, with defaults - please choose your own defaults!)

    


**Notes and hints:**
+ Remember: Everything on the server side needs to be within an appropriate context!!
    + You should use **reactive** variables to store the subsetted data (e.g. subsetted to state/region of interest). This reactive variable should then be plotted.
    + Plots should be defined *within* `renderPlot({})` constructs.
+ You will need to use `if/else` constructs for adding the theme to plots! 
+ 99% of the bugs you have are because of missing/extra commas in the UI. Welcome to shiny.
+ Remember to choose your own app theme!!! See the line in `app.R` that opens `navbarPage()`
	
### Resources

1. [All the shiny control widgets (inputs!)](https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/)
2. [All the shiny outputs](https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/)
3. [THE DEFINITIVE tutorial](https://shiny.rstudio.com/tutorial/)


### Bonus opportunities

+ Instead of using default shiny widgets, use widgets associated with the library [shinyWidgets](https://github.com/dreamRs/shinyWidgets)
+ Include a third tabPanel displaying a map of USA (using the NYT data) where states are colored by either cases or deaths. There should be an input option indicating whether cases or deaths should be displayed, and you can use whatever color scheme you want!
+ Use the library `plotly` to make your plots interactive! 
+ Toss the template out the window and make a [*Shiny Dashboard*](https://rstudio.github.io/shinydashboard/) with your own beautiful design
+ *Serve* the application with a public-facing URL with a free-tier account on [https://www.shinyapps.io/](https://www.shinyapps.io/)
