# AcousticsApp-Summer2019

## Summer Research, 2019

#### _Students:_ Michal Golovanevsky

#### _Faculty advisor:_ Hunter Glanz

### Objective

The objective of this summer research is to develop, expand, and polish an R Shiny app for doing exploratory bioacoustics.

### Deliverables

**A GitHub repository which contains the following:**

1.  An R project which contains all files relevant to the shiny app.

2.  A log of hours spent on the summer research by the student, which includes date, hours, and activity summary.

3.  A presentation to the Statistics Department.

4.  A presentation at the CSM annual research conference.

5.  A manuscript to submit to a journal to be determined.

### Specific Aims

**1.  Utilize GitHub to collaborate on project materials and updates.**

  * Karl Broman's [github tutorial](http://kbroman.org/github_tutorial/)

  * Jenny Bryan's [Happy git with R](http://happygitwithr.com/).
  
  * Also check out [using version control with RStudio](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN) and [this video on Git and RStudio](https://www.rstudio.com/resources/webinars/rstudio-essentials-webinar-series-managing-part-2/).


**2.  Adhere to good programming practices.**
  
  * Write all R code according to [Hadley Wickam's Style Guide](http://adv-r.had.co.nz/Style.html).
  
  * Use the [tidyverse style guide](http://style.tidyverse.org/) for an additional reference.
  
  * Use Hadley Wickham's [R for Data Science](http://r4ds.had.co.nz/) book as a reference (Ch19 also discusses functions).
  
  
  **3.  Create an R Shiny app that performs exploratory bioacoustics.**  

  *  RStudio's introduction and guide to [Shiny](https://shiny.rstudio.com/).
  
  *  Download [RavenLite software](http://ravensoundsoftware.com/software/raven-lite/) to familiarize yourself with working with sound data.
  
  *  Check out [RavenLite Manual](http://www.birds.cornell.edu/brp/raven/Raven14UsersManual.pdf) for information about sound data.
  
  *  Check out Bioacoustics App To-Do items below for initial objectives.
  
  *  Add "tab" to existing app to perform segmentation on the recording, and produce statistics for the segments.
  
  
   **4.  Provide documentation for the R Shiny app.**
  
  *  Write a [vignette](http://r-pkgs.had.co.nz/vignettes.html) to accompany the package.
  
  *  Consider using [pkgdown](http://pkgdown.r-lib.org/index.html) to create a website. 
  
  
  **5. Understand/Learn about bioacoustics.**
  
  *  What is a .wav file?
  
  *  What is a spectrogram?
  
  *  What is an oscillogram?
  
  *  What is a spectrum?
  
  *  What does segmentation mean with respect to a recording (.wav file)?
  
  *  Upload/Import .wav file into RavenLite and play around with it via Raven tutorial videos (through google)
  
  **6. Bioacoustics App To-Do Items and Possible Bugs?**
  
  *  Hover button for native info doesn't always work after changing parameters or for certain .wav files. Why? Fix?
  
  *  Make the max dB always print 0dB.
  
  *  Why doesn't spectrum work with dolphin .wav file?
  
  *  Make all fonts the same.
  
  *  Fix picture widths within the help options.
  
  *  Cite authors of certain pictures.
  
  *  Clickable zoom feature (oscillogram, spectrogram, or spectrum)?
  
  *  Why won't certain sample rates work for marine related files?
  
  *  Choosing a window function should change the spectrum. Why doesn't this happen right now?
  
  *  Fix the noise file.
  
  *  Better error messages.
  
  *  Add playback button with audio.
  
