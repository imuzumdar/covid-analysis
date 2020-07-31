# covid-analysis
These files contain statistical analyses that I performed to evaluate the attitudes of both physicians and the general public 
towards the COVID-19 pandemic. The findings of this research have been submitted to [Pediatrics](https://pediatrics.aappublications.org/) for publication
as the first study to assess the concerns and perspectives of pediatric faculty and trainees related COVID-19. In addition,
these findings will be presented to all faculty in the University of Pittsburgh's medicial system to inform relevant policy 
changes concerning the pandemic.

If you're interested in learning more about this project, please feel free to contact me at [imuzumdar@gmail.com](mailto:imuzumdar@gmail.com)!

## Analysis Overview
To characterize attitudes of the general public, I scraped Twitter data and performed some basic text mining to understand
relevant sentiments with regard to the pandemic. I performed some text cleaning using regex expressions and then tokenized
the tweets to remove stop words and create some simple visualizations. In this repo, you'll find a wordcloud I created and
a frequency plot of common sentiments related to covid-19. In the future, I plan to analyze geolocation data to investigate Twitter activity related to covid-19 across the country.

To measure the attitudes of physicians, I analyzed survey data collected from a tertiary hopstial. I then performed a variety of
statistical analyses. I performed nonparametric tests with repeated measures to capture levels of agreement across different 
demographic groups, fit weighted ML-inference learn-to-rank models to determine which government agencies physicians trusted the most,
created graph networks to visualize these rankings, and built ordinal logistic classifiers to measure the following:

1. Physicians' concerns related to SARS-CoV-2
2. Physicians' trust in current recommendations
3. Physicians' attitudes toward trainee roles during the pandemic

Since the data set contained over 30 response variables of interest, I also wrote automated scripts to clean the data, fit models,
perform model selection, and check all model diagnostics. Again, if you're interestd in learning more, feel free to email me
or view some of the visualizations and pdfs contained in this repo!
