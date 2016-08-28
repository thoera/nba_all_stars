![nba.png](nba.png?raw=true)

<br>

The 2016 NBA season is over (with one of the greatest comeback in history from Cleveland) and I will use that as a pretext to conduct a little analysis of the NBA All-Stars, that is the players who have been selected for the NBA All-Star Game at least once in their career.

This analysis is mainly (if not entirely) a graphical analysis made in `R` with the help of the well known `ggplot2` package. And a hint of `shiny` and `radarchart`.

Data come from a variety of web sites ([Wikipedia](https://en.wikipedia.org), [basketball-reference.com](http://www.basketball-reference.com/)) with a little bit of web scraping (`Beautiful Soup` is a `Python` library designed for that particular task and a tool of choice).

## Data

Wikipedia has a [list](https://en.wikipedia.org/wiki/List_of_NBA_All-Stars) of all the players who have been selected for the NBA All-Star Game. I decided to focus the analysis on a subset of that list: I kept only the players drafted in 1990 and after.
That means 131 of the 406 NBA All-Stars.

There are at least two reasons for that choice: 
* the NBA changed a few times how (and what) statistics per game are collected ;
* I don't know the players drafted before 1990 as well (and that's probably the main reason).

The wikipedia list is not very rich (the name of the player, the number and the year(s) of selection and that's it) so I used `Beautiful Soup` to gather more data by scraping the own Wikipedia page of each player and also  [basketball-reference.com](http://www.basketball-reference.com/) a website which has a lot of game statistics.

## All-Star one time, All-Star all the time?

The first thing we can look at is the number of selection(s) to the All-Star game.

And to be frank it was quite a surprise for me: about one third of the players have been selected only one time for the event! 

On the other hand, four players have been selected fifteen times or more: Kevin Garnett, Kobe Bryant, Shaquille O'Neal and Tim Duncan (no surprise here: four legends of the game).

![number_of_selections_histogram.png](/plots/number_of_selections_histogram.png?raw=true)

## Is there a better position to be selected? 

First let's look at the distribution of the players selected.

![positions_dotplot.png](/plots/positions_dotplot.png?raw=true)

It seems that power forwards and point guards have better chances to be selected to the All-Star Game than the others. 

But we should be careful here: a good proportion of the players have more than one position listed on their wikipedia page and I kept only the first one when it was the case. Therefore there could be some bias in this approach.

Let's talk a bit of the selection process.

There are two ways to be selected: 
* fans vote on the starters for the game, selecting three frontcourt players and two guards. 
* coaches vote for the reserves: two guards, three frontcourt players and two wild cards.

Before 2013 it was a little different: fans selected two forwards and one center instead of three frontcourt players.
Can we see that change in the data?

![positions_by_year_facets.png](/plots/positions_by_year_facets.png?raw=true)

There is no clear tendency or pattern in that plot: the number of center selected in 2013 and after is not as high as it was around 2005 but it's still really comparable with the 2010's numbers. Maybe we need a few more years to draw some conclusions about that change?

## Height and Weight

We can also investigate the height and weight of the All-Stars drafted since 1990. The first plot we can look at is a boxplot per position. No surprise here: centers are taller and heavier than point guards.

![weight_height_boxplot_positions.png](/plots/weight_height_boxplot_positions.png?raw=true)

What's interesting is the dispersion: low for the shooting guards and the power forwards and distinctly more important for the others.

We can also see some outliers (a point guard taller than 2 meters? Is that you Penny Hardaway?) and we will focus on that just below.

![weight_height_scatterplot_names.png](/plots/weight_height_scatterplot_names.png?raw=true)

We have some giants - Yao and Shaq - and also a small guy: Isaiah Thomas. Larry Johnson is also a small guy for a power forward (less than 2 meters) when Penny (yes, it was Penny) is a really tall point guard.

Even with those outliers the relation between height and weight is close to be linear. But the slope seems really different between the five positions. With `ggplot2`, it's really easy to check that visually (and we can also check the impact of the outliers at the same time).

![GIF_1](/plots/weight_height_linear_regressions_gif.gif)

## Stats, always more stats!

With a little bit of web scraping (see `scraping_basketball-reference.py` for the code) I was able to gather some game statistics from [basketball-reference.com](http://www.basketball-reference.com/). This website is rich and usefull if you like game statistics and basketball: highly recommended! Another great ressource is [stats.nba.com](http://stats.nba.com/) which has a wonderful API.

### A correlation matrix

Thanks to our web scraping we now have 25 more variables to dive a little deeper. Those features are mostly *per game* statistics and not *career* statistics so it's easier to have a fair comparison between players.      

Let's start with a correlation matrix: a usefull tool to find some relations in the data.

![heatmap.png](/plots/heatmap.png)

The darker the blue is, the stronger the positive correlation between two variables is. For instance there is a strong positive correlation between the number of turnovers and the number of assists per game.

On the other hand, a strong negative correlation is represented by a light tile. For example we can see a strong negative correlation between the number of offensive rebounds and the free throw percentage (some great examples of that are Shaq, Big Ben or DeAndre Jordan).

### Let's cluster these guys!

We can also focus a bit on the players themselves and try a clustering method. When the number of observations (i.e. the number of players in that particular case) is quite small I like to begin with a hierarchical cluster analysis which is a relatively simple method to understand and still gives pretty good results in general.

The number of observations is small enough to compute the clustering instantaneously but big enough to make the dendrogram difficult to read so I don't show it (but [here](/plots/hca.pdf) is the pdf version where you can zoom in to see the labels). 
The clustering was done with all the numeric variables (scaled) with the exception of the number of selections and the number of games played.

First things first let's look quickly at the number of players in each cluster:

| cluster | number of players |
| :---: | :---: |
| #1 | 35 |
| #2 | 47 |
| #3 | 20 |
| #4 | 29 |

There is nothing too important to highlight here except maybe the fact that the cluster #2 is a big one (more than one third of the players).

We can now graphically investigate these clusters with a few boxplots.

![hca_clusters_boxplot_stats.png](/plots/hca_clusters_boxplot_stats.png?raw=true)

With the help of these plots we can briefly describe our four clusters:
* cluster #1: a cluster of big guys (not a lot of points, few assists, good rebounders and blockers) ;
* cluster #2: mainly point guards and shooting guards (good shooters, very few rebounds and blocks...) but probably not always the franchise players ;
* cluster #3: the Stars of the All-Stars: K. Durant, L. James, K. Bryant, S. Curry and so on ;
* cluster #4: a cluster of tall guys who are good scorers (in terms of points and percentages) and also good in defense (players like K. Garnett, B. Griffin, D. Cousins, etc.).

![hca_clusters_boxplot_per.png](/plots/hca_clusters_boxplot_per.png?raw=true)

If we want to see our clusters on a 2D plot one of the easiest way is to perform a PCA and then project the players on the first two principal components (or at least two components).

Drawing the correlation circle is useful to understand what variables contribute to the axis and how ("positively" vs. "negatively"). It's also a way to confirm our interpretation of the clusters.

![pca_variable.png](/plots/pca_variable.png?raw=true)

The first dimension reflects the opposition between players who are good shooters (at long range and behind the foul line) and good creators with players who are more defensive and have a good percentage close to the circle. 
The second dimension higlights mainly the scorers.

And here is the result of the projection:

![hca_pca_projection.png](/plots/hca_pca_projection.png?raw=true)

The four clusters are well seperated and we can confirm our previous interpretation. That's a good news!

## A shiny app

I made a small app which can be used to compare up to five players (beyond that the plots becomes too difficult to read to my taste).

An example with the new Big Three:

![app.png](/app/app.png?raw=true)

If you want to give it a try, the app is available on [shinyapps.io](https://thoera.shinyapps.io/nba_all_stars/).
