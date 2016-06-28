![nba.png](nba.png?raw=true)

<br>

The 2016 NBA season is over (with one of the greatest comeback in history from Cleveland) and I will use that as a pretext to conduct a little analysis of the NBA All-Stars, that is the players who have been selected for the NBA All-Star Game at least once in their career.

This analysis is mainly (if not entirely) a graphical analysis made in `R` with the help of the well known `ggplot2` package. And a hint of `Shiny` and `radarchart`.

Data come from a variety of web sites ([Wikipedia](https://en.wikipedia.org), [Basketball-Reference.com](http://www.basketball-reference.com/)) with a little bit of web scraping (`Beautiful Soup` is a `Python` library designed for that particular task and a tool of choice).

All data and code are available.

## The Data

Wikipedia has a [list](https://en.wikipedia.org/wiki/List_of_NBA_All-Stars) of all the players who have been selected for the NBA All-Star Game. I decided to focus the analysis on a subset of that list: I kept only the players drafted in 1990 and after.
That means 131 of the 406 NBA All-Stars.

There are at least two reasons for that choice: 
* the NBA changed a few times the way (and what) statistics per game are collected ;
* I don't really know the players before 1990.

That list is not very rich (the name of the player, the number and the year(s) of selection and that's it) so I used `Beautiful Soup` to gather more data by scraping the Wikipedia page of each player and [Basketball-Reference.com](http://www.basketball-reference.com/) a website which has a lot of game statistics.

## All-Star one time, All-Star all the time?

The first thing we can look at is the number of selection(s) to the All-Star game.

And to be frank it was quite a surprise for me: about one third of the players have been selected only one time for the event! 

On the other hand, four players have been selected fifteen times or more: Kevin Garnett, Kobe Bryant, Shaquille O'Neal and Tim Duncan.

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

There is no clear tendency or pattern since 2013: the number of center selected is not as high as it was around 2005 but it's still really comparable with the 2010's numbers. Maybe we need a few more years to draw some conclusions about that change?

## Height and Weight

We can also investigate the height and weight of the All-Stars drafted since 1990. The first plot we can look at is a boxplot per position. No surprise here: centers are taller and heavier than point guards.

![weight_height_boxplot_positions.png](/plots/weight_height_boxplot_positions.png?raw=true)

What's interesting is the dispersion: low for the shooting guards and the power forwards and distinctly more important for the others.

We can also see some outliers (a point guard bigger than 2 meters? Is that you Penny Hardaway?) and we will focus on that just below.

![weight_height_scatterplot_names.png](/plots/weight_height_scatterplot_names.png?raw=true)

We have some giants - Yao and Shaq - and also a smaller guy: Isaiah Thomas. Larry Johnson is also a small guy for a power forward (less than 2 meters) when Penny (it was Penny) is a really tall point guard.

Even with those outliers the relation between height and weight is close to be linear. But the slope seems really different between the five positions. With `ggplot2`, it's really easy to check that visually (and we can also check the impact of the outliers at the same time).

![GIF_1](/plots/weight_height_linear_regressions_gif.gif)
