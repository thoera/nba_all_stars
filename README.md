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

## Number of selections

The first thing we can look at is the number of selection(s) to the All-Star game.

And to be frank it was quite a surprise for me: about one third of the players have been selected only one time for the event and just a little less than 1/3 more than three times.

![number_of_selections_histogram.png](/plots/number_of_selections_histogram.png?raw=true)




