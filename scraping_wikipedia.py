# -*- coding: utf-8 -*-

"""
A script to scrape wikipedia for each player who has ever been a NBA All-Star.
There are two main objectives:
    - to gather more data on the players ;
    - to identify the guys drafted after 1990 (we will only keep them).
"""

# Import the libraries.
import requests
from bs4 import BeautifulSoup
import pandas as pd

# Load the dataset.
data = pd.read_csv(r"..\R\nba_all_stars\nba_all_stars_clean.csv",
                   sep=",", usecols=[0], header=0, encoding="utf-8")

# Create a dictionary: the keys will be the names of the players and 
# the values will be the personnal information.
players_info = {}

# Create a list which will be filled whith the player's name when 
# an error occurs (e.g. when we land on an homonym page rather than 
# the player's page).
players_error = []

# Loop over the players.
for player in data["player"]:
    print(player)
    # Search on wikipedia the player's page.
    url = "https://en.wikipedia.org/wiki/%s" % player
    response = requests.get(url, params={"action": "render"}, timeout=10)
    soup = BeautifulSoup(response.content, "lxml")
    # Check if it's the wikipedia page of the player or if it's a 
    # disambiguation page (which should contain "may refer to" in the first
    # paragraph).
    if "may refer to:" in str(soup.find("p")):
        print("----- Disambiguation page.")
        # If it's a disambiguation page add "(basketball)" to the url.
        url = "https://en.wikipedia.org/wiki/%s" % player + " (basketball)"
        response = requests.get(url, params={"action": "render"}, timeout=10)
        soup = BeautifulSoup(response.content, "lxml")
    # Try to find the player's information on the page (in the "infobox").
    table = soup.find("table", {"class": "infobox vcard"})   
    # If there is no "infobox" fill "players_error" with that player's name.
    if table is None:
        players_error.append(player)
        print("----- There is a problem with that page!")
    else:
        # Create an empty dictionary.
        dic = {}
        # Find the tags and fill the dictionary (the keys will be the names
        # of the variables and the values will be the observations).
        for i in table.findAll("th", scope="row"):
            key = i.findAll(text=True)
            value = i.findNext("td").findAll(text=True)
            dic["".join(key)] = "".join(value)
        # Fill the main dictionary.
        players_info[player] = dic

# There are two kinds of problems: 
#   - the page reached is someone else page -> make the research more specific 
# by adding (basketball) to the url ;
#   - two basketball players have the same name -> the birth year 
# has to be specified.

# Custom the url for the players who are listed in "players_error".
for player in players_error:
    print(player)
    if player in ["Johnny Green", "Adrian Smith", "Isaiah Thomas"]:
        url = "https://en.wikipedia.org/wiki/%s" % player + " (basketball)"
    if player == "Bobby Jones":
        url = ("https://en.wikipedia.org/wiki/%s" % player +
        " (basketball, born 1951)")
    if player == "Eddie Johnson":
        url = ("https://en.wikipedia.org/wiki/%s" % player +
        " (basketball, born 1955)")
    if player == "Larry Johnson":
        url = ("https://en.wikipedia.org/wiki/%s" % player +
        " (basketball, born 1969)")
    if player == "Mike Mitchell":
        url = ("https://en.wikipedia.org/wiki/%s" % player +
        " (basketball, born 1956)")
    if player == "Cliff Robinson":
        url = ("https://en.wikipedia.org/wiki/%s" % player +
        " (basketball, born 1966)")
    response = requests.get(url, params={"action": "render"}, timeout=10)
    soup = BeautifulSoup(response.content, "lxml")
    # Try to find the player's information on the page (in the "infobox").
    table = soup.find("table", {"class" : "infobox vcard"})
    # Create an empty dictionary.
    dic = {}
    # Find the tags and fill the dictionary (the keys will be the names of 
    # the variables and the values will be the observations).
    for i in table.findAll("th", scope="row"):
        key = i.findAll(text=True)
        value = i.findNext("td").findAll(text=True)
        dic["".join(key)] = "".join(value)
    # Fill the main dictionary.
    players_info[player] = dic

# More insidious: some players have a politician for homonym or are politicians
# themselves. Politicians also have an infobox on their wikipedia page 
# so the script didn't raise an error or didn't scrape the right infobox.
# A quick fix consists of adding "(basketball)" to the url for the players
# in the first case and to scrape the second infobox in the second.

# Find the politicians and adjust the script according to the case.
politicians = []

for player in players_info:
    if "Political party" in players_info[player]:
        print(player)
        politicians.append(player)

for player in politicians:
    print(player)
    if player == "Jimmy Walker":
        url = "https://en.wikipedia.org/wiki/%s" % player + " (basketball)"
        response = requests.get(url, params={"action": "render"}, timeout=10)
        soup = BeautifulSoup(response.content, "lxml")
        # Try to find the player's information on the page (in the "infobox").
        table = soup.find("table", {"class" : "infobox vcard"})
    else:
        url = "https://en.wikipedia.org/wiki/%s" % player
        response = requests.get(url, params={"action": "render"}, timeout=10)
        soup = BeautifulSoup(response.content, "lxml")
        # Try to find the player's information in the second "infobox".
        table = soup.findAll("table", {"class": "infobox vcard"})[1]
    # Create an empty dictionary.
    dic = {}
    # Find the tags and fill the dictionary (the keys will be the names of 
    # the variables and the values will be the observations).
    for i in table.findAll("th", scope="row"):
        key = i.findAll(text=True)
        value = i.findNext("td").findAll(text=True)
        dic["".join(key)] = "".join(value)
    # Fill the main dictionary.
    players_info[player] = dic

# Convert the dictionary to a data frame.
df = pd.DataFrame(players_info).T

# Keep only the columns we are interested in.
df = df.loc[:, ["NBA draft", "Listed height", "Listed weight", "Nationality",
                "College", "High school", "Playing career", "Position"]]

# Replace "\r" & "\n" with whitespaces and then replace extra (two or more),
# leading and trailing whitespaces.
for col in df.columns:
    df[col] = df[col].str.replace("\r|\n", " ").str.replace("(  +)"," ")\
    .str.strip()

# Split the column "NBA draft".
df2 = df["NBA draft"].str.split("/", expand=True)
df2.columns = ["draft_year", "draft_round", "draft_pick"]

# Concatenate df and df2.
df = pd.concat([df, df2], axis=1)

# Clean up the columns names: remove leading and trailing whitespaces, 
# lowercase all names and replace any remaining whitespaces with underscores.
df.columns = df.columns.str.strip().str.lower().str.replace(" ", "_")

# Save it.
df.to_csv(r"..\R\nba_all_stars\nba_all_stars_info.csv", sep=";", header=True,
          index_label="player", encoding="utf-8")
