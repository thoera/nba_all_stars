# -*- coding: utf-8 -*-

"""
This script scrapes basketball-reference.com for each player who has been 
a NBA All-Star since 1990.
The objective is to gather the per game career's stats.
"""

# Import the libraries.
import requests
from bs4 import BeautifulSoup
import re
import pandas as pd

# Read the names of the players drafted in 1990 or after.
with open(r"..\R\nba_all_stars\players_1990.txt") as f:
    players = f.read().splitlines()

# Create a dictionary: the keys will be the names of the players and 
# the values will be their career statistics.
players_info = {}

categories = ["G", "GS", "MP", "FG", "FGA", "FG_p", "3P", "3PA", "3P_p", "2P", 
              "2PA", "2P_p", "eFG_p", "FT", "FTA", "FT_p", "ORB", "DRB", "TRB", 
              "AST", "STL", "BLK", "TOV", "PF", "PTS"]

players_error_url = []
players_error_name = []
players_error_date = []

for player in players:
    print(player)
    url = ("http://www.basketball-reference.com/players/" + 
           player.split(" ")[1].lower()[0] + "/" + 
           re.sub("[^a-zA-Z]+", "", player.split(" ")[1].lower())[:5] + 
           player.split(" ")[0].lower()[:2] + "01.html")
    response = requests.get(url, params={"action": "render"}, timeout=10)
    soup = BeautifulSoup(response.content, "lxml")
    try:
        name = soup.findAll("span", itemprop="title")[3].get_text()
    except IndexError:
        players_error_url.append(player)
        print("----- The url doesn't exist!")
        name = None
    if name:
        text_info_box = soup.find("div", id="info_box").findAll(["span", "a"])    
        nba_debut = re.search('.BA Debut:</span>, ' + 
                              '<a href="/boxscores/.+html">' +
                              '([A-Za-z]+ [0-9]+, [0-9]+?)</a>', 
                              str(text_info_box)).group(1)
        if name != player:
            players_error_name.append(player)
            print("----- The name doesn't match!")
        if int(nba_debut[-4:]) < 1990:
            players_error_date.append(player)
            print("----- The date doesn't match!")
        else:
            try:
                table = soup.find("table",
                                  id="per_game").find("tr", 
                                                      {"class": "stat_total"})
                values = []
                for i in table.findAll("td"):
                    values.append("".join(str(item) for item in i.contents))
                players_info[player] = dict(zip(categories, values[5:]))
            except AttributeError:
                players_error_name.append(player)
                print("----- Something's wrong!")
print("")

# The script raised an IndexError for three players: Metta World Peace, 
# Nick Van Exel and Penny Hardaway. We'll use a custom url for these guys.
custom_urls = ["http://www.basketball-reference.com/players/a/artesro01.html",
               "http://www.basketball-reference.com/players/v/vanexni01.html",
               "http://www.basketball-reference.com/players/h/hardaan01.html"]

dict_players_urls = dict(zip(players_error_url, custom_urls))

for player, url in dict_players_urls.items():
    response = requests.get(url, params={"action": "render"}, timeout=10)
    soup = BeautifulSoup(response.content, "lxml")
    table = soup.find("table", id="per_game").find("tr", 
                                                   {"class": "stat_total"})
    values = []
    for i in table.findAll("td"):
        values.append("".join(str(item) for item in i.contents))
    players_info[player] = dict(zip(categories, values[5:]))

# Some urls are wrong. They exist but they don't match with the player's name
# or with the date. We have to make a slight change to the urls: "...02.html"
for player in set(players_error_name + players_error_date):
    print(player)
    url = ("http://www.basketball-reference.com/players/" + 
           player.split(" ")[1].lower()[0] + "/" + 
           re.sub("[^a-zA-Z]+", "", player.split(" ")[1].lower())[:5] + 
           player.split(" ")[0].lower()[:2] + "02.html")
    response = requests.get(url, params={"action": "render"}, timeout=10)
    soup = BeautifulSoup(response.content, "lxml")
    try:
        name = soup.findAll("span", itemprop="title")[3].get_text()
    except IndexError:      
        print("----- The url doesn't exist!")
        name = None
    if name:
        text_info_box = soup.find("div", id="info_box").findAll(["span", "a"])    
        nba_debut = re.search('.BA Debut:</span>, ' + 
                              '<a href="/boxscores/.+html">' +
                              '([A-Za-z]+ [0-9]+, [0-9]+?)</a>', 
                              str(text_info_box)).group(1)
        if name != player:
            print("----- The name doesn't match!")
        if int(nba_debut[-4:]) < 1990:
            print("----- The date doesn't match!")
        else:
            try:
                table = soup.find("table",
                                  id="per_game").find("tr", 
                                                      {"class": "stat_total"})
                values = []
                for i in table.findAll("td"):
                    values.append("".join(str(item) for item in i.contents))
                players_info[player] = dict(zip(categories, values[5:]))
            except AttributeError:
                print("----- Something's wrong!")
print("")

# Still an error with Mo Williams: "Mo" is not his real firstname.
url = "http://www.basketball-reference.com/players/w/willima01.html"
response = requests.get(url, params={"action": "render"}, timeout=10)
soup = BeautifulSoup(response.content, "lxml")
table = soup.find("table", id="per_game").find("tr", {"class": "stat_total"})
values = []
for i in table.findAll("td"):
    values.append("".join(str(item) for item in i.contents))
players_info["Mo Williams"] = dict(zip(categories, values[5:]))

# Convert the dictionary to a data frame.
df = pd.DataFrame(players_info).T

# Save it.
# df.to_csv(r"..\R\nba_all_stars\nba_all_stars_stats.csv", sep=";",
#           header=True, encoding="utf-8")
