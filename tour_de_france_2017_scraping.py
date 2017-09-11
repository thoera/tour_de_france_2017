# -*- coding: utf-8 -*-

import requests
from bs4 import BeautifulSoup
import re
import codecs


def get_results(stage):
    url = ("http://www.cyclingnews.com/tour-de-france/stage-" + str(stage) +
           "/results/")
    response = requests.get(url, params={"action": "render"}, timeout=10)
    soup = BeautifulSoup(response.content, "lxml")

    try:
        # the general classification after the last stage
        if stage == 21:
            gc = soup.find("caption",
                           text=re.compile("^Final general classification")) \
                 .find_parent("table")
        # the general classification after all the others stages
        else:
            gc = soup.find("caption",
                           text=re.compile("^General C|classification " +
                                           "after stage " + str(stage))) \
                     .find_parent("table")

        positions = []
        riders = []
        results = []

        for row in gc.findAll("tr")[1:]:
            content = row.findAll("td")

            positions.append(content[0].contents[0])
            riders.append(content[1].contents[0])
            results.append(content[2].contents[0])

        if stage < 10:
            stage = "0" + str(stage)
        with codecs.open("data/stage_" + str(stage) + ".csv",
                         "w", "utf-8") as output:
            output.write("position;rider;result\n")

            for position, rider, result in zip(positions, riders, results):
                output.write(position + ";" + rider + ";" + result)
                output.write("\n")

    except AttributeError:
        pass

for i in range(1, 22):
    get_results(stage=i)
