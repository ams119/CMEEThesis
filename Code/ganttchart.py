#!/bin/python3
#Author: Anne Marie Saunders
#Script: ganttchart.py
#Desc: Output a gantt chart for my project. Found on stack overflow https://stackoverflow.com/questions/56029002/is-there-a-way-to-create-gantt-charts-in-python
#Arguments: project_timeline.csv
#Date: 03/30/20

import pandas as pd
import numpy as np
import altair as alt
# alt.renderers.enable('notebook') # if in jupyter

df = pd.read_csv("../Data/project_timeline.csv")
df["start"] = pd.to_datetime(df["start"])
df["end"] = pd.to_datetime(df["end"])

chart = alt.Chart(df.mark_bar().encode(
    x='start',
    x2='end',
    y=alt.Y('task', 
            sort=list(df.sort_values(["end", "start"])
                                    ["task"])), # Custom sorting
)

chart.save('../Results/gantt.png')