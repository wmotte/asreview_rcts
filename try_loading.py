#!/usr/bin/env python
#
# Try loading model and exporting probabilities
###############################################

import matplotlib.pyplot as plt

from asreview import ASReviewProject, open_state

from asreview import open_state
from asreviewcontrib.insights.plot import plot_wss

with open_state( "data.wim/labeled_data/reviews/3-rodent-studies-only.asreview", read_only = False ) as s:

    fig, ax = plt.subplots()
    plot_wss(ax, s, priors=True)

    fig.savefig("example_absolute_axis.png")

with open_state( asreview_obj = "data.wim/labeled_data/reviews/3-rodent-studies-only.asreview", read_only = True ) as piet:
    piet()

#

infile_review = "/Users/wim/Desktop/projects/asreview_rcts/data.wim/labeled_data/reviews/3-rodent-studies-only.asreview"
infile_data = "/Users/wim/Desktop/projects/asreview_rcts/data.wim/labeled_data/dataset_rodent-studies-only.csv"

from asreview.data import ASReviewData
from asreview import ASReviewProject
from asreview.models.feature_extraction import Tfidf

my_data = ASReviewData.from_file( infile_data )

my_review = ASReviewProject( infile_review )

fe = Tfidf()
X = fe.fit_transform( my_data.texts )

# (4867, 54895)
print(X.shape)
print(fe._model)