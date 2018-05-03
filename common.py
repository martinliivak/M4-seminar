import numpy as np
import pandas as pd


def scaling(train, step=1):
    return train.diff(step).dropna().abs().mean()


def mase(forecast, test, train, masep=None):
    if masep is None and train is not None:
        masep = scaling(train)

    return np.mean(np.abs(np.asarray(test) - np.asarray(forecast))) / masep
