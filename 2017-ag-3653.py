from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
def regi_plot():
if input == 1:

        plt.scatter(
            dfSLR["Income"]
            , dfSLR["Expenditure"]
            , color="red")
        plt.plot(
            dfSLR["Income"]
            , reg.fittedvalues,
            color="b")

        plt.title('Regression Line of Income and Expenditure')
        plt.xlabel('Income')
        plt.ylabel('Expenditure')

        plt.show()


    else:

        axes = plt.figure().gca(projection='3d')

        axes.scatter(

            dfMLR["x1"]
            , dfMLR["x2"]
            , dfMLR["y"]
            , color="green"

        )

        axes.set_title(titlestr)
        axes.set_xlabel(xlabstr, color="purple")
        axes.set_ylabel(ylabstr, color="purple")
        axes.set_zlabel(zlabstr, color="purple")

        # Parameters to create Shape/Grid using Fertilizer and Rainfall Data

        x_surf = dfMLR["x1"]
        y_surf = dfMLR["x2"]

        # The purpose of meshgrid is to create a rectangular grid out of an array of x and y values

        x_surf, y_surf = np.meshgrid(x_surf, y_surf)

        # To combine X and Y in order to generate Z for the surface

        exog = pd.core.frame.DataFrame({

            "x1": x_surf.ravel(), "x2": y_surf.ravel()

        })

        # Generating Predicted values for the shape

        out = reg.predict(exog=exog)

        # Generating the actual Shape now on Graph

        axes.plot_surface(

            x_surf, y_surf
            , out.values.reshape(x_surf.shape), rstride=1, cstride=1
            , alpha=0.2

        )

        plt.show()
def regi_analysis():

    print(reg.summary())
    print("\n\n", reg.params)
    print("\nFitted Values:\n", reg.fittedvalues)
    print("\nTSS =", reg.centered_tss)
    print("\nAnalysis of Variance\n", anova_lm(reg))

def numInput():
    try:

        number = int(input())

    except ValueError:

        print("You must enter a number between 1 & 3")
        numInput()

    else:

        if number == 1 or number == 2 or number == 3:
            return number
        else:
            print("You must enter a number between 1 & 3")
            numInput()
print("Enter a number to perform Linear Regression: \n1 for SLR\n2 for MLR (Yield)\n3 for MLR (y_x1_x2)\n")
input = numInput()
if input == 1:

    dfSLR = pd.DataFrame(
    {
    "Income": [80, 100, 120, 140, 160, 180, 200, 220, 240, 260],
    "Expenditure": [70, 65, 90, 95, 110, 115, 120, 140, 155, 150]
    }
    )
    print(dfSLR)

    reg = ols(formula="Expenditure ~ Income", data=dfSLR).fit()

    regi_analysis()


elif input >= 2:

    if input == 2:

        # Example 1
        dfMLR = pd.DataFrame(
            {
                "x1": [100, 200, 300, 400, 500, 600, 700],  # Fertilizer
                "x2": [10, 20, 10, 30, 20, 20, 30],         # Rainfall
                "y": [40, 50, 50, 70, 65, 65, 80]           # Yield
            }
        )
        titlestr = "Regression Line for Yield ~ Fertilizer + Rainfall"
        xlabstr = "Fertilizer"
        ylabstr = "Rainfall"
        zlabstr = "Yield"

    else:
        dfMLR = pd.DataFrame(
            {
                "x1": [0, 0, 10, 10, 20, 20],
                "x2": [0, 0, 100, 100, 400, 400],
                "y": [5, 7, 15, 17, 9, 11]
            }
        )
        titlestr = "Regression Line for y ~ x1 + x2"
        xlabstr = "x1"
        ylabstr = "x2"
        zlabstr = "x3"
    print(dfMLR)
    reg = ols(formula="y ~ x1 + x2", data=dfMLR).fit()
    regi_analysis()
    regi_plot()
