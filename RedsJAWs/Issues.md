Issues
================

-   [Pitcher JAWS](#pitcher-jaws)
-   [Names](#names)
-   [Home](#home)
-   [JAWS](#jaws)

Pitcher JAWS
------------

Currently the WAR values in the openWARData package use a pre-2013 Baseball-Reference replacement player level. After randomly choosing some players, this change only seems to have affected Pitcher WAR. I've created an [issue](https://github.com/beanumber/openWARData/issues/11) and they seem to be looking into it.

While the WAR values won't match the values at the Baseball-Reference site, it shouldn't affect the efficacy of the project too much. There isn't any mixing of pre-2013 WAR with post-2013 WAR so player comparisons are still apples to apples.

Names
-----

I worked with Baseball-Reference and FanGraphs data and there were some name conflicts. Other sources agreed with Baseball-Reference for most if not all of the instances. Other incidents involved using nicknames or not. Baseball-Reference tended to use nicknames and I'd already used their names for the other conflicts so it was easier to just stick with them. Plus some of the nicknames were too cool not to use. For Griffey Jr., I decided to not use any punctuation.

-   Ken Griffey Jr not Ken Griffey ~~Jr.~~ or ~~JR~~
-   Dick Hoblitzell not Dick ~~Hoblitzel~~
-   Mike Smith not ~~Elmer~~ Smith
-   Ed Taubensee not ~~Eddie~~ Taubensee
-   Gene Thompson not ~~Junior~~ Thompson
-   High Pockets Kelly not ~~George~~ Kelly
-   Ice Box Chamberlain not ~~Elton~~ Chamberlain

Home
----

On the home page of the dashboard, you might notice there isn't a distribution for third basemen. You might also notice that there are only two players with their primary position being third base.

JAWS
----

Table: For pitchers, the values in the weighted columns aren't weighted. I didn't want to create a separate column for pitchers.

Cleveland Dot Plots: In the legend, the label "Typical HOFer" says the value is weighted. This is ***not*** the case for pitchers or groups such as CI, MI, OF, CO, or Md.