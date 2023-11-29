Shiny apps using default (D) saved (S) live (L) and reactive (R) lists


# Introduction

Shiny applications are great. However, they are designed to be small. To get around
this for larger applications, the posit team made something they call shiny 
modules. These are difficult to comprehend and follow. As an alternative,
here I propose a simple system for managing data in a large and amorphous 
shiny application. This system uses four `reactiveValues` lists to manage
data at different stages of computation.

# The lists

The default list `D` contains unsurprisingly the default values. In R, one can
specify a list using a script and populate that list with a set of default values.
Then, in a shiny server those default values can be passed to the reactive and 
live lists (`R` and `L`) initially. 

Elements of `R` are updated whenever an input is changed in the UI, and any subsequent
downstream calculations are updated within the `R` list. In this sense, `R` is
"reactive" to what the user is doing in the application.   

- `R` responds immediately to changes in the UI's inputs
- `L` is NOT affected by `R`, creating a separation which controls the flow of information through the application



`L` is used as the `input = ` type arguments to all UI elements in the shiny application. This creates a separation between updating things in the user interface and the user interface refreshing because you have updated things. This also creates a separation between data existing and not existing at a point in time. This is incredibly important in complex shiny apps

 - `L` informs all UI inputs when they are generated.
 - `L` does NOT respond immediately to changes in the UI. `L` ONLY changes when something is confirmed (e.g. a confirm button)
 - `L` and `R` give the shiny application "memory" - it can remember the values of UI elements that DO NOT EXIST!
 

# Why all the convolution?

In a world full of dynamic UI elements which may or may not exist at a point
in time, having a place to store that information is extremely useful. Also,
having a way to reset either a part or the full app to default values just
by replacing a part of `L` with a part of `D` means that it doesn't matter how
complicated the application gets, you can always always do this with one line
of code. 


## Some examples:

Imagine that you have a shiny application which allows you to select the number
of comparators, and then for each comparator (regimen) you can also select the number of
combinates within that regimen.

The UI and server need to be able to cope with:

 - An unknown number of comparators
 - An unknown number of compounds for each comparator
 - An unknown number of different possible dosing schedules, RDIs, PAS discounts, stopping rules etc etc
 
In such a context (i.e. any so-called "generic" cost-effectiveness model), one
needs to build a data architecture that can expand to infinity in any direction.
In R, the `list` object can effectively do this as it is not limited by dimensions
or by size.  

Consequently, handling a structure like this using nested lists facilitates any
number of comparators with any number of components with any number of complicated
methods to compute treatment costs without limit (except for hardware!).

Shiny is...really bad... at coping with this issue using its in-built tools. Simply
put, one would need to dynamically name elements within input by pasting together
parts. As an example:


```R
# Make a reactivevalues list for drug costs
i_c_dr <- reactiveValues()

# Iteratively populate that list by cycling through all regimen and components within
# each regimen, pulling out inputs...
for (regimen in 1:i$n_regimen) {
  n_components <- input[[paste0("i_c_dr",drug_number,"_number_of_components")]]
  for (component_number in 1:n_components) {
    cost_list[[paste0("i_c_dr",drug_number,"_",component_number)]] <- {
      ...some code to compute ...
    }
  }
}
```

Note that a `reactiveValues` list is required here because one cannot allocate
intermediate values like vectors of drug costs to a `reactive()` in shiny when
one does not ex ante know the NAME of that reactive. That is, one cannnot define
a reactive without already knowing its name, and if one cannot know that in advance
one becomes stuck!  

Thus, a `reactiveValues` list is required. this list would then pass through to 
the intermediate calculations informing this and that. However, the input names
required would be really complex and long, and get longer and longer and longer
with more layers of data hierarchy.  

In the RCC example, we have population, line, molecule, trial and endpoint levels.
This would make things extremely complicated and difficult to follow just
to do basic things like applying hazard ratios.

Therefore, instead, we keep things to ONE list with different states of that
list being used for different purposes. Hence the `DSLR` system I came up with.

It may not be the most computationally efficient way to do this, but it is extremely
flexible and extensible. It can go to any number of methods to any number of
comparators, to any number of comparisons across any number of bases. It is ideal
for cost-effectiveness modelling.
