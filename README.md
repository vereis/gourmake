# gourmake

```gourmake``` is a computational creativity project which tries to invent novel yet sensible recipes by taking inspiration from an existing database of recipes and ingredients.


## Architecture
```gourmake```'s design has the following major components:

- a database of recipes, ingredients, preperation methods, cuisines and flavours
- a markov chain generation module, which takes a sampled set of recipes and builds a markov chain out of them, representing likely ingredient combinations
- an evolutionary algorithm module, allowing us to generate candidate recipes out of our database

### Gourmake's Database
```gourmake```'s database will allow us to select random samples out of it. A rough idea of the schema is presented:

- Recipes are implemented as tuples which look like ```{Name, [{Ingredient, PreparationMethod}...], [Cuisines...]}```
- Ingredients are implemented as tuples which look like ```{Name, MainFlavour, [SecondaryFlavours...], Texture, [PreperationMethods...]}```
- PreparationMethods are implemented as tuples which look like ```{Name, [FlavourModifiers...], [TextureModifiers...]}```
- Cuisines are implemented as tuples which look like ```{Name, [CommonIngredients...], [CommonPreperationMethods...], [CommonFlavours...]}```
- Flavours are implemented as tuples which look like ```{Name, Value}``` where ```Value``` is a normalised weight

Ingredients can then be prepared a certain way, which modifies the overall texture or flavour of said ingredient, allowing or disallowing it to fit into certain cuisine criterion which could be used as part of a fitness evaluation function. 

Cuisines determine what mixture of ingredients and preparation methods are most likely to be used, as well as providing the main flavours to look for out of a resultant recipe.

Recipes will exist primarily for the sake of allowing us to draw inspiration from existing recipes which hopefully should help us generate legitimate sounding recipes.

### Gourmake's Markov Chain
```gourmake``` when run will take a sample of N recipes and use them to generate random 'locally coherant' recipes which act as candidate solutions for ```gourmake```'s evolutionary algorithm. 

The resultant markov chain can also possibly be used by our evolutionary algorithm partially to determine fitness, as we're aiming to generate recipes which seem real and legitimate and one nice way of doing that is to compare our generated recipes to what real recipes look like.

### Gourmake's Evolutionary Algorithm
```gourmake```'s evolutionary algorithm is intended to be quite simple and it outlined below:

1) Generate a population of N pseudo-random recipes
2) Select a random sample out of the population, and out of those, select the top N% of recipes until we have selected ~50% of the recipes in our population
3) Perform crossover on these selected individuals and generate new recipes to create a new population generation
4) Perform random mutation on the resultant new generation
5) Repeat for N generations and return the top N recipes for user evaluation

For our crossover operator, we utilise a simple one point crossover for our ingredient list; for two selected ingredients, we generate a pivot point and generate two offspring where the ingredient list of parents A and B are crossed over at the pivot point.

For our mutation operator, we will do any of the following, in order of likelihood:

- Change the preperation method of one ingredient in a recipe
- Replace an ingredient with a pseudo-random one from the markov chain, preserving preperation method
- Replace an ingredient entirely with a pseudo-random one from the markov chain
- Replace an ingredient with a random one, preserving preperation method
- Add a new random ingredient
- Remove a random ingredient
- Replace an ingredient entirely with a random one

We also perform elitism whereby even if not selected, the top candidate recipe will be guarenteed to exist in the next generation.
