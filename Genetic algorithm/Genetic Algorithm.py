import math
from random import random, randint
import numpy as np

def a():
    # geneCode = [randint(0,1) for x in range(10)]
    geneCode = np.random.rand(1,100)

    return geneCode

def fitnessFunction(code):
    return np.sum(code) / code.size
    # return sum(code) / float(len(code))

def mutate(code):
    size = len(code)
    rand_val = random()
    if(rand_val < 0.5): # ok we mutate
        # print("mutate")
        i = randint(0, size - 1)
        # print("at index", i)
        # v = (code[i] + 1) % 2
        # code[i] = (code[i] + 1) % 2
        code[i] = random()
    else:
        # print("not mutate")
        pass
    return code

def crossover(male,female):
    # AssertionError(len(male) == len(female))

    splitPoint = randint(0, len(male) - 1)

    child1 = np.array(male)
    child2 = np.array(female)

    for x in range(splitPoint,len(male)):
        child1[0, x] = female[0, x]
        child2[0, x] = male[0, x]

    return child1,child2


def remove_weakling(pop):
    pop = sorted(pop, key=fitnessFunction, reverse=True)
    elements_to_keep = math.ceil(len(pop)/2)
    pop = pop[:elements_to_keep]
    return pop

def select_random_parents(pop):
    male = pop[randint(0,len(pop)-1)]
    female = pop[randint(0, len(pop) - 1)]
    return male,female

population = [a() for i in range(20)]

for generation in range(10):

    for i in range(len(population)):
        # print("generation", generation, "element",i)

        old_pop_size = len(population)
        population = remove_weakling(population)
        while(len(population) < old_pop_size):
            male,female = select_random_parents(population)
            child1,child2 = crossover(male,female)
            population.append(child1)
            if len(population) < old_pop_size:
                population.append(child2)

        population[i] = mutate(population[i])

    # print(fitnessFunction(population[0]))
    population = sorted(population, key=fitnessFunction, reverse=True)
    fitSum = sum([fitnessFunction(f) for f in population[:5]])
    # pop_top5_score = np.mean(population[:5])
    print("generation",generation, "pop_top5_score", fitSum/5)



    # l = lambda x: x+5
    # print(l(1))

    # f = fitnessFunction(element)





# print(element, " = ",f)