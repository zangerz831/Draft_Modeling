#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jun  9 21:44:44 2020

@author: zachzanger
"""

import numpy as np
import time
import pandas as pd
from collections import namedtuple
from collections import deque
import copy as copy

main_df = pd.read_excel('/Users/zachzanger/Documents/Draft/values_raw.xlsx')
pd.options.mode.chained_assignment = None  # default='warn'
pd.set_option('max_colwidth', 30)


giants_df = main_df[['Name', 'Position', 'Slot_Values', 'Giants']]
athletics_df = main_df[['Name', 'Position', 'Slot_Values', 'Athletics']]
rockies_df = main_df[['Name', 'Position', 'Slot_Values','Rockies']]
dodgers_df = main_df[['Name', 'Position', 'Slot_Values', 'Dodgers']]


Item = namedtuple("Item", ['index', 'value', 'weight', 'density'])


giants_df['value_density'] = (giants_df.loc[:,'Giants'] / giants_df.loc[:,'Slot_Values']) * -1
athletics_df['value_density'] = (athletics_df['Athletics'] / athletics_df['Slot_Values'])
rockies_df['value_density'] = (rockies_df['Rockies'] / rockies_df['Slot_Values'])
dodgers_df['value_density'] = (dodgers_df['Dodgers'] / dodgers_df['Slot_Values'])

#take slot values of players selected and subtract from bonus pool#
giants_bonus_pool = 9231800 - 4036800 - 780400 - 434300 - 497500 - 844200 - 201600
athletics_bonus_pool = 5241500 - 3481300 - 1307000
rockies_bonus_pool = 10339700 - 5432200 - 1952300 - 657600 - 100000
dodgers_bonus_pool = 5928400 - 2493900 - 483000 - 721900 - 610800 - 145000

giants_df = giants_df.sort_values(by=['Giants'], ascending = False)
giants_df = giants_df.reset_index(drop=True)
giants_tuple = list(giants_df.itertuples(name='Row', index=False))


athletics_df = athletics_df.sort_values(by=['Athletics'], ascending = False)
athletics_df = athletics_df.reset_index(drop=True)
athletics_tuple = list(athletics_df.itertuples(name='Row', index=False))

rockies_df = rockies_df.sort_values(by=['Rockies'], ascending = False)
rockies_df = rockies_df.reset_index(drop=True)
rockies_tuple = list(rockies_df.itertuples(name='Row', index=False))

dodgers_df = dodgers_df.sort_values(by=['Dodgers'], ascending = False)
dodgers_df = dodgers_df.reset_index(drop=True)
dodgers_tuple = list(dodgers_df.itertuples(name='Row', index=False))


giants_take = []
athletics_take = []
rockies_take = []
dodgers_take = []


def _optimistic_est(index, capacity, giants_tuple):
        bundle_cost = 0
        optimistic_est = 0
        capacity = capacity
        
        for i in range(len(giants_tuple)):
            player_cost = giants_tuple[i].Slot_Values
            player_value = giants_tuple[i].Giants
            
            #We can fit the whole item
            if bundle_cost + player_cost <= capacity:
                bundle_cost += player_cost
                optimistic_est += player_value

            #We can only take a fraction of the item
            #Add the fraction in and we are done
            else:
                frac = (capacity - bundle_cost) / player_cost
                bundle_cost += frac * player_cost
                optimistic_est += frac * player_value
                break
        
        #print(index, items_sorted_value_density, capacity, optimistic_est)
        return round(optimistic_est, 2)


def giants_bnb_solve_it(player_count, capacity, giants_tuple): 
        values = [0] * player_count
        weights = [0] * player_count
        taken = [0] * player_count
        
        init_capacity = capacity
        optimal = 0
        value = 0
        
        base_est = _optimistic_est(0, init_capacity, giants_tuple)
        stack = deque()
        stack.appendleft((0, 0, capacity, base_est, [0] * player_count))
        
        best_value = 0
        
        start = time.time()
        timeout = False
        while len(stack) > 0:
            if time.time() - start >= (90 * 60):
                timeout = True
                break
            
            index, value, capacity, optimistic_est, taken = stack.pop()

            #Cannot do better than an already found value
            #So prune this subtree
            if best_value > optimistic_est:
                continue
            
            #No more items
            if (index >= player_count):
                continue

            cur_item = giants_tuple[index]
            cur_cost = cur_item.Slot_Values
            cur_value = cur_item.Giants
            
            
            #Do not choose the current item
            pass_estimate = value + _optimistic_est(index + 1, capacity, copy.copy(giants_tuple[index + 1:]))
            stack.appendleft((index + 1, value, capacity, pass_estimate, copy.copy(taken)))
            
            #Choose the current item
            if (capacity >= cur_cost):
                taken[index] = 1
                take_cost = capacity - cur_cost
                take_val = value + cur_value
                best_value = max(take_val, best_value)
                if (best_value == take_val):
                    best_taken = copy.copy(taken)
                stack.appendleft((index + 1, take_val, take_cost, optimistic_est, copy.copy(taken)))

            
        print("Giants Branch and Bound Solution found in: " + str(time.time() - start) + " seconds.")
        name_list = [] 
        for i in range(0, len(best_taken)) : 
            if best_taken[i] == 1 : 
                giants_take.append(giants_tuple[i].Name) 
        
        return (best_value, init_capacity - capacity, best_taken, optimal, name_list)


giants_bnb_solve_it(len(giants_tuple), giants_bonus_pool, giants_tuple)







def athletics_optimistic_est(index, capacity, athletics_tuple):
        bundle_cost = 0
        optimistic_est = 0
        capacity = capacity
        
        for i in range(len(athletics_tuple)):
            player_cost = athletics_tuple[i].Slot_Values
            player_value = athletics_tuple[i].Athletics
            
            #We can fit the whole item
            if bundle_cost + player_cost <= capacity:
                bundle_cost += player_cost
                optimistic_est += player_value

            #We can only take a fraction of the item
            #Add the fraction in and we are done
            else:
                frac = (capacity - bundle_cost) / player_cost
                bundle_cost += frac * player_cost
                optimistic_est += frac * player_value
                break
        
        #print(index, items_sorted_value_density, capacity, optimistic_est)
        return round(optimistic_est, 2)



def athletics_bnb_solve_it(player_count, capacity, athletics_tuple): 
        values = [0] * player_count
        weights = [0] * player_count
        taken = [0] * player_count
        
        init_capacity = capacity
        optimal = 0
        value = 0
        
        base_est = athletics_optimistic_est(0, init_capacity, athletics_tuple)
        stack = deque()
        stack.appendleft((0, 0, capacity, base_est, [0] * player_count))
        
        best_value = 0
        
        start = time.time()
        timeout = False
        while len(stack) > 0:
            if time.time() - start >= (90 * 60):
                timeout = True
                break
            
            index, value, capacity, optimistic_est, taken = stack.pop()

            #Cannot do better than an already found value
            #So prune this subtree
            if best_value > optimistic_est:
                continue
            
            #No more items
            if (index >= player_count):
                continue

            cur_item = athletics_tuple[index]
            cur_cost = cur_item.Slot_Values
            cur_value = cur_item.Athletics
            
            
            #Do not choose the current item
            pass_estimate = value + athletics_optimistic_est(index + 1, capacity, copy.copy(athletics_tuple[index + 1:]))
            stack.appendleft((index + 1, value, capacity, pass_estimate, copy.copy(taken)))
            
            #Choose the current item
            if (capacity >= cur_cost):
                taken[index] = 1
                take_cost = capacity - cur_cost
                take_val = value + cur_value
                best_value = max(take_val, best_value)
                if (best_value == take_val):
                    best_taken = copy.copy(taken)
                stack.appendleft((index + 1, take_val, take_cost, optimistic_est, copy.copy(taken)))

            
        print("Athletics Branch and Bound Solution found in: " + str(time.time() - start) + " seconds.")
        name_list = [] 
        for i in range(0, len(best_taken)) : 
            if best_taken[i] == 1 : 
                athletics_take.append(athletics_tuple[i].Name) 
        
        return (best_value, init_capacity - capacity, best_taken, optimal, name_list)


athletics_bnb_solve_it(len(athletics_tuple), athletics_bonus_pool, athletics_tuple)


def rockies_optimistic_est(index, capacity, athletics_tuple):
        bundle_cost = 0
        optimistic_est = 0
        capacity = capacity
        
        for i in range(len(rockies_tuple)):
            player_cost = rockies_tuple[i].Slot_Values
            player_value = rockies_tuple[i].Rockies
            
            #We can fit the whole item
            if bundle_cost + player_cost <= capacity:
                bundle_cost += player_cost
                optimistic_est += player_value

            #We can only take a fraction of the item
            #Add the fraction in and we are done
            else:
                frac = (capacity - bundle_cost) / player_cost
                bundle_cost += frac * player_cost
                optimistic_est += frac * player_value
                break
        
        #print(index, items_sorted_value_density, capacity, optimistic_est)
        return round(optimistic_est, 2)



def rockies_bnb_solve_it(player_count, capacity, rockies_tuple): 
        values = [0] * player_count
        weights = [0] * player_count
        taken = [0] * player_count
        
        init_capacity = capacity
        optimal = 0
        value = 0
        
        base_est = rockies_optimistic_est(0, init_capacity, rockies_tuple)
        stack = deque()
        stack.appendleft((0, 0, capacity, base_est, [0] * player_count))
        
        best_value = 0
        
        start = time.time()
        timeout = False
        while len(stack) > 0:
            if time.time() - start >= (90 * 60):
                timeout = True
                break
            
            index, value, capacity, optimistic_est, taken = stack.pop()

            #Cannot do better than an already found value
            #So prune this subtree
            if best_value > optimistic_est:
                continue
            
            #No more items
            if (index >= player_count):
                continue

            cur_item = rockies_tuple[index]
            cur_cost = cur_item.Slot_Values
            cur_value = cur_item.Rockies
            
            
            #Do not choose the current item
            pass_estimate = value + rockies_optimistic_est(index + 1, capacity, copy.copy(rockies_tuple[index + 1:]))
            stack.appendleft((index + 1, value, capacity, pass_estimate, copy.copy(taken)))
            
            #Choose the current item
            if (capacity >= cur_cost):
                taken[index] = 1
                take_cost = capacity - cur_cost
                take_val = value + cur_value
                best_value = max(take_val, best_value)
                if (best_value == take_val):
                    best_taken = copy.copy(taken)
                stack.appendleft((index + 1, take_val, take_cost, optimistic_est, copy.copy(taken)))

            
        print("Rockies Branch and Bound Solution found in: " + str(time.time() - start) + " seconds.")
        name_list = [] 
        for i in range(0, len(best_taken)) : 
            if best_taken[i] == 1 : 
                rockies_take.append(rockies_tuple[i].Name) 
        
        return (best_value, init_capacity - capacity, best_taken, optimal, name_list)


rockies_bnb_solve_it(len(rockies_tuple), rockies_bonus_pool, rockies_tuple)


def dodgers_optimistic_est(index, capacity, dodgers_tuple):
        bundle_cost = 0
        optimistic_est = 0
        capacity = capacity
        
        for i in range(len(dodgers_tuple)):
            player_cost = dodgers_tuple[i].Slot_Values
            player_value = dodgers_tuple[i].Dodgers
            
            #We can fit the whole item
            if bundle_cost + player_cost <= capacity:
                bundle_cost += player_cost
                optimistic_est += player_value

            #We can only take a fraction of the item
            #Add the fraction in and we are done
            else:
                frac = (capacity - bundle_cost) / player_cost
                bundle_cost += frac * player_cost
                optimistic_est += frac * player_value
                break
        
        #print(index, items_sorted_value_density, capacity, optimistic_est)
        return round(optimistic_est, 2)



def dodgers_bnb_solve_it(player_count, capacity, dodgers_tuple):
        values = [0] * player_count
        weights = [0] * player_count
        taken = [0] * player_count
        
        init_capacity = capacity
        optimal = 0
        value = 0
        
        base_est = rockies_optimistic_est(0, init_capacity, dodgers_tuple)
        stack = deque()
        stack.appendleft((0, 0, capacity, base_est, [0] * player_count))
        
        best_value = 0
        
        start = time.time()
        timeout = False
        while len(stack) > 0:
            if time.time() - start >= (90 * 60):
                timeout = True
                break
            
            index, value, capacity, optimistic_est, taken = stack.pop()

            #Cannot do better than an already found value
            #So prune this subtree
            if best_value > optimistic_est:
                continue
            
            #No more items
            if (index >= player_count):
                continue

            cur_item = dodgers_tuple[index]
            cur_cost = cur_item.Slot_Values
            cur_value = cur_item.Dodgers
            
            
            #Do not choose the current item
            pass_estimate = value + rockies_optimistic_est(index + 1, capacity, copy.copy(rockies_tuple[index + 1:]))
            stack.appendleft((index + 1, value, capacity, pass_estimate, copy.copy(taken)))
            
            #Choose the current item
            if (capacity >= cur_cost):
                taken[index] = 1
                take_cost = capacity - cur_cost
                take_val = value + cur_value
                best_value = max(take_val, best_value)
                if (best_value == take_val):
                    best_taken = copy.copy(taken)
                stack.appendleft((index + 1, take_val, take_cost, optimistic_est, copy.copy(taken)))

            
        print("Dodgers Branch and Bound Solution found in: " + str(time.time() - start) + " seconds.")
        name_list = [] 
        for i in range(0, len(best_taken)) : 
            if best_taken[i] == 1 : 
                dodgers_take.append(dodgers_tuple[i].Name)
        
        return (best_value, init_capacity - capacity, best_taken, optimal, name_list)


dodgers_bnb_solve_it(len(dodgers_tuple), dodgers_bonus_pool, dodgers_tuple)


print("Giants Take: {}".format(giants_take))
print("Athletics Take: {}".format(athletics_take))
print("Rockies Take: {}".format(rockies_take))
print("Dodgers Take: {}".format(dodgers_take))




