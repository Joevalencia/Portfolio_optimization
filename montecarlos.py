# -*- coding: utf-8 -*-
"""
Created on Thu Aug 24 22:47:58 2023

@author: José Valencia
"""

# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime
import yfinance as yf
from finquant.efficient_frontier import EfficientFrontier
from pypfopt.discrete_allocation import DiscreteAllocation, get_latest_prices
from finquant.portfolio import build_portfolio
from pandas_datareader import data
import os

# Stile per i grafici
plt.style.use('seaborn-darkgrid')

## Imputazione

v1 = int(input('Digita il numero di asset da analizzare: '))

v2 = [input(f'Digita il ticket della Azienda {x+1}: ').upper() for x in range(v1)]

v3, v4 = input("Digita la data d'inizio (dd/mm/yyyy): "), input("Digita la data finale (dd/mm/yyyy): ")


##########################
## Data di valutazione ###
##########################

inizio = datetime(int(v3.split("/")[2]),int(v3.split("/")[1]),int(v3.split("/")[0]))
fine = datetime(int(v4.split("/")[2]),int(v4.split("/")[1]),int(v4.split("/")[0]))


#########################
### ASSET COnsiderati ###
#########################

portfolio = pd.DataFrame()
stocks = v2
print("Si stanno per scaricare ", len(stocks), "ASSET VALUES")
for stock in stocks:
    load = yf.download(stock, inizio, fine)
    portfolio[stock] = load['Adj Close']
    try:
        tick = yf.Ticker(stock)
        print('%s: %s' % (stock, tick.info['longName']))
    except:
        print('No data for %s' % stock)


def rentabilità():
    #Next, we calculate the number of days that have elapsed in our chosen time window
    asset = input("Digitare il ticker di un ASSET considerato: ").upper()
    nvda = portfolio[asset]
    time_elapsed = (nvda.index[-1] - nvda.index[0]).days
    
    #Current price / first record (e.g. price at beginning of 2009)
    #provides us with the total growth %
    total_growth = (nvda[-1] / nvda[1])
    
    #Next, we want to annualize this percentage
    #First, we convert our time elapsed to the # of years elapsed
    number_of_years = time_elapsed / 365.0
    #Second, we can raise the total growth to the inverse of the # of years
    #(e.g. ~1/10 at time of writing) to annualize our growth rate
    cagr = total_growth ** (1/number_of_years) - 1
    
    #Now that we have the mean annual growth rate above,
    #we'll also need to calculate the standard deviation of the
    #daily price changes
    std_dev = np.diff(np.log(nvda)).std()
    
  
    number_of_trading_days = 252
    std_dev = std_dev * np.sqrt(number_of_trading_days)
    
    #From here, we have our two inputs needed to generate random
    #values in our simulation
    print ("cagr (mean returns) : ", str(round(cagr,4)))
    print ("std_dev (standard deviation of return : )", str(round(std_dev,4)))
    
    return cagr, std_dev, nvda
    
f1 = rentabilità()
    
### Generazione di montecarlo
    
def montecarlo():
    
    trials = int(input("Digitare il numero di traiettorie: "))
    number_of_trials = trials
    number_of_trading_days = 252
    
    
    cagr, std_dev, nvda = f1[0], f1[1], f1[2]
    #set up an additional array to collect all possible
    #closing prices in last day of window.
    #We can toss this into a histogram
    #to get a clearer sense of possible outcomes
    closing_prices = []
    
    for i in range(number_of_trials):
        #calculate randomized return percentages following our normal distribution
        #and using the mean / std dev we calculated above
        daily_return_percentages = np.random.normal(cagr/number_of_trading_days, 
                                                    std_dev/np.sqrt(number_of_trading_days),
                                                    number_of_trading_days)+1
        price_series = [nvda[-1]]
    
        for j in daily_return_percentages:
            #extrapolate price out for next year
            price_series.append(price_series[-1] * j)
    
        #append closing prices in last day of window for histogram
        closing_prices.append(price_series[-1])
    
        #plot all random walks
        plt.plot(price_series)
    
    plt.show()
    
    #plot histogram
    plt.hist(closing_prices,bins='fd', density=True)
    
    plt.show()
    
    #from here, we can check the mean of all ending prices
    #allowing us to arrive at the most probable ending point
    mean_end_price = round(np.mean(closing_prices),2)
    print("Expected price: ", str(mean_end_price))
    
        
f2 = montecarlo()        


        
        
        
        
        
        
# import time

# print("Attenzione l'analisi si fa sulla rentabilità logaritmica diaria...")
# time.sleep(3)    
# print('Fine Tuning per ottenere la rentabilità...')
# time.sleep(4)
# print("Analisis della rentabilità completata..")
# time.sleep(1)
# print("Valore ottenuto: 2%")
    
    
        
        
        
