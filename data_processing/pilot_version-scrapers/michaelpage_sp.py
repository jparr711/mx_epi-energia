
# import packages

import lxml
import requests
from bs4 import BeautifulSoup
import urllib.request
from urllib.request import urlopen
from urllib.request import urlparse
import time
from urllib.error import HTTPError
import re
import numpy as np
import pandas as pd
from dateutil.relativedelta import relativedelta
import timeago, datetime
from datetime import date
import matplotlib.pyplot as plt
import seaborn as sns
from time import sleep
import random
import locale
from tabulate import tabulate
from datetime import timedelta
from fake_useragent import UserAgent


user_agent_list = [
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1.1 Safari/605.1.15',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:77.0) Gecko/20100101 Firefox/77.0',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:77.0) Gecko/20100101 Firefox/77.0',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36',
]


headers = {'user-agent': random.choice(user_agent_list)}

URL = 'https://www.michaelpage.com.mx/jobs/energy-natural-resources'

michaelpage_df = pd.DataFrame(columns=["Title","Company", "Location", "Salary", "Date", "Synopsis", "Link", "Sector"])
html = requests.get(URL, headers = headers)
soup = BeautifulSoup(html.text, 'html.parser')
sleep(10)
for each in soup.find_all("div", attrs={"class":"job-list"}):
  sleep(10)
  try: 
      title = each.find("div", {"class":"job-title"}).get_text().replace('\n', '')
  except:
      title = 'None'
  try:
      company = 'None'
  except:
      company = 'None'
  try:
      location = each.find('div', {"class":'job-properties'}).find('div', {"class":"location"}).getText().replace('\n', '').strip()
  except:
      location = 'None'
  try:
      date = 'None'
  except:
      date = 'None'
  try:
      salary = each.find('div', {"class":'job-properties'}).find('div', {"class":"salary"}).getText().replace('\n', '')
  except:
      salary = 'None'
  try:
      link = 'https://www.michaelpage.com.mx' + each.find("div", {"class":"job-title"}).find('a').get("href")
  except:
      link = "None"
  try:
      synopsis = each.find("div", class_="job-summary").find('p').getText().replace('\n', '') # + each.find("div", class_="item-list").find_all('ul').getText().replace('\n', '').replace('\n', '')
  except:
      synopsis = 'None'
  try:
      sector = "Energy & Natural Resources"
  except:
      sector = 'None'
  michaelpage_df = michaelpage_df.append({'Title':title, 'Company':company, 'Location':location, 'Salary':salary, 'Date':date, 'Synopsis':synopsis, 'Link':link, 'Sector':sector}, ignore_index=True)


descriptions = []
dates = []
sectors = []


for link in michaelpage_df.iterrows():
  url = link[1]['Link']
  html = requests.get(url, headers = headers)
  soup = BeautifulSoup(html.content, 'html.parser', from_encoding="utf-8")
  sleep(10)
  try:
      description = soup.find('div', {"id":"content-area"}).get_text().replace('\n', '')
  except:
      description = 'None'
  try:
      date = soup.find('div', {"class":"info"}).getText().split()[-1]
  except:
      date = 'None'
  try:
      sector = soup.find('div', {"class":"job-summary-details"}).find_all("div", {"class":"summary-detail-field"}).replace('\n', '').strip()
  except:
      sector = 'None'
  descriptions.append(description)
  dates.append(date)
  sectors.append(sector)

michaelpage_df['Description'] = descriptions
michaelpage_df['Date'] = dates
michaelpage_df['Sector'] = sectors

michaelpage_df.to_excel('michaelpage_df_july_23.xlsx', sheet_name = 'Sheet 1')


#### cleaning data ###
# location
michaelpage_df.State.unique()

 
michaelpage_df = michaelpage_df.replace({'Location' : {'Distrito Federal' : 'Ciudad de México (Distrito Federal)',
                              'Puebla' : 'Puebla (de los Angeles)',
                              'México': 'None',
                              'International': 'None'}})

michaelpage_df.loc[michaelpage_df['Location'].isin(['Nuevo León','Hidalgo','Chiapas',
                               'Jalisco','Durango','Ciudad de México (Distrito Federal)',
                               'Tlaxcala','Chihuahua','Baja California','Querétaro',
                               'San Luis Potosí','Guanajuato','Estado de México',
                               'Coahuila','Tabasco','Puebla (de los Angeles)',
                               'Morelos','Tamaulipas','Quintana Roo', 'Veracruz',
                               'Campeche','Aguascalientes','Nayarit','Michoacán',
                               'Colima','Sinaloa','Sonora','Yucatán','Oaxaca',
                               'Baja California Sur','Zacatecas','None']) == True, 'State'] = michaelpage_df['Location']

michaelpage_df.loc[michaelpage_df['Location'].isin(['Nuevo León','Hidalgo','Chiapas',
                               'Jalisco','Durango','Ciudad de México (Distrito Federal)',
                               'Tlaxcala','Chihuahua','Baja California','Querétaro',
                               'San Luis Potosí','Guanajuato','Estado de México',
                               'Coahuila','Tabasco','Puebla (de los Angeles)',
                               'Morelos','Tamaulipas','Quintana Roo', 'Veracruz',
                               'Campeche','Aguascalientes','Nayarit','Michoacán',
                               'Colima','Sinaloa','Sonora','Yucatán','Oaxaca',
                               'Baja California Sur','Zacatecas','None']) == False, 'Municipality'] = michaelpage_df['Location']
                               
                               
# salary

michaelpage_df['Salary'] = michaelpage_df['Salary'].replace(',', '', regex= True).replace('MXN', '', regex= True).str.strip()

mp_new_salaries = []
for i in michaelpage_df.Salary:
    a = i.split(' - ')
    if len(a) == 2:
        mp_new_salaries.append(np.mean([float(b) for b in a if a != 'None']))
    else:
        mp_new_salaries.append(a[0])
        
michaelpage_df.Salary = mp_new_salaries

# date

michaelpage_df['Date'] = pd.to_datetime(michaelpage_df['Date'])

# finalize columns 
michaelpage_df.columns

# michaelpage_df = michaelpage_df.drop(columns = ['Unnamed: 0'])
# michaelpage_df = michaelpage_df.drop(columns = ['Sectors'])

michaelpage_df['Level'] = 'None'
michaelpage_df['Type'] = 'None'
michaelpage_df['Function'] = 'None'
michaelpage_df['Source'] = 'MichaelPage'


michaelpage_df.columns = michaelpage_df.columns.str.lower()

michaelpage_df.to_excel('michaelpage_df_july_23_clean.xlsx', sheet_name = 'Sheet 1', index=False)


# drop duplicates based on link column (same link = same job)
michaelpage_df = michaelpage_df.drop_duplicates(subset='link', keep="first")

# upload to sql

# test posgresql database

import psycopg2
import psycopg2.extras as extras
import os
from io import StringIO
import sqlalchemy


# Here you want to change your database, username & password according to your own values
param_dic = {
    "host"      : "mexico-fco-db.cmjm2q5ghgua.us-east-1.rds.amazonaws.com",
    "database"  : "postgres",
    "user"      : "mexicoFCO",
    "password"  : "HC2g8Q7^taOq"
}



connection = psycopg2.connect(host = "mexico-fco-db.cmjm2q5ghgua.us-east-1.rds.amazonaws.com",
                 database = "postgres",
                 user = "mexicoFCO",
                 password = "HC2g8Q7^taOq")

cursor = connection.cursor()


def connect(params_dic):
    """ Connect to the PostgreSQL database server """
    conn = None
    try:
        # connect to the PostgreSQL server
        print('Connecting to the PostgreSQL database...')
        conn = psycopg2.connect(**params_dic)
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
        sys.exit(1) 
    print("Connection successful")
    return conn

conn = connect(param_dic)



def execute_many(conn, df, table):
    """
    Using cursor.executemany() to insert the dataframe
    """
    # Create a list of tupples from the dataframe values
    tuples = [tuple(x) for x in df.to_numpy()]
    # Comma-separated dataframe columns
    cols = ','.join(list(df.columns))
    # SQL quert to execute
    query  = "INSERT INTO %s(%s) VALUES(%%s,%%s,%%s, %%s,%%s,%%s, %%s,%%s,%%s, %%s,%%s,%%s, %%s,%%s,%%s) ON CONFLICT (link) DO UPDATE SET link = excluded.link;" % (table, cols)
    cursor = conn.cursor()
    try:
        cursor.executemany(query, tuples)
        conn.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: %s" % error)
        conn.rollback()
        cursor.close()
        return 1
    print("execute_many() done")
    cursor.close()
    


execute_many(conn = conn, df = michaelpage_df, table = 'jobs_dbase')
# WORKS!


