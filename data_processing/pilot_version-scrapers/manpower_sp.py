
# Manpower

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
# import relativedelta
import matplotlib.pyplot as plt
import seaborn as sns
from time import sleep
import random
from datetime import timedelta

user_agent_list = [
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1.1 Safari/605.1.15',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:77.0) Gecko/20100101 Firefox/77.0',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:77.0) Gecko/20100101 Firefox/77.0',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36',
]


headers = {'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
    'accept-encoding': 'gzip, deflate, sdch, br',
    'accept-language': 'en-GB,en-US;q=0.8,en;q=0.6',
    'upgrade-insecure-requests': '1',
    'user-agent': random.choice(user_agent_list),
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive'
    }
    
url_template = 'https://empleo.manpower.com.mx/empleos-en-mexico-y-el-mundo/energia?tm=10?page={}'

max_results = 1
i = 0

manpower_df = pd.DataFrame(columns=["Title","Company", "Municipality", "State", "Salary", "Date", "Synopsis", "Link"])

for start in range(0, max_results, 1):
    # get results from request above
    url = url_template.format(start)
    # append to full set
    html = requests.get(url, headers = headers, verify=False)
    soup = BeautifulSoup(html.text, 'html.parser')
    sleep(10)
    for each in soup.find_all("div", attrs={"id":"normalook_mod_sr"}):
                try: 
                    title = each.find("h3", {"class":"title_modn_sr"}).find('a').getText().replace('\n', '')
                except:
                    title = 'None'
                try:
                    company = 'None'
                except:
                    company = 'None'
                try:
                    location = each.find('span', {"class":'location'}).getText().replace('\n', '').split(",")[0]
                except:
                    location = 'None'
                try:
                    location_2 = each.find('span', {"class":'location'}).getText().replace('\n', '').split(",")[1]
                except:
                    location_2 = 'None'
                try:
                    date = each.find('span', {"class":'fecha_modn_sr'}).getText().replace('\n', '')
                except:
                    date = 'None'
                try:
                    salary = each.find('span', {"class":'salario_modn_sr'}).getText().replace('\n', '').replace('MXN', '').replace('Mensual', '').replace(',', '').replace('$', '')
                except:
                    salary = 'None'
                try:
                    link = 'https://empleo.manpower.com.mx' + each.find("h3", {"class":"title_modn_sr"}).find('a').get('href')
                except:
                    link = "None"
                try:
                    synopsis = each.find('span', {"class":'descrip_modn_sr'}).getText().replace('\n', '')
                except:
                    synopsis = 'None'
                manpower_df = manpower_df.append({'Title':title, 'Company':company, 'Municipality':location, 'State':location_2, 'Salary':salary, 'Date':date, 'Synopsis':synopsis, 'Link':link}, ignore_index=True)



descriptions = []
sectors = []

mpdf =  pd.DataFrame(columns=["Link", "Description", "Sector"])

for link in manpower_df.iterrows():
    url = link[1]['Link']
    html = requests.get(url, headers, verify = False)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding="utf-8")
    sleep(10)
    try:
        description = soup.find('div', {"class":"txt2_jo"}).get_text().replace('\n', '')
    except:
        description = 'None'
    try:
        sector = soup.find('div', {"id":"bg_triangle_ba"}).find_all('dd')[1].getText()
    except:
        sector = 'None'
    mpdf = mpdf.append({'Description':description, 'Sector':sector, 'Link':url}, ignore_index = True)

manpower_df = manpower_df.join(mpdf.set_index('Link'), on='Link')

manpower_df.to_excel('manpower_df_clean.xlsx', sheet_name = 'Sheet 1')

# clean - location
manpower_df.loc[manpower_df.Municipality == 'Ciudad de México', 'State'] = manpower_df.Municipality
manpower_df.loc[manpower_df.Municipality == 'Ciudad de México', 'Municipality'] = 'None'

manpower_df.loc[manpower_df['State'].isnull() & manpower_df['Municipality'].notnull(), ['Municipality', 'State']] = manpower_df.loc[manpower_df['State'].isnull() & manpower_df['Municipality'].notnull(), ['State', 'Municipality']].values


 
manpower_df = manpower_df.replace({'State' : {'Distrito Federal' : 'Ciudad de México (Distrito Federal)', 'Ciudad de México' : 'Ciudad de México (Distrito Federal)',
                              'Puebla' : 'Puebla (de los Angeles)',
                              'México': 'None',
                              'International': 'None'}})


manpower_df['Location'] = manpower_df['Municipality'] + ', ' + manpower_df['State']

# salary
manpower_df.loc[manpower_df.Salary == 'No mostrado por compañía', 'Salary'] = 'None'

manpower_df['Salary'] = manpower_df['Salary'].str.strip()


mnp_new_salaries = []

for i in manpower_df.Salary:
    a = i.split('  - ')
    if len(a) == 2:
        mnp_new_salaries.append(np.mean([float(b) for b in a if a != 'None']))
    else:
        mnp_new_salaries.append(a[0])
    
manpower_df['Salary'] = mnp_new_salaries



# date

manpower_df['Date'] = manpower_df['Date'].str.replace('Ago', 'August')
manpower_df['Date'] = manpower_df['Date'].str.replace('Sep', 'September')
manpower_df['Date'] = manpower_df['Date'].str.replace('Oct', 'October')
manpower_df['Date'] = manpower_df['Date'].str.replace('Nov', 'November')
manpower_df['Date'] = manpower_df['Date'].str.replace('Dic', 'December')
manpower_df['Date'] = manpower_df['Date'].str.replace('Jul', 'July')
manpower_df['Date'] = manpower_df['Date'].str.replace('Jun', 'June')
manpower_df['Date'] = manpower_df['Date'].str.replace('May', 'May')
manpower_df['Date'] = manpower_df['Date'].str.replace('Abr', 'April')
manpower_df['Date'] = manpower_df['Date'].str.replace('Mar', 'March')
manpower_df['Date'] = manpower_df['Date'].str.replace('Feb', 'February')
manpower_df['Date'] = manpower_df['Date'].str.replace('Ene', 'January')

manpower_df['Date'] = manpower_df['Date'].astype(str) + ' 2020'

manpower_df['Date'] = pd.to_datetime(manpower_df['Date'])

# cleaning
manpower_df['Sector'] = manpower_df['Sector'].str.replace('\t', '')
manpower_df['Description'] = manpower_df['Description'].str.replace('\t', '')



# finalize columns 
manpower_df.columns

# manpower_df = manpower_df.drop(columns = ['Unnamed: 0'])

manpower_df['Level'] = 'None'
manpower_df['Type'] = 'None'
manpower_df['Function'] = 'None'
manpower_df['Source'] = 'Manpower'

manpower_df.columns = manpower_df.columns.str.lower()

manpower_df.to_excel('manpower_df_clean.xlsx', sheet_name = 'Sheet 1', index=False)



# drop duplicates based on link column (same link = same job)
manpower_df = manpower_df.drop_duplicates(subset='link', keep="first")

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
    

execute_many(conn = conn, df = manpower_df, table = 'jobs_dbase')
# WORKS!



