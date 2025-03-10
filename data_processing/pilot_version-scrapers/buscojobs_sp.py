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
from fake_useragent import UserAgent
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


url_template = 'https://www.buscojobs.mx/search/energia_/{}'
max_results = 50
i = 0
results = []
buscojobs_df = pd.DataFrame(columns=["Title","Company","Salary", "Date", "Synopsis", "Link"])
for start in range(0, max_results, 1):
    # get results from request above
    url = url_template.format(start)
    # append to full set
    html = requests.get(url, headers = headers)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding = "utf-8")
    sleep(10)
    for each in soup.find_all(name = "div", attrs={"class":"col-lg-9"}):
                try: 
                    title = each.find("h3").get_text().replace('\n', '')
                except:
                    title = 'None'
                try:
                    company = each.find("span").get_text().replace('\n', '').replace('\t', '')
                except:
                    company = 'None'
                try:
                    date =  each.find('span', {"class":'pull-right'}).text.replace('\n', '')
                except:
                    date = 'None'
                try:
                    salary =  'None'
                except:
                    salary = 'None'
                try:
                    synopsis = each.find_all("span")[2].get_text().replace('\n', '').replace('\t', '')
                except:
                    synopsis = 'None'
                try:
                    link = each.find("a").get('href')
                except:
                    link = "None"
                buscojobs_df = buscojobs_df.append({'Title':title, 'Company':company, 'Salary':salary, 'Date':date, 'Synopsis':synopsis, 'Link':link}, ignore_index=True)
                i += 1
                if i % 1000 == 0: 
                    print("you have" + str(i) + 'results. ' + str(buscojobs_df.dropna().drop_duplicates().shape[0]) + " of these aren't rubbish.")
                    
                    
buscojobs_df[['Company', 'Location']] = buscojobs_df['Company'].str.split(pat = "-", n = 1, expand=True)
buscojobs_df[['Municipality', 'State']] = buscojobs_df['Location'].str.split(pat = ",", n = 1, expand=True)
buscojobs_df['Link'] = 'http:' + buscojobs_df['Link'].astype(str)


buscojobs_df = buscojobs_df.drop_duplicates()
buscojobs_df = buscojobs_df[buscojobs_df.Title != "None"]


bjdf = pd.DataFrame(columns=["Description", "Company", "Areas", "Location", "Link"])



descriptions = []
companies = []
locations = []
areas = []

#   x = urllib.request.urlopen(url)
#   desc = x.read()

for link in buscojobs_df.iterrows():
    url = link[1]['Link']
    html = requests.get(url, headers = headers)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding="utf-8")
    sleep(10)
    try:
        description = soup.find('div', {"class":"col-md-12 descripcion-texto"}).get_text().replace('\n', '')
    except:
        description = 'None'
    descriptions.append(description)
    try:
        company = soup.find_all('h2')[0]
    except:
        company = 'None'
    companies.append(company)
    try:
        location = soup.find_all('h2')[1]
    except:
        location = "None"
    locations.append(location)
    try:
        area = soup.find_all('h2')[2]
    except:
        area = 'None'
    areas.append(area)
    bjdf = bjdf.append({'Description':description, 'Company':company, 'Areas':area, 'Location':location, 'Link': url}, ignore_index = True)


all_columns = list(bjdf) # Creates list of all column headers
bjdf[all_columns] = bjdf[all_columns].astype(str).apply(lambda x: x.str.strip('</h2>'))
bjdf[['Municipality', 'State']] = bjdf['Location'].str.split(pat = ",", n = 1, expand=True)

bjdf['Link'] = 'h' + bjdf['Link'].astype(str)

# drop original columns and replace with ones from bjdf
buscojobs_df = buscojobs_df.drop(columns = ["Municipality", "State", "Location"])
bjdf = bjdf.drop(columns = ['Company'])


buscojobs_df['Link'] = buscojobs_df['Link'].astype(str).str.strip() 
bjdf['Link'] = bjdf['Link'].astype(str) 

test = pd.merge(buscojobs_df, bjdf, on='Link', how = 'left')
buscojobs_df = test
del(test)


buscojobs_df.to_excel('buscojobs_df_clean.xlsx', sheet_name = 'Sheet 1')

# Clean location data
buscojobs_df.loc[buscojobs_df['State'].isnull() & buscojobs_df['Municipality'].notnull(), ['Municipality', 'State']] = buscojobs_df.loc[buscojobs_df['State'].isnull() & buscojobs_df['Municipality'].notnull(), ['State', 'Municipality']].values
buscojobs_df.State = buscojobs_df.State.str.strip()
buscojobs_df.Municipality = buscojobs_df.Municipality.str.strip()
buscojobs_df.State.unique()

# Clean date
buscojobs_df['Date'] = buscojobs_df['Date'].astype(str)

# date transformation - buscojobs
def buscojobs_get_past_date(str_days_ago):
    TODAY = datetime.date.today()
    splitted = str_days_ago.split()
    if len(splitted) == 2 and splitted[1].lower() in ['ahora', 'hoy']:
        return str(TODAY.isoformat())
    elif len(splitted) == 2 and splitted[1].lower() in ['ayer']:
        date = TODAY - relativedelta(days=1)
        return str(date.isoformat())
    elif splitted[3].lower() in ['horas']:
        date = TODAY - relativedelta(hours=int(splitted[2]))
        return str(date.isoformat())
    elif splitted[3].lower() in ['días']:
        date = TODAY - relativedelta(days=int(splitted[2]))
        return str(date.isoformat())
    elif splitted[3].lower() in ['semanas']:
        date = TODAY - relativedelta(weeks=int(splitted[2]))
        return str(date.isoformat())
    elif splitted[3].lower() in ['meses']:
        date = TODAY - relativedelta(months=int(splitted[2]))
        return str(date.isoformat())
    elif splitted[3].lower() in ['años']:
        date = TODAY - relativedelta(years=int(splitted[2]))
        return str(date.isoformat())
    elif splitted[3].lower() in ['día']:
        date = TODAY - relativedelta(days=1)
        return str(date.isoformat())
    elif splitted[3].lower() in ['hora', 'hr', 'h']:
        date = TODAY - relativedelta(hours=1)
        return str(date.isoformat())
    elif splitted[3].lower() in ['semana']:
        date = TODAY - relativedelta(weeks=1)
        return str(date.isoformat())
    elif splitted[3].lower() in ['mes']:
        date = TODAY - relativedelta(months=1)
        return str(date.isoformat())
    elif splitted[3].lower() in ['año']:
        date = TODAY - relativedelta(years=1)
        return str(date.isoformat())
    else:
        return "Wrong Argument format"


dates = []
for date in buscojobs_df['Date']:
    date_def = buscojobs_get_past_date(date)
    dates.append(date_def)
    

buscojobs_df['Date'] = dates


buscojobs_df.loc[buscojobs_df['State'].isin(['Nuevo León','Hidalgo','Chiapas',
                               'Jalisco','Durango','Ciudad de México (Distrito Federal)',
                               'Tlaxcala','Chihuahua','Baja California','Querétaro',
                               'San Luis Potosí','Guanajuato','Estado de México',
                               'Coahuila','Tabasco','Puebla (de los Angeles)',
                               'Morelos','Tamaulipas','Quintana Roo', 'Veracruz',
                               'Campeche','Aguascalientes','Nayarit','Michoacán',
                               'Colima','Sinaloa','Sonora','Yucatán','Oaxaca',
                               'Baja California Sur','Zacatecas','None']) == False, 'State'] = 'None'

# no salary values to clean

# clean company

buscojobs_df['Company'] = buscojobs_df['Company'].str.strip()

# remove following
remove = ['Michael Page', 'Page Personnel', 'Job Fit', 'Working Mexico Headhunter', 'Page Personnel México',
        'Confidential', 'Confidencial', 'To view company name. See instructions below', 'Colocandote',
        'Working Mexico Headhunter', 'RemoteJobsWW', 'PageExecutive', 'Bumeran', 'Randstad', 'Adecco',
        'Cornerjobmx', 'Energyjobline', 'Computrabajo.Com', 'Confidencial', 'ListoPro',
        'Valle de Santiago', 'Texcoco', 'Benito Juárez', 'Monterrey', 'Coatzacoalcos', 'Silao', 'Mérida', 'San Martín de las Pirámides',
       'San Luis Potosí', 'Salvatierra', 'Miguel Hidalgo', 'Ecatepec de Morelos', 'Naucalpan de Juárez', 'Cuauhtémoc',
       'Veracruz', 'Salamanca', 'Juventino Rosas', 'Manzanillo', 'Oaxaca', 'Guadalajara', 'Puerto Vallarta', 'Iztapalapa', 'Guanajuato',
       'Minatitlán', 'Santa Cruz de Juventino Rosas', 'Tehuacán', 'Yautepec', 'Huixquilucan', 'Mexicali', 'Tlaquepaque', 'Morelia',
       'Guadalupe', 'Apodaca', 'Metepec', 'Lerma', 'Cuajimalpa de Morelos', 'Gustavo A. Madero', 'Toluca',
       'Tuxtla Gutiérrez', 'Zapopan', 'Atizapán de Zaragoza', 'Puebla',
       'Tlalnepantla de Baz', 'Gómez Palacio', 'Tlalpan', 'Cordoba', 'Nezahualcóyotl', 'Pachuca de Soto', 'Alvaro Obregón', 'Apizaco',
       'Lampazos de Naranjo', 'Tamazunchale', 'Santa Catarina',
       'Tlaxcala', 'Querétaro', 'Hermosillo', 'Chihuahua', 'Jiutepec', 'Paraíso', 'Torreon', 'Amatlán de los Reyes', 'Ciudad Juárez',
       'San Andrés Cholula', 'Azcapotzalco', 'Jocotitlán',
       'Aguascalientes', 'Roma', 'Zacoalco de Torres', 'Reynosa', 'Cuautitlán de García Barragán', 'Tijuana', 'Corregidora',
       'Cozumel', 'Isla Mujeres', 'Emiliano Zapata', 'Xalapa',
       'San Juan Del Rio', 'Zacatecas', 'Fresnillo', 'Los Cabos', 'Ensenada', 'Huejotzingo', 'Saltillo', 'Ixtlahuaca', 'Ahuazotepec', 'León', 'Tetla', 
       'Calera', 'Casas Grandes', 'Chalco',
       'Zapotlanejo', 'Ocoyoacac', 'Tepotzotlán', 'Otzolotepec', 'Irapuato', 'Río Bravo', 'San Martín Texmelucan',
       'Cuautitlán Izcalli', 'Macuspana', 'Temozón', 'Cárdenas', 'Teapa', 'San Juan Bautista Tuxtepec', 'Acayucan', 'Ciudad del Carmen',
       'Tecate', 'Tepic', 'Arandas', 'Cancún', 'El Espinal', 'Madero', 'Nuevo León','Hidalgo','Chiapas',
       'Jalisco','Durango','Ciudad de México (Distrito Federal)',
       'Tlaxcala','Chihuahua','Baja California','Querétaro','San Luis Potosí','Guanajuato','Estado de México',
       'Coahuila','Tabasco','Puebla (de los Angeles)','Morelos','Tamaulipas','Quintana Roo', 'Veracruz',
       'Campeche','Aguascalientes','Nayarit','Michoacán','Colima','Sinaloa','Sonora','Yucatán','Oaxaca',
       'Baja California Sur','Zacatecas']


buscojobs_df.loc[buscojobs_df['Company'].isin(remove) == True, 'Company'] = "None"



# finalize columns 
buscojobs_df.columns

buscojobs_df['Sector'] = buscojobs_df['Areas']
buscojobs_df = buscojobs_df.drop(columns = ['Areas'])
buscojobs_df['Level'] = 'None'
buscojobs_df['Type'] = 'None'
buscojobs_df['Function'] = 'None'
buscojobs_df['Source'] = 'Buscojobs'


buscojobs_df.columns = buscojobs_df.columns.str.lower()

buscojobs_df.to_excel('buscojobs_df_clean.xlsx', sheet_name = 'Sheet 1', index = False)


# drop duplicates based on link column (same link = same job)
buscojobs_df = buscojobs_df.drop_duplicates(subset='link', keep="first")

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
    

execute_many(conn = conn, df = buscojobs_df, table = 'jobs_dbase')
# WORKS!


