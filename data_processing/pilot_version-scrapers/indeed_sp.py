
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


# Indeed
url_template = 'https://www.indeed.com.mx/trabajo?q=energia&l=Mexico&sort=date&start={}'

max_results = 500

i = 0

headers = {'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
    'accept-encoding': 'gzip, deflate, sdch, br',
    'accept-language': 'en-GB,en-US;q=0.8,en;q=0.6',
    'referer': 'https://www.glassdoor.com/',
    'upgrade-insecure-requests': '1',
    'user-agent': random.choice(user_agent_list),
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive'
    }

sleep(10)
results = []
# create empty dframe
df_indeed = pd.DataFrame(columns=["Title","Location","Company","Salary", "Synopsis", "Link", "Date"])
# scrape
for start in range(0, max_results, 1):
    # get results from request above
    url = url_template.format(start)
    # append to full set
    html = requests.get(url, headers = headers)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding = "utf-8")
    sleep(10)
    for each in soup.find_all(class_="result"):
        try: 
            title = each.find(class_='jobtitle').text.replace('\n', '')
        except:
            title = 'None'
        try:
            location = each.find('span', {'class':"location" }).text.replace('\n', '')
        except:
            location = 'None'
        try: 
            company = each.find(class_='company').text.replace('\n', '')
        except:
            company = 'None'
        try:
            date = each.find('span', {'class':'date'}).text
        except:
            date = 'None'
        try:
            link = each.find(class_='jobtitle').get('href')
        except:
            link = 'None'
        try:
            salary = each.find('span', {'class':'no-wrap'}).text
        except:
            salary = 'None'
        try:
            synopsis = each.find('div', {'class':"summary"}).text.replace('\n', '')
        except:
            synopsis = 'None'
        df_indeed = df_indeed.append({'Title':title, 'Location':location, 'Company':company, 'Salary':salary, 'Synopsis':synopsis, 'Link':link, 'Date':date}, ignore_index=True)


# drop duplicates    
df_indeed = df_indeed.drop_duplicates()

# Complete link and split location into Mun and State
df_indeed[['Municipality', 'State']] = df_indeed['Location'].str.split(pat = ",", n = 1, expand=True)
df_indeed['Link'] = 'https://www.indeed.com.mx' + df_indeed['Link'].astype(str)

df_indeed['Link'] = df_indeed['Link'].astype(str)


ind_descriptions = []

df_indeed_test = pd.DataFrame(columns=["Description", "Link"])


for link in df_indeed.iterrows():
    url = link[1]['Link']
    html = requests.get(url, headers = headers)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding="utf-8")
    sleep(10)
    try:
        description = soup.find('div', {"id":"jobDescriptionText"}).text.replace('\n', '')
    except:
        description = 'None'
    df_indeed_test = df_indeed_test.append({'Description': description, 'Link': url}, ignore_index = True)

df_indeed = df_indeed.join(df_indeed_test.set_index('Link'), on='Link')

# df_indeed.to_excel('df_indeed_july_23.xlsx', sheet_name = 'Sheet 1')

####  clean data ##### 

# location

# place misplaced state and municipality into correct cols
df_indeed.loc[df_indeed['State'].isnull() & df_indeed['Municipality'].notnull(), ['Municipality', 'State']] = df_indeed.loc[df_indeed['State'].isnull() & df_indeed['Municipality'].notnull(), ['State', 'Municipality']].values

# remove MX CTY out of Municipality (it's a state)
df_indeed['Municipality'] = df_indeed['Municipality'].str.replace('Ciudad de México', 'None')

# standardize state names
df_indeed = df_indeed.replace({'State' : {'Ciudad de México' : 'Ciudad de México (Distrito Federal)',
                              ' N. L.' : 'Nuevo León', 
                              ' Hgo.' : 'Hidalgo', 
                              ' Chis.' : 'Chiapas', 
                              ' Jal.' : 'Jalisco', 
                              ' CDMX' : 'Ciudad de México (Distrito Federal)' ,
                              ' Tlax.' : 'Tlaxcala', 
                              ' Chih.' : 'Chihuaua', 
                              ' B.C.' : 'Baja California', 
                              ' Qro.' : 'Querétaro', 
                              ' S. L. P.' : 'San Luis Potosí', 
                              ' Gto.' : 'Guanajuato',
                              ' Dgo.' : 'Durango',
                              ' Méx.' : 'Estado de México', 
                              ' Coah.' : 'Coahuila', 
                              ' Tab.' : 'Tabasco', 
                              ' Pue.' : 'Puebla (de los Angeles)',
                              ' Mor.' : 'Morelos', 
                              ' Tamps.' : 'Tamaulipas', 
                              ' QRoo.' : 'Quintana Roo',
                              ' Ver.' : 'Veracruz', 
                              ' Camp.' : 'Campeche', 
                              ' Ags.' : 'Aguascalientes',
                              ' Nay.' : 'Nayarit', 
                              ' Mich.' : 'Michoacán', 
                              ' Col.' : 'Colima', 
                              ' Sin.' : 'Sinaloa',
                              ' Son.' : 'Sonora', 
                              ' Yuc.' : 'Yucatán', 
                              ' Oax.' : 'Oaxaca', 
                              ' B.C.S.' : 'Baja California Sur', ' Zac.' : 'Zacatecas'}})

df_indeed['State'].unique()

df_indeed.loc[df_indeed['State'].isin(['Nuevo León','Hidalgo','Chiapas',
                               'Jalisco','Durango','Ciudad de México (Distrito Federal)',
                               'Tlaxcala','Chihuahua','Baja California','Querétaro',
                               'San Luis Potosí','Guanajuato','Estado de México',
                               'Coahuila','Tabasco','Puebla (de los Angeles)',
                               'Morelos','Tamaulipas','Quintana Roo', 'Veracruz',
                               'Campeche','Aguascalientes','Nayarit','Michoacán',
                               'Colima','Sinaloa','Sonora','Yucatán','Oaxaca',
                               'Baja California Sur','Zacatecas','None']) == False, 'State'] = 'None'


# clean company

df_indeed['Company'] = df_indeed['Company'].str.strip()

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


df_indeed.loc[df_indeed['Company'].isin(remove) == True, 'Company'] = "None"


# date

# date transformation - indeed
def indeed_get_past_date(str_days_ago):
    TODAY = datetime.date.today() - relativedelta(days = 7)
    splitted = str_days_ago.split()
    if len(splitted) == 2 and splitted[1].lower() in ['ahora']:
        return str(TODAY.isoformat())
    elif len(splitted) == 1 and splitted[0].lower() in ['hoy']:
        date = TODAY - relativedelta(days=1)
        return str(date.isoformat())
    elif len(splitted) == 3 and splitted[2].lower() in ['días', 'día']:
        date = TODAY - relativedelta(days=int(splitted[1]))
        return str(date.isoformat())
    elif len(splitted) == 5 and splitted[4].lower() in ['días']:
        date = TODAY - relativedelta(days=30)
        return str(date.isoformat())
    
    else:
        return "Wrong Argument format"


in_dates = []
for in_date in df_indeed['Date']:
    in_date = indeed_get_past_date(in_date)
    in_dates.append(in_date)
    

df_indeed['Date'] = in_dates

# salary

df_indeed['Salary'] = df_indeed['Salary'].replace('\\n', '', regex= True)

df_indeed[['Salary', 'Frequency']] = df_indeed['Salary'].str.split('por|al', expand = True)

# df_indeed[(~df_indeed.Salary.str.contains('por hora')) & (~df_indeed.Salary.str.contains('al mes'))
#                    & (~df_indeed.Salary.str.contains('por semana')) & (~df_indeed.Salary.str.contains('al día'))]

df_indeed.Salary = df_indeed.Salary.str.replace(',', '').str.replace('$', '')

new_salaries = []
for i in df_indeed.Salary:
    a = i.split('-')
    if len(a) == 2:
        new_salaries.append(np.mean([float(b) for b in a if a != 'None']))
    else:
        new_salaries.append(a[0])
        
df_indeed.Salary = new_salaries

df_indeed['Salary'] = pd.to_numeric(df_indeed['Salary'], errors = "coerce")
df_indeed.Salary = df_indeed.Salary.astype(float)


df_indeed['Salary'] = np.where(df_indeed['Frequency']=='año', df_indeed['Salary'].div(12), df_indeed['Salary'])
df_indeed['Salary'] = np.where(df_indeed['Frequency']=='semana', df_indeed['Salary']*4, df_indeed['Salary'])
df_indeed['Salary'] = np.where(df_indeed['Frequency']=='día', df_indeed['Salary']*30, df_indeed['Salary'])

df_indeed = df_indeed.drop(columns = ['Frequency'])

# finalize columns 
# df_indeed.columns

df_indeed['Sector'] = 'None'
df_indeed['Level'] = 'None'
df_indeed['Type'] = 'None'
df_indeed['Function'] = 'None'
df_indeed['Source'] = 'Indeed'


df_indeed.columns = df_indeed.columns.str.lower()

# drop duplicates based on link column (same link = same job)
df_indeed = df_indeed.drop_duplicates(subset='link', keep="first")

df_indeed.to_excel('df_indeed_clean.xlsx', sheet_name = 'Sheet 1', index = False)

### upload to sql ###


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
    


execute_many(conn = conn, df = df_indeed, table = 'jobs_dbase')
# WORKS!
