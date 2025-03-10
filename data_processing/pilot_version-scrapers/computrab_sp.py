
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
from datetime import timedelta
import matplotlib.pyplot as plt
import seaborn as sns
from time import sleep
import random
import locale
from tabulate import tabulate
from fake_useragent import UserAgent

user_agent_list = [
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.1.1 Safari/605.1.15',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:77.0) Gecko/20100101 Firefox/77.0',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36',
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:77.0) Gecko/20100101 Firefox/77.0',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36',
]

## Computrabajo
URL = 'https://www.computrabajo.com.mx/trabajo-de-energia?q=energia'


headers = {'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
    'accept-encoding': 'gzip, deflate, sdch, br',
    'accept-language': 'en-GB,en-US;q=0.8,en;q=0.6',
    'referer': 'https://www.glassdoor.com/',
    'upgrade-insecure-requests': '1',
    'user-agent': random.choice(user_agent_list),
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive'
    }


url_template = 'https://www.computrabajo.com.mx/trabajo-de-energia?p={}&by=publicationtime&q=energia/'
max_results = 100
i = 0
results = []
computrabajo_df_more = pd.DataFrame(columns=["Title","Municipality","State","Company","Salary", "Synopsis", "Link"])
for start in range(0, max_results, 1):
    # get results from request above
    url = url_template.format(start)
    # append to full set
    html = requests.get(url, headers = headers)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding = "utf-8")
    sleep(10)
    for each in soup.find_all(name="div", attrs={"class":"bRS"}):
                try: 
                    title = each.find('a', class_="js-o-link").get("title").replace('\n', '').replace('Empleo de ', '')
                except:
                    title = 'None'
                try:
                    location = each.find('div', class_="w_100").contents[3].text.replace('\n', '').split(",", maxsplit=1)[1]
                except:
                    location = 'None'
                try:
                    location_2 = each.find('div', class_="w_100").contents[3].text.replace('\n', '').split(",", maxsplit=1)[0]
                except:
                    location_2 = 'None'
                try:
                    date = each.find('span', class_='dO')
                except:
                    date = 'None'
                try: 
                    link = 'http://www.computrabajo.com.mx' + each.find('a', class_="js-o-link").get("href").replace('\n', '')
                except:
                    link = 'None'
                try: 
                    company = each.find('a', class_="fc_blue").get("title").replace('\n', '').replace('Empleos en ', '')
                except:
                    company = 'None'
                try:
                    salary =  each.find('meta', itemprop="baseSalary").get("content").replace('\n', '')
                except:
                    salary = 'None'
                try:
                    synopsis = each.find("p").text.replace('\n', '')
                except:
                    synopsis = 'None'
                computrabajo_df_more = computrabajo_df_more.append({'Title':title, 'Municipality':location, 'State':location_2, 'Company':company, 'Salary':salary, 'Synopsis':synopsis, 'Date':date, 'Link':link}, ignore_index=True)            



computrabajo_df_more = computrabajo_df_more.drop_duplicates()


computrabajo_df_more = computrabajo_df_more.drop(columns = ["Salary"])

cptr = pd.DataFrame(columns=["Link", "Description", "Salary"])

# loop through and pull descriptions and salaries
ct_descriptions = []
ct_salaries = []
for link in computrabajo_df_more.iterrows():
    url = link[1]['Link']
    x = requests.get(url, headers = headers)
    soup = BeautifulSoup(x.content,"html.parser")
    sleep(10)
    try:
        description = soup.find('ul', {"class":"p0 m0"}).get_text().replace('\n', '').replace('\r', '')
    except:
        description = 'None'
    try:
        salary = soup.find('section', {"class":"boxWhite"}).find('span', itemprop="baseSalary").find('meta', itemprop='value').get('content')
    except:
        salary = 'None'
    cptr = cptr.append({'Description': description, 'Salary': salary, 'Link': url}, ignore_index = True)

# computrabajo_df_more['Description'] = ct_descriptions
# computrabajo_df_more['Salary'] = ct_salaries


computrabajo_df_more = computrabajo_df_more.join(cptr.set_index('Link'), on='Link')

computrabajo_df_more['Date'] = computrabajo_df_more['Date'].astype(str).str.replace(r"(\s*\<.*?\>\s*)","")



# clean date


computrabajo_df_more['Date'] = computrabajo_df_more['Date'].astype(str) + ' 2020'

computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('agosto', 'August')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('septiembre', 'September')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('octubre', 'October')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('noviembre', 'November')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('diciembre', 'December')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('julio', 'July')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('junio', 'June')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('mayo', 'May')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('abril', 'April')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('marzo', 'March')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('febrero', 'February')
computrabajo_df_more['Date'] = computrabajo_df_more['Date'].str.replace('enero', 'January')


computrabajo_df_more.loc[computrabajo_df_more['Date'].str.contains('Hoy'), 'Date'] = datetime.date.today() 
computrabajo_df_more.loc[computrabajo_df_more['Date'].str.contains('Ayer'), 'Date'] = datetime.date.today() - timedelta(days=1)

# computrabajo_df_more.loc[computrabajo_df_more["Date"].str.contains('08-') == True, 'Date'] = computrabajo_df_more["Date"].str.replace(' 2020', '')


computrabajo_df_more["Date"] = pd.to_datetime(computrabajo_df_more['Date'], format="%Y-%m-%d", errors="coerce").fillna(pd.to_datetime(computrabajo_df_more['Date'], format="%d %B %Y", errors="coerce"))





# location
computrabajo_df_more['State'] = computrabajo_df_more['State'].str.strip()
computrabajo_df_more = computrabajo_df_more.replace({'State' : {'Ciudad de México DF' : 'Ciudad de México (Distrito Federal)',
                                          'Puebla' : 'Puebla (de los Angeles)'}})


computrabajo_df_more.loc[computrabajo_df_more['State'].isin(['Nuevo León','Hidalgo','Chiapas',
                               'Jalisco','Durango','Ciudad de México (Distrito Federal)',
                               'Tlaxcala','Chihuahua','Baja California','Querétaro',
                               'San Luis Potosí','Guanajuato','Estado de México',
                               'Coahuila','Tabasco','Puebla (de los Angeles)',
                               'Morelos','Tamaulipas','Quintana Roo', 'Veracruz',
                               'Campeche','Aguascalientes','Nayarit','Michoacán',
                               'Colima','Sinaloa','Sonora','Yucatán','Oaxaca',
                               'Baja California Sur','Zacatecas','None']) == False, 'State'] = 'None'

computrabajo_df_more['Location'] = computrabajo_df_more['Municipality'] + ', ' + computrabajo_df_more['State']


# clean company

computrabajo_df_more['Company'] = computrabajo_df_more['Company'].str.strip()

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


computrabajo_df_more.loc[computrabajo_df_more['Company'].isin(remove) == True, 'Company'] = "None"



# finalize columns 
computrabajo_df_more.columns

computrabajo_df_more['Sector'] = 'None'
computrabajo_df_more['Level'] = 'None'
computrabajo_df_more['Type'] = 'None'
computrabajo_df_more['Function'] = 'None'
computrabajo_df_more['Source'] = 'Computrabajo'


computrabajo_df_more.columns = computrabajo_df_more.columns.str.lower()

computrabajo_df_more.to_excel('computrabajo_df_more_clean.xlsx', sheet_name = 'Sheet 1', index = False)

# drop duplicates based on link column (same link = same job)
computrabajo_df_more = computrabajo_df_more.drop_duplicates(subset='link', keep="first")

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
    
 


execute_many(conn = conn, df = computrabajo_df_more, table = 'jobs_dbase')
# WORKS!

