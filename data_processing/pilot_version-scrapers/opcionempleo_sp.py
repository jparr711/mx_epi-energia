
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
    'referer': 'https://www.glassdoor.com/',
    'upgrade-insecure-requests': '1',
    'user-agent': random.choice(user_agent_list),
    'Cache-Control': 'no-cache',
    'Connection': 'keep-alive'
    }


url_template = 'https://www.opcionempleo.com.mx/empleo-energia.html?p={}&sort=date'
max_results = 10
i = 0
results = []
opcionempleo_df = pd.DataFrame(columns=["Title","Company", "Location", "Salary", "Date", "Synopsis", "Link"])
for start in range(0, max_results, 1):
    # get results from request above
    url = url_template.format(start)
    # append to full set
    html = requests.get(url, headers = headers)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding = "utf-8")
    sleep(10)
    for each in soup.find_all("article", attrs={"class":"job"}):
                try: 
                    title = each.find("h2").find("a").get("title").replace('\n', '')
                except:
                    title = 'None'
                try:
                    company = each.find("p", class_="company").get_text().replace('\n', '')
                except:
                    company = 'None'
                try:
                    location = each.find('ul', {"class":'details'}).findChildren()[0].getText().replace('\n', '').strip()
                except:
                    location = 'None'
                try:
                    date =  each.find('span', {"class":'badge'}).text.replace('\n', '')
                except:
                    date = 'None'
                try:
                    salary = each.find('ul', {"class":'details'}).findChildren()[3].getText().replace('\n', '').strip()
                except:
                    salary = 'None'
                try:
                    link = 'http://www.opcionempleo.com.mx' + each.find("h2").find("a").get("href").replace('\n', '')
                except:
                    link = "None"
                try:
                    synopsis = each.find("div", class_="desc").getText().replace('\n', '')
                except:
                    synopsis = 'None'
                opcionempleo_df = opcionempleo_df.append({'Title':title, 'Company':company, 'Location':location, 'Salary':salary, 'Date':date, 'Synopsis':synopsis, 'Link':link}, ignore_index=True)

opcionempleo_df = opcionempleo_df.drop_duplicates()
opcionempleo_df = opcionempleo_df[opcionempleo_df.Title != "None"]

descriptions = []

opemp = pd.DataFrame(columns=["Description", "Link"])


for link in opcionempleo_df.iterrows():
    url = link[1]['Link']
    html = requests.get(url)
    soup = BeautifulSoup(html.content, 'html.parser', from_encoding="utf-8")
    sleep(10)
    try:
        description = soup.find('section', {"class":"content"}).get_text().replace('\n', '')
    except:
        description = 'None'
    descriptions.append(description)
    opemp = opemp.append({'Description':description, 'Link':url}, ignore_index = True)

# opcionempleo_df['Description'] = descriptions

# split location into two
opcionempleo_df[['Municipality', 'State']] = opcionempleo_df['Location'].str.split(pat = ",", n = 1, expand=True)

# drop duplicates    
opcionempleo_df = opcionempleo_df.drop_duplicates()

# join
opcionempleo_df = opcionempleo_df.join(opemp.set_index('Link'), on='Link')

# save temp file
# opcionempleo_df.to_excel('opcionempleo_df_july_23(1).xlsx', sheet_name = 'Sheet 1')


### clean data - opcion empleo

#### location ####

opcionempleo_df.loc[opcionempleo_df['State'].isnull() & opcionempleo_df['Municipality'].notnull(), ['Municipality', 'State']] = opcionempleo_df.loc[opcionempleo_df['State'].isnull() & opcionempleo_df['Municipality'].notnull(), ['State', 'Municipality']].values


opcionempleo_df['Municipality'] = opcionempleo_df['Municipality'].str.replace('Ciudad de México', 'None')

opcionempleo_df.State = opcionempleo_df.State.str.strip()
opcionempleo_df.Municipality = opcionempleo_df.Municipality.str.strip()

opcionempleo_df = opcionempleo_df.replace({'State' : {'Ciudad de México' : 'Ciudad de México (Distrito Federal)',
                              'N.L.' : 'Nuevo León', 
                              'Hgo.' : 'Hidalgo', 
                              'Chis.' : 'Chiapas', 
                              'Jal.' : 'Jalisco', 'Dgo.': 'Durango',
                              'CDMX' : 'Ciudad de México (Distrito Federal)' ,
                              'D.F.' : 'Ciudad de México (Distrito Federal)' ,
                              'Tlax.' : 'Tlaxcala', 
                              'Chih.' : 'Chihuahua', 
                              'B.C.' : 'Baja California', 
                              'Qro.' : 'Querétaro', 
                              'S.L.P.': 'San Luis Potosí', 
                              'Gto.' : 'Guanajuato',
                              'Méx.' : 'Estado de México',
                              'Coah.' : 'Coahuila', 
                              'Tab.' : 'Tabasco', 
                              'Pue.' : 'Puebla (de los Angeles)',
                              'Mor.' : 'Morelos', 
                              'Tamps.': 'Tamaulipas', 
                              'QRoo.' : 'Quintana Roo',
                              'Ver.' : 'Veracruz', 
                              'Camp.' : 'Campeche', 
                              'Ags.' : 'Aguascalientes',
                              'Nay.' : 'Nayarit', 
                              'Mich.' : 'Michoacán', 
                              'Col.' : 'Colima', 
                              'Sin.' : 'Sinaloa',
                              'Son.' : 'Sonora', 
                              'Yuc.' : 'Yucatán', 
                              'Oax.' : 'Oaxaca', 
                              'B.C.S.': 'Baja California Sur', 
                              'Zac.' : 'Zacatecas',
                              'México': 'None'}})


opcionempleo_df.loc[opcionempleo_df['State'] == 'Ciudad de México (Distrito Federal)', ['State', 'Municipality']]


opcionempleo_df.loc[opcionempleo_df['State'] == 'Roma', 'Municipality'] = 'Roma'

opcionempleo_df[['State']] = opcionempleo_df[['State']].replace(['Roma', 'Cuenca'], ['Ciudad de México (Distrito Federal)', 'None'])

opcionempleo_df.loc[opcionempleo_df['State'].isin(['Nuevo León','Hidalgo','Chiapas',
                               'Jalisco','Durango','Ciudad de México (Distrito Federal)',
                               'Tlaxcala','Chihuahua','Baja California','Querétaro',
                               'San Luis Potosí','Guanajuato','Estado de México',
                               'Coahuila','Tabasco','Puebla (de los Angeles)',
                               'Morelos','Tamaulipas','Quintana Roo', 'Veracruz',
                               'Campeche','Aguascalientes','Nayarit','Michoacán',
                               'Colima','Sinaloa','Sonora','Yucatán','Oaxaca',
                               'Baja California Sur','Zacatecas','None']) == False, 'State'] = 'None'



#### date ####
def opemp_get_past_date(str_days_ago):
    TODAY = datetime.date.today() - relativedelta(days = 7)
    splitted = str_days_ago.split()
    if len(splitted) == 3 and splitted[1].lower() in ['minutos']:
        return str(TODAY.isoformat())
    elif len(splitted) == 3 and splitted[2].lower() in ['horas']:
        return str(TODAY.isoformat())
    elif len(splitted) == 1 and splitted[1].lower() in ['ahora']:
        return str(TODAY.isoformat())
    elif len(splitted) == 3 and splitted[2].lower() in ['días', 'día']:
        date = TODAY - relativedelta(days=int(splitted[1]))
        return str(date.isoformat())
    elif len(splitted) == 3 and splitted[2].lower() in ['días']:
        date = TODAY - relativedelta(days=30)
        return str(date.isoformat())
    elif len(splitted) == 3 and splitted[2].lower() in ['mes']:
        date = TODAY - relativedelta(months=12)
        return str(date.isoformat())
    elif len(splitted) == 3 and splitted[2].lower() in ['meses']:
        date = TODAY - relativedelta(months =int(splitted[1]))
        return str(date.isoformat())
    else:
        return "Wrong Argument format"


op_dates = []
for op_date in opcionempleo_df['Date']:
    op_date = opemp_get_past_date(op_date)
    op_dates.append(op_date)
    

opcionempleo_df['Date'] = op_dates


### salary ###


opcionempleo_df['Salary'] = opcionempleo_df['Salary'].replace('\\n', '', regex= True)

opcionempleo_df['Frequency'] = opcionempleo_df['Salary'].str.rsplit(' ', 1).str[-1]


opcionempleo_df['Salary'] = opcionempleo_df.apply(lambda row : row['Salary'].replace(str(row['Frequency']), ''), axis=1)

# opcionempleo_df[(~opcionempleo_df.Salary.str.contains('por hora')) & (~opcionempleo_df.Salary.str.contains('al mes'))
#                    & (~opcionempleo_df.Salary.str.contains('por semana')) & (~opcionempleo_df.Salary.str.contains('al día'))]

opcionempleo_df.Salary = opcionempleo_df.Salary.str.replace('.', '').str.replace('$', '')
opcionempleo_df.Salary = opcionempleo_df.Salary.str.replace('a', '-').str.strip()

op_new_salaries = []
for i in opcionempleo_df.Salary:
    a = i.split(' - ')
    if len(a) == 2:
        op_new_salaries.append(np.mean([float(b) for b in a if a != 'None']))
    else:
        op_new_salaries.append(a[0])
        
opcionempleo_df.Salary = op_new_salaries

opcionempleo_df['Salary'] = pd.to_numeric(opcionempleo_df['Salary'], errors = "coerce")
opcionempleo_df.Salary = opcionempleo_df.Salary.astype(float)


opcionempleo_df['Salary'] = np.where(opcionempleo_df['Frequency']=='año', opcionempleo_df['Salary'].div(12), opcionempleo_df['Salary'])
opcionempleo_df['Salary'] = np.where(opcionempleo_df['Frequency']=='semana', opcionempleo_df['Salary']*4, opcionempleo_df['Salary'])
opcionempleo_df['Salary'] = np.where(opcionempleo_df['Frequency']=='día', opcionempleo_df['Salary']*30, opcionempleo_df['Salary'])

opcionempleo_df = opcionempleo_df.drop(columns = ['Frequency'])

# clean company

opcionempleo_df['Company'] = opcionempleo_df['Company'].str.strip()

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


opcionempleo_df.loc[opcionempleo_df['Company'].isin(remove) == True, 'Company'] = "None"


# finalize columns 
# opcionempleo_df.columns


opcionempleo_df['Level'] = 'None'
opcionempleo_df['Sector'] = 'None'
opcionempleo_df['Type'] = 'None'
opcionempleo_df['Function'] = 'None'
opcionempleo_df['Source'] = 'OpcionEmpleo'


opcionempleo_df.columns = opcionempleo_df.columns.str.lower()


# drop duplicates based on link column (same link = same job)
opcionempleo_df = opcionempleo_df.drop_duplicates(subset='link', keep="first")

opcionempleo_df.to_excel('opcionempleo_df_clean.xlsx', sheet_name = 'Sheet 1', index = False)


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
    

execute_many(conn = conn, df = opcionempleo_df, table = 'jobs_dbase')
# WORKS!
