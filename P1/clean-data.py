# clean-data.py
#
# Download and prepare the data for use from a postgres instance.
#######################################

from configparser import ConfigParser

import pandas as pd
import psycopg2


def config(filename='database.ini', section='postgresql'):
    # create a parser
    parser = ConfigParser()
    # read config file
    parser.read(filename)

    # get section, default to postgresql
    db = {}
    if parser.has_section(section):
        params = parser.items(section)
        for param in params:
            db[param[0]] = param[1]
    else:
        raise Exception('Section {0} not found in the {1} file'.format(section, filename))

    return db


def gather(query):
    conn = psycopg2.connect(**config())
    cur = conn.cursor()
    cur.execute(query)
    cols = [d[0] for d in cur.description]
    data = cur.fetchall()
    conn.close()

    return pd.DataFrame(data=data, columns=cols)


if __name__ == '__main__':
    df = gather('select * from skater_data where season=2019;')

    print(df.shape)
    print(df.head())
