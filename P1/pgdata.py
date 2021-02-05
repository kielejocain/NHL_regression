# pgdata.py
#
# Download and prepare the data for use from a postgres instance.
#######################################

from configparser import ConfigParser

import pandas as pd
from sqlalchemy import create_engine


def configure_engine(filename='database.ini', section='postgresql'):
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

    return create_engine(f'postgresql+psycopg2://{db["user"]}:{db["password"]}@{db["host"]}/{db["database"]}')


def gather(query):
    engine = configure_engine()
    conn = engine.connect()
    data = pd.read_sql(query, conn)
    conn.close()

    return data


def write(table, data, **kwargs):
    engine = configure_engine()
    conn = engine.connect()
    data.to_sql(name=table, con=conn, **kwargs)
    conn.close()


if __name__ == '__main__':
    df = gather('select * from skaterstats where season=2019;')

    print(df.shape)
    print(df.head())
