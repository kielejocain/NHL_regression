# Python NHL Models

## Setup

for Linux Mint, I needed to install the Postgres and Python development libraries before my psycopg2 Python module could
install:

    sudo apt-get install libpq-dev python-dev

After that, I was able to install my Python requirements without issue.

    pip install -r requirements.txt

To connect to the local Postgres DB, you'll need a `database.ini` file in this folder that looks like:

    [postgresql]
    host=localhost
    database=mydatabase
    user=myuser
    password=mypassword


