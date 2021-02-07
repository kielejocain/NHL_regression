# model.py
#
# The driving script for the modeling process.
##############################################

from itertools import accumulate

import matplotlib.pyplot as plt
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.ensemble import RandomForestRegressor
from sklearn.preprocessing import StandardScaler

import pgdata


def clean_season(season_data, max_games, skip_count):
    """Takes in a season of NHL skater data and prepares it to be modeled."""
    output = season_data.set_index('nhl_num')
    for c in list(output.columns)[skip_count:]:
        output[c] = output[c] / output['games_played']
    output['games_played'] = output['games_played'] / max_games
    return output


if __name__ == '__main__':
    # constants
    nhl_games_played = {2012: 48, 2019: 70, 2020: 56}  # seasons since 2009-10 shorter than 82 games
    pca_out = False      # whether or not we need to save off pca graphs/components
    start_season = 2009  # first year of first season (assuming typical October start)
    end_season = 2019    # first year of last season (assuming typical October start)
    skip_cols = 3        # skip id, season, and games_played columns for most computations

    # gather seasonal data from postgres
    data = []
    for season in range(start_season, end_season+1):
        raw = pgdata.gather(f'select * from skaterstats where season = {season}')
        data.append(clean_season(raw, nhl_games_played.get(season, 82), skip_cols))

    # create PCA dataset
    pca_data = pd.concat(data)

    # filter out players that played fewer than 10 games, drop games played and team name columns
    pca_data = pca_data.loc[pca_data.games_played >= 0.1]
    pca_data = pca_data.iloc[:, skip_cols:]

    # scale and transform the data
    scaler = StandardScaler()
    scaler.fit(pca_data)
    X_pca = scaler.transform(pca_data)

    pca = PCA(svd_solver='full')
    pca.fit(X_pca)

    # Save off PCA explanations
    if pca_out:
        variances = [0] + list(accumulate(pca.explained_variance_ratio_))
        plt.plot(range(0, len(variances)), variances)
        plt.savefig('pca_vars.png')

        pca_comps = pd.DataFrame(data=pca.components_, columns=pca_data.columns, dtype=float)
        pca_comps['variance'] = pca.explained_variance_ratio_
        pca_comps.to_csv('pca_comps.csv', index=False)

    # Build input set
    idxs = []
    Xs = []
    for i in range(0, len(data)-1):
        idxs.append(data[i].index.intersection(data[i+1].index))
    for i, idx in enumerate(idxs):
        Xs.append(data[i].loc[idx])
    X = pca.transform(scaler.transform(pd.concat(Xs[:-1]).iloc[:, skip_cols:]))[:, :20]
    X_test = pca.transform(scaler.transform(Xs[-1].iloc[:, skip_cols:]))[:, :20]

    # need to correct games played to account for seasonal variance
    stats = [
        'games_played',
        'es_goals', 'pp_goals', 'sh_goals',
        'es_assists', 'pp_assists', 'sh_assists',
        'es_goals_for', 'pp_goals_for', 'sh_goals_for',
        'es_goals_against', 'pp_goals_against', 'sh_goals_against',
        'es_toi', 'pp_toi', 'sh_toi',
        'shots', 'fo_wins', 'fo_losses',
        'minors', 'majors', 'misconducts', 'game_misconducts'
    ]
    preds = {}
    for stat in stats:
        # build target
        ys = []
        for i, idx in enumerate(idxs):
            ys.append(data[i+1].loc[idx, stat])
        y = pd.concat(ys[:-1])
        idx = ys[-1].index

        # train random forests
        print(f'Training {stat}')
        # train a forest
        forest = RandomForestRegressor(max_features=4, oob_score=True, n_jobs=-1, max_samples=0.8)
        forest.fit(X, y)

        # test model on out-of-sample season
        pred = pd.Series(forest.predict(X_test), index=idx, name=stat)
        preds[stat] = pred
        print(f'Correlation for {stat}: {pred.corr(Xs[-1][stat])}')

    pred_df = pd.concat(preds, axis=1)

    pred_df['games_played'] = pred_df['games_played'] * nhl_games_played.get(end_season + 1, 82)
    for col in list(pred_df.columns)[1:]:
        pred_df[col] = pred_df[col] * pred_df['games_played']

    # need penalty_minutes
    pred_df['goals'] = pred_df['es_goals'] + pred_df['pp_goals'] + pred_df['sh_goals']
    pred_df['assists'] = pred_df['es_assists'] + pred_df['pp_assists'] + pred_df['sh_assists']
    pred_df['es_points'] = pred_df['es_goals'] + pred_df['es_assists']
    pred_df['pp_points'] = pred_df['pp_goals'] + pred_df['pp_assists']
    pred_df['sh_points'] = pred_df['sh_goals'] + pred_df['sh_assists']
    pred_df['points'] = pred_df['es_points'] + pred_df['pp_points'] + pred_df['sh_points']
    pred_df['plus_minus'] = pred_df['es_goals_for'] + pred_df['sh_goals_for'] - pred_df['es_goals_against'] - pred_df['sh_goals_against']
    pred_df['toi'] = pred_df['es_toi'] + pred_df['pp_toi'] + pred_df['sh_toi']
    pred_df['shot_pct'] = pred_df['goals'] / pred_df['shots']
    pred_df['penalty_minutes'] = 2 * pred_df['minors'] + 5 * pred_df['majors'] + 10 * (pred_df['misconducts'] + pred_df['game_misconducts'])

    # with joining done, pull nhl_num out of index and generate one called 'id'
    pred_df.reset_index(inplace=True)
    pred_df.index.set_names(names='id', inplace=True)

    pgdata.write('skatpred', pred_df, if_exists='replace')
