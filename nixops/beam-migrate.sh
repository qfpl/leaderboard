(cd ../src;
 beam-migrate migrate \
     --backend Database.Beam.Postgres.Migrate \
     -M Leaderboard.Schema \
     --host $1 \
     --database $2 \
     --user $3)
