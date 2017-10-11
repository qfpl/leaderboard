create table playerToLadder (
  foreign key player references players (id),
  foreign key ladder references ladders (id),
  primary key(player, ladder)
)
