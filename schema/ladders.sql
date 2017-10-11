create table ladders (
  id serial primary key,
  name text not null,
  foreign key owner references players (id) not null
)
