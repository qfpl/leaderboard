create table ratings (
  id serial primary key,
  rating float8 not null,
  dev float8 not null,
  vol float8 not null,
  inactivity integer not null,
  age integer not null,
)
