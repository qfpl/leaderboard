create table players (
  id serial primary key,
  firstName text not null,
  lastName text,
  email text not null,
  foreign key rating references ratings (id) not null
)
