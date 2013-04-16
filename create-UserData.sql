CREATE TABLE UserData (
  allowDataUriScheme boolean,
  fingerprint varchar(35),
  userid text,
  ipid varchar(15),
  output_illusion_height int,
  output_illusion_hidden boolean,
  output_illusion_width int,
  output_questionCounter_hidden boolean,
  output_weightControl_hidden boolean,
  pixelratio real,
  q int,
  systime timestamp, 
  url_hostname text,
  url_pathname varchar(20),
  url_port varchar(10),
  url_protocol varchar(10),
  url_search text,
  weight real,
  xy varchar(1)
);

create index userid on UserData(userid);
create index ipid on UserData(ipid);
create index xy on UserData(xy);
create index weight on UserData(weight);
create index fingerprint on UserData(fingerprint);