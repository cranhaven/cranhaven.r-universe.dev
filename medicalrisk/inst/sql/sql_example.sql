-- Worked example of how to apply the techniques in this R package to a SQL context
-- Patrick McCormick <patrick.mccormick@alum.mit.edu>

-- MSSQL 2008
-- sample table
create table ehr (
  mrn char(12) primary key,
  icddx_1 char(10),
  icddx_2 char(10),
  icddx_3 char(10)
);

-- data from the VT inp dataset
insert into ehr values
('0001010', '71596', '4254', '4280'),
('0001020', '71536', '2724', '4019'),
('0001030', '71516', '2724', '4019');

-- subset of values from the icd9cm_elixhauser_quan dataset
create table icd9cm_elixhauser_quan (
  icd char(10) primary key,
  chf int,
  htn int
);

-- in reality this would have all ICD9 codes that are in the map
insert into icd9cm_elixhauser_quan values 
  ('D39891', 1, 0),
  ('D40201', 1, 0),
  ('D4010', 0, 1),
  ('D4011', 0, 1),
  ('D4019', 0, 1);

-- flatten ehr to one row per ICD code
select x.mrn, x.icd
into ehr_flat
from (
  select mrn, 'D'+icddx_1 as icd
  from ehr
  union all
  select mrn, 'D'+icddx_2
  from ehr
  union all
  select mrn, 'D'+icddx_3
  from ehr
) x
order by mrn
go

-- apply the map
select e.mrn, 
isnull(max(i.chf),0) as chf, 
isnull(max(i.htn),0) as htn
from ehr_flat e
left join icd9cm_elixhauser_quan i on e.icd=i.icd
group by e.mrn

