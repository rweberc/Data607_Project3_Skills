drop schema if exists project3;
create schema project3;
use project3;

#--create indeed tables
drop table if exists indeed_edu; 
create table indeed_edu(indeed_edu_id integer primary key, 
education_level varchar(150) not null, 
count integer, 
job_count integer);

#--populate the indeed education table 
insert into indeed_edu (indeed_edu_id, education_level, count, job_count)
	values(1, 'bachelor', 0, 60), 
	(2, 'master', 15, 60), 
	(3, 'phd', 21, 60);

#--indeed skills table 
drop table if exists indeed_skills;
create table indeed_skills (indeed_skill_id integer primary key, 
skill varchar(150), 
count integer, 
job_count integer);

#--populate indeed skills
insert into indeed_skills (indeed_skill_id, skill, count, job_count)
	values(1, 'hadoop', 19, 60), 
	(2, 'python', 44, 60), 
	(3, 'sql', 30, 60), 
	(4, 'nosql', 12, 60), 
	(5, 'r', 39, 60), 
	(6, 'spark', 19, 60), 
	(7, 'sas', 18, 60), 
	(8, 'excel', 20, 60), 
	(9, 'hive', 13, 60), 
	(10, 'c', 16, 60), 
	(11, 'java', 15, 60), 
	(12, 'tableau', 14, 60);



#---zip Rec tables
drop table if exists zip_edu; 
create table zip_edu(zip_edu_id integer primary key, 
education_level varchar(150) not null, 
count integer, 
percent decimal(18, 2));

#--populate the zip education table 
insert into zip_edu (zip_edu_id, education_level, count, percent)
	values(1, 'bachelor', 230, 0.61), 
	(2, 'master', 157, 0.41), 
	(3, 'phd', 19, 0.05);

#--zip skill table 
#--zip skills table 
drop table if exists zip_skills;
create table zip_skills (zip_skill_id integer primary key, 
skill varchar(150), 
count integer, 
percent decimal(18, 2));

#--populate skill table 
#--populate zip skills
insert into zip_skills (zip_skill_id, skill, count, percent)
	values(1, 'hadoop', 185, 0.49), 
	(2, 'python', 156, 0.41), 
	(3, 'sql', 209, 0.55), 
	(4, 'nosql', 71, 0.19), 
	(5, 'r', 80, 0.21), 
	(6, 'spark', 151, 0.40), 
	(7, 'sas', 33, 0.09), 
	(8, 'excel', 29,0.08), 
	(9, 'hive', 109, 0.29), 
	(10, 'c', 42, 0.11), 
	(11, 'java', 146, 0.39), 
	(12, 'tableau', 66, 0.17);


#--reddit edu tables 
drop table if exists reddit_edu; 
create table reddit_edu(reddit_edu_id integer primary key, 
education_level varchar(150) not null, 
count integer, 
percent decimal(18, 2));

#--populate the reddit education table 
insert into reddit_edu (reddit_edu_id, education_level, count, percent)
	values(1, 'bachelor', 39, 0.31), 
	(2, 'master', 83, 0.66), 
	(3, 'phd', 46, 0.37);


#--reddit skills table 
drop table if exists reddit_skills;
create table reddit_skills (reddit_skill_id integer primary key, 
skill varchar(150), 
count integer, 
percent decimal(18, 2));

#--populate skill table 
#--populate reddit skills
insert into reddit_skills (reddit_skill_id, skill, count, percent)
	values(1, 'python', 80, 0.64), 
	(2, 'sql', 49, 0.39), 
	(3, 'nosql', 8, 0.06), 
	(4, 'r', 75, 0.60), 
	(5, 'spark', 27, 0.22), 
	(7, 'sas', 14, 0.11), 
	(8, 'excel', 29,0.23), 
	(9, 'hive', 10, 0.08), 
	(10, 'c', 19, 0.15), 
	(11, 'java', 23, 0.18), 
	(12, 'tableau', 19, 0.15);
