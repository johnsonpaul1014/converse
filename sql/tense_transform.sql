create table tense_transform (
	tense_transform_id 	integer unsigned auto_increment not null,
	words_to_transform	varchar(128),
	new_word						varchar(64),
	primary key(tense_transform_id)
);