create table word_part_of_speech (
	word_part_of_speech_id integer unsigned auto_increment not null,
	word_id	integer unsigned not null,
	part_of_speech_id integer unsigned not null,
	primary key(word_part_of_speech_id)
);