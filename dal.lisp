;;; DAL (Data access layer) for converse

(in-package :converse)

(defun db-connect()
	"Connects to the database, disconnecting first to prevent errors from timed out connections"
	(if clsql:*DEFAULT-DATABASE*
		(clsql:disconnect :error nil))
	(clsql:connect '("localhost" "converse" "converse_rw" "converse_rw") :database-type :mysql :if-exists :old))

(defun insert-sentence (sentence)
	"Inserts a sentence"
	(db-connect)
	(clsql:query (concatenate 'string "INSERT INTO sentence (sentence_text) VALUES('" sentence "')")))

(defun get-sentences ()
	"Gets all the sentences"
	(db-connect)
	(clsql:query "SELECT * FROM sentence"))

(defun last-two-chars (word)
	"Gets a string of the last two characters of the word passed in.  Assumes the word length is greater than 1."
	(subseq word (- (length word) 2)))

(defun get-parts-of-speech (word)
	"Gets the parts of speech from the database.  If they don't exist, get them from the web and store them for next time."
	(let ((word-length (length word)))
		(if (and (> 1 word-length) (equal (last-two-chars word) "'s") (equal (last-two-chars word) "s'"))
			(if (and (not (is-a-question-word word)) (upper-case-p (aref word 0)))
				'("Adjectives")
				'("adjective"))
			(if (string-in-list word *PREPOSITIONS*)
				'("preposition")
				(if (string-in-list word *ARTICLES*)
					'("article")
					(if (and (> word-length 4) (equal "ing" (subseq word (- word-length 3))))
						'("participle" "noun")
					
						;; It could be an adjective if part of a possesive, i.e. Bone Boy's favorite bone
						(if (and (not (is-a-question-word word)) (upper-case-p (aref word 0)))
							'("Noun" "Adjective")

							(progn (db-connect)
								(let ((result (clsql:query (concatenate 'string "SELECT part_of_speech_name FROM word INNER JOIN word_part_of_speech ON word.word_id = "
															"word_part_of_speech.word_id INNER JOIN part_of_speech ON word_part_of_speech.part_of_speech_id = "
															"part_of_speech.part_of_speech_id WHERE word.word_name = '" word "'") :flatp t)))
									(if result
										result
										(let ((parts-of-speech (get-web-parts-of-speech word)))
											(insert-parts-of-speech (if (is-a-question-word word) (string-downcase word) word) parts-of-speech)
											parts-of-speech)))))))))))

(defun insert-parts-of-speech (word parts-of-speech)
	(db-connect)
	(clsql:execute-command (concatenate 'string "INSERT INTO word (word_name) VALUES ('" word "')"))
	(let ((word-id (prin1-to-string (car (clsql:query "SELECT LAST_INSERT_ID()")))))
		(dolist (part-of-speech parts-of-speech)
			(clsql:execute-command (concatenate 'string "INSERT INTO word_part_of_speech (word_id, part_of_speech_id) VALUES (" word-id 
				", (SELECT part_of_speech_id FROM part_of_speech WHERE part_of_speech_name = '" part-of-speech "'))")))))

(defun get-tense-transform (words-to-transform)
	(db-connect)
	(clsql:query (concatenate 'string "SELECT new_word FROM tense_transform WHERE words_to_transform = '" words-to-transform "'") :flatp t))

(defun get-reverse-tense-transform (word-to-transform)
	(db-connect)
	(clsql:query (concatenate 'string "SELECT words_to_transform FROM tense_transform WHERE new_word = '" (mysql-escape word-to-transform) "'") :flatp t))

(defun insert-tense-transform (words-to-transform new-word)
	(db-connect)
	(clsql:execute-command (concatenate 'string "INSERT INTO tense_transform (words_to_transform, new_word) VALUES ('" words-to-transform "', '"
		new-word "')")))

(defun mysql-escape (string-to-escape)
	"Escapes ' characters"
	(let ((new-string ""))
		(dotimes (i (length string-to-escape))
			(let ((current-char (char string-to-escape i)))
				(if (equal current-char #\')
					(setf new-string (concatenate 'string new-string "\\\'"))
					(setf new-string (concatenate 'string new-string (string current-char))))))
		new-string))