;;; Converse - A responding program

(defpackage converse
  (:export converse main)
  (:use common-lisp)
	(:import-from :port http-get))

(in-package converse)

(setf CUSTOM:*SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* t)
(defconstant *CONVERSE-DIR* "C:\\converse\\")
(defconstant *SENTENCE-FILE* (concatenate 'string *CONVERSE-DIR* "sentences.txt"))
(defconstant *PREPOSITIONS* '("about" "above" "across" "after" "against" "along" "among" "around" "at" "before" "behind" "below" "beneath" "beside" 
	"between" "beyond" "but" "by" "despite" "down" "during" "except" "for" "from" "in" "inside" "into" "near" "of" "off" "on" "onto" "out" 
	"outside" "over" "past" "since" "through" "throughout" "till" "to" "toward" "under" "underneath" "until" "up" "upon" "with" "within" "without"))
(defconstant *QUESTION-WORDS* '("who" "what" "where" "why" "when" "how"))
(defconstant *ADJECTIVE-QUESTION-WORDS* '("is" "was" "were" "are" "am"))
(defconstant *VERB-QUESTION-WORDS* '("does" "do" "did" "will" "would" "can" "could" "should"))
(defconstant *OPTIONAL-PARTICIPLES* '("doing"))
(defconstant *OPTIONAL-ADVERBS* '("really"))
(defconstant *ARTICLES* '("a" "the" "an" "this"))
(defconstant *TENSE-ADVERBS* '("tonight" "yesterday" "today" "tomorrow"))
(defconstant *SUBJECT-REGEX* "( article)?( adjective|(( Adjective)+ Adjectives))*(( Noun| article Noun)+| noun| pronoun)")
(defparameter *DEBUG* t)

(defun main())

(defun converse (sentence)
	"Main function.  Called with a sentence or question, such as (converse \"How old is Paul?\") or (converse \"Paul is 31 years old.\")"
  (if (is-question sentence) 
    (answer sentence)
    (record sentence)))

(defun is-question (sentence)
	"Determines if a sentence is a question"
  (if (equal #\? (char (reverse sentence) 0)) t nil))

(defun is-statement (sentence) 
	"Determines if a sentence is a statement"
  (if (equal #\. (char (reverse sentence) 0)) t nil))
  
(defun answer (question)   
	"Answers a question"
  (let ((words (reverse-personal-references (break-into-words question))))
		(let ((first-word (string-downcase (car words))))
			(cond ((string-in-list first-word *QUESTION-WORDS*) (answer-question-word-question words))
						((string-in-list first-word *ADJECTIVE-QUESTION-WORDS*) (answer-boolean-adjective-question words))
						((string-in-list first-word *VERB-QUESTION-WORDS*) (answer-boolean-verb-question words))
						(t "I didn't understand your question. Please try to rephrase it.")))))

(defun reverse-personal-references (words)
	"Reverse 'I', 'we' and 'you' references"
	(let ((new-words '()) (words-length (list-length words)))
		(dotimes (i words-length)
			(let ((word (string-downcase (nth i words))))
				(cond ((or (equal word "i") (equal word "we")) (append-to-list new-words (if new-words "you" "You")))
							((equal word "am") (append-to-list new-words "are"))
							((equal word "are") (append-to-list new-words "am"))
							((and (equal word "you") (equal (1- words-length) i)) (append-to-list new-words "me"))
							((equal word "you") (append-to-list new-words "I"))
							(t (append-to-list new-words (nth i words))))))
		new-words))

(defun answer-question-word-question (words)
	"Answers a question word question."

	(let ((question-word (string-downcase (car words))) (potential-boolean-question-word (cadr words)))

		;; If the question word is followed by one of the adjective words, the first form is used.  This would be in a case such as "Where is Paul going?".
		(if (string-in-list potential-boolean-question-word *ADJECTIVE-QUESTION-WORDS*)

			;; There are two types: "What is good?" and "What is Paul?".  Notice that the first type will have just a noun after the adjective question word, not
			;; an adjective, proper noun or article
			(if (string-in-list "noun" (get-parts-of-speech (caddr words)))
				
				;; Use the simple form, looking for a subject to match this predicate
				(find-word-set (list "" (build-string-from-words (cdr words))) question-word)

				;; Looking for a subject and predicate in the sentences
				(find-word-set (build-word-set (get-words-and-parts-of-speech words (concatenate 'string "^(pronoun|noun) verb" *SUBJECT-REGEX* 
					"($|( adjective)* preposition| participle| adverb)"))) question-word))
				
			;; Or it could be one of the verb question words
			(if (string-in-list potential-boolean-question-word *VERB-QUESTION-WORDS*)

				;; First check for a why question.  These are in the form "Why does Paul like beer?"
				(if (equal question-word "why")
					(find-word-set (build-word-set (get-words-and-parts-of-speech words (concatenate 'string 
						"^(pronoun|noun) verb" *SUBJECT-REGEX* " verb"))) question-word)
			
					;; Treat the other question words all the same
					(find-word-set (build-word-set (get-words-and-parts-of-speech words (concatenate 'string "^(pronoun|noun) verb" *SUBJECT-REGEX*
						"($|( adverb)* verb)"))) question-word))
				
					;; Otherwise, it is in the simple form of "Who runs the store?" or "What eats fish?" and can use the entire sentence after the question word
					;; as the predicate and nothing for the subject.
					(progn (print-debug "Using last question word form")
						(find-word-set (list "" (build-string-from-words (cdr words))) question-word))))))

(defun answer-boolean-adjective-question (words)
	"Answers a yes or no adjective question"
	(check-for-word-set (build-word-set (get-words-and-parts-of-speech words (concatenate 'string "^verb" *SUBJECT-REGEX* "( adverb)*(( adjective)+| preposition|(( article)? noun))")))))

(defun answer-boolean-verb-question (words)
	"Answers a yes or no verb question"
	(check-for-word-set (build-word-set (get-words-and-parts-of-speech words (concatenate 'string "^verb" *SUBJECT-REGEX* "( adverb)*( verb)+")))))

(defun build-word-set (words-and-parts-of-speech)
	"Gets the subject and predicate to look for in the sentence file."
	(print-debug words-and-parts-of-speech)
	(let ((subject "") (predicate "") (prepositional-phrases #()) (tense-adverbs '()) (found-verb-phrase nil) (subject-ended nil) (noun-found nil) (found-prepositional-phrase nil)
			(prepositional-phrase-ctr -1) (does-have-subject (has-subject words-and-parts-of-speech)))
		(if *DEBUG* (format t "~%Does have subject = ~A" does-have-subject))
		(dolist (word-and-part-of-speech words-and-parts-of-speech)
			(let ((word (car word-and-part-of-speech)) (part-of-speech (cadr word-and-part-of-speech)))
				(cond 

							;; Any tense adverbs go in their own array
							((string-in-list word *TENSE-ADVERBS*)
								(append-to-list tense-adverbs word))

							;; Ignore any verb question words or optional words while building as long as a verb phrase has been found.  They are implied.
							((and found-verb-phrase (or (string-in-list word *OPTIONAL-ADVERBS*) (string-in-list word *OPTIONAL-PARTICIPLES*) (string-in-list word *VERB-QUESTION-WORDS*)))
								nil)

							;; Look for prepositions and prepositional phrases first so they can be lopped off and placed at the end of the search
							((equal part-of-speech "preposition")
								(setf found-prepositional-phrase t)
								(increment prepositional-phrase-ctr)
								(setf prepositional-phrases (adjust-array prepositional-phrases (1+ prepositional-phrase-ctr)))
								(setf (aref prepositional-phrases prepositional-phrase-ctr) "")
								(append-to-string (aref prepositional-phrases prepositional-phrase-ctr) word))
							(found-prepositional-phrase (append-to-string (aref prepositional-phrases prepositional-phrase-ctr) word))

							;; If the verb phrase hasn't been found and we haven't reached a verb yet, keep looking.
							((and (not found-verb-phrase) (not (equal part-of-speech "adverb")) (not (equal part-of-speech "verb"))) 
								nil) 

							;; Found the verb.  Start the predicate.
							((and (not found-verb-phrase) (or (equal part-of-speech "verb") (equal part-of-speech "adverb"))) 
									(if *DEBUG* (format t "~%Verb phrase found = ~A" word))
									(append-to-string predicate word)
									(setf found-verb-phrase t))

							;; Check to see if the verb phrase has been found and the subject hasn't ended.  If the sentence has a subject,
							;; it starts here.
							((and found-verb-phrase (not subject-ended) does-have-subject)

									;; A noun has been found if the following is true.
									(if (or (equal part-of-speech "Noun") (equal part-of-speech "noun") (equal part-of-speech "pronoun"))
										(progn

											(if *DEBUG* (format t "~%A noun has been found = ~A" word))

											;; If a proper noun, there could be more nouns and articles to follow, i.e. Paul Howard Johnson of Richfield
											(if (equal part-of-speech "Noun")
												(append-to-string subject word)

												;; The only (or last) noun has been found
												(progn (setf subject-ended t)

													;; Another noun was found
													(if noun-found

														;; The subject alread ended before this word
														(append-to-string predicate word)

														;; This is the last word of the subject
														(append-to-string subject word))))
									
											;; We definitely found a noun
											(setf noun-found t))

										;; Not a noun or pronoun
										(if noun-found

											;; The subject has ended and this word is part of the predicate because a noun has been found
											(progn (append-to-string predicate word)
												(setf subject-ended t))

											;; No noun has been found yet, but the subject hasn't ended.  Append to the subject.
											(append-to-string subject word))))

							;; We must be in the predicate
							(t (append-to-string predicate word)))))

		;; Buid the search list
		(let ((search-list (list subject predicate)))
			(dotimes (i (length prepositional-phrases))
				(append-to-list search-list (aref prepositional-phrases i)))
			(dolist (tense-adverb tense-adverbs)
				(append-to-list search-list tense-adverb))
			search-list)))

(defmacro increment (variable-to-increment)
	"Increments a number"
	`(setf ,variable-to-increment (1+ ,variable-to-increment)))

(defmacro decrement (variable-to-decrement)
	"Decrements a number"
	`(setf ,variable-to-decrement (1- ,variable-to-decrement)))

(defun has-subject (words-and-parts-of-speech)
	"If the main question (without prepositional phrases) ends in a verb or adverb and is a doing question, the subject is in the sentence.  
	Otherwise we are looking for what the subject is. Example: \"Where does Paul go?\" versus \"Who does the laundry?\.
	Why questions always have a subject."
	(or (equal (string-downcase (caar words-and-parts-of-speech)) "why") 
			(not (string-in-list (caadr words-and-parts-of-speech) *VERB-QUESTION-WORDS*))
			(and (string-in-list (caadr words-and-parts-of-speech) *VERB-QUESTION-WORDS*) 
				(let ((last-part-of-speech (cadar (last (remove-prepositional-phrases words-and-parts-of-speech)))))
					(or (equal "verb" last-part-of-speech) (equal "adverb" last-part-of-speech))))))

(defun remove-prepositional-phrases (words-and-parts-of-speech)
	"Goes through until the first preposition is hit and removes this and the rest of the words from the list"
	(print-debug "In remove-prepositional-phrases")
	(let ((new-list '()))
		(dolist (word-and-part-of-speech words-and-parts-of-speech new-list)
			(if (equal (cadr word-and-part-of-speech) "preposition")
				(progn (print-debug new-list)
					(return new-list))
				(append-to-list new-list word-and-part-of-speech)))))

(defun print-debug (line)
	"Prints a debug line if in debug mode"
	(if *DEBUG* (print line)))

(defun param-debug (description param)
	"Prints a formatted line for one ~A parameter if in debug mode"
	(if *DEBUG* (format t (concatenate 'string "~%" description " = ~A") param)))

(defun get-words-and-parts-of-speech (words pattern)
	"Analyzes the words by getting all the parts of speech for each and building a list of all possible sentences.  
	It then tries to find the correct sentence structure using pattern matching. Once this is done, it builds a list of
	each word and part of speech."
	(print-debug pattern)
	(let ((parts-of-speech '()))
		(dolist (word words)
			(setf parts-of-speech (append parts-of-speech (list (get-parts-of-speech word)))))
		(print-debug parts-of-speech)

		;; Build a list for each potential sentence with every part of speech.
		
		;; The algorithm is as follows:

		;; 1) Each word has a boolean cycled variable stored in an array that is set to true when it cycles through all its parts of speech.
		;; 2) A second array contains the current position inside the part of speech list for each word.
		;; 3) If it is the first word or the previous word has cycled, increment the pointer used to build the sentence for that word.
		;; 4) Continue until the last word is cycled
		;; 5) If a matching sentence is found, break out of the loop
		(let ((parts-of-speech-length (list-length parts-of-speech)))
			(let ((i 0) (cycled (make-array (list-length parts-of-speech) :initial-element nil))
						(current-positions (make-array (list-length parts-of-speech) :initial-element 0)))

				;; The i parameter is there to insure no infinite loops; they shouldn't happen.
				(let ((chosen-sentence (loop until (or (= i 1000000) (aref cycled (1- parts-of-speech-length))) do

					(increment i)
					(print-debug cycled)
					(print-debug current-positions)
					(let ((current-sentence ""))
						(print-debug "Before dotimes")
						(dotimes (j (list-length parts-of-speech))
							(print-debug current-sentence)
							(let ((word-parts-of-speech (nth j parts-of-speech)))
								(append-to-string current-sentence (nth (aref current-positions j) word-parts-of-speech))
								(if (or (= j 0) (aref cycled (1- j)))
									(progn 
										(if (not (= j 0)) (setf (aref cycled (1- j)) nil))
										(if (= (aref current-positions j) (1- (list-length word-parts-of-speech)))
											(progn (setf (aref cycled j) t)
												(setf (aref current-positions j) 0))
											(setf (aref current-positions j) (1+ (aref current-positions j))))))))
						(print-debug "Finished current sentence")
						(print-debug current-sentence)
						(let ((match-list (multiple-value-list (cl-ppcre:scan-to-strings pattern current-sentence))))
							(print-debug match-list)
							(if (not (equal match-list '(nil)))
								(return current-sentence)))))))
							
					;; Now that we have the chosen sentence or have finished, break it into words and build a list of each sentence word and its part of speech
					(if chosen-sentence
						(progn (print-debug chosen-sentence)
							(let ((words-parts-of-speech (break-into-words chosen-sentence)))
								(let ((words-and-parts-of-speech
									(do ((i 0 (1+ i))
												(current-words-and-parts-of-speech '()))
											((= i (list-length words)) current-words-and-parts-of-speech)
											(append-to-list current-words-and-parts-of-speech (list (nth i words) (nth i words-parts-of-speech))))))
									words-and-parts-of-speech)))))))))

(defmacro append-to-list (list-to-append-to item)
	`(setf ,list-to-append-to (append ,list-to-append-to (list ,item))))

(defmacro append-to-string (string-to-append-to string-to-append)
	`(setf ,string-to-append-to (concatenate 'string ,string-to-append-to 
		(if (equal ,string-to-append-to "") "" " ") ,string-to-append)))

(defun string-in-list (test-string test-list)
	"Tests to see if string is in a list"
	(if (not test-list)
		nil
		(if (equal test-string (car test-list))
			t
			(string-in-list test-string (cdr test-list)))))

(defun skip-string (string-to-skip words)
	"Skips a string within a word list and returns a string of the words without it"
	(let ((new-words '()) (words-to-test (break-into-words string-to-skip)))
		(dolist (word words)
			(if (equal (car words-to-test) word)
				(pop words-to-test)
				(setf new-words (append new-words (list word)))))
		new-words))

(defun build-string-from-words (word-list)
	"Builds a string from a list of words"
  (if (= (list-length word-list) 1)
		(car word-list)
	  (if word-list
			(concatenate 'string (car word-list) " " (build-string-from-words (cdr word-list)))
			"")))

(defun break-into-words (sentence)
	"Breaks a sentence into a word list"
	(if (equal sentence "") 
		'()
		(if (or (is-question sentence) (is-statement sentence))
			(break-into-words (subseq sentence 0 (- (length sentence) 1)))
			(let ((y 0) (words '()))
				(setf sentence (concatenate 'string sentence " "))
				(dotimes (x (length sentence) words)
					(if (or (equal (char sentence x) #\space) (equal x (1- (length sentence))))
						(progn (setf words (append words (list (remove-commas (subseq sentence y x)))))
							(setf y (1+ x)))))))))

(defun remove-commas (word) 
	"Removes any commas and returns the word"
	(let ((new-word ""))
		(dotimes (i (length word))
			(let ((current-char (char word i)))
				(if (not (equal current-char #\,))
					(setf new-word (concatenate 'string new-word (string current-char))))))
		new-word))
                  
(defun write-to-file (name content mode)
	"Writes data to a file"
  (with-open-file (stream name :external-format charset:iso-8859-1
                           :direction :output
                           :if-exists mode
                           :if-does-not-exist :create)
    (format stream content)))

(defun forget (sentence)
	"Removes the sentence from the *SENTENCE-FILE*"
  (let ((new-file ""))
    (with-open-file (stream *SENTENCE-FILE* :direction :input)
      (do ((line (read-line stream nil) (read-line stream nil)))
        ((null line))
        (if (equal sentence line)
           (print "Found and forgotten")
           (setf new-file (concatenate 'string new-file (add-return line))))))
    (write-to-file *SENTENCE-FILE* new-file ':supersede))
  "Done")

(defun do-read-line-func (file func-ptr &optional all-false-func)
	"Does a function for every line read from a file"
  (let ((all-false t))
    (with-open-file (stream file :direction :input)
      (do ((line (read-line stream nil) (read-line stream nil)))
        ((null line))
        (if (funcall func-ptr line)
          (setf all-false nil))))
    (if (and all-false-func all-false)        
			(funcall all-false-func))))

(defun find-phrase (phrase)
	"Looks for a phrase in the *SENTENCE-FILE* and prints it if found"
  (do-read-line-func *SENTENCE-FILE*
    #'(lambda (line)
        (if (search (string-downcase phrase) (string-downcase line))
					(progn (print line)
          t))) 
    #'(lambda () (print-not-found-line phrase))))

(defun find-word-set (word-set question-word)
	"Finds a sentence that contains all the words in the word set, in order.  For example, in \"How far is the east tower from the west
	 tower?\", '(\"east tower\" \"from\" \"west tower\") could be passed in to find \"The east tower is 70 miles from the west tower.\""
	(print-debug word-set)
	(let ((full-word-set (get-full-word-set word-set)))
		(do-read-line-func *SENTENCE-FILE*
			#'(lambda (line)
					(if (word-set-in-line full-word-set line)
						(progn (print line)
							t)))

			;; If there is no subject, the phrase used in the not found phrase is the question word plus the predicate
			#'(lambda () (print-not-found-line 
				(if (equal (car word-set) "") 
					(concatenate 'string (string-downcase question-word) " " (cadr word-set))
					(car word-set)))))))

(defun check-for-word-set (word-set)
	"Checks for a word set and prints yes if found"
	(print-debug word-set)
  (with-open-file (stream *SENTENCE-FILE* :direction :input)
		(let ((full-word-set (get-full-word-set word-set)))
			(do ((line (read-line stream nil) (read-line stream nil)))
					((null line) "I can't find any information about that.")
				(if (word-set-in-line full-word-set line)
					(if (or (search " no " line) (search " not " line))
						(return "No.")
						(return "Yes.")))))))

(defun get-full-word-set (word-set)
	"Gets the full word set: word-set, the tense transform of the word set and the reverse tense transform of the word set"
	(let ((full-word-set '()))
		(dolist (word-group word-set)
			(append-to-list full-word-set (list word-group (tense-transform word-group) (reverse-tense-transform word-group))))
		full-word-set))

(defun word-set-in-line (word-set line)
	"Returns true if all the words in word-set are in line"
	(if (not (equal (car word-set) '("" "")))
		(let ((word-set-length (list-length word-set)) (downcase-line (string-downcase line)))
			(if (= word-set-length 0) 
				nil
				(let ((match-list (multiple-value-list 
						(cl-ppcre:scan (concatenate 'string (string-downcase (caar word-set)) "(\\.|\\s)") downcase-line))))
					(if (car match-list)
						(if (= word-set-length 1)
							t
							(word-set-in-line (cdr word-set) line))
						(let ((transformed-match-list (multiple-value-list
								(cl-ppcre:scan (concatenate 'string (string-downcase (cadar word-set)) "(\\.|\\s)") downcase-line))))
							(if (car transformed-match-list)
								(if (= word-set-length 1)
									t
									(word-set-in-line (cdr word-set) line))
								(let ((reverse-transformed-match-list (multiple-value-list
										(cl-ppcre:scan (concatenate 'string (string-downcase (caddar word-set)) "(\\.|\\s)") downcase-line))))
									(if (car reverse-transformed-match-list)
										(if (= word-set-length 1)	
											t
											(word-set-in-line (cdr word-set) line))))))))))))

(defun tense-transform (word-group)
	"Transforms a group of words into the same set with a verb phrase replaced by a single verb, i.e. 'did run' => replaced by 'ran'"
	(let ((new-words "") (old-words (break-into-words word-group)))
		(dotimes (i (list-length old-words))
			(let ((current-word (string-downcase (nth i old-words))) (next-word (string-downcase (nth (1+ i) old-words))))
				(if (string-in-list current-word *VERB-QUESTION-WORDS*)
					(let ((transform (get-tense-transform (concatenate 'string current-word " " next-word))))
						(if transform
							(progn (setf i (1+ i))
								(append-to-string new-words (car transform)))
							(if (equal current-word "do")
								nil
								(if (equal current-word "does")
									(progn (setf i (1+ i))
										(append-to-string new-words (concatenate 'string next-word "s")))
									(if (equal current-word "did")
										(progn (setf i (1+ i))
											(if (equal (subseq next-word (1- (length next-word))) "e")
												(append-to-string new-words (concatenate 'string next-word "d"))
												(append-to-string new-words (concatenate 'string next-word "ed"))))
										(append-to-string new-words current-word))))))
					(append-to-string new-words current-word))))
		new-words))

(defun reverse-tense-transform (word-group)
	"Does the reverse operation of a tense transform, i.e. 'ran' => 'did run'"
	(let ((new-words "") (old-words (break-into-words word-group)))
		(dolist (word old-words)
			(let ((reverse-transform (get-reverse-tense-transform word)))
				(if reverse-transform
					(append-to-string new-words (car reverse-transform))
					(append-to-string new-words word))))
		new-words))

(defun print-not-found-line (phrase)
	"Prints an informational sentence about the fact that nothing was found"
	(format nil "I couldn't find the information you requested about ~A. Can you tell me anything?" phrase))
                        
(defun record (sentence)
	"Puts the appropriate information from the sentence in the appropriate file"
	(write-to-file *SENTENCE-FILE* (add-return (concatenate 'string (build-string-from-words (reverse-personal-references (break-into-words sentence))) ".")) 
		':append)
	"Thanks for the information.")

(defun add-return (line)
	"Adds a newline"
  (concatenate 'string line (string #\return)))

(defun list-file (file-name)
	"Lists a file"
  (do-read-line-func file-name #'print)
  "Done")

(defun get-web-parts-of-speech (word)
	"Returns all parts of speech in a list"
	(let ((http-response (http-get "www.yourdictionary.com" 80 (concatenate 'string "/" word)))
				(parts-of-speech '()))
		(cl-ppcre:do-scans (match-start match-end regex-starts regex-ends "<span class=\"pos\">([\\w|\\s|\\w]+)</span>" http-response)
		(setf parts-of-speech (append parts-of-speech (list (subseq http-response (aref regex-starts 0) (aref regex-ends 0))))))
		(if (equal parts-of-speech nil)

			;; Test the word without the 's' or 'es' if it has one
			(let ((match-values (multiple-value-list (cl-ppcre:scan-to-strings "((\\w+)es$)|((\\w+)s$)" word))))
				(if (equal match-values '(nil))
					(prompt-user-for-part-of-speech word)
					
					;; The cdr contains all the submatches.  The second or fourth argument would have the match.
					(let ((submatches (cadr match-values)))
						(if (equal (aref submatches 0) nil)
				
							;; Matched the 's'
							(get-parts-of-speech (aref submatches 3))

							;; Matched the 'es'
							(get-parts-of-speech (aref submatches 1))))))
			(remove-extra-words parts-of-speech))))

(defun prompt-user-for-part-of-speech (word)
	"Prompts the user for the part of speech if nothing can be found in the dictionary.  If enter is pressed than '(\"npos\") is stored."
	(format t "I couldn't find a part of speech for ~A. Could you tell me what it is? If not, press <Enter>:" word)
	(let ((pos (read-line)))
		(if (equal pos "")
			'("npos")
			(list pos))))

(defun is-a-question-word (word)
	"Returns true if in any of the question word lists"
	(let ((lowercase-word (string-downcase word)))
		(or (string-in-list lowercase-word *QUESTION-WORDS*) (string-in-list lowercase-word *VERB-QUESTION-WORDS*) 
			(string-in-list lowercase-word *ADJECTIVE-QUESTION-WORDS*))))
 
(defun remove-extra-words (list-of-words)
	"Removes any initial words from a list of words"
	(dotimes (i (list-length list-of-words) list-of-words)
		(let ((position-of-space (position #\space (nth i list-of-words) :from-end t)))
			(if position-of-space
				(setf (nth i list-of-words) (subseq (nth i list-of-words) (1+ position-of-space)))))))
